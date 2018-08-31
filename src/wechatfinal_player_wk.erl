%%%-------------------------------------------------------------------
%%% @author ubuntu
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. 八月 2018 9:02 PM
%%%-------------------------------------------------------------------
-module(wechatfinal_player_wk).
-include("wechatfinal_cmd_pb.hrl").
-author("ubuntu").

-behaviour(gen_server).

%% API
-export([start_link/1,
	send_msg/2,
	sign_in/1,
	sign_out/1]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3,
	get_online_player/0]).

-define(SERVER, ?MODULE).
-record(state, {self::atom()}).

%%%===================================================================
%%% API
%%%===================================================================
%% @doc 注册一个玩家进程
-spec sign_in(Player::atom()) -> atom().
sign_in(Player) when is_atom(Player)->
	wechatfinal_player_sup:start_child(Player).

%% @doc 关闭一个玩家进程
-spec sign_out(Player::atom()) -> atom().
sign_out(Player) when is_atom(Player) ->
	%% 下线wk进程
	io:format("[~p]正在注销。。。",[Player]),
	WkPlayer = wechatfinal_util:get_wk_name(Player),
	wechatfinal_player_sup:stop_child(WkPlayer),
	io:format("~p 已下线～~n",[Player]),
	%% 下线ws进程
	case whereis(Player) =/= undefined of
		 true -> Player ! {stop_conn};
		 false -> ok
	end,
	%% 广播一下 通知各个群
	wechatfinal_broadcast_wk:send_offline_user_to_room(Player).
	

%% @doc 异步发送消息
-spec send_msg(Sender::atom(),Decoder::term()) -> term().
send_msg(Sender,Decoder) ->
	gen_server:cast(Sender,{send_msg,Decoder}).


%% @doc 查看玩家在线列表
-spec get_online_player() -> list().
get_online_player() -> wechatfinal_player_sup:get_child_name().

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
%%-spec(start_link(Player::atom()) ->
%%	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Player) ->
	gen_server:start_link({local, Player}, ?MODULE, [Player], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
	{ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term()} | ignore).
init([Self|_]) ->
	erlang:send_after(30000,self(),{check_ws_alive}),
	{ok, #state{self  = Self}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
	State :: #state{}) ->
	{reply, Reply :: term(), NewState :: #state{}} |
	{reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
	{stop, Reason :: term(), NewState :: #state{}}).

%% @doc 登出用户
handle_call({sign_out_player}, _From, State) ->
	{stop, normal, State};

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #state{}}).
%% @doc 发送消息
handle_cast({send_msg,Decoder}, State) ->
	Sender = Decoder#msg.data#data.chat#chat.sender,
	Body = Decoder#msg.data#data.chat#chat.body,
	Receiver = Decoder#msg.data#data.chat#chat.receiver,
	Type = Decoder#msg.data#data.chat#chat.type,
	Timer = Decoder#msg.data#data.chat#chat.timer,
	
	io:format("进来了send_msg"),
	
	%% 把消息转发出去
	ReceiverAtom = binary_to_atom(Receiver,utf8),
	EncodeMsg = wechatfinal_cmd_pb:encode_msg(Decoder),
	ReceiverAtom ! {resp_msg,EncodeMsg},
	
	Online = case Type of
				private -> case whereis(ReceiverAtom) of
					           undefined ->  [];
					           _ ->  [ReceiverAtom]
						   end;
				public -> wechatfinal_room_wk:get_room_online_users(ReceiverAtom)
			 end,
	io:format("Online:~p~n",[Online]),
	%% 把消息存起来
	wechatfinal_mnesia:add_msgs(Sender,Body,Receiver,Type,Timer,Online),
	
	{noreply, State};

handle_cast(_Request, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #state{}}).

%% @doc 推送在线消息
handle_info({send_online_msg,MsgEncode}, #state{self = Self}=State) ->
	WsName = wechatfinal_util:get_ws_name(Self),
	io:format("进到player info方法,WsName:~p~n",[WsName]),
	WsName ! {resp_msg,MsgEncode},
	{noreply, State};

handle_info({check_ws_alive}, State) ->
	{_,WkName} = process_info(self(),registered_name),
	WsName = wechatfinal_util:get_ws_name(WkName),
	case whereis(WsName) of
		undefined -> wechatfinal_player_wk:sign_out(WkName);
		_ -> erlang:send_after(30000,self(),{check_ws_alive})
	end,
	io:format("正在检查websocket进程...~n"),
	{noreply, State};

handle_info(_Info, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
	State :: #state{}) -> term()).
terminate(_Reason, _State) ->
	ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
	Extra :: term()) ->
	{ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
