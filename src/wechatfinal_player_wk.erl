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
	get_online_player/0,
	load_private_chat_history/1]).

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
-spec sign_out(WkPlayer::atom()) -> atom().
sign_out(WkPlayer) when is_atom(WkPlayer) ->
	
	io:format("[~p]正在注销。。。~n",[WkPlayer]),
	WsPlayer = wechatfinal_util:get_ws_name(WkPlayer),
	%% 下线ws进程
	case whereis(WsPlayer) =/= undefined of
	true -> WsPlayer ! {stop_conn};
	false -> ok
	end,
	
	%% 下线wk进程
	wechatfinal_player_sup:stop_child(WkPlayer),
	%% 广播一下 通知各个群
	wechatfinal_broadcast_wk:send_offline_user_to_room(WsPlayer),
	wechatfinal_broadcast_wk:send_online_msg(),
	io:format("~p 已下线～~n",[WkPlayer]).
	

%% @doc 异步发送消息
-spec send_msg(Sender::atom(),Decoder::term()) -> term().
send_msg(Sender,Decoder) ->
	gen_server:cast(Sender,{send_msg,Decoder}).

%% @doc 加载私人聊天记录
-spec load_private_chat_history(Decode::term()) -> term().
load_private_chat_history(Decode) ->
	Sender = Decode#msg.data#data.history#history.sender,
	SenderAtom = binary_to_atom(Sender,utf8),
	Receiver = Decode#msg.data#data.history#history.receiver,
	PageNum = Decode#msg.data#data.history#history.pageNum,
	MsgList = wechatfinal_mnesia:q_msg_page2(Sender,Receiver,5,PageNum),
	NewMsgList = [#chat{sender = S,body = B,receiver = R,display = Receiver,unread = atom_to_binary(lists:member(SenderAtom,O),utf8)}||{S,B,R,_Ty,_Ti,O} <- MsgList],
	Msg = #msg{type = history,data = #data{history = #history{sender = Sender,receiver = Receiver,history = NewMsgList}}},
	EncodeMsg = wechatfinal_cmd_pb:encode_msg(Msg),
	SenderAtom ! {resp_msg,EncodeMsg},
	[wechatfinal_mnesia:update_msg(S,B,R,Ty,Ti,O,SenderAtom)||{S,B,R,Ty,Ti,O} <- MsgList].

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
	
	ReceiverAtom = binary_to_atom(Receiver,utf8),
	EncodeMsg = wechatfinal_cmd_pb:encode_msg(Decoder),
	%% 如果用户在线，就把消息发出去
	case whereis(ReceiverAtom) =/= undefined of
		true  ->ReceiverAtom ! {resp_msg,EncodeMsg};
		false ->ok
	end,
	Online = case Type of
				private -> case whereis(ReceiverAtom) of
					           undefined ->  [];
					           _ ->  [ReceiverAtom]
						   end;
				public -> wechatfinal_room_wk:get_room_online_users(ReceiverAtom)
			 end,
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
	WsName ! {resp_msg,MsgEncode},
	{noreply, State};

handle_info({check_ws_alive}, State) ->
	{_,WkName} = process_info(self(),registered_name),
	WsName = wechatfinal_util:get_ws_name(WkName),
	io:format("正在检查websocket进程[~p]是否存活...~n",[WsName]),
	case whereis(WsName) of
	undefined -> wechatfinal_player_wk:sign_out(WkName);
	_ -> erlang:send_after(30000,self(),{check_ws_alive})
	end,
	{noreply, State};

handle_info({stop_player_wk}, State) ->
	{stop,normal, State};
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
