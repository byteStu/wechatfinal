%%%-------------------------------------------------------------------
%%% @author ubuntu
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. 八月 2018 8:08 PM
%%%-------------------------------------------------------------------
-module(wechatfinal_broadcast_wk).
-include("wechatfinal_cmd_pb.hrl").
-author("ubuntu").

-behaviour(gen_server).

%% API
-export([start_link/0,send_online_msg/0,send_room_msg/0,send_online_user_to_room/1,send_offline_user_to_room/1,send_exit_group_user_to_room/2]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
%% @doc 推送在线用户
-spec send_online_msg() -> term().
send_online_msg() -> gen_server:cast(?SERVER,{send_online_msg}),ok.

%% @doc 推送房间信息
-spec send_room_msg() -> term().
send_room_msg() -> gen_server:cast(?SERVER,{send_room_msg}).

%% @doc 推送刚上线的用户给各个房间
-spec send_online_user_to_room(UserNameAtom::atom()) -> term().
send_online_user_to_room(UserNameAtom) ->
	%% 查询所有房间
	RoomList = wechatfinal_room_wk:get_room_list(),
	[X ! {advice_room_of_online_user,X,UserNameAtom}||X<-RoomList].

%% @doc 推送下线用户到各个房间
-spec send_offline_user_to_room(UserNameAtom::term()) -> term().
send_offline_user_to_room(UserNameAtom) ->
	%% 查询所有房间
	RoomList = wechatfinal_room_wk:get_room_list(),
	[X ! {advice_room_of_offline_user,X,UserNameAtom}||X<-RoomList].

%% @doc 通知群里，有人退群了
-spec send_exit_group_user_to_room(RoomNameAtom::atom(),UserNameAtom::atom()) -> term().
send_exit_group_user_to_room(RoomNameAtom,UserNameAtom) ->
	RoomNameAtom ! {advice_client_room,{RoomNameAtom,UserNameAtom,public}}.
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
init([]) ->
	{ok, #state{}}.

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

%% @doc 推送在线消息
handle_cast({send_online_msg}, State) ->
	%% 获得在线用户列表
	UserList = wechatfinal_player_wk:get_online_player(),
	NewUserList = [wechatfinal_util:get_ws_name(X)||X <- UserList],
	UserListBinary = wechatfinal_util:atomList_join_to_binary(NewUserList),
	Msg = #msg{type = online,data = #data{online = #online{type = private,body = UserListBinary}}},
	MsgEncode = wechatfinal_cmd_pb:encode_msg(Msg),
	[X ! {send_online_msg,MsgEncode} ||X <- UserList],
	{noreply, State};

%% @doc 推送房间消息
handle_cast({send_room_msg}, State) ->
	%% 获得在线用户列表
	UserList = wechatfinal_player_wk:get_online_player(),
	%% 处理消息推送
	do_send_room_msg(UserList),
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

%% @doc 处理房间消息推送
-spec do_send_room_msg(Parm::list()) ->term().
do_send_room_msg([]) ->
	ok;
do_send_room_msg([UserAtom|T]) ->
	NewUserAtom = wechatfinal_util:get_ws_name(UserAtom),
	RoomList = wechatfinal_mnesia:q_all_groups_by_uname(NewUserAtom),
	io:format("~p的群列表为：~p~n",[NewUserAtom,RoomList]),
	RoomListBinary = wechatfinal_util:atomList_join_to_binary(RoomList),
	Msg = #msg{type = online,data = #data{online = #online{type = public,body = RoomListBinary}}},
	MsgEncode = wechatfinal_cmd_pb:encode_msg(Msg),
	UserAtom !  {send_online_msg,MsgEncode},
	do_send_room_msg(T).