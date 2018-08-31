%%%-------------------------------------------------------------------
%%% @author ubuntu
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. 八月 2018 8:46 PM
%%%-------------------------------------------------------------------
-module(wechatfinal_room_wk).
-include("wechatfinal_cmd_pb.hrl").
-author("ubuntu").

-behaviour(gen_server).

%% API
-export([start_link/1,handle_room/2,get_room_list/0,call/2,cast/2,get_room_online_users/1]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
	]
	).

-define(SERVER, ?MODULE).

-record(state, {userList}). %% 存放的是 ws进程的名字

%%%===================================================================
%%% API
%%%===================================================================
%% @doc 处理房间相关的请求
-spec handle_room(Sender::atom(),Decoder::term()) -> term().
handle_room(Sender,Decoder) ->
	case Decoder#msg.data#data.room#room.type of
		 create -> create_room(Decoder);
		 add ->  add_user_to_room(Decoder);
		 remove -> remove_user_to_room(Decoder);
		 drop -> drop_room(Decoder);
		 flush -> flush_room_member(Decoder);
		 history -> load_room_chat_history(Sender,Decoder)
	end.
	

%% @doc 创建房间
-spec create_room(Decoder::term()) -> term().
create_room(Decoder) ->
	RoomName = Decoder#msg.data#data.room#room.rname,
	RoomNameAtom = binary_to_atom(RoomName,utf8),
	UserName = Decoder#msg.data#data.room#room.body,
	UserNameAtom = binary_to_atom(UserName,utf8),
	io:format("创建房间,新建房间名：~p~n",[RoomNameAtom]),
	%% 开启房间
	wechatfinal_room_sup:start_child(RoomNameAtom),
	erlang:send_after(1000,RoomNameAtom,{add_creator,UserNameAtom}),
	%% 将群保存起来
	wechatfinal_mnesia:create_groups_by_name(RoomNameAtom,UserNameAtom).

%% @doc 添加用户
-spec add_user_to_room(Decoder::term()) -> term().
add_user_to_room(Decoder) ->
	RoomName = Decoder#msg.data#data.room#room.rname,
	RoomNameAtom = binary_to_atom(RoomName,utf8),
	User = Decoder#msg.data#data.room#room.body,
	UserAtom = binary_to_atom(User,utf8),
	call(RoomNameAtom,{add_user,RoomNameAtom,UserAtom}).
	

%% @doc 移除用户
-spec remove_user_to_room(Decoder::term()) -> term().
remove_user_to_room(Decoder) ->
	RoomName = Decoder#msg.data#data.room#room.rname,
	RoomNameAtom = binary_to_atom(RoomName,utf8),
	User = Decoder#msg.data#data.room#room.body,
	UserAtom = binary_to_atom(User,utf8),
	call(RoomNameAtom,{remove_user,RoomNameAtom,UserAtom}).
	
	
%% @doc 销毁房间
-spec drop_room(Decoder::term()) -> term().
drop_room(Decoder) ->
	RoomName = Decoder#msg.data#data.room#room.body,
	RoomNameAtom = binary_to_atom(RoomName,utf8),
	wechatfinal_room_sup:stop_child(RoomNameAtom),
	call(RoomNameAtom,{drop_room}).

%% @doc 刷新房间成员
-spec flush_room_member(Decoder::term()) -> term().
flush_room_member(Decoder) ->
	RoomName = Decoder#msg.data#data.room#room.rname,
	RoomNameAtom = binary_to_atom(RoomName,utf8),
	gen_server:cast(RoomNameAtom,{flush_room_member,RoomNameAtom}).

%% @doc 加载聊天记录
-spec load_room_chat_history(Sender::atom(),Decoder::term()) -> term().
load_room_chat_history(Sender,Decoder) ->
	RoomName = Decoder#msg.data#data.room#room.rname,
	%%RoomNameAtom = binary_to_atom(RoomName,utf8),
	MsgList = wechatfinal_mnesia:q_msg_by_gname(RoomName),
	NewMsgList = [#chat{sender = S,body = B,receiver = RoomName,display = RoomName,unread = atom_to_binary(lists:member(Sender,O),utf8)}||{S,B,_R,_Ty,_Ti,O} <- MsgList],
	Msg = #msg{type = room,data = #data{room = #room{type = history,rname = RoomName,history = NewMsgList}}},
	EncodeMsg = wechatfinal_cmd_pb:encode_msg(Msg),
	Sender ! {resp_msg,EncodeMsg},
	[wechatfinal_mnesia:update_msg(S,B,R,Ty,Ti,O,Sender)||{S,B,R,Ty,Ti,O} <- MsgList].

%% @doc 获得所有房间列表
-spec get_room_list() -> list().
get_room_list() ->
	wechatfinal_room_sup:get_child_name().

%% @doc 获得房间所有在线成员
-spec get_room_online_users(GnameAtom::atom()) -> list().
get_room_online_users(GnameAtom) ->
	call(GnameAtom,{get_room_online_users}).

%% @doc gen_server的封装
-spec call(Pid::pid()|atom(),Msg::term()) -> term().
call(Pid,Msg) ->
	gen_server:call(Pid,Msg).

%% @doc gen_server的封装
-spec cast(Pid::pid()|atom(),Msg::term()) -> term().
cast(Pid,Msg) ->
	gen_server:cast(Pid,Msg).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Room::atom()) ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Room) ->
	gen_server:start_link({local, Room}, ?MODULE, [], []).

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
	%% 启动进程时检测群成员
	erlang:send_after(1000,self(),{check_room_of_all_online_user}),
	{ok, #state{userList = []}}.

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

%% @doc 移除用户
handle_call({remove_user,RoomNameAtom,UserAtom}, _From, #state{userList = UserList}=State) ->
	%% 删除用户
	wechatfinal_mnesia:remove_groups(RoomNameAtom,UserAtom),
	%% 更新State
	NewState = State#state{userList = UserList -- [UserAtom]},
	%% 广播一下
	wechatfinal_broadcast_wk:send_room_msg(),
	%% 系统消息，通知一下
	wechatfinal_broadcast_wk:send_exit_group_user_to_room(RoomNameAtom,UserAtom),
	%% 刷新群成员
	erlang:send_after(1000,RoomNameAtom,{flush_room_member,RoomNameAtom}),
	{reply, ok, NewState};

%% @doc 添加用户
handle_call({add_user,RoomNameAtom,UserAtom}, _From, #state{userList = UserList}=State) ->
	%% 将 user 存起来
	wechatfinal_mnesia:add_groups(RoomNameAtom,UserAtom),
	%% 更新 State
	NewUserList1 = UserList -- [UserAtom],
	NewUserList2 = NewUserList1 ++ [UserAtom],
	NewState = State#state{userList = NewUserList2},
	%% 广播一下
	wechatfinal_broadcast_wk:send_room_msg(),
	{reply, ok, NewState};

%% @doc 解散本群
handle_call({drop_room}, _From, State) ->
	{stop, normal, State};

%% @doc 获得房间所有的在线成员
handle_call({get_room_online_users}, _From, #state{userList = UserList}=State) ->
	io:format("~n房间在线成员:~p~n",[UserList]),
	{reply, UserList, State};

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

%% @doc 刷新群成员
handle_cast({flush_room_member,RoomNameAtom}, #state{userList = UserList}=State) ->
	Msg = #msg{type = room,data = #data{room = #room{type = flush,rname = atom_to_binary(RoomNameAtom,utf8),body = wechatfinal_util:atomList_join_to_binary(UserList)}}},
	EncodeMsg = wechatfinal_cmd_pb:encode_msg(Msg),
	[X ! {resp_msg,EncodeMsg}||X <- UserList],
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

%% @doc 发送消息
handle_info({resp_msg,EncodeMsg}, #state{userList = UserList}=State) ->
	Decoder = wechatfinal_cmd_pb:decode_msg(EncodeMsg,msg),
	Sender = binary_to_atom(Decoder#msg.data#data.chat#chat.sender,utf8),
	[X ! {resp_msg,EncodeMsg} || X <- UserList,whereis(X) =/= undefined,X =/= Sender],
	{noreply, State};

%% @doc 群通知，用户上线
handle_info({advice_room_of_online_user,RoomAtom,UserNameAtom}, #state{userList = UserList}=State) ->
	{_,_,Users} = wechatfinal_mnesia:q_groups(RoomAtom),
	case lists:member(UserNameAtom,Users) of
		 true ->UserList -- [UserNameAtom],
			    NewUserList = UserList ++ [UserNameAtom],
			    NewState = State#state{userList = NewUserList};
		 false -> NewState = State
	end,
	{noreply, NewState};

%% @doc 群通知，用户下线
handle_info({advice_room_of_offline_user,RoomAtom,UserNameAtom}, #state{userList = UserList}=State) ->
	NewUserList = UserList -- [UserNameAtom],
	NewState = State#state{userList = NewUserList},
	io:format("新的群成员：~p,下线的成员:~p,房间是：~p~n",[NewUserList,UserNameAtom,RoomAtom]),
	erlang:send_after(1000,self(),{advice_client_room,{RoomAtom,UserNameAtom,private}}),
	{noreply, NewState};

%% @doc 当房间进程因为意外情况挂掉后，检测在线用户
handle_info({check_room_of_all_online_user},State) ->
	{_,RoomAtom} = process_info(self(),registered_name),
	{_,_,Users} = wechatfinal_mnesia:q_groups(RoomAtom),
	OnlineUser = [X|| X <- Users,whereis(X) =/= undefined],
	NewState = State#state{userList = OnlineUser},
	{noreply, NewState};

%% @doc 推送房间信息给客户端
handle_info({advice_client_room,{RoomAtom,UserNameAtom,Type}}, State) ->
	%% 刷新该群的群列表
	cast(RoomAtom,{flush_room_member,RoomAtom}),
	%% 推送系统消息: 某某下线了
	Msg = #msg{type = system,data = #data{system = #system{type = Type,receiver = atom_to_binary(RoomAtom,utf8),body = atom_to_binary(UserNameAtom,utf8)}}},
	EncodeMsg = wechatfinal_cmd_pb:encode_msg(Msg),
	RoomAtom ! {resp_msg,EncodeMsg},
	{noreply, State};

%% @doc 创建房间时，添加创建者
handle_info({add_creator,UserNameAtom}, #state{userList = UserList}=State) ->
	NewState = State#state{userList = [UserNameAtom|UserList]},
	erlang:send_after(1000,self(),{add_creator_after}),
	{noreply, NewState};

%% @doc 添加创建者后，后续通知
handle_info({add_creator_after}, State) ->
	%% 广播一下
	wechatfinal_broadcast_wk:send_room_msg(),
	{noreply, State};

%% @doc 刷新群成员
handle_info({flush_room_member,RoomNameAtom}, State) ->
	cast(RoomNameAtom,{flush_room_member,RoomNameAtom}),
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
