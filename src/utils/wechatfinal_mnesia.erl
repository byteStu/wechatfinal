%%%-------------------------------------------------------------------
%%% @author ubuntu
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. 八月 2018 6:42 PM
%%%-------------------------------------------------------------------
-module(wechatfinal_mnesia).
-include_lib("stdlib/include/qlc.hrl").
-author("ubuntu").

%% API
-export([start/0,stop/0,create_table/0,init_tables/0,
         q_users/1,
         create_schema/0,
         delete_table/1,
         q_all_users/0,
         q_users_password/1,
         add_msgs/5,
         create_groups_by_name/2,
         create_groups_by_record/1,
         drop_groups/1,
         add_groups/2,
         remove_groups/2,
         q_groups/1,
         init_mnesia/0,
         q_all_groups/0,
         q_msg_by_gname/1,
         q_all_groups_by_uname/1
         ]).

-record(users,{uname,pwd}).
-record(groups,{gname,uname}).
-record(msgs,{sender,body,receiver,msgtype,sendtimer}).

start() -> mnesia:start(),mnesia:wait_for_tables([users,groups,msgs],1000),ok.

stop() -> mnesia:stop().

create_schema() ->  mnesia:create_schema([node()]).

create_table() ->
    mnesia:create_table(users, [{attributes,record_info(fields,users)},{disc_copies,[node()]},{type,set}]),
    mnesia:create_table(groups, [{attributes,record_info(fields,groups)},{disc_copies,[node()]},{type,set}]),
    mnesia:create_table(msgs, [{attributes,record_info(fields,msgs)},{disc_copies,[node()]},{type,bag}]).

init_mnesia() ->
    create_schema(),
    start(),
    create_table().
    

delete_table(Table) -> mnesia:delete_table(Table).


%% 初始化表
init_tables() ->
    mnesia:clear_table(users),
    F = fun() ->
        lists:foreach(fun mnesia:write/1, example_tables())
        end,
    mnesia:transaction(F).

%% @doc 用户表相关信息
q_users(UserName) ->
    do(qlc:q([X || X <- mnesia:table(users),X#users.uname =:= UserName])).
q_users_password(UserName) ->
    do(qlc:q([X#users.pwd || X <- mnesia:table(users),X#users.uname =:= UserName])).
q_all_users() ->
    do(qlc:q([X || X <- mnesia:table(users)])).

%% @doc 群表相关
%% -record(groups,{gname,uname}).

%% @doc 新建一个群通过名字
-spec create_groups_by_name(Gname::term(),UserName::atom()) -> term().
create_groups_by_name(Gname,UserName) ->
    Row = #groups{gname = Gname,uname = [UserName] },
    F = fun() -> mnesia:write(Row) end,
    mnesia:transaction(F).

%% @doc 新建一个群通过record
-spec create_groups_by_record(Group::term()) -> term().
create_groups_by_record(Group) ->
    F = fun() -> mnesia:write(Group) end,
    mnesia:transaction(F).

%% @doc 删除一个群
-spec drop_groups(Gname::term()) -> term().
drop_groups(Gname) ->
    Oid = {groups,Gname},
    F = fun() -> mnesia:delete(Oid) end,
    mnesia:transaction(F).

%% @doc 添加一个用户
-spec add_groups(Gname::atom(),Uname::atom()) -> term().
add_groups(Gname,Uname) ->
    %% 将群查出来
    Group = q_groups(Gname),
    Unames = Group#groups.uname,
    NewUnames1 = Unames -- [Uname],
    NewUnames2 = NewUnames1 ++ [Uname],
    NewGroup = Group#groups{uname = NewUnames2},
    %% 删除群
    %%drop_groups(Gname),
    %% 添加群
    create_groups_by_record(NewGroup).

%% @doc 移除一个用户
-spec remove_groups(Gname::atom(),Uname::atom()) -> term().
remove_groups(Gname,Uname) ->
    %% 将群查出来
    Group = q_groups(Gname),
    Unames = Group#groups.uname,
    NewUnames = Unames -- [Uname],
    NewGroup = Group#groups{uname = NewUnames},
    %% 删除群
    %%drop_groups(Gname),
    %% 添加群
    create_groups_by_record(NewGroup).

%% @doc 查询群
-spec q_groups(Gname::term()) ->term().
q_groups(Gname) ->
    [H|_] = do(qlc:q([X|| #groups{gname = G} = X <- mnesia:table(groups),G =:= Gname])),
    H.

%% @doc 查询所有群的名字
-spec q_all_groups() -> list().
q_all_groups() ->
    do(qlc:q([ G || #groups{gname = G} <- mnesia:table(groups)])).

%% @doc 根据用户查询群列表
-spec q_all_groups_by_uname(Uname::atom()) -> list().
q_all_groups_by_uname(Uname) ->
    do(qlc:q([ G || #groups{gname = G,uname = U} <- mnesia:table(groups),lists:member(Uname,U)])).

%% -record(msgs,{sender,body,receiver,msgtype,sendtimer}).
%% @doc 消息表相关

%% @doc 添加一行消息记录
-spec add_msgs(Sender::term(),Body::term(),Receiver::term(),MsgType::term(),Sendtimer::term()) -> term().
add_msgs(Sender,Body,Receiver,MsgType,Sendtimer) ->
    Row = #msgs{sender = Sender,body = Body,receiver = Receiver,msgtype = MsgType,sendtimer = Sendtimer},
    F = fun() -> mnesia:write(Row) end,
    mnesia:transaction(F).

%% 查询聊天记录
-spec q_msg_by_gname(RoomNameAtom::atom()) -> term().
q_msg_by_gname(RoomName) ->
    QList = do(qlc:q([ {S,B,T} || #msgs{sender = S,body = B,receiver = R,sendtimer = T} <- mnesia:table(msgs),R =:= RoomName])),
    lists:keysort(3,QList).
    

example_tables() ->
    [
        %% The user table
        {users,<<"def">>,<<"root">>},
        {users,<<"root">>,<<"root">>},
        {users,<<"abc">>,<<"root">>},
        {users,<<"zs">>,<<"root">>},
        {users,<<"ls">>,<<"root">>},
        {users,<<"ww">>,<<"root">>}
    ].

do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.
