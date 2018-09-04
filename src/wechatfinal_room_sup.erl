%%%-------------------------------------------------------------------
%%% @author ubuntu
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. 八月 2018 9:26 PM
%%%-------------------------------------------------------------------
-module(wechatfinal_room_sup).
-author("ubuntu").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1,start_child/1,stop_child/1,get_child_name/0]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc 开房
-spec start_child(RoomNameAtom::atom()) -> term().
start_child(RoomNameAtom) ->
	ChildSpec = {RoomNameAtom,{wechatfinal_room_wk,start_link,[RoomNameAtom]},permanent,brutal_kill,worker,[wechatfinal_room_wk]},
	supervisor:start_child(?SERVER,ChildSpec).

%% @doc 关房
-spec stop_child(RoomNameAtom::atom()) ->term().
stop_child(RoomNameAtom) ->
	Pid = whereis(RoomNameAtom),
	supervisor:terminate_child(?SERVER,Pid),
	supervisor:delete_child(?SERVER,Pid).

%% @doc 返回子节点名字列表
-spec get_child_name() -> list().
get_child_name() ->
	Room1 = supervisor:which_children(?SERVER),
	[RoomName ||{RoomName,_,_,_} <- Room1].
%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
	{ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
		MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
		[ChildSpec :: supervisor:child_spec()]
	}} |
	ignore |
	{error, Reason :: term()}).
init([]) ->
	RestartStrategy = one_for_one,
	MaxRestarts = 1000,
	MaxSecondsBetweenRestarts = 3600,
	
	SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
	
	{ok, {SupFlags, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

