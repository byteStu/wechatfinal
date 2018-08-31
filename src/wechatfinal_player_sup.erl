%%%-------------------------------------------------------------------
%%% @author ubuntu
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. 八月 2018 8:50 PM
%%%-------------------------------------------------------------------
-module(wechatfinal_player_sup).
-author("ubuntu").

-behaviour(supervisor).

%% API
-export([start_link/0,start_child/1,stop_child/1,get_child_name/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc 创建玩家进程
-spec start_child(Player::atom()) -> term().
start_child(Player) ->
	ChildSpec = {Player,{wechatfinal_player_wk,start_link,[Player]},permanent,5000,worker,[wechatfinal_player_wk]},
	supervisor:start_child(?SERVER,ChildSpec).

%% @doc 关闭玩家进程
-spec stop_child(Player::atom()) ->term().
stop_child(Player) ->
	supervisor:terminate(?SERVER,Player),
	supervisor:delete_child(?SERVER,Player).

%% @doc 返回子节点名字列表
-spec get_child_name() -> list().
get_child_name() ->
	Player1 = supervisor:which_children(?SERVER),
	[PlayerName ||{PlayerName,_,_,_} <- Player1].

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
