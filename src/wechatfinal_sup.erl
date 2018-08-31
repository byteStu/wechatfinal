%%%-------------------------------------------------------------------
%% @doc wechatfinal top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(wechatfinal_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    %% 初始化数据库
    wechatfinal_mnesia:init_mnesia(),
    wechatfinal_ets:create_token_table(),
    
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    
    %% 只启动一个子进程，类型是 worker
    Process = [ #{id => wechatfinal_broadcast_wk,   %%  给子进程设置一个名字，supervisor 用这个名字标识这个进程。
                start => {wechatfinal_broadcast_wk, start_link, []}, %% 启动时调用的 Module:Function(Args)
                restart => permanent,  %% 永远需要重启
                shutdown => 2000, %% 关闭时不需要等待，直接强行杀死进程
                type => worker,
                modules => [wechatfinal_broadcast_wk]},
        
                #{id => wechatfinal_player_sup,   %%  给子进程设置一个名字，supervisor 用这个名字标识这个进程。
                start => {wechatfinal_player_sup, start_link, []}, %% 启动时调用的 Module:Function(Args)
                restart => permanent,  %% 永远需要重启
                shutdown => 2000, %% 关闭时不需要等待，直接强行杀死进程
                type => supervisor,
                modules => [wechatfinal_player_sup]},
    
                #{id => wechatfinal_room_sup,   %%  给子进程设置一个名字，supervisor 用这个名字标识这个进程。
                start => {wechatfinal_room_sup, start_link, []}, %% 启动时调用的 Module:Function(Args)
                restart => permanent,  %% 永远需要重启
                shutdown => 2000, %% 关闭时不需要等待，直接强行杀死进程
                type => supervisor,
                modules => [wechatfinal_room_sup]},
    
                #{id => wechatfinal_initapp_sup,   %%  给子进程设置一个名字，supervisor 用这个名字标识这个进程。
                start => {wechatfinal_initapp_sup, start_link, []}, %% 启动时调用的 Module:Function(Args)
                restart => permanent,  %% 永远需要重启
                shutdown => 2000, %% 关闭时不需要等待，直接强行杀死进程
                type => supervisor,
                modules => [wechatfinal_initapp_sup]}
        
    ],
    {ok, {SupFlags, Process}}.

%%====================================================================
%% Internal functions
%%====================================================================
