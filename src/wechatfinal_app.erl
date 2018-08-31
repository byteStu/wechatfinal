%%%-------------------------------------------------------------------
%% @doc wechatfinal public API
%% @end
%%%-------------------------------------------------------------------

-module(wechatfinal_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    %% 路由配置
    Routes = [
        {
            '_',[
            {"/websocket",wechatfinal_ws_handler,[]},
            {"/chat",cowboy_static,{priv_file,wechatfinal,"static/html/chat.html"}},
            {"/login",cowboy_static,{priv_file,wechatfinal,"static/html/login.html"}},
            {"/submit",wechatfinal_login_handler,[]},
            %% 静态资源文件路径
            {"/static/[...]",cowboy_static,{priv_dir,wechatfinal,"static"}}
        ]
        }
    ],
    Dispatch = cowboy_router:compile(Routes),
    {ok,Port} = application:get_env(http_port),
    {ok, _} = cowboy:start_clear(http, [{port, Port}],
        #{env => #{dispatch => Dispatch}}),
    wechatfinal_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
