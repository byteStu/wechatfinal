%%%-------------------------------------------------------------------
%%% @author ubuntu
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. 八月 2018 2:39 AM
%%%-------------------------------------------------------------------
-module(wechatfinal_login_handler).
-author("ubuntu").

%% API
-export([init/2,check_login/3]).

init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),
    HasBody = cowboy_req:has_body(Req0),
    Req = maybe_echo(Method, HasBody, Req0),
    {ok, Req, Opts}.

maybe_echo(<<"POST">>, true, Req0) ->
    {ok, PostVals, Req} = cowboy_req:read_urlencoded_body(Req0),
    UserName = proplists:get_value(<<"user">>, PostVals),
    Password = proplists:get_value(<<"password">>, PostVals),
    check_login(UserName,Password,Req);
maybe_echo(<<"POST">>, false, Req) ->
    cowboy_req:reply(400, [], <<"Missing body.">>, Req);
maybe_echo(_, _, Req) ->
    %% Method not allowed.
    cowboy_req:reply(405, Req).

echo(undefined, Req) ->
    cowboy_req:reply(400, [], <<"Missing echo parameter.">>, Req);
echo({redirect,_Echo}, Req) ->
    cowboy_req:reply(302, #{
        <<"Location">> => list_to_binary(_Echo)
    }, Req);
echo({reponse,_Echo}, Req) ->
    cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain;charset=utf-8">>
    }, _Echo, Req).

%% 登录验证
check_login(UserName,Pasword,Req) when is_binary(UserName),is_binary(Pasword)->
    try
        [P|_] = wechatfinal_mnesia:q_users_password(UserName),
        io:format("密码:~p~n",[P]),
        if
            Pasword =:= P-> Token = wechatfinal_util:create_token(),
                            io:format("login_handler里面的Token:~p~n",[Token]),
                            wechatfinal_ets:save_token(Token,UserName),
                            Req1 = cowboy_req:set_resp_cookie(<<"token">>,Token,Req,#{path => <<"/">>}),
                            Req2 = cowboy_req:set_resp_cookie(<<"user">>,UserName,Req1,#{path => <<"/">>}),
                            echo({redirect,"/chat"},Req2);
            false -> echo({reponse,"login fail"},Req)
        end
    catch
        error:_ -> io:format("出错了:~p~n"),
                    echo({reponse,"login fail"},Req)
    end.


