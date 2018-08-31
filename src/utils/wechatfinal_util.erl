%%%-------------------------------------------------------------------
%%% @author ubuntu
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. 八月 2018 11:48 PM
%%%-------------------------------------------------------------------
-module(wechatfinal_util).
-author("ubuntu").

%% API
-export([create_token/0,get_ws_name/1,get_wk_name/1,atomList_join_to_binary/1]).

create_token() ->
    os:cmd("uuidgen") -- "\n".

%% @doc 根据gen_server的name获得对应的websocket进程的名字
-spec get_ws_name(WkName::atom()) -> atom().
get_ws_name(WkName) when is_atom(WkName)->
    WkNameList = atom_to_list(WkName),
    [WsNameList|_] = string:tokens(WkNameList,"_"),
    list_to_atom(WsNameList).
 
%% @doc 根据websocket进程名字获得对应的gen_server的名字
-spec get_wk_name(WsName::atom()) -> atom().
get_wk_name(WsName) when is_atom(WsName) ->
    WsNameList = atom_to_list(WsName),
    WkNameList = WsNameList ++ "_" ++ "proxy" ++ "_" ++ "wk",
    list_to_atom(WkNameList).

%% @doc list元素为atom，转换为一个以，隔开的binary字符串
-spec atomList_join_to_binary(List::list()) -> term().
atomList_join_to_binary(List) when is_list(List)->
    case List =:= [] of
        true  -> <<"">>;
        false -> NewList1 = lists:foldl(fun(X,Str) ->Str ++ atom_to_list(X) ++ "," end,"",List),
                 NewList2 = lists:droplast(NewList1),
                 list_to_binary(NewList2)
    end.
    
