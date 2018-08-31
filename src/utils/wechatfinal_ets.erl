%%%-------------------------------------------------------------------
%%% @author ubuntu
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. 八月 2018 12:30 AM
%%%-------------------------------------------------------------------
-module(wechatfinal_ets).
-author("ubuntu").

%% API
-export([create_token_table/0,save_token/2,get_username/1]).

create_token_table() ->
    ets:new(tokens,[set,public,named_table]).

save_token(Token,UserName) -> ets:insert(tokens,{Token,UserName}).

get_username(Token) ->[Res|_] = ets:lookup(tokens,Token),{_,UserName} = Res,UserName.