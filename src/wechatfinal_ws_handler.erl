-module(wechatfinal_ws_handler).
-include("wechatfinal_cmd_pb.hrl").
-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
init(Req, State) ->
	io:format("进入ws的init"),
	Cookies = cowboy_req:parse_cookies(Req),
	Token = lists:keyfind(<<"token">>,1,Cookies),
	io:format("ws里面的Token:~p~n",[Token]),
	{cowboy_websocket, Req, [Token|State],#{idle_timeout => 1800000}}.
websocket_init(State) ->
	io:format("当前进程：~p~n",[self()]),
	{_,Token} = lists:keyfind(<<"token">>,1,State),
	UserName = wechatfinal_ets:get_username(binary_to_list(Token)),
	UserNameAtom = binary_to_atom(UserName,utf8),
	%% ws 注册
	register(UserNameAtom,self()),
	%% 开启玩家进程
	WkName = wechatfinal_util:get_wk_name(UserNameAtom),
	case erlang:whereis(WkName) =:= undefined of
		 true  -> wechatfinal_player_wk:sign_in(WkName),
				  wechatfinal_broadcast_wk:send_online_user_to_room(UserNameAtom);
		 false -> pass
	end,
	%% 广播一下
	wechatfinal_broadcast_wk:send_online_msg(),
	wechatfinal_broadcast_wk:send_room_msg(),
	wechatfinal_broadcast_wk:send_online_user_to_room(UserNameAtom),
	
	{ok, State}.

websocket_handle({binary, Msg}, State) ->
	Decode = wechatfinal_cmd_pb:decode_msg(Msg,msg),
	case Decode#msg.type of
		chat -> io:format("聊天消息~n"),
				Sender = binary_to_atom(Decode#msg.data#data.chat#chat.sender,utf8),
				NewSender = wechatfinal_util:get_wk_name(Sender),
				wechatfinal_player_wk:send_msg(NewSender,Decode);
		online -> io:format("在线消息~n"),
			       wechatfinal_player_wk:sign_out(wechatfinal_util:get_wk_name(binary_to_atom(Decode#msg.data#data.online#online.body,utf8)));
		room -> io:format("房间消息~n"),
			{_,Sender} = erlang:process_info(self(), registered_name),
				wechatfinal_room_wk:handle_room(Sender,Decode);
		system -> io:format("系统消息~n");
		history -> io:format("私人聊天记录~n"),
				   wechatfinal_player_wk:load_private_chat_history(Decode);
		_ -> ok
	end,
	{ok, State}.

websocket_info({timeout, _Ref, _Msg}, State) ->
	{ok, State};
websocket_info({resp_msg,Msg}, State) ->
	%%io:format("resp_msg被调用了~n"),
	%%io:format("resp_msg,Msg:~p~n",[wechatfinal_cmd_pb:decode_msg(Msg,msg)]),
	{reply,{binary,Msg},State};
websocket_info({stop_conn}, State) ->
	io:format("stop_conn被调用了~n"),
	{stop, State};
websocket_info(_Info, State) ->
	{ok, State}.




