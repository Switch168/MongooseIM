-module(mod_momo).
-author('andrewvmail@gmail.com').
-behaviour(gen_mod).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_privacy.hrl").

-export([start/2, stop/1, momo/1]).

start(Host, Opts) ->
  ?INFO_MSG("MOMO START ============", []),
  ejabberd_hooks:add(filter_local_packet, Host, ?MODULE, momo, 0),
  ok.

stop(Host) ->
  ?INFO_MSG("MOMO STOP ==============", []),
  ejabberd_hooks:delete(filter_local_packet, Host, ?MODULE, momo, 0),
  ok.

momo({From, To = #jid{lserver = Host, user = User, server = Server}, Acc, Packet}) ->
  ?INFO_MSG("MOMO WHAT ============== FROM:  ~p", [Host]),
  ?INFO_MSG("MOMO WHAT ============== USER: ~p", [User]),
%%  ?INFO_MSG("MOMO WHAT ============== Acc: ~p", [Acc]),
%%  ?INFO_MSG("MOMO WHAT ============== Packet: ~p", [Packet]),

  UserListRecord = ejabberd_hooks:run_fold(privacy_get_user_list, Server, #userlist{}, [User, Server]),
  {Acc1, Res} = mongoose_privacy:privacy_check_packet(Acc, Server, User, UserListRecord, To, From, out),
  ?INFO_MSG("MOMO WHAT ============== Packet: ~p", [Res]),
  ?INFO_MSG("MOMO WHAT ============== Packet: ~p", [Acc1]),
  case Res of
    block ->
      Err = jlib:make_error_reply(Acc, ?ERR_NOT_ACCEPTABLE_BLOCKED),
      ejabberd_router:route(To, From, Err),
      ?INFO_MSG("MOMO WHAT ============== Rerouting: ~p", [Err]),
      drop;
    _ -> {From, To, Acc, Packet}
  end.

