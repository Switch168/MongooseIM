-module(mod_early_block).
-author('andrew@switch168.com').
-behaviour(gen_mod).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_privacy.hrl").

-export([start/2, stop/1, momo/1]).

start(Host, Opts) ->
  ejabberd_hooks:add(filter_local_packet, Host, ?MODULE, momo, 0),
  ok.

stop(Host) ->
  ejabberd_hooks:delete(filter_local_packet, Host, ?MODULE, momo, 0),
  ok.

momo({From, To = #jid{lserver = Host, user = User, server = Server}, Acc, Packet}) ->
  UserListRecord = ejabberd_hooks:run_fold(privacy_get_user_list, Server, #userlist{}, [User, Server]),
  {Acc1, Res} = mongoose_privacy:privacy_check_packet(Acc, Server, User, UserListRecord, To, From, out),
  case Res of
    block ->
      Err = jlib:make_error_reply(Acc, ?ERR_NOT_ACCEPTABLE_BLOCKED),
      ejabberd_router:route(To, From, Err),
      drop;
    _ -> {From, To, Acc, Packet}
  end.

