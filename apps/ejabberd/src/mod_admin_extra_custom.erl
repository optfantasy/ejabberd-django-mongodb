-module(mod_admin_extra_custom).
-author('gage.tseng@geniecapital.com').

-behaviour(gen_mod).

-export([start/2, stop/1,
	 %% Roster
	 auto_friend/4,
     auto_friends/3,
     unfriend/4
	 % auto_friends/3,
	 % delete_rosteritem/4
	]).
-include("ejabberd.hrl").
-include("ejabberd_commands.hrl").
-include("mod_roster.hrl").
-include("jlib.hrl").


%%%
%%% gen_mod
%%%

start(_Host, _Opts) ->
    ejabberd_commands:register_commands(commands()).

stop(_Host) ->
    ejabberd_commands:unregister_commands(commands()).


%%%
%%% Register commands
%%%

commands() ->

    [
     #ejabberd_commands{name = auto_friend, tags = [roster],
		desc = "Add an item to a user's roster",
		module = ?MODULE, function = auto_friend,
		args = [{localuser, string}, {localserver, string},
			{user, string}, {server, string}],
		result = {res, rescode}},
	
    #ejabberd_commands{name = auto_friends, tags = [roster],
   		desc = "Add items to a user's roster",
   		module = ?MODULE, function = auto_friends,
   		args = [{localuser, string}, {localserver, string},
   			{users, string}],
   		result = {res, rescode}},
	
     #ejabberd_commands{name = unfriend, tags = [roster],
		desc = "Delete an item from a user's roster",
		module = ?MODULE, function = unfriend,
		args = [{localuser, string}, {localserver, string},
			{user, string}, {server, string}],
		result = {res, rescode}}
    ].


%%%
%%% Roster
%%%

auto_friend(LocalUser, LocalServer, User, Server) ->
    add_rosteritem(LocalUser, LocalServer, User, Server, "", "", "both"),
    add_rosteritem(User, Server, LocalUser, LocalServer, "", "", "both").

unfriend(LocalUser, LocalServer, User, Server) ->
	delete_rosteritem(LocalUser, LocalServer, User, Server),
    delete_rosteritem(User, Server, LocalUser, LocalServer).

auto_friends(LocalUser, LocalServer, Users) ->
	USERS = re:split(Users, ":", [{return, list}]),
    case subscribe_all(LocalUser, LocalServer, USERS) of
	ok ->
	    ok;
	error ->
	    error
    end.

subscribe_all(LU, LS, Users) ->
	case Users of
		[User | Others] ->
		    case auto_friend(list_to_binary(LU), list_to_binary(LS), list_to_binary(User), list_to_binary(LS)) of
			ok ->
				subscribe_all(LU, LS, Others),
			    ok;
			_ ->
			    error
		    end;
		[] ->
			ok
	end.

add_rosteritem(LocalUser, LocalServer, User, Server, Nick, Group, Subs) ->
    case add_rosteritem(LocalUser, LocalServer, User, Server, Nick, Group, Subs, []) of
    {atomic, ok} ->
        push_roster_item(LocalUser, LocalServer, User, Server, {add, Nick, Subs, Group}),
        ok;
    _ ->
        error
    end.

add_rosteritem(LU, LS, User, Server, Nick, Group, Subscription, Xattrs) ->
    subscribe(LU, LS, User, Server, Nick, Group, Subscription, Xattrs).

subscribe(LocalUser, LocalServer, User, Server, Nick, Group, Subs, _Xattrs) ->
    SubscriptionS = case is_list(Subs) of
            true -> list_to_binary(Subs);
            _ -> Subs
        end,
    ItemEl = build_roster_item(User, Server, {add, Nick, SubscriptionS, Group}),
    {ok, M} = loaded_module(LocalServer,[mod_roster_odbc,mod_roster]),
    M:set_items(
    LocalUser, LocalServer,
    {xmlelement,<<"query">>,
            [{<<"xmlns">>,"jabber:iq:roster"}],
            [ItemEl]}).

delete_rosteritem(LocalUser, LocalServer, User, Server) ->
    case unsubscribe(LocalUser, LocalServer, User, Server) of
    {atomic, ok} ->
        push_roster_item(LocalUser, LocalServer, User, Server, remove),
        ok;
    _  ->
        error
    end.

unsubscribe(LU, LS, User, Server) ->
    ItemEl = build_roster_item(User, Server, remove),
    {ok, M} = loaded_module(LS,[mod_roster_odbc,mod_roster]),
    M:set_items(
    LU, LS,
    {xmlelement,<<"query">>,
            [{<<"xmlns","jabber:iq:roster">>}],
            [ItemEl]}).

loaded_module(_Domain,_Options) ->
    {ok, mod_roster}.

push_roster_item(LU, LS, U, S, Action) ->
    lists:foreach(fun(R) ->
              push_roster_item(LU, LS, R, U, S, Action)
          end, ejabberd_sm:get_user_resources(LU, LS)).

push_roster_item(LU, LS, R, U, S, Action) ->
    LJID = jlib:make_jid(LU, LS, R),
    BroadcastEl = build_broadcast(U, S, Action),
    ejabberd_router:route(LJID, LJID, BroadcastEl),
    Item = build_roster_item(U, S, Action),
    ResIQ = build_iq_roster_push(Item),
    ejabberd_router:route(LJID, LJID, ResIQ).

build_roster_item(U, S, {add, Nick, Subs, Group}) ->
    {xmlelement, <<"item">>,
     [{<<"jid">>, jlib:jid_to_binary(jlib:make_jid(U, S, ""))},
      {<<"name">>, Nick},
      {<<"subscription">>, Subs}],
     [{xmlelement, <<"group">>, [], [{xmlcdata, Group}]}]
    };
build_roster_item(U, S, remove) ->
    {xmlelement, <<"item">>,
     [{<<"jid">>, jlib:jid_to_binary(jlib:make_jid(U, S, ""))},
      {<<"subscription">>, <<"remove">>}],
     []
    }.

build_iq_roster_push(Item) ->
    {xmlelement, "iq",
     [{"type", "set"}, {"id", "push"}],
     [{xmlelement, "query",
       [{"xmlns", ?NS_ROSTER}],
       [Item]
      }
     ]
    }.

build_broadcast(U, S, {add, _Nick, Subs, _Group}) ->
    build_broadcast(U, S, list_to_atom(Subs));
build_broadcast(U, S, remove) ->
    build_broadcast(U, S, none);
%% @spec (U::string(), S::string(), Subs::atom()) -> any()
%% Subs = both | from | to | none
build_broadcast(U, S, SubsAtom) when is_atom(SubsAtom) ->
    {xmlelement, <<"broadcast">>, [],
     [{item, {U, S, ""}, SubsAtom}]
    }.

