-module(mod_mongodb).

-behaviour(gen_mod).
-behaviour(gen_server).

-export([start/2,
    stop/1,
    find/2,
    find/3,
    save/2,
    save/3,
    update/3,
    update/4,
    remove/2,
    remove/3
]).

-export([init/1, start_link/2, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).


-include("ejabberd.hrl").

-define(PROCNAME, ?MODULE).

-record(state, {
    host,
    db,
    conn
}).

%% gen_mod callbacks

start(Host, Opts) ->
    % ?INFO_MSG("~p starting...==================================", [?MODULE]),
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    ChildSpec =
        {Proc,
            {?MODULE, start_link, [Host, Opts]},
            temporary,
            1000,
            worker,
            [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:call(Proc, stop),
    supervisor:delete_child(ejabberd_sup, Proc),
    ok.


%% gen_server callbacks

start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE, [Host, Opts], []).

init([Host, Opts]) ->
    inets:start(),
    case catch ets:new(rem_host, [set, named_table]) of
        rem_host -> ok;
        _ ->
            catch ets:delete(rem_host),
            catch ets:new(rem_host, [set, named_table])
    end,
    ?INFO_MSG("*** INIT MONGODB ", []),
    ets:insert(rem_host, {mod_mongo_host, Host}),
    MHost = gen_mod:get_opt(hosts, Opts, ["localhost:27017"]),
    DB = gen_mod:get_opt(db, Opts, "xmpp"),
    mongodb:replicaSets(ej_mongo, MHost),
    mongodb:connect(ej_mongo),
    
    Conn = mongoapi:new(ej_mongo, list_to_binary(DB)),
    Conn:set_encode_style(mochijson),
    
    {ok, #state{
        host = MHost,
        db = DB,
        conn = Conn
    }}.

terminate(_Reason, _State) ->
    mongodb:deleteConnection(ej_mongo),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call({find, Collection, Query}, _From, State=#state{conn=Conn}) ->
    ?INFO_MSG("===== handle_call find: ~p~n~n", [{Collection, Query, State}]),
    Result = Conn:find(Collection, Query, undefined, 0, 1),
    {reply, Result, State};

handle_call({save, Collection, Rec}, _From, State=#state{conn=Conn}) ->
    ?INFO_MSG("===== handle_call save: ~p~n~n", [{Collection, Rec, State}]),
    ResultId = Conn:save(Collection, Rec),
    ?INFO_MSG("===== handle_call save Result: ~p~n~n", [{ResultId}]),
    {reply, ResultId, State};

handle_call({update, Collection, Query, Rec}, _From, State=#state{conn=Conn}) ->
    ?INFO_MSG("===== handle_call update: ~p~n~n", [{Collection, Query, Rec, State}]),
    Result = Conn:update(Collection, Query, Rec, []),
    ?INFO_MSG("===== handle_call update Result: ~p~n~n", [{Result}]),
    {reply, Result, State};

handle_call({remove, Collection, Query}, _From, State=#state{conn=Conn}) ->
    ?INFO_MSG("===== handle_call find: ~p~n~n", [{Collection, Query, State}]),
    Result = Conn:remove(Collection, Query),
    {reply, Result, State};

handle_call(Request, _From, State) ->
    ?INFO_MSG("Unexpected call: ~p~n~n", [Request]),
    {reply, ok, State}.

handle_cast(CAST, State) ->
    ?INFO_MSG("===== mongo handle_cast/2: ~p~n~n", [{CAST,State}]),
    {noreply, State}.

handle_info(Info, State) ->
    ?INFO_MSG("===== mongo handle_info/2: ~p~n~n", [{Info,State}]),
    {noreply, State}.

get_host() ->
    case catch ets:lookup(rem_host, mod_mongo_host) of
        [] ->
            <<"localhost">>;
        [{mod_mongo_host, Host}] ->
            Host;
        _ ->
            <<"localhost">>
    end.

% mod_mongodb:find("auth_user", [{"_id", {oid, <<"50dab76eb82ef048ec000010">>}}]).
find(Collection, Query) ->
    Host = get_host(),
    find(Host, Collection, Query).

find(Host, Collection, Query) ->
    ?INFO_MSG("===== find/3: ~p~n~n", [{Host, Collection, Query}]),
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:call(Proc, {find, Collection, Query}).

save(Collection, Rec) ->
    Host = get_host(),
    save(Host, Collection, Rec).

save(Host, Collection, Rec) ->
    ?INFO_MSG("===== save/3: ~p~n~n", [{Host, Collection, Rec}]),
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:call(Proc, {save, Collection, Rec}).    

update(Collection, Query, Rec) ->
    Host = get_host(),
    update(Host, Collection, Query, Rec).

update(Host, Collection, Query, Rec) ->
    ?INFO_MSG("===== update/4: ~p~n~n", [{Host, Collection, Query, Rec}]),
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:call(Proc, {update, Collection, Query, Rec}).

remove(Collection, Query) ->
    Host = get_host(),
    remove(Host, Collection, Query).

remove(Host, Collection, Query) ->
    ?INFO_MSG("===== remove/3: ~p~n~n", [{Host, Collection, Query}]),
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:call(Proc, {remove, Collection, Query}).

