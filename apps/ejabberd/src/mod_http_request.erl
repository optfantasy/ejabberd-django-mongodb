-module(mod_http_request).

-behaviour(gen_mod).
-behaviour(gen_server).

-export([start/2,
    stop/1,
    post/2
    ]).

-export([init/1, start_link/2, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).


-include("ejabberd.hrl").

-define(PROCNAME, ?MODULE).

-record(rem_host, {id, host}).

%% gen_mod callbacks

start(Host, Opts) ->
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
    % application:start(jobs),
    inets:start(),
    setup_database(),
    mnesia:dirty_write(#rem_host{id="hostid",host=Host}),

    % Plimit = gen_mod:get_opt(process_limit, Opts, 4),
    % jobs:add_queue(httprequest, [
    %     {regulators, [{counter, [
    %                     {name, httprequest},
    %                     {limit, Plimit}
    %                   ]}]}
    % ]),
    {ok, {}}.

setup_database() ->
    migrate_database(),
    mnesia:create_table(rem_host,
            [{ram_copies, [node()]},
             {attributes, record_info(fields, rem_host)}]).

migrate_database() ->
    case catch mnesia:table_info(rem_host, attributes) of
        [id, host] ->
        ok;
        _ ->
        mnesia:delete_table(rem_host)
    end.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call({post, URL, Data}, _From, State) ->
    % io:format("===== handle_call post: ~p~n~n", [{URL, Data}]),
    httpc:request(post,
        {URL,
            [],
            "application/x-www-form-urlencoded",
            Data
        }, [], []),
    % spawn( fun() ->
    %     jobs:run(httprequest, fun() -> httpc:request(post,
    %         {URL,
    %             [],
    %             "application/x-www-form-urlencoded",
    %             Data
    %         }, [], []) end)
    % end),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_CAST, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

get_host() ->
    case catch mnesia:dirty_read(rem_host, "hostid") of
        [#rem_host{host=Host}] ->
            Host;
        _ ->
            <<"localhost">>
    end.

post(URL, Data) ->
    Host = get_host(),
    post(Host, URL, Data).

post(Host, URL, Data) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:call(Proc, {post, URL, Data}).


