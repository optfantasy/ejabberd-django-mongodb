-module(mod_apns).

-behaviour(gen_mod).
% -behaviour(gen_server).

-export([start/2,
    stop/1
]).

-include("ejabberd.hrl").

-export([init/1, start_link/2, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

%% gen_mod callbacks
-define(PROCNAME, ?MODULE).
-record(apns, {
    apple_host = "gateway.sandbox.push.apple.com",
    apple_port = 2195,
    cert_file = "../etc/certs/mycert_dev.pem",
    key_file = undefined,
    cert_password = undefined,
    timeout = 30000,
    feedback_port = 2196,
    feedback_host = "feedback.sandbox.push.apple.com",
    feedback_timeout = 18000000
}).

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

start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    ?INFO_MSG("*** ********** ====== ~p ~n~n", [Proc]),
    gen_server:start_link({local, Proc}, ?MODULE, [Host, Opts], []).

init([_Host, Opts]) ->
    inets:start(),
    ?INFO_MSG("***===== INIT apns", []),
    
    APPLE_HOST = gen_mod:get_opt(apple_host, Opts, "gateway.sandbox.push.apple.com"),
    CERT_FILE = gen_mod:get_opt(cert_file, Opts, "../etc/certs/mycert_dev.pem"),
    
    {ok, #apns{
        apple_host = APPLE_HOST,
        cert_file = CERT_FILE
    }}.

terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call(Request, _From, State) ->
    ?INFO_MSG("Unexpected call: ~p~n~n", [Request]),
    {reply, ok, State}.

handle_cast({send, Token, BODY}, State) ->
    ?INFO_MSG("push Message: ~p ~p ~p ~n~n", [Token, BODY, State]),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.




