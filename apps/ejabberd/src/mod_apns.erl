-module(mod_apns).

-behaviour(gen_mod).
% -behaviour(gen_server).

-export([start/2,
    stop/1
]).

-include("ejabberd.hrl").
-include_lib("apns/include/apns.hrl").

-export([init/1, start_link/2, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

%% gen_mod callbacks
-define(PROCNAME, ?MODULE).
-record(state, {
    apnspid = undefined
}).

handle_apns_error(MsgId, Status) ->
    ?INFO_MSG("error: ~p - ~p~n", [MsgId, Status]).

handle_apns_delete_subscription(Data) ->
    ?INFO_MSG("delete subscription: ~p~n", [Data]).

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
    ?INFO_MSG("***===== INIT apns", []),
    {ok, #state{}}.

terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call(Request, _From, State) ->
    ?INFO_MSG("Unexpected call: ~p~n~n", [Request]),
    {reply, ok, State}.

handle_cast({send, Token, BODY}, State=#state{apnspid=CurrentPid}) ->
    ?INFO_MSG("push Message: ~p ~p ~p ~n~n", [Token, BODY, State]),

    APNSPid = case CurrentPid of
        undefined ->
            CONN = apns:connect(apnsconn, fun handle_apns_error/2, fun handle_apns_delete_subscription/1),
            Pid = case CONN of
                {ok, Ppid} -> Ppid;
                {error, {already_started, Ppid}} -> Ppid;
                {error, Reason} ->
                    ?DEBUG("apns connect error ~p ~n~n", [Reason]),
                    {error, Reason}
            end;
        Pid ->
            Pid
    end,
    apns:send_message(apnsconn, #apns_msg{
      alert  = BODY,
      sound  = "default",
      device_token = Token
    }),
    {noreply, State#state{apnspid=APNSPid}}.

handle_info(_Info, State) ->
    {noreply, State}.




