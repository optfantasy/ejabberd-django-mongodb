-module(mod_chat_log_mongodb).

-behaviour(gen_mod).
-behaviour(gen_server).

-export([start/2,
    stop/1,
    log_user_send/3
]).

-export([init/1, start_link/2, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

% -include_lib("exmpp/include/exmpp.hrl").
% -include_lib("exmpp/include/exmpp_jid.hrl").

-include("ejabberd.hrl").

-define(PROCNAME, ?MODULE).
-define(INTERVAL, 30000). % flush to mongo every 2 seconds
-define(POSTCOUNT,50).

-record(state, {
	host,
	db,
	collection,
	conn,
    api_url
}).
-record(jid, {
  raw  :: binary() | undefined,   %% original JID
  node     :: binary() | undefined, %% prepared node
  luser     :: binary() | undefined, %% prepared node
  lserver     :: binary() | undefined, %% prepared node
  domain   :: binary() | undefined, %% prepared domain
  resource :: binary() | undefined  %% prepared resource
}).
-record(xmlel, {
        name,
        attrs
        }).

%% gen_mod callbacks

start(Host, Opts) ->
    ?INFO_MSG("~p starting...", [?MODULE]),
    HostB = Host,
	ejabberd_hooks:add(user_send_packet, HostB, ?MODULE, log_user_send, 55),
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
    HostB = list_to_binary(Host),
    ejabberd_hooks:delete(user_send_packet, HostB, ?MODULE, log_user_send, 50),
	Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
	gen_server:call(Proc, stop),
	supervisor:delete_child(ejabberd_sup, Proc),
    ok.


%% gen_server callbacks

start_link(Host, Opts) ->
	Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
	?INFO_MSG("*** ********** ~p", [Proc]),
	gen_server:start_link({local, Proc}, ?MODULE, [Host, Opts], []).

init([_Host, Opts]) ->
    inets:start(),
    ?INFO_MSG("*** INIT", []),
    case catch ets:new(gulu_group_timestamp, [ordered_set, public, named_table]) of
        gulu_group_timestamp -> ok;
        _ ->
            catch ets:delete(gulu_group_timestamp),
            ets:new(gulu_group_timestamp, [ordered_set, public, named_table])
    end,
	timer:send_interval(?INTERVAL, update_message_timestamp),
	
    API_URL = gen_mod:get_opt(gulu_api_url, Opts, "http://localhost:8000"),
	Host = gen_mod:get_opt(hosts, Opts, ["localhost:27017"]),
	DB = gen_mod:get_opt(db, Opts, "xmpp"),
	Collection = gen_mod:get_opt(collection, Opts, "message"),
	mongodb:replicaSets(xmpp_mongo, Host),
	mongodb:connect(xmpp_mongo),
	
	Conn = mongoapi:new(xmpp_mongo, list_to_binary(DB)),
	Conn:set_encode_style(mochijson),
	
	{ok, #state{
		host = Host,
		db = DB,
		collection = Collection,
		conn = Conn,
        api_url = API_URL
	}}.

terminate(_Reason, _State) ->
	mongodb:deleteConnection(xmpp_mongo),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

handle_call(Request, _From, State) ->
	?INFO_MSG("Unexpected call: ~p", [Request]),
	{reply, ok, State}.

handle_cast({apns, MSG_UUID, BODY, FromJid}, S=#state{conn=Conn, collection=Coll, api_url=API_URL}) ->
    ?INFO_MSG("push Message: ~p ~p", [MSG_UUID, BODY]),
    apns_by_django(MSG_UUID, BODY, FromJid, API_URL),
    {noreply, S};

handle_cast({save, Rec}, S=#state{conn=Conn, collection=Coll}) ->
    ?INFO_MSG("Save Message: ~p", [Rec]),
    Conn:save(Coll, Rec),
    {noreply, S};

handle_cast({update, Rec, CombineUUID, CombineBody, Timestamp, MicroTime}, S=#state{conn=Conn, collection=Coll}) ->
    ?INFO_MSG("update Message: ~p ~p ~p", [Rec, CombineUUID, CombineBody]),
    Conn:update(Coll, [{<<"msg_uuid">>,CombineUUID}], [
            {<<"content">>,{set,CombineBody}},
            {<<"timestamp">>, {set, Timestamp}},
            {<<"timestamp_micro">>, {set, MicroTime}}
        ], []),
    {noreply, S}.

handle_info(flush, S=#state{conn=Conn, collection=Coll}) ->
    %?INFO_MSG("flushing chat log to mongo...", []),

    %% separate out the ets timestamps and records
    {Keys, Vals} = lists:foldl(fun({Key, Val}, {Keys, Vals}) -> {[Key|Keys], 
        [Val|Vals]} end, {[], []}, flush()),
    
    case {Keys, Vals} of
        {[], []} ->
            ok;
        {Times, Recs} ->
            %% insert the records into mongo
            ok = Conn:batchInsert(Coll, Recs),
            
            %% remove the ets entries we just saved to mongo
            lists:foldl(fun(Key, _) -> ets:delete(?MODULE, Key) end, [], Times),
            ok
    end,
    
    %?INFO_MSG("flushed ~p messages", [length(Keys)]),
    {noreply, S};

handle_info(update_message_timestamp, State=#state{api_url=API_URL}) ->
    GroupTime = ets:match_object(gulu_group_timestamp, {'_','_'}),
    lists:foldl(fun(Key, _) -> ets:delete_object(gulu_group_timestamp, Key) end, [], GroupTime),
    case GroupTime of
        [] ->
            ok;
        _ ->
            GT = [ [{<<"group_id">>,G},{<<"ts">>,T}] || {G,T} <- GroupTime],
            % io:format('kkkkkkkkk ~p ~p ~n',[GroupTime, GT]),
            Count = lists:flatlength(GT),
            Div = Count div ?POSTCOUNT,
            % io:format('========= ~p ~p ~n', [Count,Div]),
            case Count rem ?POSTCOUNT of
                Rem when Rem > 0 ->
                    Div2 = Div + 1;
                Rem ->
                    Div2 = Div
            end,
            % io:format('====================== ~p ~n', [Div2]),
            Fun = fun(Num) ->
                % io:format('----------------1 ~p ~p ~p ~n', [Num, (Num-1)*?POSTCOUNT+1, ?POSTCOUNT]),
                NewGT = lists:sublist(GT, (Num-1)*?POSTCOUNT+1, ?POSTCOUNT),
                % io:format('----------------2 ~p ~n', [NewGT]),
                GTString = binary_to_list(iolist_to_binary(mochijson2:encode(NewGT))),
                % io:format('----------------3 ~p ~n', [GTString]),
                EncodeGT = encode(string:join(["{\"content\":",GTString,"}"],"")),
                % io:format('----------------4 ~p ~n', [EncodeGT]),
                Data = string:join(["post_key=2vsAATy79N&json=", EncodeGT], ""),
                URL = string:join([API_URL, "/newapi/message/update_group_ts/"], ""),
                % io:format('ffffffff ~p ~p ~p ~n', [API_URL,URL,Data]),
                httpc:request(post,
                    {URL,
                        [],
                        "application/x-www-form-urlencoded",
                        Data
                    }, [], [])
                end,
            lists:foreach(Fun, lists:seq(1, Div2)),
            ok
    end,
    {noreply, State};

handle_info(Info, State) ->
    %?INFO_MSG("Unexpected info: ", [Info]),
    {noreply, State}.

%% ejabberd hook callback

log_user_send(From, To, Packet) ->
    log_packet(From, To, Packet).

%% private

log_packet(From, To, Packet=#xmlel{name='message', attrs=Attrs}) ->
    % Type = exmpp_xml:get_attribute_from_list(Attrs, <<"type">>, <<>>),
    Type = xml:get_attr_s("type", Attrs), 
	case Type of
		"error" -> %% we don't log errors
			?DEBUG("dropping error: ~s", [xmpp_xml:document_to_list(Packet)]),
			ok;
		_ ->
			save_packet(From, To, Packet, Type)
	end;    
log_packet(_From, _To, _Packet) ->
    ok.

save_packet(From, To, Packet, Type) ->
	% Body = exmpp_xml:get_cdata(exmpp_xml:get_element(Packet, "body")),
	% MsgUUID = exmpp_xml:get_attribute_as_binary(Packet, <<"msguuid">>, ""),
        % CombineUUID = exmpp_xml:get_attribute_as_binary(Packet, <<"combine">>, ""),

	Body = xml:get_cdata(xml:get_element(Packet, "body")),
	MsgUUID = xml:get_tag_attr(Packet, "msguuid"),
        CombineUUID = xml:get_tag_attr(Packet, "combine"),

    %%CombineBody = exmpp_xml:get_attribute_as_binary(Packet, <<"combine_body">>, ""),
	case Body of
		<<"">> -> %% don't log empty messages
			?DEBUG("not logging empty message from ~p",[From]),
			ok;
		_ ->
                        % TODO: How to directly handle string (list) to implement these JID operations ?			
    			% FromJid = exmpp_jid:prep_node_as_list(From),
			% FromHost = exmpp_jid:prep_domain_as_list(From),
			% FromResource = exmpp_jid:prep_resource_as_list(From),
			% ToJid = exmpp_jid:prep_node_as_list(To),
			% ToHost = exmpp_jid:prep_domain_as_list(To),


            FromJid = From#jid.luser,
			FromHost = From#jid.lserver,
			FromResource = From#jid.resource,
			ToJid = To#jid.luser,
			ToHost = To#jid.lserver,
			ToResource = To#jid.resource,			

			Timestamp = unix_timestamp(),
			MicroTime = now_us(erlang:now()),

          	Rec = [
	        	{<<"_from_user">>, prepare(FromJid)},
                {<<"from_host">>, prepare(FromHost)},
                {<<"from_resource">>, prepare(FromResource)},
                {<<"_to_group">>, prepare(ToJid)},
                {<<"to_host">>, prepare(ToHost)},
                {<<"to_resource">>, prepare(ToResource)},
                {<<"content">>, Body},
                {<<"timestamp">>, Timestamp},
                {<<"timestamp_micro">>, MicroTime},
                {<<"msg_type">>, Type},
                {<<"msg_uuid">>, MsgUUID},
                {<<"in_timeline">>, false},
                {<<"_doing_timeline">>, false}
            ],

            case Type of
                <<"groupchat">> ->
                    ets:insert(gulu_group_timestamp, {prepare(ToJid), Timestamp}),
                    ?INFO_MSG("ready to send... ~p", [Rec]),
                    Proc = gen_mod:get_module_proc(FromHost, ?PROCNAME),
                    ?INFO_MSG("ready to send...proc ~p", [Proc]),
                    case CombineUUID of
                        "" ->
                            gen_server:cast(Proc, {save, Rec}),
                            gen_server:cast(Proc, {apns, binary_to_list(MsgUUID), unicode:characters_to_list(Body), FromJid}),
                            ?INFO_MSG("ready to sended ~p", [Proc]);
                        _ ->
                            % CombineBody = exmpp_xml:get_cdata(exmpp_xml:get_element(Packet, "combinebody")),
                            CombineBody = xml:get_cdata(xml:get_element(Packet, "combinebody")),

                            gen_server:cast(Proc, {update, Rec, CombineUUID, CombineBody, Timestamp, MicroTime}),
                            gen_server:cast(Proc, {apns, binary_to_list(CombineUUID), unicode:characters_to_list(Body), FromJid}),
                            ?INFO_MSG("ready to update ~p ~p ~p", [Proc, CombineUUID, CombineBody])
                    end;
                _ ->
                    ?DEBUG("only logging groupchat: typr = ~p",[Type]),
                    ok
            end
	end.

apns_by_django(MSG_UUID, BODY, FromJid, API_URL) ->
    ?INFO_MSG("APNS DATA : ~p ~p", [MSG_UUID, BODY]),
    Data = string:join(["post_key=2vsAATy79N&msg_uuid=", MSG_UUID, "&body=", encode(BODY), "&username=", FromJid], ""),
    ?INFO_MSG("APNS DATA : ~p", [Data]),
    URL = string:join([API_URL, "/newapi/message/apns/"], ""),
    httpc:request(post,
        {URL,
            [],
            "application/x-www-form-urlencoded",
            Data
        }, [], []).
    
flush() ->
    flush(now_us(erlang:now()), [], ets:next(?MODULE, 0)).

flush(_, Acc, '$end_of_table') ->
    Acc;
flush(MaxKey, Acc, CurrKey) when CurrKey < MaxKey ->
    [Res] = ets:lookup(?MODULE, CurrKey),
    flush(MaxKey, [Res|Acc], ets:next(?MODULE, CurrKey));
flush(MaxKey, Acc, CurrKey) when CurrKey >= MaxKey ->
    Acc.

unix_timestamp() ->
    unix_timestamp(calendar:universal_time()).

unix_timestamp(DT) ->
    Epoch = {{1970,1,1},{0,0,0}},
    calendar:datetime_to_gregorian_seconds(DT) -
        calendar:datetime_to_gregorian_seconds(Epoch).
		
now_us({MegaSecs,Secs,MicroSecs}) ->
	(MegaSecs*1000000 + Secs)*1000000 + MicroSecs. 

prepare(Val) ->
    case Val of
        undefined -> 
            <<"">>;
	    Val when is_list(Val) ->
		    list_to_binary(Val);
        _ -> 
            Val
    end.

encode([C | Cs]) when C >= $a, C =< $z ->
  [C | encode(Cs)];
encode([C | Cs]) when C >= $A, C =< $Z ->
  [C | encode(Cs)];
encode([C | Cs]) when C >= $0, C =< $9 ->
  [C | encode(Cs)];
  
encode([C | Cs]) when C == 16#20 -> % space to +
  [$+ | encode(Cs)];

% unreserved
encode([C = $- | Cs]) ->
  [C | encode(Cs)];
encode([C = $_ | Cs]) ->
  [C | encode(Cs)];
encode([C = 46 | Cs]) -> % .
  [C | encode(Cs)];
encode([C = $! | Cs]) ->
  [C | encode(Cs)];
encode([C = $~ | Cs]) ->
  [C | encode(Cs)];
encode([C = $* | Cs]) ->
  [C | encode(Cs)];
encode([C = 39 | Cs]) -> % '
  [C | encode(Cs)];
encode([C = $( | Cs]) ->
  [C | encode(Cs)];
encode([C = $) | Cs]) ->
  [C | encode(Cs)];

encode([C | Cs]) when C =< 16#7f ->
  escape_byte(C)
  ++ encode(Cs);
  
encode([C | Cs]) when (C >= 16#7f) and (C =< 16#07FF) ->
  escape_byte((C bsr 6) + 16#c0)
  ++ escape_byte(C band 16#3f + 16#80)
  ++ encode(Cs);

encode([C | Cs]) when (C > 16#07FF) ->
  escape_byte((C bsr 12) + 16#e0) % (0xe0 | C >> 12)
  ++ escape_byte((16#3f band (C bsr 6)) + 16#80) % 0x80 | ((C >> 6) & 0x3f)
  ++ escape_byte(C band 16#3f + 16#80) % 0x80 | (C >> 0x3f)
  ++ encode(Cs);

encode([C | Cs]) ->
  escape_byte(C) ++ encode(Cs);

encode([]) -> [].

% from edoc_lib source
hex_octet(N) when N =< 9 ->
    [$0 + N];
hex_octet(N) when N > 15 ->
    hex_octet(N bsr 4) ++ hex_octet(N band 15);
hex_octet(N) ->
    [N - 10 + $a].

escape_byte(C) ->
  H = hex_octet(C),
  normalize(H).

%% Append 0 if length == 1
normalize(H) when length(H) == 1 ->
  "%0" ++ H;

normalize(H) ->
  "%" ++ H.


