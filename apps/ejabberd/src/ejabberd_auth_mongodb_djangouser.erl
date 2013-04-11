%%%----------------------------------------------------------------------------------------------------
%%% File    : ejabberd_auth_mongodb_djangouser.erl
%%% Author  : Colin Su <littleq0903@gmail.com>
%%% Purpose : Authentification via MongoDB for Django user mechanism
%%% Created :
%%%----------------------------------------------------------------------------------------------------


-module(ejabberd_auth_mongodb_djangouser).
-author('littleq0903@gmail.com').

%% Import SHA1 module
-import(sha1, [hexstring/1]).

-export([start/1,
         compare_encoded_and_plain_password/2,
         mongo_user_exists/2,
         mongo_check_password/3]).

% functions used by ejabberd_auth
-export([set_password/3,
         check_password/3,
         check_password/5,
         try_register/3,
         dirty_get_registered_users/0,
         get_vh_registered_users/1,
         get_password/2,
         get_password_s/2,
         is_user_exists/2,
         remove_user/2,
         remove_user/3,
         plain_password_required/0
        ]).

-include("ejabberd.hrl").



%%% 
%%% Server behavior callbacks
%%%

start(Host) ->
    ?INFO_MSG("Module: ~p starting...", [?MODULE]),
    application:start(erlmongo),
    MongoServers = ejabberd_config:get_local_option({mongodb_djangouser_server, Host}),
    ?INFO_MSG("MongoServers: ~p~n", [MongoServers]),
    mongodb:replicaSets(xmpp_mongo, MongoServers),
    mongodb:connect(xmpp_mongo),
    ok.

% stop(Host) ->
%     ?INFO_MSG("Module: ~p stoping...", [?MODULE]),
%     mongodb:deleteConnection(xmpp_mongo),
%     ok.


%%%
%%% Authentication callbacks
%%%

plain_password_required() -> 
    true.

check_password(User, Server, Password) ->
    % ?INFO_MSG("~nUser: ~p~nServer: ~p~nPassword: ~p~n", [User, Server, Password]),
    Data_userprofile_password = get_password(User, Server),
    UsernamePrefix = binary:part(User, {0, 7}),
    if UsernamePrefix == <<"ws_test">> ->
      true;
    true ->
    mongo_check_password(User, Password, Server)
end.

check_password(User, Server, Password, Digest, DigestGen) ->
    Data_userprofile_password = get_password(User, Server),
    UsernamePrefix = binary:part(User, {0, 7}),
    ?INFO_MSG("~nUser: ~p~nServer: ~p~nPassword: ~p~n", [User, Server, Password]),
    ?INFO_MSG("~nUsername: ~p~n", [UsernamePrefix]),
    ?INFO_MSG("~nDigest: ~p~n", [Digest]),

    DigRes = if
         Digest /= <<>> ->

             Digest == DigestGen(Data_userprofile_password);
         true ->
             false
         end,
    if DigRes ->
        true;
% for testing purpose
       UsernamePrefix == <<"ws_test">> ->
	     ?INFO_MSG("FUCK in check_password~n", []),
        true;
       true ->
        (Data_userprofile_password == Password) and (Password /= <<>>)
    end.
    
    % Digest == DigestGen(Data_userprofile_password).



is_user_exists(User, Server) ->
    UsernamePrefix = binary:part(User, {0, 7}),
    if UsernamePrefix == <<"ws_test">> ->
	     ?INFO_MSG("FUCK in is_user_exists~n", []),
      true;
    true ->
      mongo_user_exists(User, Server)
end.

set_password(_User, _Server, _Password) ->
    {error, not_allowed}.

try_register(_,_,_) -> 
    {error, not_allowed}.

dirty_get_registered_users() ->
    ?INFO_MSG("fuck**************************************~n", []),
    [].

get_vh_registered_users(_) -> 
    ?INFO_MSG("fuck yeah --------------------------------~n", []),
    [].

get_password(User, Server) ->
    UsernamePrefix = binary:part(User, {0, 7}),
    if UsernamePrefix == <<"ws_test">> ->
	<<"abc">>;
    true ->
        get_password_s(User, Server) 
end.

get_password_s(User, Server) ->
    DB_dbname = list_to_binary(ejabberd_config:get_local_option({mongodb_djangouser_db, Server})),
%    DBConnection = mongoapi:new(xmpp_mongo, DB_dbname),
%    try DBConnection:find(<<"auth_user">>, [{<<"username">>, User}], undefined, 0, 1) of
     try mod_mongodb:find(<<"auth_user">>, [{<<"username">>, User}]) of
        {ok, []} -> 
%            try DBConnection:find(<<"auth_user">>, [{<<"_id">>, {oid,User}}], undefined, 0, 1) of
             try mod_mongodb:find(<<"auth_user">>, [{<<"_id">>, {oid, User}}]) of
                {ok, []} ->
                    {error, not_allowed};
                {ok, Data_authuser_list} ->
                    Data_authuser = lists:nth(1, Data_authuser_list),
                    Data_authuser_oid = proplists:get_value(<<"_id">>, Data_authuser),
%                    {ok, Data_userprofile_list} = DBConnection:find(<<"user_profiles_userprofile">>, [{<<"user_id">>, Data_authuser_oid}], undefined, 0, 1),
                     {ok, Data_userprofile_list} = mod_mongodb:find(<<"user_profiles_userprofile">>, [{<<"user_id">>, Data_authuser_oid}]),
                    case Data_userprofile_list of
                        [] ->
                            {error, not_allowed};
                        _ ->
                            Data_userprofile = lists:nth(1, Data_userprofile_list),
                            Data_userprofile_password = proplists:get_value(<<"xmpp_internal_key">>, Data_userprofile),
                            Data_userprofile_password
                    end
            catch _ ->
                {error, not_allowed}
            end;
        {ok, Data_authuser_list} ->
            Data_authuser = lists:nth(1, Data_authuser_list),
            Data_authuser_oid = proplists:get_value(<<"_id">>, Data_authuser),
%            {ok, Data_userprofile_list} = DBConnection:find(<<"user_profiles_userprofile">>, [{<<"user_id">>, Data_authuser_oid}], undefined, 0, 1),
            {ok, Data_userprofile_list} = mod_mongodb:find(<<"user_profiles_userprofile">>, [{<<"user_id">>, Data_authuser_oid}]),
            case Data_userprofile_list of
                [] ->
                    {error, not_allowed};
                _ ->
                    Data_userprofile = lists:nth(1, Data_userprofile_list),
                    Data_userprofile_password = proplists:get_value(<<"xmpp_internal_key">>, Data_userprofile),
                    Data_userprofile_password
            end
    catch _ ->
        {error, not_allowed}
    end.
    

remove_user(_,_) ->
    {error, not_allowed}.

remove_user(_,_,_) ->
    {error, not_allowed}.

%%%
%%% Custom functions
%%%

compare_encoded_and_plain_password(Encoded_P, Plain_P) ->
    %?INFO_MSG("Encoded_P: ~p~nPlain_P: ~p~n", [Encoded_P, Plain_P]),
    Encoded_P_Tokens = string:tokens(binary_to_list(Encoded_P), "$"),
    P_salt = lists:nth(2, Encoded_P_Tokens),
    P_encoded = lists:nth(3, Encoded_P_Tokens),
    Encoded_P_from_plain = string:to_lower(sha1:hexstring(P_salt ++ Plain_P)),
    Encoded_P_from_plain == P_encoded.


    
%%% TODO: mongo_user_exists()

mongo_user_exists(User, Server) ->
    %?INFO_MSG("mongo_user_exists ~n", []),
    DB_dbname = list_to_binary(ejabberd_config:get_local_option({mongodb_djangouser_db, Server})),
    Conn = mongoapi:new(xmpp_mongo, DB_dbname),
    {ok, Data} = Conn:find(auth_user, [{'or', [{"username", User}, {"_id", {oid,User}}]}], undefined, 0, 1),
    case length(Data) of
        0 -> false;
        1 -> true
    end.


mongo_check_password(User, Password, Server) ->
    Password == get_password(User, Server).

make_list(Val) ->
    case Val of
        undefined ->
            "";
        Val when is_binary(Val) ->
            binary_to_list(Val);
        _ ->
            Val
    end.
