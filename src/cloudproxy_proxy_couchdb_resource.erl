%% Based on https://bitbucket.org/bryan/wmexamples/src/fa8104e75550/src/couchdb_proxy.erl
%%
%% @author Bryan Fink
%% @doc couchdb_proxy is intended to be a simple webmachine resource
%%      for proxying Webmachine requests to CouchDB.  In theory, it's
%%      general enough to be a simple proxy to most any other HTTP
%%      service, but I make no guarantees about assumption it makes that
%%      are CouchDB-specific.
%%
%%      Load this with a dispatch line like:
%%      {['*'], couchdb_proxy, {ExternalPath, CouchPath}}.
%%      Where:
%%        ExternalPath is the base path to this resource, like
%%          "http://localhost:8000/"
%%        CouchPath is the base path to your CouchDB server, like
%%          "http://localhost:5984/"
%%
%%      Another useful example might be:
%%        {["couch", '*'], couchdb_proxy,
%%         {"http://localhost:8000/couch/",
%%          "http://localhost:5984/"}}
%%      Which would redirect requests from
%%        http://localhost:8000/couch/DATABASE/KEY
%%      to
%%        http://localhost:5984/DATABASE/KEY
-module(cloudproxy_proxy_couchdb_resource).
-export([init/1,
         service_available/2]).

-include_lib("webmachine/include/webmachine.hrl").

init(Config) ->
%%	{{trace, "/tmp"}, Config}.
	{ok, Config}.

%% request to couchdb is made in service_available, such that
%% if couchdb isn't up, we return 503 Service Unavailable, as expected
service_available(RP, C={_ExternalPath, CouchPath}) ->
    %% point path at couchdb server
    Path = lists:append(
             [CouchPath,
              wrq:disp_path(RP),
              case wrq:req_qs(RP) of
                  [] -> [];
                  Qs -> [$?|mochiweb_util:urlencode(Qs)]
              end]),

    %% translate webmachine details to ibrowse details
    Headers = cloudproxy_utils:clean_request_headers(mochiweb_headers:to_list(wrq:req_headers(RP))),
    Method = cloudproxy_utils:wm_to_ibrowse_method(wrq:method(RP)),
    ReqBody = case wrq:req_body(RP) of
                  undefined -> [];
                  B -> B
              end,

    case ibrowse:send_req(Path, Headers, Method, ReqBody) of
        {ok, Status, CouchHeaders, RespBody} ->
            RespHeaders = cloudproxy_utils:fix_location(CouchHeaders, C),

            %% stop resource processing here and return whatever
            %% couchdb wanted to return
            {{halt, list_to_integer(Status)},
             wrq:set_resp_headers(RespHeaders,
                                  wrq:set_resp_body(RespBody, RP)),
             C};
        _ ->
            {false, RP, C}
    end.

