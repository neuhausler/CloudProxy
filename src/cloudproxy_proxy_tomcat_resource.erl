%% @author Marcel Neuhausler
%% @copyright 2012 Marcel Neuhausler
%%
%%    Licensed under the Apache License, Version 2.0 (the "License");
%%    you may not use this file except in compliance with the License.
%%    You may obtain a copy of the License at
%%
%%        http://www.apache.org/licenses/LICENSE-2.0
%%
%%    Unless required by applicable law or agreed to in writing, software
%%    distributed under the License is distributed on an "AS IS" BASIS,
%%    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%    See the License for the specific language governing permissions and
%%    limitations under the License.

-module(cloudproxy_proxy_tomcat_resource).

-export([init/1, service_available/2]).

-include_lib("webmachine/include/webmachine.hrl").

init(Config) ->
%%	{{trace, "/tmp"}, Config}.
	{ok, Config}.


%% if tomcat isn't up, we return 503 Service Unavailable, as expected
service_available(RequestData, Context={_ExternalPath, TomcatPath}) ->
	Headers = cloudproxy_utils:clean_request_headers(mochiweb_headers:to_list(wrq:req_headers(RequestData))),
    Method  = cloudproxy_utils:wm_to_ibrowse_method(wrq:method(RequestData)),
    
    Path = lists:append(
		[
			TomcatPath,
			wrq:disp_path(RequestData),
			case wrq:req_qs(RequestData) of
				[] -> [];
				Qs -> [$?|mochiweb_util:urlencode(Qs)]
			end
		]),
    
	ReqBody = case wrq:req_body(RequestData) of
		undefined -> [];
		Body -> Body
	end,

	case ibrowse:send_req(Path, Headers, Method, ReqBody) of
		{ok, Status, CouchHeaders, RespBody} ->
			RespHeaders = cloudproxy_utils:fix_location(CouchHeaders, Context),
			{{halt, list_to_integer(Status)}, wrq:set_resp_headers(RespHeaders, wrq:set_resp_body(RespBody, RequestData)), Context};
		_Otherwise ->
			{false, RequestData, Context}
    end.

