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

-module(cloudproxy_wm_cloudrover_execute).

-export([init/1, service_available/2]).

-include_lib("webmachine/include/webmachine.hrl").

init(Config) ->
%%	{{trace, "/tmp"}, Config}.
	{ok, Config}.


service_available(RequestData, Context={mapping, MappingFile}) ->
	{ok, Mapping}   = file:consult(filename:join([filename:dirname(code:which(?MODULE)), "..", MappingFile])),
	{ok, Locations} = get_option(locations, Mapping),
	{ok, Cmds}      = get_option(cmds,      Mapping),

	{ok, Location} = dict:find(location, wrq:path_info(RequestData)),
	{ok, Domain}   = dict:find(domain,   wrq:path_info(RequestData)),
	{ok, Cmd}      = dict:find(cmd,      wrq:path_info(RequestData)),

	case list_to_atom(Location) of
		all -> Destinations = Locations;
		_OtherWise -> Destinations = [lists:keyfind(list_to_atom(Location), 1, Locations)]
	end,

	{ok, CmdInfo} = get_option(list_to_atom(Domain ++ "." ++ Cmd), Cmds),
  	{ok, CmdURL}  = get_option(cmd, CmdInfo),
	
	run_it(Locations, Destinations, CmdURL),
	{{halt, 200}, wrq:set_resp_headers([], wrq:set_resp_body("done", RequestData)), Context}.


run_it(_Locations, [], _CmdURL) ->
	done;
	
run_it(Locations, [{_, BaseURL}|Tail], CmdURL) ->
	CloudRoverURL = BaseURL ++ "/sh" ++ CmdURL,
	ibrowse:send_req(CloudRoverURL, [], get),
	run_it(Locations, Tail, CmdURL).


%% Utils

get_option(Option, Options) ->
	case lists:keyfind(Option, 1, Options) of
		false -> {ok, foo};
		{Option, Value} -> {ok, Value}
	end.

