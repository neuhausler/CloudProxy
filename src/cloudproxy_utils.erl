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
%%
%%    Some of the functions are adapted from rebar/rebar_utils
%%    https://github.com/basho/rebar/blob/master/src/rebar_utils.erl

-module(cloudproxy_utils).
-export(
	[
		clean_request_headers/1,
		wm_to_ibrowse_method/1,
		fix_location/2
	]).

%%
%% API Functions
%%

%% ibrowse will recalculate Host and Content-Length headers,
%% and will muck them up if they're manually specified
clean_request_headers(Headers) ->
    [{K,V} || {K,V} <- Headers,
              K /= 'Host', K /= 'Content-Length'].

%% webmachine expresses method as all-caps string or atom,
%% while ibrowse uses all-lowercase atom
wm_to_ibrowse_method(Method) when is_list(Method) ->
    list_to_atom(string:to_lower(Method));
wm_to_ibrowse_method(Method) when is_atom(Method) ->
    wm_to_ibrowse_method(atom_to_list(Method)).

%% couchdb returns a fully-qualified URI in Location -
%% hack off the couch host, and drop in this proxy host
fix_location([], _) -> [];
fix_location([{"Location", CouchDataPath}|Rest],
             {ExternalPath, CouchPath}) ->
    DataPath = lists:nthtail(length(CouchPath), CouchDataPath),
    [{"Location", ExternalPath++DataPath}|Rest];
fix_location([H|T], C) ->
    [H|fix_location(T, C)].

