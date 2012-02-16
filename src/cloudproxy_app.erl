%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the cloudproxy application.

-module(cloudproxy_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for cloudproxy.
start(_Type, _StartArgs) ->
    cloudproxy_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for cloudproxy.
stop(_State) ->
    ok.
