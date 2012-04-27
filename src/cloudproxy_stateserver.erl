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

-module(cloudproxy_stateserver).
-behaviour(gen_server).

-record(state,
	{
		logAttack     = false,
		attackGateway = undefined
	}
).

%% gen_server callbacks
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

%% public APIs
-export(
	[
		start/1,
		logForAttackTurnedOn/0,
		getAttackGateway/0
	]).

start(Config) ->
	error_logger:info_report("stateserver start called"),
	gen_server:start_link({local,?MODULE}, ?MODULE, Config, []).


logForAttackTurnedOn() -> gen_server:call(?MODULE,  logforattackturnedon).
getAttackGateway()     -> gen_server:call(?MODULE,  getattackgateway).



%% gen_server callbacks

init(Config) ->
	error_logger:info_report("stateserver init called"),
	{ok, LogAttack}     = get_option(log_attack, Config),
	{ok, AttackGateway} = get_option(attack_gateway, Config),
	State = #state{logAttack = LogAttack, attackGateway = AttackGateway},
	{ok, State}.


handle_call(logforattackturnedon, _From, Context) ->
	{reply, Context#state.logAttack, Context};

handle_call(getattackgateway, _From, Context) ->
	{reply, Context#state.attackGateway, Context};



handle_call(stop, _From, State) ->
	{stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

terminate(_Reason, _State) ->
	error_logger:info_report("stateserver terminate called"),
	ok.

%% default implementation of some other callbacks
handle_cast(_Message, State) -> {noreply, State}.
handle_info(_Message, State) -> {noreply, State}.
code_change(_OldVersion, State, _Extra) -> {ok, State}.


%% Utils
%%

get_option(Option, Options) ->
	case lists:keyfind(Option, 1, Options) of
		false -> {ok, foo};
		{Option, Value} -> {ok, Value}
	end.
