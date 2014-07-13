%%
%%   Copyright (c) 2012, Dmitry Kolesnikov
%%   All Rights Reserved.
%%
%%   Licensed under the Apache License, Version 2.0 (the "License");
%%   you may not use this file except in compliance with the License.
%%   You may obtain a copy of the License at
%%
%%       http://www.apache.org/licenses/LICENSE-2.0
%%
%%   Unless required by applicable law or agreed to in writing, software
%%   distributed under the License is distributed on an "AS IS" BASIS,
%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%   See the License for the specific language governing permissions and
%%   limitations under the License.
%%
-module(pns_tests).
-include_lib("eunit/include/eunit.hrl").

app_test() ->
   application:start(pns).

register_test() ->
   ok = pns:register(a, self()),
   ok = pns:register(test, b, self()),
   ok = pns:register(test, {a, b}, self()),
   {'EXIT', _} = (catch pns:register(a, self()) ),
   {'EXIT', _} = (catch pns:register(test, b, self()) ),
   {'EXIT', _} = (catch pns:register(test, {a, b}, self()) ).

whereis_test() ->
   Self = self(),
   Self = pns:whereis(a),
   Self = pns:whereis(test, b),
   Self = pns:whereis(test, {a, b}).

re_register_test() ->
   spawn(fun() -> ok = pns:register(test, c, self()) end),
   timer:sleep(100),
   ok   = pns:register(test, c, self()),
   Self = self(),
   Self = pns:whereis(test, c).

multiple_test() ->
   Pid  = spawn(fun() -> ok = pns:register(test, d, [self()]), timer:sleep(10000) end),
   timer:sleep(100),
   ok   = pns:register(test, d, [self()]),
   true = lists:member(Pid,    pns:whereis(test, d)),
   true = lists:member(self(), pns:whereis(test, d)).

unregister_test() ->
   ok = pns:unregister(a),
   ok = pns:unregister(test, b),
   ok = pns:unregister(test, {a,b}).

use_native_test() ->
   ok   = pns:register(a, self()),
   Self = self(),
   Self = erlang:whereis(a).

use_urn_test() ->
   ok   = pns:register({urn, test, b}, self()),
   Self = self(),
   Self = pns:whereis(test, b).





