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
%%   @description
%%     process name-space
-module(pns).
-author('Dmitry Kolesnikov <dmkolesnikov@gmail.com>').
-include_lib("stdlib/include/qlc.hrl").

-export([start/0]).
-export([
   register/2
,  register/3
,  register_name/2
,  unregister/1
,  unregister/2
,  unregister_name/1
,  whereis/1
,  whereis/2
,  whereis_name/1
,  lookup/1
,  lookup/2
,  map/2
,  fold/3
,  '!'/2
,  '!'/3
,  pid/1
,  pid/2
]).

-export_type([ key/0 ]).

-type key() :: {urn, atom(), _} | _.

%%
%% start application
start() ->
   application:start(pns).

%%
%% associates key with a pid
%% fails with badarg if association exists 
%% and associated process is alive
-spec register(any(), any()) -> ok.
-spec register(any(), any(), any()) -> ok.

register(Key, Pid)
 when is_atom(Key) ->
   erlang:register(Key, Pid), ok;
register({urn, Ns, Key}, Pid) ->
   pns:register(Ns, Key, Pid);
register(Key, Pid) ->
   pns:register(local, Key, Pid).

register(Ns, Key, Pid) ->
   case ets:insert_new(pns, new_val(Ns, Key, Pid)) of
      true  -> 
         ok;
      false ->
         % existed key might be associated with dead processes
         % parallel register to same key are plausible
         % handle update via server to serialize writes
         case gen_server:call(pns_reg, {register, new_val(Ns, Key, Pid)}) of
            ok -> 
               ok;
            _  -> 
               exit({badarg, {Ns, Key}})
         end
   end.

%%
%% register support for `via` protocol
-spec register_name(key(), pid()) -> yes.

register_name(Key, Pid) ->
   ok = pns:register(Key, Pid),
   yes.


%%
%% removes key registration
-spec unregister(any()) -> ok.
-spec unregister(any(), any()) -> ok.

unregister(Key)
 when is_atom(Key) ->
   erlang:unregister(Key), ok;
unregister({urn, Ns, Key}) ->
   pns:unregister(Ns, Key);
unregister(Key) ->
   pns:unregister(local, Key).

unregister(Ns, Key) ->
   ets:delete(pns, {Ns, Key}),
   ok.

%%
%% unregister support for `via` protocol
-spec unregister_name(key()) -> ok.

unregister_name(Key) ->
   pns:unregister(Key).

%%
%% returns the pid associated with key. 
%% returns undefined if the name is not registered or process is dead   
-spec whereis(any()) -> pid() | undefined.
-spec whereis(any(), any()) -> pid() | undefined.

whereis(Key)
 when is_atom(Key) ->
   erlang:whereis(Key);
whereis({urn, Ns, Key}) ->
   pns:whereis(Ns, Key);
whereis(Key) ->
   pns:whereis(local, Key).

whereis(Ns, Key) ->
   case ets:lookup(pns, {Ns, Key}) of
      [Val] -> get_val(Val);
      _     -> undefined
   end.   

%%
%% returns pid associated with key, support for `via` protocol
-spec whereis_name(key()) -> pid().

whereis_name(Key) ->
   pns:whereis(Key).

%%
%% lookup
-spec lookup(any()) -> [pid()].
-spec lookup(any(), any()) -> [pid()].

lookup(Mask) ->
   lookup(local, Mask).

lookup(Ns, Mask) ->
   List = ets:select(pns, [{ {{Ns, Mask}, '_'}, [], ['$_'] }]),
   [{Key, Val} || {{_, Key}, Val} <- List, is_valid(Val)].

%%
%% map function over name space
%% Fun = fun({Uid, Pid}) 
-spec map(function(), atom()) -> list().

map(Fun, Ns0) ->
   qlc:e(
      qlc:q([ 
         Fun({Key, Val}) 
         || {{Ns, Key}, Val} <- ets:table(pns), Ns =:= Ns0, is_valid(Val)
      ])
   ).
   
%%
%% fold function
-spec fold(function(), any(), atom()) -> list().

fold(Fun, Acc0, Ns0) ->
   qlc:fold(Fun, Acc0, 
      qlc:q([ 
         {Uid, Pid} || {{Ns, Uid}, Pid} <- ets:table(pns), Ns =:= Ns0, is_valid(Pid)
      ])
   ).      

%%
%% send message to processes matching mask 
'!'(Mask, Msg) ->
   pns:'!'(local, Mask, Msg).

'!'(Ns, Mask, Msg) ->
   List = ets:select(pns, [{ {{Ns, Mask}, '_'}, [], ['$_'] }]),
   [Pid ! Msg || {{_, _}, Pid} <- List, is_pid(Pid), is_process_alive(Pid)].

%%
%%
pid(Key) ->
   pns:pid(local, Key).

pid(Ns, Key) ->
   fun() -> pns:register(Ns, Key) end.


%%-----------------------------------------------------------------------------
%%
%% private
%%
%%-----------------------------------------------------------------------------

%%
%% check if value is valid
is_valid(X) 
 when is_pid(X) ->
   is_process_alive(X);
is_valid(_) ->
   true.

%%
%% create named value container
new_val(Ns, Key, Pid) ->
   {{Ns, Key}, Pid}.

get_val({{_, _}, Pid})
 when is_pid(Pid) ->
   case is_process_alive(Pid) of
      true  -> Pid;
      false -> undefined
   end;      

get_val({{_, _}, [Pid|_]=Val})
 when is_pid(Pid) ->
   case [X || X <- Val, is_valid(X)] of
      []   -> undefined;
      List -> List
   end;

get_val({{_, _}, Val}) ->
   Val.
