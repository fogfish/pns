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
-author(dmkolesnikov@gmail.com).
-include_lib("stdlib/include/qlc.hrl").

-export([
   start/0,

   %% process registry api
   register/1, register/2, register/3, 
   unregister/1, unregister/2, 
   whereis/1, whereis/2, 
   lookup/1,  lookup/2,
   map/2, fold/3,
   '!'/2, '!'/3, 
   pid/1, pid/2,

   %% attribute registry
   put/2, put/3, 
   get/1, get/2,
   remove/1, remove/2
]).

%%
%% start application
start() ->
   application:start(pns).

%%
%% associates key with a pid
%% fails with badarg if association exists 
%% and associated process is alive
-spec(register/1 :: (any()) -> ok).
-spec(register/2 :: (any(), any()) -> ok).
-spec(register/3 :: (any(), any(), pid()) -> ok).

register(Key) ->
   pns:register(local, Key).

register(Ns, Key) ->
   pns:register(Ns, Key, self()).

register(Ns, Key, Pid)
 when is_pid(Pid) ->
   case ets:insert_new(pns, {{Ns, Key}, Pid}) of
      true  -> 
         ok;
      false ->
         % existed key might be associated with dead processes
         % parallel register to same key are plausible
         % handle update via server
         case gen_server:call(pns_reg, {register, Ns, Key, Pid}) of
            ok -> 
               ok;
            _  -> 
               throw({badarg, {Ns, Key}})
         end
   end.

%%
%% removes key registration
-spec(unregister/1 :: (any()) -> ok).
-spec(unregister/2 :: (any(), any()) -> ok).

unregister(Key) ->
   pns:unregister(local, Key).

unregister(Ns, Key) ->
   ets:delete(pns, {Ns, Key}),
   ok.

%%
%% returns the pid associated with key. 
%% returns undefined if the name is not registered or process is dead   
-spec(whereis/1 :: (any()) -> pid() | undefined).
-spec(whereis/2 :: (any(), any()) -> pid() | undefined).

whereis(Key) ->
   pns:whereis(local, Key).

whereis(Ns, Key) ->
   case ets:lookup(pns, {Ns, Key}) of
      [{_, Pid}] when is_pid(Pid) ->
         case is_process_alive(Pid) of
            true  -> Pid;
            false -> undefined
         end;      
      _            -> 
         undefined
   end.   
 
%%
%% lookup
-spec(lookup/1 :: (any()) -> [pid()]).
-spec(lookup/2 :: (any(), any()) -> [pid()]).

lookup(Mask) ->
   lookup(local, Mask).

lookup(Ns, Mask) ->
   List = ets:select(pns, [{ {{Ns, Mask}, '_'}, [], ['$_'] }]),
   [{Key, Pid} || {{_, Key}, Pid} <- List, is_pid(Pid), is_process_alive(Pid)].

%%
%% map function over name space
%% Fun = fun({Uid, Pid}) 
-spec(map/2 :: (function(), atom()) -> list()).

map(Fun, Ns0) ->
   qlc:e(
      qlc:q([ 
         Fun({Uid, Pid}) 
         || {{Ns, Uid}, Pid} <- ets:table(pns), Ns =:= Ns0, is_pid(Pid), is_process_alive(Pid)
      ])
   ).
   
%%
%% fold function
-spec(fold/3 :: (function(), any(), atom()) -> list()).

fold(Fun, Acc0, Ns0) ->
   qlc:fold(Fun, Acc0, 
      qlc:q([ 
         {Uid, Pid} || {{Ns, Uid}, Pid} <- ets:table(pns), Ns =:= Ns0, is_pid(Pid), is_process_alive(Pid)
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


%%
%%
put(Key, Val) ->
   pns:put(local, Key, Val).

put(Ns, Key, Val) ->
   ets:insert(pns, {{Ns, Key}, Val}),
   ok.

%%
%%
get(Key) ->
   pns:get(local, Key).

get(Ns, Key) ->
   case ets:lookup(pns, {Ns, Key}) of
      [{_, Val}] -> Val;
      _          -> undefined
   end.   

remove(Key) ->
   pns:remove(local, Key).

remove(Ns, Key) ->
   ets:delete(pns, {Ns, Key}),
   ok.
