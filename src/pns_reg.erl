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
%%     local registry
-module(pns_reg).
-author('Dmitry Kolesnikov <dmkolesnikov@gmail.com>').

-export([
   start_link/0 
  ,init/1
  ,terminate/2
  ,handle_call/3
  ,handle_cast/2
  ,handle_info/2
  ,code_change/3
]).

%%
%% create new registry
-spec(start_link/0 :: () -> {ok, pid()} | {error, any()}).

start_link() ->
   gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%-----------------------------------------------------------------------------
%%
%% gen_server
%%
%%-----------------------------------------------------------------------------

init(_) ->
   _ = ets:new(pns, [
      public,
      named_table,
      ordered_set,   %% !? or set
      {read_concurrency, true}
   ]),
   {ok, undefined}.

terminate(_, _) ->
   ok.

%%
%% server-based registration serializes access to registry 
%% allow to avoid race condition on key
handle_call({register, {{Ns, Key}, Val}=Req}, _, S) ->
   case pns:whereis(Ns, Key) of
      undefined ->
         ets:insert(pns, Req),
         {reply, ok, S};
      L when is_list(L), is_list(Val) ->
         ets:insert(pns, {{Ns, Key}, lists:usort(Val ++ L)}),
         {reply, ok, S};
      L when is_list(L) ->
         ets:insert(pns, {{Ns, Key}, lists:usort([Val|L])}),
         {reply, ok, S};
      _         ->
         {reply, conflict, S}
   end;

handle_call(_, _, S) ->
   {noreply, S}.

%%
handle_cast(_, S) ->
   {noreply, S}.

%%
handle_info(_, S) ->
   {noreply, S}.

%%
code_change(_OldVsn, S, _Extra) ->
   {ok, S}. 

%%-----------------------------------------------------------------------------
%%
%% private
%%
%%-----------------------------------------------------------------------------

   