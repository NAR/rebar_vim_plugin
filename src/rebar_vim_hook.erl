%% Copyright (c) 2013 James Fish
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
-module(rebar_vim_hook).

%% ct hook api

-export([id/1]).
-export([init/2]).
-export([pre_init_per_suite/3]).
-export([post_init_per_suite/4]).
-export([pre_end_per_suite/3]).
-export([post_end_per_suite/4]).
-export([pre_init_per_group/3]).
-export([post_init_per_group/4]).
-export([pre_end_per_group/3]).
-export([post_end_per_group/4]).
-export([pre_init_per_testcase/3]).
-export([post_end_per_testcase/4]).
-export([on_tc_fail/3]).
-export([on_tc_skip/3]).
-export([terminate/1]).

-record(state,  {rebar_vim_device :: rebar_vim_io:device(),
                 suite = undefined :: 'undefined' | {'suite', atom()},
                 file = undefined :: 'undefined' | string(),
                 dict = undefined :: 'undefined' | dict(),
                 group = undefined :: 'undefined' | {'group', atom()},
                 failed = 0 :: non_neg_integer(),
                 skipped = 0 :: non_neg_integer(),
                 total = 0 :: non_neg_integer()}).

-type state() :: #state{}.
-type config() :: list().
-type return() :: term().
-type reason() :: term().

-spec id(list()) -> user.
id(_Opts) ->
    user.

-spec init(user, list()) -> {ok, state()}.
init(Id, _Opts) ->
    {ok, RVDevice} = rebar_vim_io:open(Id),
    {ok, #state{rebar_vim_device=RVDevice, total=0}}.

-spec pre_init_per_suite(module(), config(), state()) -> {config(), state()}.
pre_init_per_suite(Suite, Config, State) ->
    File = rebar_vim_util:module_to_source(Suite),
    Dict = rebar_vim_util:module_to_dict(Suite),
    {Config, State#state{suite={suite, Suite}, file=File, dict=Dict}}.

-spec post_init_per_suite(module(), config(), return(), state()) ->
    {return(), state()}.
post_init_per_suite(_Suite, _Config, Return, State) ->
    {Return, State}.

-spec pre_end_per_suite(module(), config(), state()) -> {config(), state()}.
pre_end_per_suite(_Suite, Config, State) ->
    {Config, State}.

-spec post_end_per_suite(module(), config(), return(), state()) ->
    {return(), state()}.
post_end_per_suite(_Suite, _Config, Return, State) ->
    {Return, State#state{suite=undefined, file=undefined, dict=undefined}}.

-spec pre_init_per_group(atom(), config(), state()) -> {config(), state()}.
pre_init_per_group(Group, Config, State) ->
    {Config, State#state{group={group, Group}}}.

-spec post_init_per_group(atom(), config(), return(), state()) ->
    {return(), state()}.
post_init_per_group(_Group, _Config, Return, State) ->
    {Return, State}.

-spec pre_end_per_group(atom(), config(), state()) -> {config(), state()}.
pre_end_per_group(_Group, Config, State) ->
    {Config, State}.

-spec post_end_per_group(atom(), config(), return(), state()) ->
    {return(), state()}.
post_end_per_group(_Group, _Config, Return, State) ->
    {Return, State#state{group=undefined}}.

-spec pre_init_per_testcase(atom(), config(), state()) -> {config(), state()}.
pre_init_per_testcase(_TC, Config, State = #state{total=Total}) ->
    {Config, State#state{total=(Total+1)}}.

-spec post_end_per_testcase(atom(), config(), return(), state()) ->
    {return(), state()}.
post_end_per_testcase(_TC, _Config, Return, State) ->
    {Return, State}.

-spec on_tc_fail(atom(), reason(), state()) -> state().
on_tc_fail(TC, Reason, State = #state{suite={suite, _Suite}, file=File,
                                      dict=Dict, group=Group, failed=Failed,
                                      rebar_vim_device=RVDevice}) ->
    write(RVDevice, Dict, failed, File, Group, TC, Reason),
    State#state{failed=(Failed+1)}.

-spec on_tc_skip(atom(), reason(), state()) -> state().
on_tc_skip(TC, Reason, State = #state{suite={suite, _Suite}, file=File,
                                      dict=Dict, group=Group, skipped=Skipped,
                                      rebar_vim_device=RVDevice}) ->
    write(RVDevice, Dict, skipped, File, Group, TC, Reason),
    State#state{skipped=(Skipped+1)}.

-spec terminate(state()) -> ok.
terminate(#state{rebar_vim_device=RVDevice}) ->
    rebar_vim_io:close(RVDevice).

%% internal

write(RVDevice, Dict, ErrorType, File, Group, TC, Reason) ->
    Line = get_line(TC, Dict),
    Arity = get_arity(TC),
    Reason2 = rebar_vim_io:format_reason(Group, TC, Arity, Reason),
    rebar_vim_io:format(RVDevice, File, Line, ErrorType, Reason2).

get_line(TC, Dict) ->
    case dict:find(TC, Dict) of
         {ok, Line} ->
             Line;
         _error ->
             none
    end.

get_arity(TC) when TC == init_per_group orelse TC == end_per_group ->
    2;
get_arity(_TC) ->
    1.
