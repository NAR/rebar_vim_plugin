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
-module(rebar_vim_listener).

-behaviour(eunit_listener).

-export([start/1]).

-export([init/1]).
-export([handle_begin/3]).
-export([handle_end/3]).
-export([handle_cancel/3]).
-export([terminate/2]).

-record(state, {rebar_vim_device :: rebar_vim_io:device(),
                sources = dict:new() :: dict(),
                groups = dict:new() :: dict()}).

-type state() :: #state{}.
-type data() :: [{atom(), term()}].

-spec start(list()) -> pid() | {pid(), reference()}.
start(Opts) ->
    eunit_listener:start(?MODULE, Opts).

-spec init(list()) -> state().
init(_Opts) ->
    {ok, RVDevice} = rebar_vim_io:open(user),
    #state{rebar_vim_device=RVDevice}.

-spec handle_begin(test | group, data(), state()) -> state().
handle_begin(group, Data, State = #state{groups=Groups}) ->
    {id, Id} = lists:keyfind(id, 1, Data),
    case lists:keyfind(desc, 1, Data) of
        {desc, ""} ->
            Groups2 = dict:store(Id, undefined, Groups),
            State#state{groups=Groups2};
        {desc, Desc} ->
            Groups2 = dict:store(Id, Desc, Groups),
            State#state{groups=Groups2};
        false ->
            State
    end;
handle_begin(test, Data, State = #state{sources=Sources}) ->
    Module = get_module(Data),
    case dict:is_key(Module, Sources) of
        true ->
            State;
        false ->
            File = rebar_vim_util:module_to_source(Module),
            Sources2 = dict:store(Module, File, Sources),
            State#state{sources=Sources2}
    end.

-spec handle_end(group | test, data(), state()) -> state().
handle_end(group, _Data, State) ->
    State;
handle_end(test, Data, State) ->
    case lists:keyfind(status, 1, Data) of
        {status, ok} ->
            State;
        {status, {skipped, Reason}} ->
            handle_error(skipped, Reason, Data, State);
        {status, {error, Reason}} ->
            handle_error(failed, Reason, Data, State)
    end.

-spec handle_cancel(group | test, data(), state()) -> state().
handle_cancel(group, _Data, State) ->
    State;
handle_cancel(test, Data, State) ->
    {reason, Reason} = lists:keyfind(reason, 1, Data),
    handle_error(skipped, Reason, Data, State).

-spec terminate(data(), state()) -> ok.
terminate(Data, #state{rebar_vim_device=RVDevice}) ->
    rebar_vim_io:write(RVDevice, Data),
    rebar_vim_io:close(RVDevice).

%% internal

handle_error(ErrorType, Reason, Data, State = #state{rebar_vim_device=RVDevice,
                                                     sources=Sources,
                                                     groups=Groups}) ->
    Module = get_module(Data),
    File = dict:fetch(Module, Sources),
    Line = proplists:get_value(line, Data, none),
    Reason2 = format_reason(ErrorType, Reason, Data, Groups),
    rebar_vim_io:format(RVDevice, File, Line, ErrorType, Reason2),
    State.

get_module(Data) ->
    {source, {Module, _Function, _Arity}} = lists:keyfind(source, 1, Data),
    Module.

format_reason(ErrorType, Reason, Data, Groups) ->
    {source, {_Module, Function, Arity}} = lists:keyfind(source, 1, Data),
    {id, Id} = lists:keyfind(id, 1, Data),
    Group = get_group_desc(Id, Groups),
    rebar_vim_io:format_reason(Group, Function, Arity, {ErrorType, Reason}).

get_group_desc(Id, Groups) ->
    [_|GroupIdRev] = lists:reverse(Id),
    GroupId = lists:reverse(GroupIdRev),
    case dict:find(GroupId, Groups) of
        {ok, undefined} ->
            undefined;
        {ok, Desc} ->
            {group, Desc};
        error ->
            undefined
    end.
