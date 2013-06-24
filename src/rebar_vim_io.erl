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
-module(rebar_vim_io).

%% api
-export([open/0]).
-export([open/1]).
-export([write/2]).
-export([format/5]).
-export([close/1]).
-export([format_reason/4]).

-record(device, {mod :: file | io,
                 io_device :: io:device()}).

-type device() :: #device{}.
-type line() :: pos_integer() | none.
-type error_type() :: error | warning | failed | skipped.

-export_type([device/0]).

%% api

-spec open() -> {ok, device()}.
open() ->
    IoDevice = erlang:group_leader(),
    open(IoDevice).

-spec open(io:device() | file:filename()) -> {ok, device()}.
open(IoDevice) when is_pid(IoDevice) orelse is_atom(IoDevice) ->
    {ok, #device{mod=io, io_device=IoDevice}};
open(Filename) when is_binary(Filename) orelse is_list(Filename) ->
    {ok, IoDevice} = file:open(Filename, [write, delayed_write]),
    Filename2 = filename:absname(Filename),
    io:format("Check output file `~s' for details~n~n", [Filename2]),
    {ok, #device{mod=file, io_device=IoDevice}}.

-spec write(device(), term()) -> 'ok'.
write(#device{io_device=IoDevice}, Term) ->
    io:write(IoDevice, Term).

-spec format(device(), file:filename(), line(), error_type(), iolist()) ->
    'ok'.
format(#device{io_device=IoDevice}, Filename, Line, ErrorType, Desc) ->
    Line2 = format_line(Line),
    ErrorType2 = format_error_type(ErrorType),
    io:format(IoDevice, "~s:~s: ~s~s~n", [Filename, Line2, ErrorType2, Desc]).

-spec close(device()) -> 'ok'.
close(#device{mod=file, io_device=IoDevice}) ->
    ok = file:close(IoDevice);
close(_) ->
    ok.

-spec format_reason(atom() | iolist(), atom(), non_neg_integer(), term()) ->
    iolist().
format_reason(Group, Function, Arity, Reason) ->
    Testcase = format_testcase(Group, Function, Arity),
    Reason2 = io_lib:format("~p", [Reason]),
    Reason3 = re:replace(Reason2, "\\s+", "\s", [global, {return, list}]),
    io_lib:format("~s ~s", [Testcase, Reason3]).

%% internal

format_error_type(warning) ->
    "Warning: ";
format_error_type(skipped) ->
    "Skipped: ";
format_error_type(failed) ->
    "Failed: ";
format_error_type(_) ->
    "".

format_line(none) ->
    "none";
format_line(Int) ->
    io_lib:format("~B", [Int]).

format_testcase(undefined, Function, Arity) ->
    format_testcase(Function, Arity);
format_testcase({group, Group}, Function, Arity) ->
    [format_testcase(Function, Arity), io_lib:format(" (~s)", [Group])].

format_testcase(Function, Arity) ->
    io_lib:format("~s/~B", [Function, Arity]).
