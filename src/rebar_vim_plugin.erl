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
-module(rebar_vim_plugin).

%% rebar command api
-export([vimct/2]).
-export([vimeunit/2]).

%% rebar command api

-spec vimct(term(), term()) -> ok | {error, term()}.
vimct(Config, AppFile) ->
    Config2 = make_verbose(Config),
    Config3 = add_hook(Config2),
    rebar_ct:ct(Config3, AppFile).

-spec vimeunit(term(), term()) -> ok | {error, term()}.
vimeunit(Config, AppFile) ->
    Config2 = add_listener(Config),
    rebar_eunit:eunit(Config2, AppFile).

%% internal

make_verbose(Config) ->
    case rebar_config:is_verbose(Config) of
         true ->
             Config;
         false ->
             %% This doesn't actually change the level for rebar_log but should
             %% trick the ct command into thinking verbose mode is on.
             VerboseLevel = rebar_log:default_level() + 1,
             rebar_config:set_global(Config, verbose, VerboseLevel)
    end.

add_hook(Config) ->
    Params = rebar_config:get(Config, ct_extra_params, ""),
    Params2 =  add_hook_params(Params),
    rebar_config:set(Config, ct_extra_params, Params2).

add_hook_params(Params) ->
    case re:replace(Params, "-ct_hooks ",
                   "-ct_hooks rebar_vim_hook [] and ", [{return, list}]) of
        Params ->
            "-ct_hooks rebar_vim_hook [] " ++ Params;
        Params2 ->
            Params2
    end.

add_listener(Config) ->
    Opts = rebar_config:get_list(Config, eunit_opts, []),
    Opts2 = [{report, {rebar_vim_listener, []}} | Opts],
    rebar_config:set(Config, eunit_opts, Opts2).
