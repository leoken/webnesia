%% @author Bruno Pedro <bpedro@tarpipe.com>
%% @copyright 2010 tarpipe.com.

%% @doc Callbacks for the webnesia application.

-module(webnesia_app).
-author('Bruno Pedro <bpedro@tarpipe.com>').

-behaviour(application).
-export([start/2, stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for webnesia.
start(_Type, _StartArgs) ->
    webnesia_deps:ensure(),
    webnesia_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for webnesia.
stop(_State) ->
    ok.


%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
