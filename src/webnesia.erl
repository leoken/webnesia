%% @author Bruno Pedro <bpedro@tarpipe.com>
%% @copyright 2010 tarpipe.com.

%% @doc TEMPLATE.

-module(webnesia).
-author('Bruno Pedro <bpedro@tarpipe.com>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @spec start() -> ok
%% @doc Start the webnesia server.
start() ->
    webnesia_deps:ensure(),
    ensure_started(crypto),
    ensure_started(mnesia),
    application:start(webnesia).

%% @spec stop() -> ok
%% @doc Stop the webnesia server.
stop() ->
    Res = application:stop(webnesia),
    application:stop(crypto),
    application:stop(mnesia),
    Res.
