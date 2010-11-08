%% @author Bruno Pedro <bpedro@tarpipe.com>
%% @copyright 2010 tarpipe.com.

%% @doc Web server for webnesia.

-module(webnesia_web).
-author('Bruno Pedro <bpedro@tarpipe.com>').

-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            case Path of
                "db/_start" ->
                    Req:respond({200, [{"Content-type", "application/json"}], webnesia_db:start()});
                "_all_tables" ->
                    Req:respond({200, [{"Content-type", "application/json"}], webnesia_db:tables()});
                "_utils/" ++ File ->
                    Req:serve_file(File, DocRoot);
                "favicon.ico" ->
                    Req:not_found();
                Table ->
                    case re:run(Table, "(.+?)/(.+)", [{capture, [1, 2]}]) of
                        {match, Matches} ->
                            QueryString = Req:parse_qs(),
                            [TableName, What] = [ string:substr(Table, StartIndex + 1, Count) || {StartIndex, Count} <- Matches],
                            case What of
                                "_all_records" ->
                                    Req:respond({200, [{"Content-type", "application/json"}], webnesia_db:list(TableName, list_to_integer(proplists:get_value("limit", QueryString, "0")), list_to_integer(proplists:get_value("skip", QueryString, "0")))});
                                Id ->
                                    Req:respond({200, [{"Content-type", "application/json"}], webnesia_db:read(TableName, Id)})
                            end;     
                        _ ->
                            Req:respond({200, [{"Content-type", "application/json"}], webnesia_db:info(Table)})
                    end
            end;
        'POST' ->
            case Path of
                Table ->
                    Req:respond({200, [{"Content-type", "application/json"}], webnesia_db:save(Table, Req:recv_body())})
            end;
        'PUT' ->
            case Path of
                Table ->
                    Req:respond({200, [{"Content-type", "application/json"}], webnesia_db:create_table(Table, Req:recv_body())})
            end;
        'DELETE' ->
            Req:respond({200, [{"Content-type", "text/html"}], webnesia_db:delete_table(Path)});
        _ ->
            Req:respond({501, [], []})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
