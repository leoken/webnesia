%% @author Bruno Pedro <bpedro@tarpipe.com>
%% @copyright 2010 tarpipe.com.

%% @doc TEMPLATE.

-module(webnesia_db).
-author('Bruno Pedro <bpedro@tarpipe.com>').

-include_lib("stdlib/include/qlc.hrl").

-export ([start/0]).
-export ([info/1]).
-export ([tables/0]).
-export ([create_table/2]).
-export ([delete_table/1]).
-export ([list/1]).
-export ([list/2]).
-export ([list/3]).
-export ([save/2]).
-export ([read/2]).

%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
start () ->
    mnesia:create_schema([node()]),
    webnesia_response:encode(mnesia:start()).

%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
info (Table) ->
    webnesia_response:encode({struct, [
                                {table_name, list_to_atom(Table)},
                                {attributes, mnesia:table_info(list_to_atom(Table), attributes)},
                                {number_of_attributes, mnesia:table_info(list_to_atom(Table), arity) - 1},
                                {size, mnesia:table_info(list_to_atom(Table), size)}
                               ]}).

%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
tables () ->
    webnesia_response:encode(mnesia:system_info(tables)).

%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
create_table (Table, JSONData) ->
    Attributes = [list_to_atom(binary_to_list(Attribute)) || Attribute <- mochijson2:decode(JSONData)],
    case mnesia:create_table(list_to_atom(Table), [{attributes, Attributes}]) of
        {atomic, ok} ->
            webnesia_response:encode(ok);
        {aborted, Reason} ->
            webnesia_response:encode({struct, [Reason]})
    end.

%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
delete_table (Table) ->
    case mnesia:delete_table(list_to_atom(Table)) of
        {atomic, ok} ->
            webnesia_response:encode(ok);
        {aborted, Reason} ->
            webnesia_response:encode({struct, [Reason]})
    end.

%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
list (Table, 0, Offset) ->
    list(Table, mnesia:table_info(list_to_atom(Table), size), Offset);

%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
list (Table, Limit, 0) ->
    list(Table, Limit);

%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
list (Table, Limit, Offset) ->
    {atomic, Records} = mnesia:transaction(fun() -> C = qlc:cursor(qlc:q([X||X<-mnesia:table(list_to_atom(Table))])), qlc:next_answers(C, Offset), qlc:next_answers(C, Limit) end ),
    webnesia_response:encode_records(Records, mnesia:table_info(list_to_atom(Table), size), Limit, Offset).

%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
list (Table, 0) ->
    list(Table);

%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
list (Table, Limit) ->
    {atomic, Records} = mnesia:transaction(fun() -> C = qlc:cursor(qlc:q([X||X<-mnesia:table(list_to_atom(Table))])), qlc:next_answers(C, Limit) end ),
    webnesia_response:encode_records(Records, mnesia:table_info(list_to_atom(Table), size), Limit, 0).

%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
list (Table) ->
    {atomic, Records} = mnesia:transaction(fun() -> mnesia:match_object(list_to_atom(Table), mnesia:table_info(list_to_atom(Table), wild_pattern), write) end),
    webnesia_response:encode_records(Records, mnesia:table_info(list_to_atom(Table), size), mnesia:table_info(list_to_atom(Table), size), 0).

%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
save (Table, JSONData) ->
    try mochijson2:decode(JSONData) of
        {struct, Data} ->
            Record = list_to_tuple([list_to_atom(Table)] ++ [ Value || {_, Value} <- Data ]),
            webnesia_response:encode(tuple_to_list(mnesia:transaction(fun() -> mnesia:write(list_to_atom(Table), Record, write) end)))
    catch
        throw:Term -> Term;
        exit:Reason -> {'EXIT',Reason};
        error:Reason -> {'ERROR',{Reason,erlang:get_stacktrace()}}
    end.

%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
read (Table, Key) ->
    {atomic, Records} = mnesia:transaction(fun() -> mnesia:read(list_to_atom(Table), mochijson2:decode(Key)) end),
    webnesia_response:encode_records(Records, mnesia:table_info(list_to_atom(Table), size), mnesia:table_info(list_to_atom(Table), size), 0).