%% @author Bruno Pedro <>
%% @copyright 2010 tarpipe.com.

%% @doc TEMPLATE.

-module(webnesia_response).
-author('2010 <bpedro@tarpipe.com>').

-export ([encode/1]).
-export ([encode_records/4]).

%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
encode (Data) ->
    mochijson2:encode(Data).

%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
encode_records (Records, Size, Limit, Offset) ->
    encode({struct, [{total_rows, Size}, {number_of_rows, length(Records)}, {limit, Limit}, {offset, Offset}, {rows, records_to_structs(Records)}]}).

%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
records_to_structs ([]) ->
    [];

%--------------------------------------------------------------------
%% @doc
%%
%% Convert {Table, [V1, V2, ...]} info {struct, [{K1, V1}]} ??
%%
%% @end
%%--------------------------------------------------------------------
records_to_structs ([Record | Tail]) ->
    [Table | Values] = tuple_to_list(Record),
    Attributes = mnesia:table_info(Table, attributes),
    lists:merge(fun(_, _) -> true end, [{struct, lists:zip(Attributes, Values)}], records_to_structs(Tail)).

