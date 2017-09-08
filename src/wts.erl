%%--------------------------------------------------------------------
%% @doc Timestamp support functions.
%%--------------------------------------------------------------------
-module(wts).

-export([timestamp/0,
         to_absolute/1,
         duration/2]).

-export_type([timestamp/0]).

-type microseconds_timestamp() :: integer().
-type native_time() :: integer().
-type native_offset() :: integer().

-type timestamp() :: {native_time(), native_offset()}.

%% @doc Returns a high resolution timestamp.
-spec timestamp() -> timestamp().
timestamp() ->
    {erlang:monotonic_time(), erlang:time_offset()}.

%% @doc Convert the timestamp to an absolute time, in microseconds since the epoch.
-spec to_absolute(timestamp()) -> microseconds_timestamp().
to_absolute({Timestamp, Offset}) ->
    erlang:convert_time_unit(Timestamp + Offset, native, microsecond).

%% @doc Calculate the different between two timestamp, in microseconds.
-spec duration(timestamp(), timestamp()) -> microseconds_timestamp().
duration({Timestamp1, _}, {Timestamp2, _}) ->
    erlang:convert_time_unit(Timestamp2 - Timestamp1, native, microsecond).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

to_absolute_test() ->
    TS = {Timestamp, Offset} = timestamp(),
    ?assert(to_absolute(TS) =:= erlang:convert_time_unit(Timestamp + Offset, native, microsecond)).

duration_test() ->
    T1 = {Timestamp, _Offset} = timestamp(),
    T2 = {Timestamp + 5000, 100000000},
    ?assert(duration(T1, T2) =:= erlang:convert_time_unit(5000, native, microsecond)).

-endif.
