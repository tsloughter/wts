%%--------------------------------------------------------------------
%% @doc Timestamp support functions.
%% This library is for use with the suggested Erlang VM args for time
%% warp mode and time correction:
%%
%% <pre><code>
%% $ cat config/vm.args
%% ...
%% +c true
%% +C multi_time_warp
%% </code></pre>
%%
%% These settings require that you have only
%% <a href="http://erlang.org/doc/apps/erts/time_correction.html#Time_Warp_Safe_Code">Time Warp Safe Code</a>.
%%
%% Time correction is enabled by Erlang by default if it supported by
%% your system, while the default  time warp mode is `no_time_warp'
%% because this is how the runtime behaved before the addition of time
%% warp modes.
%%
%% Example usage to find the time elapsed while performing some function
%% and converting to a timestamp that is in microseconds since the epoch,
%% so has meaning outside of the Erlang runtime. The `wts' timestamp
%% uses the Erlang runtime's `native' time unit for the best accuracy
%% and precision and only converts to microseconds when returning the
%% result of `duration/2' and `to_absolute/1'.
%%
%% <pre><code>
%% > StartTime = wts:timestamp().
%% > %% do something
%% > EndTime = wts:timestamp().
%%
%% %% get the duration in microseconds
%% > wts:duration(StartTime, EndTime).
%%
%% %% convert to microseconds since the epoch
%% > wts:to_absolute(StartTime).
%% </code></pre>
%% @end
%%--------------------------------------------------------------------
-module(wts).

-export([timestamp/0,
         to_absolute/1,
         duration/2,

         rfc3339/1]).

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

%% @doc Converts the timestamp to rfc3339 format string.
-spec rfc3339(timestamp()) -> binary().
rfc3339({Timestamp, Offset}) ->
    calendar:system_time_to_rfc3339(Timestamp+Offset).

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
