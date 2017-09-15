## wts

Warp the Timestamps or something.


### Usage

This library is for use with the suggested Erlang VM args for time warp mode and time correction:

```
$ cat config/vm.args
...
+c true
+C multi_time_warp
```

These settings require that you have only [Time Warp Safe Code](http://erlang.org/doc/apps/erts/time_correction.html#Time_Warp_Safe_Code).

Time correction is enabled by Erlang by default if it supported by your system, while the default time warp mode is `no_time_warp` because this is how the runtime behaved before the addition of time warp modes.

Example usage to find the time elapsed while performing some function and converting to a timestamp that is in microseconds since the epoch, so has meaning outside of the Erlang runtime. The `wts` timestamp uses the Erlang runtime's `native` time unit for the best accuracy and precision and only converts to microseconds when returning the result of `duration/2` and `to_absolute/1`.

```
> StartTime = wts:timestamp().
> %% do something
> EndTime = wts:timestamp().

%% get the duration in microseconds
> wts:duration(StartTime, EndTime).

%% convert to microseconds since the epoch
> wts:to_absolute(StartTime).
```

Additionally there is support for converting to an rfc3339 format string:

```
> wts:rfc3339(wts:timestamp()).
{ok,<<"2017-09-15T02:48:11.118825Z">>}
```

### Credits

Created thanks to Elixir's [Tapper timestamp module](https://github.com/Financial-Times/tapper/blob/master/lib/tapper/timestamp.ex#L32).
