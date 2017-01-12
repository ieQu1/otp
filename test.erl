-module(test).

-export([main/1, prisoner/1, exec/1, port/0, canary/0, kill/0]).

kill() ->
    Canary = spawn_link(
               fun() ->
                       receive
                           A -> ok
                       after 1000 ->
                           ok
                       end,
                       io:format("Canary exited~n")
               end),
    erlang:spawn_in_jail(erlang, exit, [Canary, kill], 13).

main(_) ->
  Canary = spawn_link(fun() -> canary() end),
  MyPid = self(),
  Jailed = erlang:spawn_in_jail(test, prisoner, [[Canary, MyPid]], 42),
  listener(Canary, Jailed).

canary() ->
  receive
    finish -> ok;
    A ->
      io:format("!!! ERROR !!!! Canary process (~p) got ~p~n", [self(), A]),
      throw(nok)
  after 1000 ->
    throw(timeout)
  end.

listener(CanaryPid, JailedPid) ->
  receive
    ping ->
      listener(CanaryPid, JailedPid);
    {'EXIT', CanaryPid, _} ->
      io:format("Test failed.");
    {'EXIT', JailedPid, _Ret} ->
      CanaryPid ! finish,
      io:format("Test passed.")
  end.

exec(F) ->
   F().

prisoner(L) ->
  erlang:process_flag(trap_exit, true),
  %io:format("I'm ~p, a prisoner here~n", [self()]),
  lists:map(
    fun(X) ->
       Val = X!ping,
       %io:format("~p ! ~p~n", [X, Val]),
       Val
    end,
    L).

port() ->
  erlang:spawn_in_jail(test, exec, [fun() -> os:cmd("touch HACKED!!!!!") end], 42).
