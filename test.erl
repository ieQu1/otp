-module(test).

-export([main/0, prisoner/1, exec/1, port/0, kill/1, link_kill/1, 
         link_run/1, fatality/1, send_local_msg/1, port/1, print/1]).

print(Jail) ->
    spawn_test(Jail, io, format, ["HAXXX!!!~n"]).

main() ->
    [F(true)||F<-[ fun fatality/1 
                 , fun port/1
                 , fun link_kill/1
                 , fun kill/1
                 ]].

spawn_test(true, M, F, A) ->
    erlang:spawn_in_jail(M, F, A, 42);
spawn_test(false, M, F, A) ->
    erlang:spawn(M, F, A).

port(Jailed) ->
    File = "./HAXXXXED!!!11",
    file:delete(File),
    spawn_test(Jailed, os, cmd, ["touch " ++ File]),
    receive after 1000 -> ok
    end,
    Fail = filelib:is_file(File),
    if Fail ->
            file:delete(File),
            throw(you_have_been_hacked);
       true ->
            ok
    end.

fatality(Jailed) ->
    Pid = list_to_pid("<0.0.0>"),
    spawn_test(Jailed, erlang, exit, [Pid, kill]).

link_kill(Jail) ->
    Canary = spawn_link(
               fun() ->
                       receive
                           A -> ok
                       after 1000 ->
                           ok
                       end,
                       io:format("OK: Canary survived~n")
               end),
    spawn_test(Jail, test, link_run, [Canary]).
        
link_run(Pid) ->
    link(Pid),
    throw(test_test).

kill(Jail) ->
    Canary = spawn_link(
               fun() ->
                       receive
                           A -> ok
                       after 1000 ->
                           ok
                       end,
                       io:format("OK: Canary survived~n")
               end),
    if Jail ->
            erlang:spawn_in_jail(erlang, exit, [Canary, kill], 13);
       true ->
            erlang:spawn(erlang, exit, [Canary, kill])
    end.

send_local_msg(true) ->
  Canary = spawn_link(
             fun() ->  
                     receive
                         finish -> ok;
                         A ->
                             io:format("!!! ERROR !!!! Canary process (~p) got ~p~n", 
                                       [self(), A]),
                             throw(nok)
                     after 1000 ->
                             throw(timeout)
                     end
             end),
  MyPid = self(),
  Jailed = erlang:spawn_in_jail(test, prisoner, [[Canary, MyPid]], 42),
  listener(Canary, Jailed).

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
