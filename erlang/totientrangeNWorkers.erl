-module(totientrangeNWorkers).
-export([hcf/2,
 	 relprime/2,
	 euler/1,
   worker/0,
   server/0,
   start_server/0
	]).

%% TotientRange.erl - Sequential Euler Totient Function (Erlang Version)
%% compile from the shell: >c(totientrange).
%% run from the shell:     >totientrange:sumTotient(1,1000).

%% Phil Trinder 20/10/2018

%% This program calculates the sum of the totients between a lower and an 
%% upper limit. It is based on earlier work by: Nathan Charles, 
%% Hans-Wolfgang Loidl and Colin Runciman

%% The comments provide (executable) Haskell specifications of the functions

%% hcf x 0 = x
%% hcf x y = hcf y (rem x y)

hcf(X,0) -> X;
hcf(X,Y) -> hcf(Y,X rem Y).

%% relprime x y = hcf x y == 1

relprime(X,Y) -> 
  V = hcf(X,Y),
  if 
    V == 1 
      -> true;
    true 
      -> false
  end.

%%euler n = length (filter (relprime n) (mkList n))

euler(N) -> 
  RelprimeN = fun(Y) -> relprime(N,Y) end,  
  length (lists:filter(RelprimeN,(lists:seq(1,N)))).

%% Take completion timestamp, and print elapsed time

printElapsed(S,US) ->
  {_, S2, US2} = os:timestamp(),
                       %% Adjust Seconds if completion Microsecs > start Microsecs
  if
    US2-US < 0 ->
      S3 = S2-1,
      US3 = US2+1000000;
    true ->
      S3 = S2,
      US3 = US2
  end,
  io:format("Server: Time taken in Secs, MicroSecs ~p ~p~n",[S3-S,US3-US]).

worker() ->
  receive
    {range, Lower, Upper} ->
      io:format("Worker: Computing Range ~p ~p~n", [Lower, Upper]),
      Res = lists:sum(lists:map(fun euler/1,lists:seq(Lower, Upper))),
      server ! {reply, self(), Res},
      worker();
    finished ->
      io:format("Worker: Finished~n"),
      exit(normal)
  end.

server() -> 
  receive
    {range, Lower, Upper, NWorkers} ->
      {_, S, US} = os:timestamp(),

      Ranges = [{L+1, L+(Upper div NWorkers)}
                || L <- lists:seq(Lower-1, Upper-1), (L rem (Upper div NWorkers) == 0) or (L == Lower-1)],
      Pids = [ spawn(totientrangeNWorkers, worker, []) || _ <- lists:seq(1, NWorkers)],

      [Pid ! {range, L, U} || {Pid, {L, U}} <- lists:zip(Pids, Ranges)],

      Totients = [ receive {reply, Pid, Res} -> io:format("Server: Received Sum ~p~n", [Res]), Res end || Pid <- Pids ],

      [Pid ! finished || Pid <- Pids],

      Res = lists:sum(Totients),
      io:format("Server: Sum of totients: ~p~n", [Res]),
      printElapsed(S,US),
      server();
    finished ->
      io:format("Server: Finished~n"),
      exit(normal)
  end. 
  
start_server() ->
  register(server,spawn(totientrangeNWorkers,server,[])).