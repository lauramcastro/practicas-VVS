%%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Laura M. Castro
%%% @copyright 2015
%%% @doc Documentation of the module `primos'.
%%%
%%% This program is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program. If not, see [http://www.gnu.org/licenses].
%%% @end
%%%-------------------------------------------------------------------
-module(primos).
-behaviour(tarea).

% Public API (according to spec).
-export([do/1]).

% Public API (required to start/stop components).
-export([programar/2]).

%% Private API (required for implementation purposes).
-export([loop/2]).

% Internal macros
-define(TASKNAME, ?MODULE).

%%-------------------------------------------------------------------
%% @doc Task creation and configuration.
%%      Receives a numerical ID (which will be used to determine the
%%      name of the output file), and a positive number (which must be
%%      greater or equal than 2) which represents the upper bound of
%%      primes to be found (this is done following the Sieve of
%%      Erathostenes, as in
%%      [https://en.wikipedia.org/wiki/Sieve_of_Erathostenes]).
%%      It returns a reference to the task, ready to be executed.
%% @end
%%-------------------------------------------------------------------
-spec programar(ID :: integer(),
		N  :: integer()) -> {atom(),
				     pid()}.
programar(ID, N) when is_integer(N), N > 1 ->
    {?TASKNAME,
     spawn(?MODULE, loop, [ID, N])}.

%%-------------------------------------------------------------------
%% @doc Executes a task.
%%      Receives a task (reference) to be executed, and triggers its
%%      execution. As an example, a task to calculate primes smaller
%%      than a given value of 20, hence configured with ID=45 (for
%%      instance) and N=20 (as upper-bound value), will write the
%%      output <tt>[2,3,5,7,11,13,17,19]</tt> in a file named
%%      `output-45.dat'.
%% @end
%%-------------------------------------------------------------------
-spec do(Tarea :: pid()) -> ok | {error, Message :: term()}.
do(Tarea) ->
    [TrappingExits] = [ Value || {trap_exit,Value} <-process_info(self())],
    process_flag(trap_exit, true),
    link(Tarea),
    Tarea ! {do, self()},
    receive
	{done, Tarea} ->
	    unlink(Tarea),
	    process_flag(trap_exit, TrappingExits),
	    ok;
	{'EXIT', Tarea, Whatever} ->
	    process_flag(trap_exit, TrappingExits),
	    {error, Whatever}
    end.



% Internal functions
% @private
loop(ID, N) ->
    receive
	{do, Who} ->
	    % this can take a lot of time and resources if N is large
	    % (as a reference, N=2000000 can take up to 8 minutes in a regular laptop)
	    {ok, ListOfPrimes} = erathostenes(N),
	    % this will break and the process will die if the task fails
	    FileName = "output-" ++ erlang:integer_to_list(ID) ++ ".dat",
	    % this will break and the process will die if the file cannot be written
	    {ok, File} = file:open(FileName, [write]),
	    ok = io:write(File, ListOfPrimes),
	    ok = file:close(File),
	    Who ! {done, self()}
    end.
	
erathostenes(N) ->
    {ok, sieve(lists:seq(2, N), [])}.

sieve([], Primes) ->
    lists:reverse(Primes);
sieve(_Candidates = [H|T], Primes) ->
    NewCandidates = [X || X <- T, X rem H /= 0],
    sieve(NewCandidates, [H|Primes]).
