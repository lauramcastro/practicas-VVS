%%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Laura M. Castro
%%% @copyright 2015
%%% @doc Documentation of the module `cliente'.
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
-module(cliente).

% Public API (according to spec).
-export([peticion/1]).

% Public API (required to start/stop components).
-export([planificar/1, abandonar/1]).

%% Private API (required for implementation purposes).
-export([loop/1]).

%%-------------------------------------------------------------------
%% @doc Client needs dispatch.
%%      Receives the client whose needs are to be dispatched.
%%      This is the way to ask a given client for their next need.
%%      It returns a configured task, ready to be executed.
%%      If questioned about their last need, the client will die
%%      peacefully after replying.
%% @end
%%-------------------------------------------------------------------
-spec peticion(Cliente :: pid()) -> {TipoDeTarea :: atom(),
				     Tarea :: pid()}.
peticion(Cliente) when is_pid(Cliente) ->
    Cliente ! {next_need, self()},
    receive
	{next_task, Task} -> Task
    end.

%%-------------------------------------------------------------------
%% @doc Creates a new client.
%%      Receives the list of needs (tasks, created and configured),
%%      which may not be empty. It returns the new client, ready to
%%      be questioned about their needs one at a time.
%% @end
%%-------------------------------------------------------------------
-spec planificar(Tareas :: list({TipoDeTarea :: atom(),
				 Tarea :: pid()})) -> pid().
planificar(Tareas=[_H|_T]) -> % this pattern-matching ensures that the list is not empty
    spawn(?MODULE, loop, Tareas).

%%-------------------------------------------------------------------
%% @doc Sends word to a client that it should stop its execution.
%%      Receives the client to be stopped.
%% @end
%%-------------------------------------------------------------------
-spec abandonar(Cliente :: pid()) -> ok.
abandonar(Cliente) when is_pid(Cliente) ->
    Cliente ! stop,
    ok.



% Internal functions (estados)
% @private
loop(Tasks) ->
    receive
	{next_need, Who} ->
	    case Tasks of
		[LastTask] -> Who ! {next_task, LastTask},
			      ok;
		[NextTask | MoreTasks] -> Who ! {next_task, NextTask},
					  loop(MoreTasks)
	    end;
	stop ->
	    ok
    end.
