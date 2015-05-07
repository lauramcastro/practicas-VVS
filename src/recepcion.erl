%%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Laura M. Castro
%%% @copyright 2015
%%% @doc Documentation of the module `recepcion'.
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
-module(recepcion).

% Public API (according to spec).
-export([entrar/1]).

% Public API (required to start/stop components).
-export([abrir/2, cerrar/0]).

%% Private API (required for implementation purposes).
-export([loop/3]).

% Internal macros
-define(RECEPCION, ?MODULE).

%%-------------------------------------------------------------------
%% @doc Client admission.
%%      Receives a client (reference), which will be either
%%      accepted in (boolean value `true' is returned) if there is
%%      room, or dismissed (boolean value `false' is returned).
%% @end
%%-------------------------------------------------------------------
-spec entrar(Cliente :: pid()) -> boolean().
entrar(Cliente) when is_pid(Cliente) ->
    ?RECEPCION ! {is_there_room, Cliente, self()},
    receive
	yes_come_in        -> true;
	no_come_back_later -> false
    end.

%%-------------------------------------------------------------------
%% @doc Starts the component.
%%      Receives the maximum number of allowed clients in the queue,
%%      which must be greater than zero.
%%      Receives the list of (created, started) specialists
%%      (references), which may not be empty.
%% @end
%%-------------------------------------------------------------------
-spec abrir(NClientes :: integer(),
	    Especialistas :: list(pid())) -> ok.
abrir(NClientes,
      Especialistas) when NClientes > 0,
			  length(Especialistas) > 0 ->
    true = register(?RECEPCION, spawn(?MODULE,
				      loop,
				      [[],NClientes,Especialistas])),
    ok.

%%-------------------------------------------------------------------
%% @doc Shuts down the component.
%%      Since this component does not start the specialists, NOR does
%%      it take care of shutting them down.
%% @end
%%-------------------------------------------------------------------
-spec cerrar() -> ok.
cerrar() ->
    ?RECEPCION ! time_to_close,
    ok.



% Internal functions
% @private
loop(WaitingClients, MaxClients, Specialists) ->
    receive
	{is_there_room, Client, PID} when length(WaitingClients) < MaxClients ->
	    PID ! yes_come_in,
	    % TODO: enforce that the same client is not in the queue two times in a row
	    loop(WaitingClients ++ [Client],
		 MaxClients,
		 Specialists);
	time_to_close ->
	    % we do not do anything with waiting clients or specialists at the moment
	    ok;
	_ -> % flush unwanted messages
	    loop(WaitingClients, MaxClients, Specialists)
    after 1000 ->
            % every second try to schedule the first client with a specialist
	    {NewWaitingClients, NewSpecialists} = schedule(WaitingClients, Specialists, []),
	    loop(NewWaitingClients,
		 MaxClients,
		 NewSpecialists)
    end.

schedule([], Pending, Reviewed) ->
    {[], Pending ++ Reviewed}; % all clients could be seen by specialists
schedule(Clients, [], Reviewed) ->
    {Clients, Reviewed};       % there are no more specialists available right now
schedule([C|MoreClients], [S|MoreSpecialists], Reviewed) ->
    case especialista:recibir(S) of
	true  -> 
	    especialista:atender(S,C),
	    schedule(MoreClients, MoreSpecialists, Reviewed++[S]);
	false ->
	    schedule([C|MoreClients], MoreSpecialists, Reviewed++[S])
    end.
