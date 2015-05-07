%%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Laura M. Castro
%%% @copyright 2015
%%% @doc Documentation of the module `especialista'.
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
-module(especialista).
-behaviour(gen_fsm).

% Public API (according to spec).
-export([recibir/1, atender/2]).

% Public API (required to start/stop components).
-export([alta/1, baja/1]).

%% Private API (required for implementation purposes).
-export([disponible/2, ocupado/2, saturado/2]). % estados
-export([disponible/3, ocupado/3, saturado/3]). % estados
-export([init/1, handle_event/3, terminate/3]). % gen_fsm
-export([code_change/4, handle_info/3, handle_sync_event/4]). % gen_fsm (unused)

%%-------------------------------------------------------------------
%% @doc Checks if an specialist is ready to see a client.
%%      Receives the specialist (reference) to be asked.
%% @end
%%-------------------------------------------------------------------
-spec recibir(Especialista :: pid()) -> boolean().
recibir(Especialista) when is_pid(Especialista) ->
    case catch gen_fsm:sync_send_event(Especialista, can_see, 500) of
	{'EXIT',{timeout,{gen_fsm,sync_send_event,[Especialista,can_see,500]}}} -> false;
	Else -> Else
    end.

%%-------------------------------------------------------------------
%% @doc Assigns a client to a specialist, to be taken care of.
%%      Receives the specialist (reference) to take care of the
%%      client, and the client (reference) to be taken care of.
%% @end
%%-------------------------------------------------------------------
-spec atender(Especialista :: pid(),
	      Cliente :: pid()) -> ok.
atender(Especialista, Cliente) when is_pid(Especialista),
				    is_pid(Cliente) ->
    gen_fsm:send_event(Especialista, {see, Cliente}).

%%-------------------------------------------------------------------
%% @doc Hires (starts) an specialist.
%%      Receives the level of tolerance before the specialist will
%%      need a break (tolerance), which must be greater than zero
%%      and represents milliseconds. If taking care of the client's
%%      demand (task) takes more than their tolerance, a specialist
%%      would be too tired after seeing that client and will need
%%      a break to recover. The break would involve resting for as
%%      many requests as seconds of difference there were between
%%      the tolerance level and the time to complete the
%%      demand (task).
%% @end
%%-------------------------------------------------------------------
-spec alta(Tolerancia :: integer()) -> {ok, pid()}.
alta(Tolerancia) when Tolerancia > 0 ->
    gen_fsm:start(?MODULE, Tolerancia, []).

%%-------------------------------------------------------------------
%% @doc Releases (stops) an specialist.
%%      Receives the specialist (reference) to be released.
%% @end
%%-------------------------------------------------------------------
-spec baja(Especialista :: pid()) -> ok.
baja(Especialista) when is_pid(Especialista) ->
    gen_fsm:send_all_state_event(Especialista, stop).



% Internal functions (estados)
% --- --- --- async --- --- ---
% @private
disponible({see, _Client}, Tolerance) ->
    {next_state, disponible, Tolerance}.

% @private
ocupado({see, Client}, Tolerance) ->
    {TypeOfTask, Task} = cliente:peticion(Client),
    {ElapsedTime, ok} = timer:tc(TypeOfTask, do, [Task]),
    case (ElapsedTime >= (Tolerance*1000)) of
	true  -> {next_state, saturado,  [Tolerance, (ElapsedTime-Tolerance*1000) div 1000000]};
	false -> {next_state, disponible, Tolerance}
    end.

% @private
saturado({see, _Client}, [Tolerance, 0]) ->
    {next_state, disponible, Tolerance};
saturado({see, _Client},   [Tolerance, Saturation]) ->
    {next_state, saturado, [Tolerance, Saturation-1]}.

% --- --- --- sync --- --- --- 
% @private
disponible(can_see, _From, Tolerance) ->
    {reply, true, ocupado, Tolerance}.

% @private
ocupado(can_see, _From, Tolerance) ->
    {reply, false, ocupado, Tolerance}.

% @private
saturado(can_see, _From, [Tolerance, 0]) ->
    {reply, true, disponible, Tolerance};
saturado(can_see, _From, [Tolerance, Saturation]) ->
    {reply, false, saturado, [Tolerance, Saturation-1]}.

% Internal functions (gen_fsm)
% @private
init(Tolerancia) ->
    {ok, disponible, Tolerancia}.

% @private
handle_event(stop, _State, _StateData) ->
    {stop, normal, []}.

% @private
terminate(normal, _State, _StateData) ->
    ok.

% Internal functions (gen_fsm, unused)
% @private
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

% @private
handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

% @private
handle_sync_event(_Event, _From, StateName, StateData) ->
    {next_state, StateName, StateData}.
