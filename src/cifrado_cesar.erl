%%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Laura M. Castro
%%% @copyright 2015
%%% @doc Documentation of the module `cifrado_cesar'.
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
-module(cifrado_cesar).
-behaviour(tarea).

% Public API (according to spec).
-export([do/1]).

% Public API (required to start/stop components).
-export([programar/4]).

%% Private API (required for implementation purposes).
-export([loop/4]).

% Internal macros
-define(TASKNAME, ?MODULE).

%%-------------------------------------------------------------------
%% @doc Task creation and configuration.
%%      Receives a numerical ID (which will be used to determine the
%%      name of the output file), a string representing the alphabet
%%      (which cannot be the empty string), the string to be encoded,
%%      and the offset to perform the Caesar cipher
%%      (as in [https://en.wikipedia.org/wiki/Caesar_cipher]).
%%      It returns a reference to the task, ready to be executed.
%% @end
%%-------------------------------------------------------------------
-spec programar(ID :: integer(),
		Alfabeto :: string(),
		Cadena :: string(),
		Desplazamiento :: integer()) -> {atom(),
						 pid()}.
programar(ID, Alfabeto, Cadena, Desplazamiento) when length(Alfabeto) > 0 ->
    {?TASKNAME,
     spawn(?MODULE, loop, [ID, Alfabeto, Cadena, Desplazamiento])}.

%%-------------------------------------------------------------------
%% @doc Executes a task.
%%      Receives a task (reference) to be executed, and triggers its
%%      execution. As an example, a Caesar cipher task configured
%%      with ID=96, alphabet "<em>abcdefghijklmnopqrstuvwxyz</em>",
%%      string "<em>the five boxing wizards jump quickly</em>" and
%%      offset 3, will write the output "<em>wkh ilyh eralqj zlcdugv
%%      mxps txlfnob</em>" in a file named `output-96.dat'.
%% @end
%%-------------------------------------------------------------------
-spec do(Tarea :: pid()) -> ok.
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
loop(ID, Alphabet, String, Offset) ->
    receive
	{do, Who} ->
	    % this will break and the process will die if the task fails
	    {ok, EncodedString} = encode(Alphabet, String, Offset rem length(Alphabet), []),
	    FileName = "output-" ++ erlang:integer_to_list(ID) ++ ".dat",
	    % this will break and the process will die if the file cannot be written
	    ok = file:write_file(FileName, EncodedString),
	    Who ! {done, self()}
    end.

encode(_Alphabet, [], _OffSet, EncodedString)  ->
    {ok, lists:reverse(EncodedString)};
encode(Alphabet, [32|T], OffSet, EncodedString) ->
    % blank spaces are not encoded
    encode(Alphabet, T, OffSet, [32 | EncodedString]);
encode(Alphabet, [H|T], OffSet, EncodedString) ->
    case lists:member(H, Alphabet) of
	true ->
	    [H|Reminder] = lists:dropwhile(fun(X) -> X =/= H end, Alphabet),
	    EncodedH = case OffSet =< length(Reminder) of
			   true  -> lists:nth(OffSet, Reminder);
			   false -> lists:nth(OffSet - length(Reminder), Alphabet)
		       end,
	    encode(Alphabet, T, OffSet, [EncodedH | EncodedString]);
	false ->
	    {error, character_not_in_alphabet}
    end.
