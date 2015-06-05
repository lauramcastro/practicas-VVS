-module(cliente_tests).
-include_lib("eunit/include/eunit.hrl").

% SETUP
start() ->
	T = primos:programar(0, random:uniform(100)),
	{cliente:planificar([T]), T}.

% CLEANUP
stop(Cl) ->
	cliente:abandonar(Cl). 

% TESTS
clienteSinTareas_test_() ->
	[?_assertException(error, function_clause, cliente:planificar([]))
	].

clientePeticion_test_() ->
	{setup,
	fun() -> start() end,
	fun({Cl, _}) -> stop(Cl) end,
	fun({Cl, T}) -> [
	?_assertEqual(T, cliente:peticion(Cl))
	] end
	}.
