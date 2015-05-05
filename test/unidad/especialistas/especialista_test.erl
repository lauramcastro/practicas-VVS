-module(especialista_test).
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%% SETUP & CLEANUP %%%%%%%%%%%%%%%%%%%%

start() ->
	{ok, Esp} = especialista:alta(100),
	Esp.

stop(Esp) -> especialista:baja(Esp).
	
start_2(Tarea) ->
	{ok, Esp} = especialista:alta(100),
	Cli = cliente:planificar([Tarea]),
	{Esp, Cli}.

stop_2(Esp, Cli) ->
	especialista:baja(Esp),
	cliente:abandonar(Cli).

%%%%%%%%%%%%%%%%%%%% TEST FUNCTIONS %%%%%%%%%%%%%%%%%%%%

disponible_recibir_test_() ->
	{setup,
	 fun() -> start() end,
	 fun(Esp) -> stop(Esp) end,
	 fun(Esp) ->
	 	[?_assert(especialista:recibir(Esp))]
	 end}.

disponible_atender_test_() ->
	{setup,
	 fun() -> start_2(primos:programar(1, 100)) end,
	 fun({Esp, Cli}) -> stop_2(Esp, Cli) end,
	 fun({Esp, Cli}) ->
	 	[?_assertEqual(ok, especialista:atender(Esp, Cli)),
		 ?_assertEqual(true, especialista:recibir(Esp))]  % Esp disponible
	 end}.

ocupado_recibir_test_() ->
	{setup,
	 fun() -> start() end,
	 fun(Esp) -> stop(Esp) end,
	 fun(Esp) ->
	 	especialista:recibir(Esp),
		[?_assertEqual(false, especialista:recibir(Esp))]  % Esp ocupado
	 end}.

ocupado_atender_1_test_() ->
	{setup,
	 fun() -> start_2(primos:programar(1, 100)) end,
	 fun({Esp, Cli}) -> stop_2(Esp, Cli) end,
	 fun({Esp, Cli}) ->
	 	especialista:recibir(Esp),
		[?_assertEqual(ok, especialista:atender(Esp, Cli)),
	 	?_assertEqual(true, especialista:recibir(Esp))]  % Esp disponible
	 end}.

ocupado_atender_2_test_() ->
	{setup,
	 fun() -> start_2(primos:programar(1, 3000000)) end,
	 fun({Esp, Cli}) -> stop_2(Esp, Cli) end,
	 fun({Esp, Cli}) ->
	 	especialista:recibir(Esp),
		[?_assertEqual(ok, especialista:atender(Esp, Cli)),
	 	?_assertEqual(false, especialista:recibir(Esp))]  % Esp saturado
	 end}.

saturado_recibir_1_test_() ->
	{setup,
	 fun() -> start_2(primos:programar(1, 3000000)) end,
	 fun({Esp, Cli}) -> stop_2(Esp, Cli) end,
	 fun({Esp, Cli}) ->
	 	especialista:recibir(Esp),
		especialista:atender(Esp, Cli),
	 	[?_assertEqual(false, especialista:recibir(Esp))]  % Esp saturado
	 end}.

saturado_atender_1_test_() ->
	{setup,
	 fun() -> start_2(primos:programar(1, 3000000)) end,
	 fun({Esp, Cli}) -> stop_2(Esp, Cli) end,
	 fun({Esp, Cli}) ->
	 	especialista:recibir(Esp),
		especialista:atender(Esp, Cli),
		Cli2 = cliente:planificar([primos:programar(2, 100)]),
	 	[?_assertEqual(ok, especialista:atender(Esp, Cli2)),
		 ?_assertEqual(false, especialista:recibir(Esp))]  % Esp saturado
	 end}.
