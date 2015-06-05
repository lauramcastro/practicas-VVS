-module(recepcion_tests).
-include_lib("eunit/include/eunit.hrl").

% SETUP
start() ->
	{_, Esp} = especialista:alta(100),
	Esp.

start2() ->
	Cl = cliente:planificar([primos:programar(0, random:uniform(100))]),
	{_, Esp} = especialista:alta(100),
	recepcion:abrir(1, [Esp]),
	{Cl, Esp}.

start3() ->
	Cl1 = cliente:planificar([primos:programar(0, random:uniform(100))]),
	Cl2 = cliente:planificar([primos:programar(1, random:uniform(100))]),
	{_, Esp} = especialista:alta(100),
	recepcion:abrir(1, [Esp]),
	recepcion:entrar(Cl1),
	{Cl1, Cl2, Esp}.

% CLEANUP
stop(Esp) ->
	especialista:baja(Esp).

stop2({Cl, Esp}) ->
	recepcion:cerrar(),
	cliente:abandonar(Cl),
	especialista:baja(Esp).

stop3({Cl1, Cl2, Esp}) ->
	recepcion:cerrar(),
	cliente:abandonar(Cl1),
	cliente:abandonar(Cl2),
	especialista:baja(Esp).

% TESTS
ceroMaxClientes_test_() ->
	{setup,
	fun() -> start() end,
	fun(Esp) -> stop(Esp) end,
	fun(Esp) -> [
	?_assertException(error, function_clause, recepcion:abrir(0, [Esp]))
	] end
	}.

ceroEspecialistas_test_() ->
	[?_assertException(error, function_clause, recepcion:abrir(1, []))
	].

recepcionLibre_test_() ->
	{setup,
	fun() -> start2() end,
	fun(All) -> stop2(All) end,
	fun ({Cl, _}) -> [
	?_assertEqual(true, recepcion:entrar(Cl))
	] end
	}.

recepcionLlena_test_() ->
	{setup,
	fun() -> start3() end,
	fun(All) -> stop3(All) end,
	fun ({_, Cl2, _}) -> [
	?_assertEqual(false, recepcion:entrar(Cl2))
	] end
	}.
