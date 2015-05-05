-module(disponible_recibir).
-include_lib("eunit/include/eunit.hrl").

disponible_recibir_test() ->
	{ok, Esp} = especialista:alta(100),
	?assertEqual(true, especialista:recibir(Esp)).

otro_test() ->
	{setup,
	 fun() -> start() end,                    % set up
	 fun(Esp) -> especialista:baja(Esp) end,  % clean up
	 fun(Esp) ->                              % test
	 	?assert(especialista:recibir(Esp))
	 end}.

start() ->
	{ok, Esp} = especialista:alta(100),
	Esp.