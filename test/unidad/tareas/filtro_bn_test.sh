-module(primos_test).
-include_lib("eunit/include/eunit.hrl").


start(ID, N) ->
	FileName = "output-" ++ erlang:integer_to_list(ID) ++ ".dat",
	{_, Pid} = primos:programar(1, N),
	{Pid, FileName}.

stop(FileName) ->
	%{ok} = file:delete(FileName). Que no gustar?
	os:cmd("rm " ++ FileName).


%%%%%%%%%%%%%%%%%%%% TEST FUNCTIONS %%%%%%%%%%%%%%%%%%%%

filtro_bn_test_() ->
	N=100,
	{setup,
	fun() -> start(1, N) end,
	fun({_, FileName}) -> stop(FileName) end,
	fun({Pid, FileName}) -> [
		?_assertEqual(ok, primos:do(Pid)),
		?_assertEqual("0\n", os:cmd("bash test_primos.sh "
			++FileName++" "++erlang:integer_to_list(N)))
	]
	end}.

primos1000_test_() ->
	N=1000,
	{setup,
	fun() -> start(1, N) end,
	fun({_, FileName}) -> stop(FileName) end,
	fun({Pid, FileName}) -> [
		?_assertEqual(ok, primos:do(Pid)),
		?_assertEqual("0\n", os:cmd("bash test_primos.sh "
			++FileName++" "++erlang:integer_to_list(N)))
	]
	end}.

primos10000_test_() ->
	N=10000,
	{setup,
	fun() -> start(1, N) end,
	fun({_, FileName}) -> stop(FileName) end,
	fun({Pid, FileName}) -> [
		?_assertEqual(ok, primos:do(Pid)),
		?_assertEqual("0\n", os:cmd("bash test_primos.sh "
			++FileName++" "++erlang:integer_to_list(N)))
	]
	end}.

%primos_strange_test_() ->
%	{setup,
%	fun() -> ok end,
%	fun() -> ok end,
%	fun() -> [
%		?_assertEqual(ok, primos:programar(10000000000000000000000000000000000000000, 1000))
%	]
%	end}.


		% Result = catch open_file(ID),
		% close FD if it was opened (Result == {ok, FD})
		%?assertMatch({ok,_FD}, catch open_file(ID)),
		%?_assertException(badmatch, open_file(ID)),
		%?_assert(FileDescriptor, open_file(ID))]
%metatest_test_() ->
%    {foreach,
%     ???? , X <- ?VALUES}.

%disponible_atender_test_() ->
%	{setup,
%	 fun() -> start_2(primos:programar(1, 100)) end,
%	 fun({Esp, Cli}) -> stop_2(Esp, Cli) end,
%	 fun({Esp, Cli}) ->
%	 	[?_assertEqual(ok, especialista:atender(Esp, Cli)),
%		 ?_assertEqual(true, especialista:recibir(Esp))]  % Esp disponible
%	 end}.
%
%ocupado_recibir_test_() ->
%	{setup,
%	 fun() -> start() end,
%	 fun(Esp) -> stop(Esp) end,
%	 fun(Esp) ->
%	 	especialista:recibir(Esp),
%		[?_assertEqual(false, especialista:recibir(Esp))]  % Esp ocupado
%	 end}.
%
%ocupado_atender_1_test_() ->
%	{setup,
%	 fun() -> start_2(primos:programar(1, 100)) end,
%	 fun({Esp, Cli}) -> stop_2(Esp, Cli) end,
%	 fun({Esp, Cli}) ->
%	 	especialista:recibir(Esp),
%		[?_assertEqual(ok, especialista:atender(Esp, Cli)),
%	 	?_assertEqual(true, especialista:recibir(Esp))]  % Esp disponible
%	 end}.
%
%ocupado_atender_2_test_() ->
%	{setup,
%	 fun() -> start_2(primos:programar(1, 3000000)) end,
%	 fun({Esp, Cli}) -> stop_2(Esp, Cli) end,
%	 fun({Esp, Cli}) ->
%	 	especialista:recibir(Esp),
%		[?_assertEqual(ok, especialista:atender(Esp, Cli)),
%	 	?_assertEqual(false, especialista:recibir(Esp))]  % Esp saturado
%	 end}.
%
%saturado_recibir_1_test_() ->
%	{setup,
%	 fun() -> start_2(primos:programar(1, 3000000)) end,
%	 fun({Esp, Cli}) -> stop_2(Esp, Cli) end,
%	 fun({Esp, Cli}) ->
%	 	especialista:recibir(Esp),
%		especialista:atender(Esp, Cli),
%	 	[?_assertEqual(false, especialista:recibir(Esp))]  % Esp saturado
%	 end}.
%
%saturado_atender_1_test_() ->
%	{setup,
%	 fun() -> start_2(primos:programar(1, 3000000)) end,
%	 fun({Esp, Cli}) -> stop_2(Esp, Cli) end,
%	 fun({Esp, Cli}) ->
%	 	especialista:recibir(Esp),
%		especialista:atender(Esp, Cli),
%		Cli2 = cliente:planificar([primos:programar(2, 100)]),
%	 	[?_assertEqual(ok, especialista:atender(Esp, Cli2)),
%		 ?_assertEqual(false, especialista:recibir(Esp))]  % Esp saturado
%	 end}.
