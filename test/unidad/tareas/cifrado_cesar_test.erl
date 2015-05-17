-module(cifrado_cesar_test).
-include_lib("eunit/include/eunit.hrl").


start(ID, Alf, Cad, Desp) ->
	FileName = "output-" ++ erlang:integer_to_list(ID) ++ ".dat",
	{_, Pid} = cifrado_cesar:programar(ID, Alf, Cad, Desp),
	{Pid, FileName}.

stop(FileName) ->
	%{ok} = file:delete(FileName). Que no gustar?
	os:cmd("rm " ++ FileName).


%%%%%%%%%%%%%%%%%%%% TEST FUNCTIONS %%%%%%%%%%%%%%%%%%%%

%cifrado_cesar1_test_() ->
%	{setup,
%	fun() -> start(1, "ABCDEFGHIJKLMNÑOPQRSTUVWXYZ", "aaa", 2) end,
%	fun({_, FileName}) -> stop(FileName) end,
%	fun({Pid, FileName}) -> [
%		?_assertExit(character_not_in_alphabet, catch cifrado_cesar:do(Pid))
%		%?_assertEqual("ccc", os:cmd("cat "++FileName))
%	]
%	end}.

cifrado_cesar2_test_() ->
	{setup,
	fun() -> start(1, "ABCDEFGHIJKLMNÑOPQRSTUVWXYZ", "AAA", 2) end,
	fun({_, FileName}) -> stop(FileName) end,
	fun({Pid, FileName}) -> [
		?_assertEqual(ok, cifrado_cesar:do(Pid)),
		?_assertEqual("CCC", os:cmd("cat "++FileName))
	]
	end}.

cifrado_cesar3_test_() ->
	{setup,
	fun() -> start(1, "ABCDEFGHIJKLMNÑOPQRSTUVWXYZ", "ABC", 2) end,
	fun({_, FileName}) -> stop(FileName) end,
	fun({Pid, FileName}) -> [
		?_assertEqual(ok, cifrado_cesar:do(Pid)),
		?_assertEqual("CDE", os:cmd("cat "++FileName))
	]
	end}.

cifrado_cesar4_test_() ->
	{setup,
	fun() -> start(1, "ABCDEFGHIJKLMNÑOPQRSTUVWXYZ", "", 2) end,
	fun({_, FileName}) -> stop(FileName) end,
	fun({Pid, FileName}) -> [
		?_assertEqual(ok, cifrado_cesar:do(Pid)),
		?_assertEqual("", os:cmd("cat "++FileName))
	]
	end}.

cifrado_cesar5_test_() ->
	{setup,
	fun() -> start(1, "aáeéií", "aáe", 3) end,
	fun({_, FileName}) -> stop(FileName) end,
	fun({Pid, FileName}) -> [
		?_assertEqual(ok, cifrado_cesar:do(Pid)),
		?_assertEqual("éií", os:cmd("cat "++FileName))
	]
	end}.

cifrado_cesar6_test_() ->
	{setup,
	fun() -> start(1, "aabcdefghijklmnñopqrstuvwxyz", "abc", 2) end,
	fun({_, FileName}) -> stop(FileName) end,
	fun({Pid, _}) -> [
		?_assertEqual(error, cifrado_cesar:do(Pid))
	]
	end}.

cifrado_cesar7_test_() ->
	{setup,
	fun() -> start(1, "AÁÉÍÓÚu", "AAA", 6) end,
	fun({_, FileName}) -> stop(FileName) end,
	fun({Pid, FileName}) -> [
		?_assertEqual(ok, cifrado_cesar:do(Pid)),
		?_assertEqual("uuu", os:cmd("cat "++FileName))
	]
	end}.

cifrado_cesar8_test_() ->
	{setup,
	fun() -> start(1, "ABCDEFGHIJKLMNÑOPQRSTUVWXYZ", "AAA", 27) end,
	fun({_, FileName}) -> stop(FileName) end,
	fun({Pid, FileName}) -> [
		?_assertEqual(ok, cifrado_cesar:do(Pid)),
		?_assertEqual("AAA", os:cmd("cat "++FileName))
	]
	end}.

cifrado_cesar9_test_() ->
	{setup,
	fun() -> start(1, "ABCDEFGHIJKLMNÑOPQRSTUVWXYZ", "AAA", 0) end,
	fun({_, FileName}) -> stop(FileName) end,
	fun({Pid, FileName}) -> [
		?_assertEqual(ok, cifrado_cesar:do(Pid)),
		?_assertEqual("AAA", os:cmd("cat "++FileName))
	]
	end}.




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
