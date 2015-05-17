-module(primos_test).
-include_lib("eunit/include/eunit.hrl").

-define(OUT_PATH, "/tmp/").
-define(SCRIPTS_PATH, "test/unidad/tareas/").


start(ID, N) ->
	{ok, PATH} = file:get_cwd(),
	FileName = PATH ++"/"++ "output-" ++ erlang:integer_to_list(ID) ++ ".dat",

% NOTA:
% Al cambiar el directorio de trabajo en erlang, las referencias de los módulos 
% cargados al arrancar, se transladan al directorio especificado:
% PWD = /home/rodrigo/vvs/
% erlc -pa ebin -o ebin ...
% Carga los módulos de /home/rodrigo/vvs/ebin
% file:set_cwd(?OUT_PATH)
% Ahora la carpeta de módulos se translada a /tmp/ebin
% Dondo no hay ningún módulo, provocando un error al tratar de invocar a la
% función primos:programar().
%
% Para solventar este problema, especificar la ruta completa al cargar el
% módulo:
% erlc -pa /home/rodrigo/vvs/ebin -o ebin ...

%	file:set_cwd(?OUT_PATH),
	{_, Pid} = primos:programar(ID, N),
	{Pid, FileName}.

stop(FileName) ->
	%{ok} = file:delete(FileName). Que no gustar?
	os:cmd("rm " ++ FileName).


%%%%%%%%%%%%%%%%%%%% TEST FUNCTIONS %%%%%%%%%%%%%%%%%%%%

primos100_test_() ->
	N=100,
	{setup,
	fun() -> start(1, N) end,
	fun({_, FileName}) -> stop(FileName) end,
	fun({Pid, FileName}) -> [
		?_assertEqual(ok, primos:do(Pid)),
		?_assertEqual("0\n", os:cmd("bash "++?SCRIPTS_PATH++"test_primos.sh "
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
		?_assertEqual("0\n", os:cmd("bash "++?SCRIPTS_PATH++"test_primos.sh "
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
		?_assertEqual("0\n", os:cmd("bash "++?SCRIPTS_PATH++"test_primos.sh "
			++FileName++" "++erlang:integer_to_list(N)))
	]
	end}.

primos_large_id_test_() ->
	N=100,
	ID=100000000000000000000000000000000000000000000000000000000000000000000000,
	{setup,
	fun() -> start(ID, N) end,
	fun({_, FileName}) -> stop(FileName) end,
	fun({Pid, FileName}) -> [
		?_assertEqual(ok, primos:do(Pid)),
		?_assertEqual("0\n", os:cmd("bash "++?SCRIPTS_PATH++"test_primos.sh "
			++FileName++" "++erlang:integer_to_list(N)))
	]
	end}.

primos_large_neg_id_test_() ->
	N=100,
	ID=-100000000000000000000000000000000000000000000000000000000000000000000000,
	{setup,
	fun() -> start(ID, N) end,
	fun({_, FileName}) -> stop(FileName) end,
	fun({Pid, FileName}) -> [
		?_assertEqual(ok, primos:do(Pid)),
		?_assertEqual("0\n", os:cmd("bash "++?SCRIPTS_PATH++"test_primos.sh "
			++FileName++" "++erlang:integer_to_list(N)))
	]
	end}.


primos2_test_() ->
	{setup,
	fun() -> ok end,
	fun(_) -> ok end, %stop(FileName) end,
	fun(_) -> [
		?_assertMatch({'EXIT',{undef,_}}, catch primos:program(1, -1))
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
