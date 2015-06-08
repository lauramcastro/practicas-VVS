-module(primos_prop).

-include_lib("proper/include/proper.hrl").

-define(OUT_PATH, "/tmp/").
-define(SCRIPTS_PATH, "test/unidad/tareas/").


start(ID, N) ->
	{ok, PATH} = file:get_cwd(),
	FileName = PATH ++"/"++ "output-" ++ erlang:integer_to_list(ID) ++ ".dat",
	{_, Pid} = primos:programar(ID, N),
	{Pid, FileName}.

stop(FileName) ->
	os:cmd("rm " ++ FileName).

%%%%%%%%%%%%%%%%%%%% TEST FUNCTIONS %%%%%%%%%%%%%%%%%%%%

prime_check(Id, N) ->
	{Pid, FileName} = start(Id, N),
	primos:do(Pid),
	Res = "0\n" == os:cmd("bash "++?SCRIPTS_PATH++"test_primos.sh "
		++ FileName ++ " " ++ erlang:integer_to_list(N)),
	stop(FileName),
	Res.

mayor_dos() ->
	?SUCHTHAT({Id, N}, {integer(), integer()},
	N >= 2).

prop_prime() ->
	?FORALL({Id, N},
	mayor_dos(),
	prime_check(Id, N)).
