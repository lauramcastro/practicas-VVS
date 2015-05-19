-module(primos_test_nf).
-include_lib("eunit/include/eunit.hrl").

primos_nf_test_()->
    {timeout, 5 * 60, fun caso_de_prueba/0}. % in order to avoid the test case to fail due to test timeout

caso_de_prueba() ->
    {MegaSecs1,Secs1,_MicroSecs1} = erlang:now(),
    {_TASKNAME, Proc} = primos:programar(1,100000),
    BeginSecs = MegaSecs1 * 1000000 + Secs1,
    Proc!{do,self()},
    receive
	{State, Proc} ->
	    {MegaSecs,Secs,_MicroSecs} = erlang:now(),
	    EndSecs = MegaSecs * 1000000 + Secs,
	    ?assertMatch(State,done),
	    Dif = (EndSecs - BeginSecs),
	    ?assert(Dif < 10)
    end.
