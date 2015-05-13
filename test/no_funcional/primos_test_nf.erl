-module(primos_test_nf).
-include_lib("eunit/include/eunit.hrl").




primos_nf_test_()->
	{MegaSecs1,Secs1,_MicroSecs1} = erlang:now(),
	{_TASKNAME, Proc} = primos:programar(1,1000000),
	BeginSecs = MegaSecs1 * 1000000 + Secs1,
	Proc!{do,self()},
	receive
	{State, Proc} ->
		{MegaSecs,Secs,_MicroSecs} = erlang:now(),
		EndSecs = MegaSecs * 1000000 + Secs,
		?_assertEqual(State,done),
		Dif = (EndSecs - BeginSecs),
		?_assertEqual(Dif < 60,true)
	end.
