-module(cifrado_cesar_test_nf).
-include_lib("eunit/include/eunit.hrl").

longString(N,C) -> longString([],N,C).

longString(L,0,_)  -> lists:flatten(L);

longString(L,N,C) -> longString([C|L],N-1,C).

cesar_nf_test()->
	{MegaSecs1,Secs1,_MicroSecs1} = erlang:now(),
	{_TASKNAME, Proc} = cifrado_cesar:programar(1,"ABCDEFGHIJKLMNOPQRSTUVWXYZ",longString(10000000,"B"),3),
	BeginSecs = MegaSecs1 * 1000000 + Secs1,
	Proc!{do,self()},
	receive
	{State, Proc} ->
		{MegaSecs,Secs,_MicroSecs} = erlang:now(),
		EndSecs = MegaSecs * 1000000 + Secs,
		?assertMatch(State,done),
		Dif = (EndSecs - BeginSecs),
		?assertEqual(Dif < 60,true)
	end.
