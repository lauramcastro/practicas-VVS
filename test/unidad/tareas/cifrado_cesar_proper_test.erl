-module(cifrado_cesar_proper_test).
-include_lib("proper/include/proper.hrl").
-export([start/4, stop/1, gen_basic/1, gen_valid/1]).

% IMPORTANTE: Probar todas las propiedades con el parámetro "constraint_tries" >= 1000
% proper:quickcheck(cifrado_cesar_proper_test:prop_length(), {constraint_tries, 1000}).

%%%%%%%%%%%%%%%%%% SETUP & CLEAN UP %%%%%%%%%%%%%%%%%%%%

start(ID, Alf, Cad, Desp) ->
	FileName = "output-" ++ erlang:integer_to_list(ID) ++ ".dat",
	{_, Pid} = cifrado_cesar:programar(ID, Alf, Cad, Desp),
	{Pid, FileName}.

stop(FileName) ->
	%{ok} = file:delete(FileName). Que no gustar?
	os:cmd("rm " ++ FileName).


%%%%%%%%%%%%%%%%%%% DATA GENERATION %%%%%%%%%%%%%%%%%%%%

% Generar un alfabeto, una cadena y un desplazamiento con distintas características.
% El único parámetro indica si el desplazamiento debe ser múltiplo de la 
% longitud del alfabeto o no.

% Los símbolos de la cadena no pertenecen necesariamente al alfabeto.
gen_basic(M) ->
	?LET({Alf, Cad, Desp},
		 {non_empty(list(integer(32, 126))), non_empty(list(integer(32, 126))), non_neg_integer()},
		 if M -> {Alf, Cad, Desp - (Desp rem length(Alf))};
		 	true -> {Alf, Cad, Desp}
		 end).

% Todos los símbolos de la cadena pertenecen al alfabeto.
gen_valid(M) ->
	?SUCHTHAT({Alf, Cad, _},
	gen_basic(M), 
	lists:all(fun(X) -> lists:member(X, Alf) end, Cad)).
	

%%%%%%%%%%%%%%%%%%%% TEST FUNCTIONS %%%%%%%%%%%%%%%%%%%%

% La longitud de la salida siempre coincide con la de la entrada.
prop_length() ->
	?FORALL({Alf, Cad, Desp},
			gen_valid(false),
			collect({Alf, Cad, Desp},
			begin
				{Pid, FileName} = start(1, Alf, Cad, Desp),
				cifrado_cesar:do(Pid),
				Out = os:cmd("cat "++FileName),
				stop(FileName),
				length(Cad) == length(Out)
			end)).

% La salida no contiene símbolos que no pertenecen al alfabeto.
prop_out() ->
	?FORALL({Alf, Cad, Desp},
			gen_valid(false),
			collect({Alf, Cad, Desp},
			begin
				{Pid, FileName} = start(1, Alf, Cad, Desp),
				cifrado_cesar:do(Pid),
				Out = os:cmd("cat "++FileName),
				stop(FileName),
				lists:all(fun(X) -> lists:member(X, Alf) end, Out)
			end)).

% Si D es el desplazamiento y N la longitud del alfabeto,
% cuando D mod N = 0 la salida es idéntica a la entrada.
prop_id() ->
	?FORALL({Alf, Cad, Desp},
			gen_valid(true),
			collect({Alf, Cad, Desp},
			begin
				{Pid, FileName} = start(1, Alf, Cad, Desp),
				cifrado_cesar:do(Pid),
				Out = os:cmd("cat "++FileName),
				stop(FileName),
				Out == Cad
			end)).

% La siguiente propiedad no se cumple cuando se permiten símbolos
% repetidos en el alfabeto.
% Si D es el desplazamiento y N la longitud del alfabeto,
% cuando D mod N =/= 0 la salida es distinta de la entrada.
% prop_not_id() ->
% 	?FORALL({Alf, Cad, Desp},
% 			gen_valid(false),
% 			collect({Alf, Cad, Desp},
% 			begin
% 				{Pid, FileName} = start(1, Alf, Cad, Desp),
% 				cifrado_cesar:do(Pid),
% 				Out = os:cmd("cat "++FileName),
% 				stop(FileName),
% 				if (Desp rem length(Alf)) /= 0 -> Out /= Cad;
% 				   true -> true 
% 				end
% 			end)).

% Si la entrada contiene símbolos que no pertenecen al alfabeto,
% la tarea devuelve un error
prop_in() ->
	fails(?FORALL({Alf, Cad, Desp},
			gen_basic(false),
			collect({Alf, Cad, Desp},
			begin
				{Pid, FileName} = start(1, Alf, Cad, Desp),
				cifrado_cesar:do(Pid),
				Out = os:cmd("cat "++FileName),
				stop(FileName),
				length(Cad) == length(Out)
			end))).
