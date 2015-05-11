MODULES = src/cliente.erl src/recepcion.erl src/especialista.erl \
	  src/tarea.erl src/cifrado_cesar.erl src/filtro_bn.erl \
	  src/primos.erl

all: compile

compile: $(MODULES)
	test -d ebin/ || mkdir ebin/
	erlc -pa ebin -o ebin $(MODULES)

edoc: $(MODULES)
	erl -noshell -run edoc_run files '["src/cliente.erl", "src/recepcion.erl", "src/especialista.erl", "src/tarea.erl", "src/cifrado_cesar.erl", "src/filtro_bn.erl", "src/primos.erl"]' '[{dir,"doc/html"}]'

dialyzer: $(MODULES)
	erlc +debug_info -pa ebin -o ebin $(MODULES)
	dialyzer -Wunmatched_returns \
                 -Werror_handling    \
                 -Wrace_conditions   \
                 -Wunderspecs ebin/*beam

test: compile
	erlc -pa ebin -o ebin test/unidad/especialistas/especialista_test.erl
	erlc -pa ebin -o ebin test/integracion/recepcion-especialista/disponible_recibir.erl
	erl -noshell -pa ebin -eval 'eunit:test(especialista_test)' -eval 'init:stop()'
	erl -noshell -pa ebin -eval 'eunit:test(disponible_recibir)' -eval 'init:stop()'
#	erl -noshell -pa ebin -eval 'proper:quickcheck(module_test:prop())' -eval 'init:stop()'

clean:
	rm -f output-*
	rm -f ebin/*
	rm -f *~ *.log **/*~ **/*.bak **/*.mdr

mrproper: clean
	rm -rf doc/html/edoc-info doc/html/*.html doc/html/*.css doc/html/*.png
