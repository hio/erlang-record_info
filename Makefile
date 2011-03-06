
ERL=erl
ERLC=erlc

all:
	mkdir -p _build/record_info/ebin
	mkdir -p _build/record_info/include
	mkdir -p _build/t/ebin
	cp -f src/record_info.hrl _build/record_info/include
	cd src && $(ERL) -make
	cd t   && $(ERL) -pz ../_build/record_info/ebin -make
	$(ERL) -pz _build/record_info/ebin -pz _build/t/ebin -noinput -eval 'test:test(), init:stop()'

clean:
	rm -rf *.beam _build

check: all
	test -e _build/check.plt || time dialyzer --build_plt --apps erts kernel stdlib compiler --output_plt _build/check.plt
	dialyzer --plt _build/check.plt src/*.erl t/*.erl -I _build/record_info/include/ -pa _build/record_info/ebin
