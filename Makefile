
ERL=erl
ERLC=erlc
DIALYZER=dialyzer

all:
	mkdir -p _build/record_info/ebin
	mkdir -p _build/record_info/include
	mkdir -p _build/examples/ebin
	mkdir -p _build/t/ebin
	cp -f src/record_info.hrl _build/record_info/include
	$(MAKE) -C src
	$(MAKE) -C t
	$(MAKE) -C examples
	$(ERL) -pz _build/record_info/ebin -pz _build/examples/ebin -noinput -run example main
	$(ERL) -pz _build/record_info/ebin -pz _build/examples/ebin -noinput -run example_2 main
	$(ERL) -pz _build/record_info/ebin -pz _build/t/ebin -noinput -run test main
	rm -f _build/test.plt
	# $(DIALYZER) --build_plt --output_plt _build/test.plt -r _build/t/ebin

clean:
	rm -rf *.beam _build

check: all
	test -e _build/check.plt || time dialyzer --build_plt --apps erts kernel stdlib compiler --output_plt _build/check.plt
	$(DIALYZER) --plt _build/check.plt src/*.erl t/*.erl -I _build/record_info/include/ -pa _build/record_info/ebin
