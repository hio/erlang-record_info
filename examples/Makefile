ERL=erl
ERL_INC=-pz ../_build/record_info/ebin -pz ../_build/examples/ebin -pz ../_build/t/ebin
ERL_MAKE=-noinput -eval 'init:stop(case make:all() of up_to_date -> 0; _ -> 1 end)'
all:
	$(ERL) $(ERL_INC) $(ERL_MAKE)
