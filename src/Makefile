ERL=erl
ERL_INC=
ERL_MAKE=-noinput -eval 'init:stop(case make:all() of up_to_date -> 0; _ -> 1 end)'
all:
	$(ERL) $(ERL_INC) $(ERL_MAKE)
