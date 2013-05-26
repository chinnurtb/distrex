RELNAME=distrex

all:
	rebar compile && relx

run:
	cd _rel/bin; ./$(RELNAME) console
