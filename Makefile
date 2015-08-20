APP=deathtoll
REBAR ?= $(shell which rebar3 2>/dev/null || which ./rebar3)

.PHONY: compile clean distclean config start test

all: compile

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

distclean:
	rm -rfv _build log

config:
	cp -nv priv/app.example.config priv/app.config

start:
	exec erl +K true -pz _checkouts/*/ebin -pz _build/default/lib/*/ebin -boot start_sasl -config priv/app.config -s lager -s $(APP)

test:
	$(REBAR) eunit skip_deps=true
