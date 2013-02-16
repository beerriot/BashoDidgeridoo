.PHONY: rel deps test

all: deps compile

compile:
	@./rebar compile

app:
	@./rebar compile skip_deps=true

deps:
	@./rebar get-deps

clean:
	@./rebar clean

distclean: clean
	@./rebar delete-deps

test: all
	@./rebar skip_deps=true eunit

run: compile
	erl \
	-name phone1@127.0.0.1 \
	-setcookie phone \
	-pa deps/*/ebin \
	-pa apps/*/ebin \
	-eval "application:start(sasl)" \
	-eval "application:start(crypto)" \
	-eval "application:start(webmachine)" \
	-eval "application:start(riak_core)" \
	-eval "application:start(riak_music)"

rel: compile
	@./rebar generate
	@chmod 755 rel/basho_banjo/bin/banjo

rellink:
	$(foreach app,$(wildcard apps/*), rm -rf rel/basho_banjo/lib/$(shell basename $(app))* && ln -sf $(abspath $(app)) rel/basho_banjo/lib;)
	$(foreach dep,$(wildcard deps/*), rm -rf rel/basho_banjo/lib/$(shell basename $(dep))* && ln -sf $(abspath $(dep)) rel/basho_banjo/lib;)
