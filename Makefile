.PHONY: deps test

all: rebar deps compile

compile:
	./rebar compile

deps:
	./rebar get-deps

clean:
	./rebar clean
	rm -rf test.*-temp-data

distclean: clean 
	./rebar delete-deps

test: all
	./rebar skip_deps=true eunit

docs:
	./rebar skip_deps=true doc

dialyzer: compile
	@dialyzer -Wno_return -c ./ebin

run:
	erl -name pns@127.0.0.1 -setcookie nocookie -pa ./deps/*/ebin -pa ./ebin +P 10000000

rebar:
	curl -O http://cloud.github.com/downloads/basho/rebar/rebar
	chmod ugo+x rebar

