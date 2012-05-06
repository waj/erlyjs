REBAR:=$(shell which rebar || echo ./rebar)

all: compile

compile:
	@$(REBAR) compile

clean:
	rm -rf erl_crash.dump
	@$(REBAR) clean

check: compile
	erl -pa ebin -noshell -run erlyjs_testsuite run -s init stop
