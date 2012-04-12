all: compile

compile:
	rebar compile

clean:
	rm -rf erl_crash.dump
	rebar clean

check: compile
	erl -pa ebin -noshell -run erlyjs_testsuite run -s init stop
