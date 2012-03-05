all: compile

compile:
	rebar compile

clean:
	rebar clean

tests: compile
	erl -pa ebin -noshell -run erlyjs_testsuite run -s init stop
