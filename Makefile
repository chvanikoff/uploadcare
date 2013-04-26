REBAR = `which rebar`

all: clean compile

compile:
	@( $(REBAR) compile )

clean:
	@( $(REBAR) clean )

.PHONY: all compile clean