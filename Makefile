REBAR = ~/rebar3/rebar3
all: compile
compile: $(REBAR)
	$(REBAR) compile
test: $(REBAR)
	$(REBAR) eunit
$(REBAR):
	cd rebar3/ ; ./bootstrap
clean:
	rm -rf _build
