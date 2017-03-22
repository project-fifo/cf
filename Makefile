TERMCAP_FILE=http://code.metager.de/source/raw/OpenBSD/src/share/termtypes/termtypes.master

src/termcap.erl: termtypes.master.clean mk-termcap.escript termcap.erl
	./mk-termcap.escript termtypes.master.clean  > src/cf_term.erl

mk-termcap.escript:

termcap.erl:

clean:
	[ -f termtypes.master ] && rm termtypes.master || true
	[ -f termtypes.master.clean ] && rm termtypes.master.clean || true

compile:
	rebar3 compile

termtypes.master.clean: termtypes.master
	cat termtypes.master | grep -v '^#' | grep -v '^\s*$$' > termtypes.master.clean

termtypes.master:
	curl -O $(TERMCAP_FILE)
