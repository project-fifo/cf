TERMCAP_FILE=http://code.metager.de/source/raw/OpenBSD/src/share/termtypes/termtypes.master

compile:
	rebar3 compile

src/termcap.erl: termtypes.master.clean
	cp termcap.erl src/termcap.erl
	./mk-termcap.escript termtypes.master.clean  >> src/termcap.erl

clean:
	[ -f termtypes.master ] && rm termtypes.master || true
	[ -f termtypes.master.clean ] && rm termtypes.master.clean || true
	
termtypes.master.clean: termtypes.master
	cat termtypes.master | grep -v '^#' | grep -v '^\s*$$' > termtypes.master.clean

termtypes.master:
	curl -O $(TERMCAP_FILE)
