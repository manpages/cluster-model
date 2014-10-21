PROJECT = clusterm

PKG_FILE = /tmp/.erlang.mk.packages

erl: all
		 @ERL_LIBS=.:deps erl -pa ../cluster-model/ebin deps/*/ebin -s clusterm_app

include erlang.mk
