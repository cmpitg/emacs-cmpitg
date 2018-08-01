.PHONY: install-deps install-bin

install-deps:
	emacs --quick --batch \
		--load src/init-bare.el

install-bin:
	ln --verbose --symbolic --interactive $(PWD)/bin/rmacs $(HOME)/bin/
