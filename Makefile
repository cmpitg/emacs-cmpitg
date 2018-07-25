.PHONY: install-deps install-bin

install-deps:
	emacs --quick --batch \
		--load src/bootstrap-config.el \
		--load src/package-install.el

install-bin:
	ln --verbose --symbolic --interactive $(PWD)/bin/rmacs $(HOME)/bin/
