.PHONY: install-deps install-bin archive-elpa

install-deps:
	emacs --quick --batch \
		--load src/bootstrap-config.el \
		--load src/package-install.el

install-bin:
	ln --verbose --symbolic --interactive $(PWD)/bin/rmacs $(HOME)/bin/

archive-elpa:
	emacs --quick --batch \
		--load src/bootstrap-config.el \
		--load src/package-install.el \
		--eval "(elpamr-create-mirror-for-installed)"
