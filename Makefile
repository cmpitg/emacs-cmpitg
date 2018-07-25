.PHONY: install-deps

install-deps:
	emacs --quick --batch \
		--load src/bootstrap-config.el \
		--load src/package-install.el
