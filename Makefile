.PHONY: install-deps install-bin archive-elpa profile open-package-installation

install-deps:
	emacs --quick --batch \
		--load src/bootstrap-config.el \
		--load src/package-install.el
	# Rust language server: https://github.com/rust-lang/rls
	rustup component add rls rust-analysis rust-src

open-package-installation:
	emacs --quick --load src/bootstrap-config.el src/package-install.el

install-bin:
	ln --verbose --symbolic --interactive $(PWD)/bin/rmacs $(HOME)/bin/

archive-elpa:
	emacs --quick --batch \
		--load src/bootstrap-config.el \
		--load src/package-install.el \
		--eval "(elpamr-create-mirror-for-installed)"

profile:
	bin/profile-ee
