.PHONY: install-deps install-bin profile-startup

install-deps:
	# Rust language server: https://github.com/rust-lang/rls
	rustup component add rls rust-analysis rust-src

install-bin:
	ln --verbose --symbolic --interactive $(PWD)/bin/rmacs $(HOME)/bin/
	ln --verbose --symbolic --interactive $(PWD)/bin/ebare $(HOME)/bin/
	ln --verbose --symbolic --interactive $(PWD)/bin/ffn $(HOME)/bin/
	# ln --verbose --symbolic --interactive $(PWD)/straight-default.el $(HOME)/.emacs.d/straight/versions/default.el

profile-startup:
	bin/profile-ee
