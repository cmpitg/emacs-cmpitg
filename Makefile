.PHONY: install-deps install-bin profile-startup

install-deps:
	# Rust language server: https://github.com/rust-lang/rls
	rustup component add rls rust-analysis rust-src
	mkdir -p $(HOME)/.emacs.d/straight/versions/
	ln --verbose --symbolic --interactive $(PWD)/straight-default.el $(HOME)/.emacs.d/straight/versions/default.el
	rmacs --debug-on-error --verbose --name installation --shape edit --verbose --one-off eval '(progn (straight-thaw-versions) (message-box "Installation completed!"))'

profile-startup:
	bin/profile-ee
