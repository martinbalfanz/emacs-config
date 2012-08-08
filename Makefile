# path to repository
LN = ln -s ~/emacs-config

all: cleanup
	$(LN)/init.el ~/.emacs
	$(LN)/.emacs.d ~/.emacs.d
	$(LN)/.emacs-auth ~/.emacs-auth
	$(LN)/.emacs-personal ~/.emacs-personal

cleanup:
	rm -f ~/.emacs
	rm -rf ~/.emacs.d
	rm -f ~/.emacs-auth
	rm -f ~/.emacs-personal
