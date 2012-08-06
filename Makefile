# path to repository
LN = ln -s ~/emacs-config

all: cleanup
	$(LN)/.emacs ~/.emacs
	$(LN)/.emacs.d ~/.emacs.d
	$(LN)/.emacs-auth ~/.emacs-auth

cleanup:
	rm -f ~/.emacs
	rm -rf ~/.emacs.d
	rm -f ~/.emacs-auth