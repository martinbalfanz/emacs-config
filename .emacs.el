;;;;; -*- emacs-lisp -*-
;;;;;
;;;;; Emacs Configuration File (.emacs)
;;;;;
;;;;;

(require 'cl)


;;;;
;;;; paths
;;;;

(add-to-list 'load-path "~/.emacs.d")

;; setting paths for OS X (homebrew)
(when (eq system-type 'darwin)
  (setenv "PATH"
          (concat
           "/usr/local/bin" ":"
           "/usr/local/sbin" ":"
           "/usr/local/Cellar/ruby/1.9.2-p290/bin" ":"
           "/usr/local/texlive/2011/bin/x86_64-darwin" ":"
           (getenv "PATH")))
  (setq exec-path
        '("/usr/local/bin"
          "/usr/local/sbin"
          "/usr/local/Cellar/ruby/1.9.2-p290/bin"
          "/usr/local/texlive/2011/bin/x86_64-darwin"
          "/usr/bin"
          "/bin"
          "/usr/sbin"
          "/sbin"
          "/usr/X11/bin"
          "/usr/local/Library/Contributions/examples")))


;;;;
;;;; OS X specific keybindings
;;;;

;; only for carbon emacs
(when (not (featurep 'aquamacs))
  (setq mac-option-key-is-meta nil
        mac-command-key-is-meta t
        mac-command-modifier 'meta
        mac-option-modifier nil))

;;;;
;;;; basics
;;;;

(setq inhibit-startup-screen t		;; remove startup screen
      scroll-conservatively 101		;; never recenter point when scrolling
      initial-scratch-message nil	;; start with an empty scratch buffer
      ring-bell-function (lambda ())	;; no annoying bell ringing
      require-final-newline nil		;; don't force new line at end of file
      help-window-select t              ;; select help buffer when opened
      line-move-visual nil              ;; move point by logical lines
      default-major-mode 'text-mode
)

(setq-default cursor-type 'bar		;; cursor style
              ; indent-tabs-mode nil	;; indentation nerver inserts tabs
	      )

(fset 'yes-or-no-p 'y-or-n-p)	;; allow y or n as answers

(auto-compression-mode 1)	;; file editing inside archives
(auto-image-file-mode 1)	;; display image file as image

;;; basic appearance
(column-number-mode 1)		;; show column in modeline
(display-battery-mode -1)
(display-time-mode -1)
(line-number-mode 1)		;; show line number in modeline

(if (eq system-type 'darwin)	;; On OS X the menu bar does appear anyway, so
    (menu-bar-mode 1)		;; I like it to be fully functional.
  (menu-bar-mode -1))		;; (Even though I never use it.)

(scroll-bar-mode -1)		;; hide scrollbars
(toggle-scroll-bar -1)
(tool-bar-mode -1)		;; hide toolbar
(blink-cursor-mode -1)		;; I don't like blinking cursors
(transient-mark-mode -1)
(show-paren-mode 1)		;; highlight matching parenthesis

(mouse-wheel-mode -1)		;; I don't use the mouse at all, so I don't
				;; want to scroll accidentally whenever I touch it.

;;; coding system
(prefer-coding-system       'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment   'utf-8)
(set-file-name-coding-system 'utf-8)
(set-locale-environment "UTF-8")


;;;;
;;;; session management
;;;; Note: may not work in aquamacs.
;;;;

(desktop-save-mode 1)

(setq desktop-dirname "~/"
      desktop-base-file-name ".emacs.desktop"
      history-length 500)

(mapc (lambda (global)
        (add-to-list 'desktop-globals-to-save global))
      '(global-mark-ring
        mark-ring
        kmacro-ring
        kill-ring
        file-name-history
        register-alista))

(add-hook 'auto-save-hook
          (lambda () (desktop-save-in-desktop-dir)))

(savehist-mode 1)


;;;;
;;;; paredit
;;;; thanks to https://github.com/danlei
;;;;

(add-to-list 'load-path "~/.emacs.d/paredit")

(when (require 'paredit "paredit" t)
  (mapc (lambda (hook) (add-hook hook (lambda () (paredit-mode 1))))
        '(slime-mode-hook
          slime-repl-mode-hook
          emacs-lisp-mode-hook
          ielm-mode-hook
          scheme-mode-hook
          inferior-scheme-mode-hook
          inferior-qi-mode-hook
          qi-mode-hook))
  (setq clojure-enable-paredit t))


(add-hook 'paredit-mode-hook
          (lambda ()
            (define-keys paredit-mode-map
                '((")" paredit-close-parenthesis)
                  ("M-)" paredit-close-parenthesis-and-newline)
                  ("}" paredit-close-curly)
                  ("{" paredit-open-curly)
                  ("M-{" paredit-wrap-curly)
                  ("M-[" paredit-wrap-square)
                  ("M-f" paredit-forward)
                  ("C-M-f" forward-word)
                  ("M-b" paredit-backward)
                  ("C-M-b" backward-word)
                  ("M-u" backward-up-list)
                  ("C-M-u" upcase-word)
                  ("M-ö" down-list)
                  ("M-t" transpose-sexps)
                  ("C-M-t" transpose-words)
                  ("<M-backspace>" paredit-backward-kill-word)
                  ("<C-backspace>" backward-kill-sexp)
                  ("M-k" kill-sexp)
                  ("M-a" slime-beginning-of-defun)
                  ("M-e" slime-end-of-defun)
                  ("C-M-a" backward-sentence)
                  ("C-M-e" forward-sentence)
                  ("M-q" indent-pp-sexp)
                  ("C-M-q" fill-paragraph)))))


;;;;
;;;; elisp
;;;;

(defun indent-and-pc-complete (n)
  (interactive "p")
  (indent-for-tab-command)
  (PC-lisp-complete-symbol))

(defun mb-eval-and-execute ()
  "Evaluate and execute current defun."
  (interactive)
  (funcall (eval-defun-2)))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (eldoc-mode 1)
            (define-keys emacs-lisp-mode-map
              '(("TAB" indent-and-pc-complete)
                ("C-c C-e" mb-eval-and-execute)))))

(add-hook 'lisp-interaction-mode-hook
          (lambda ()
            (eldoc-mode 1)
            (define-keys lisp-interaction-mode-map
              '(("TAB" indent-and-pc-complete)
                ("<C-return>" eval-print-last-sexp)))))


;;;;
;;;; misc functions
;;;;

(defun insert-date (prefix)
  "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
  (interactive "P")
  (let ((format (cond ((not prefix) "%d.%m.%Y")
                      ((not prefix) "%d.%m.%y")
                      ((equal prefix '(4)) "%Y-%m-%d")
                      ((equal prefix '(16)) "%A, %d. %B %Y")))
        (system-time-locale "de_DE"))
    (insert (format-time-string format))))

(defun define-keys (mode-map keybindings)
  "Takes a mode map, and a list of (key function-designator)
lists. The functions are bound to the keys in the given mode-map.
Keys are in kbd format."
  (mapc (lambda (keybinding)
          (destructuring-bind (key function) keybinding
            (define-key mode-map (read-kbd-macro key) function)))
        keybindings))

(defun global-set-keys (keybindings)
  "Takes a list of (key function-designator) lists.
The functions are globally bound to the keys. Keys
are in kbd format."
  (mapc (lambda (keybinding)
          (destructuring-bind (key function) keybinding
            (global-set-key (read-kbd-macro key) function)))
        keybindings))


;;;;
;;;; epilogue
;;;;

(load "~/.emacs-personal" t)
(load "~/.emacs-auth" t)

(server-start)

;;;; .emacs ends here
