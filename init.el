;;;;; -*- mode: emacs-lisp -*-
;;;;;
;;;;; Emacs Configuration File (.emacs)
;;;;;
;;;;;

;;;; Code:

;;;;_. Initialization
(require 'cl)


;;;;_ , paths

(defvar user-init-directory (file-name-directory user-init-file)
  "Directory of init-file.")
(defvar user-tmp-directory (expand-file-name "tmp" user-init-directory)
  "Directory for temporary files.
   e.g. used for: - org-mode persistent clock
                  - savehist
                  - desktop")

;; dir of init.el
(add-to-list 'load-path user-init-directory)
;; dir of user site-lisp
(add-to-list 'load-path user-emacs-directory)

;; setting paths for OS X (homebrew)
(when (eq system-type 'darwin)
  (setenv "PATH"
          (concat
           "/usr/local/bin" ":"
           "/usr/local/sbin" ":"
           "~/.rvm/bin" ":"
           "/usr/texbin/" ":"
           "/usr/local/texlive/2011/bin/x86_64-darwin" ":"
           (getenv "PATH")))
  (setq exec-path
        '("/usr/local/bin"
          "/usr/local/sbin"
          "~/.rvm/bin"
          "/usr/texbin/"
          "/usr/local/texlive/2011/bin/x86_64-darwin"
          "/usr/bin"
          "/bin"
          "/usr/sbin"
          "/sbin"
          "/usr/X11/bin"
          "/usr/local/Library/Contributions/examples")))


;;;;_ , use-package & diminsh

(add-to-list 'load-path (expand-file-name "use-package" user-emacs-directory))
(require 'use-package)
(eval-when-compile
  (setq use-package-verbose (null byte-compile-current-file)))

(use-package diminish
  :load-path "diminish")


;;;;_ , OS X specific keybindings

;; only for carbon emacs
(when (not (featurep 'aquamacs))
  (setq mac-option-key-is-meta nil
        mac-command-key-is-meta t
        mac-command-modifier 'meta
        mac-option-modifier nil))

;;;;_ , basics

(setq inhibit-startup-screen t          ;; remove startup screen
      scroll-conservatively 101         ;; never recenter point when scrolling
      initial-scratch-message nil       ;; start with an empty scratch buffer
      ring-bell-function (lambda ())    ;; no annoying bell ringing
      require-final-newline nil         ;; don't force new line at end of file
      help-window-select t              ;; select help buffer when opened
      line-move-visual nil              ;; move point by logical lines
      default-major-mode 'text-mode
      show-trailing-whitespace t        ;; show trailing whitespace
      debug-on-error t                  ;; show backtrace on error
      backup-directory-alist `((".*" . ,(expand-file-name
                                         "~/.emacs-backups/")))
      auto-save-file-name-transforms `((".*" ,(expand-file-name
                                               "~/.emacs-backups/") t))
      vc-make-backup-files t)

(setq-default cursor-type 'bar          ;; cursor style
              indent-tabs-mode nil      ;; indentation never inserts tabs
              lisp-indent-offset nil)

(fset 'yes-or-no-p 'y-or-n-p)           ;; allow y or n as answers

(put 'narrow-to-region 'disabled nil)   ;; enable narrowing
(put 'erase-buffer 'disabled nil)

(auto-compression-mode 1)       ;; file editing inside archives
(auto-image-file-mode 1)        ;; display image file as image

;;; basic appearance
(column-number-mode 1)          ;; show column in modeline
(display-battery-mode 1)
(display-time-mode -1)
(line-number-mode 1)            ;; show line number in modeline

(if (eq system-type 'darwin)    ;; On OS X the menu bar does appear anyway, so
    (menu-bar-mode 1)           ;; I like it to be fully functional.
  (menu-bar-mode -1))           ;; (Even though I never use it.)

(when window-system
  (scroll-bar-mode -1)          ;; hide scrollbars
  (toggle-scroll-bar -1)
  (tool-bar-mode -1))           ;; hide toolbar
(blink-cursor-mode -1)          ;; I don't like blinking cursors
(transient-mark-mode -1)
(show-paren-mode 1)             ;; highlight matching parenthesis

(mouse-wheel-mode -1)           ;; I don't use the mouse at all, so I don't
;; want to scroll accidentally whenever I touch it.

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;;; coding system
(prefer-coding-system        'utf-8-unix)
(set-default-coding-systems  'utf-8-unix)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-language-environment    'utf-8)
(set-file-name-coding-system 'utf-8)
(set-locale-environment      "UTF-8")

(setq system-uses-terminfo nil)

;;;;_ , mode-line
;;; vgl. http://emacs-fu.blogspot.de/2011/08/customizing-mode-line.html
;;;      http://amitp.blogspot.de/2011/08/emacs-custom-mode-line.html
;;;      http://idorobots.org/2013/03/30/greader-alternative/
;; (make-face 'mode-line-font-awesome)
;; (set-face-attribute 'mode-line-font-awesome nil
;;                     :inherit 'mode-line-face
;;                     :family "FontAwesome")

;; (defvar mb-default-mode-line mode-line-format)
;; (setq-default mode-line-format
;;               '((:eval mb-default-mode-line)
;;                 (:propertize "" face mode-line-font-awesome)))

;; (setq-local mode-line-format
;;             '(("%e"
;;                mode-line-front-space
;;                mode-line-mule-info
;;                mode-line-client
;;                mode-line-modified
;;                mode-line-remote
;;                mode-line-frame-identification
;;                mode-line-buffer-identification
;;                "   "
;;                mode-line-position
;;                (vc-mode vc-mode)
;;                "  "
;;                mode-line-modes
;;                mode-line-misc-info
;;                mode-line-end-spaces)))


;;;;_ , calendar settings
(setq calendar-week-start-day 1
      european-calendar-style t)

;;;;_ , misc functions

(defun indent-buffer ()
  "Reindents the whole buffer."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max)))

(defun indent-defun ()
  "Indent the current defun."
  (interactive)
  (save-excursion
    (mark-defun)
    (indent-region (region-beginning) (region-end))))

(defun google ()
  "Google the selected region if any, display a query prompt otherwise."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string
     (if (region-active-p)
         (buffer-substring (region-beginning) (region-end))
       (read-string "Google: "))))))

(defun insert-date (prefix)
  "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
  (interactive "P")
  (let ((format (cond ((not prefix) "%d.%m.%Y")
                      ((equal prefix '(4)) "%Y-%m-%d")
                      ((equal prefix '(16)) "%A, %d. %B %Y")))
        (system-time-locale "en_US"))
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

(defun mb-kill-and-join-forward (&optional arg)
  "If at end of line, join with following; otherwise kill line.
Deletes whitespace at join."
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (delete-indentation t)
    (kill-line arg)))

(defun mb-newline-beneath ()
  "Add a new newline beneath the current line."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun mb-newline-above ()
  "Add a newline above the current line."
  (interactive)
  (previous-line)
  (end-of-line)
  (newline-and-indent))

(defun mb-beginning-of-line (&optional arg)
  "Toggle point between beginning of line and first non-whitespace."
  (interactive "P")
  (if (and (bolp) (not arg))
      (back-to-indentation)
    (beginning-of-line arg)))

(defun buffer-contains-regex-p (regex)
  "Check if buffer contains a given REGEX."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (if (search-forward-regexp regex nil t)
          t
        nil))))

(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))


;;;;_ , winner-mode
(winner-mode 1)
(windmove-default-keybindings)


;;;;_ , global key bindings

(global-set-keys '(("C-k" mb-kill-and-join-forward)
                   ("C-o" mb-newline-beneath)
                   ("C-S-o" mb-newline-above)
                   ("C-a" mb-beginning-of-line)
                   ;; ("C-. d" insert-date)
                   ("C-h C-f" find-function)
                   ;; ("C-. g" google)
                   ))


;;;;_ , session management
;;;; Note: may not work in aquamacs.

;; (desktop-save-mode nil)

;; (setq desktop-dirname user-tmp-directory
;;       desktop-base-file-name ".emacs.desktop"
;;       savehist-file (expand-file-name "history" user-tmp-directory)
;;       history-length 500)

;; (mapc (lambda (global)
;;         (add-to-list 'desktop-globals-to-save global))
;;       '(global-mark-ring
;;         mark-ring
;;         kmacro-ring
;;         kill-ring
;;         file-name-history
;;         register-alista))

;; (add-hook 'auto-save-hook
;;           (lambda () (desktop-save-in-desktop-dir)))

;; (savehist-mode 1)

;;;;_ , reading-mode
(define-minor-mode reading-mode
  "A minor mode providing a better reading experience."
  nil " Reading" nil
  (if reading-mode
      (progn
        (variable-pitch-mode 1)
        (setq line-spacing 5))
    (variable-pitch-mode nil)
    (setq line-spacing (default-value 'line-spacing))))


;;;;_ , info-mode
(add-hook 'info-mode-hook
          (lambda ()
            (reading-mode 1))
          t)

;;;;_ , gnupg
(require 'epa-file)
(epa-file-enable)


;;;;_. Packages
;;;;_ , ace-jump-mode

(use-package ace-jump-mode
  :load-path "ace-jump-mode"
  :bind ("C-. j" . ace-jump-mode))


;;;;_ , expand-region

(use-package expand-region
  :load-path "expand-region"
  :bind ("C-#" . er/expand-region))


;;;;_ , occur

(add-hook 'occur-mode-hook
          (lambda ()
            (next-error-follow-minor-mode 1)
            (hl-line-mode 1)
            (local-set-key (kbd "n") 'next-line)
            (local-set-key (kbd "p") 'previous-line)))

(add-hook 'occur-edit-mode-hook
          (lambda ()
            (local-unset-key (kbd "n"))
            (local-unset-key (kbd "p"))))

(defadvice occur
  (after mb-switch-to-occur last () activate)
  "Switch to occur window automatically."
  (other-window 1))

(defadvice ibuffer-do-occur
  (after mb-ibuffer-switch-to-occur last () activate)
  "Switch to occur window automatically."
  (other-window 1))


;;;;_ , electric-pair-mode
;; (mapc (lambda (hook) (add-hook hook (lambda () (electric-pair-mode 1))))
;;       '(text-mode-hook
;;         org-mode-hook
;;         html-mode-hook
;;         js2-mode-hook
;;         css-mode-hook
;;         web-mode-hook))


;;;;_ , paredit
;;;; thanks to https://github.com/danlei

(use-package paredit
  :load-path "paredit"
  :commands paredit-mode
  :diminish paredit-mode
  :init
  (progn
    (mapc (lambda (hook) (add-hook hook (lambda () (paredit-mode 1))))
          '(slime-mode-hook
            ;; slime-repl-mode-hook
            clojure-mode-hook
            nrepl-mode-hook
            emacs-lisp-mode-hook
            lisp-mode-hook
            ielm-mode-hook
            scheme-mode-hook
            inferior-scheme-mode-hook
            inferior-qi-mode-hook
            qi-mode-hook))
    (setq clojure-enable-paredit t))

  :config
  (progn
    (defadvice paredit-kill (around mb-paredit-kill () activate)
      (if (eolp)
          (delete-indentation t)
        ad-do-it))

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
        ("M-a" beginning-of-defun)
        ("M-e" end-of-defun)
        ("C-M-a" backward-sentence)
        ("C-M-e" forward-sentence)
        ("M-q" indent-pp-sexp)
        ("C-M-q" fill-paragraph)))))


;;;;_ , emacs-lisp

(defun dhl-lisp-indent-and-complete (n)
  (interactive "p")
  (indent-for-tab-command)
  (lisp-complete-symbol))

(defun dhl-lisp-eval-print-defun ()
  (interactive)
  (end-of-defun)
  (eval-print-last-sexp))

(defun mb-eval-and-execute ()
  "Evaluate and execute current defun."
  (interactive)
  (funcall (eval-defun-2)))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (eldoc-mode 1)
            (diminish 'eldoc-mode)
            (highlight-symbol-mode 1)
            (highlight-defined-mode 1)
            (define-keys emacs-lisp-mode-map
              '(("TAB" dhl-lisp-indent-and-complete)
                ("<C-return>" dhl-lisp-eval-print-defun)
                ("C-c C-e" mb-eval-and-execute)))))

(add-hook 'lisp-mode-hook ;; 'lisp-interaction-mode-hook
          (lambda ()
            (eldoc-mode 1)
            (diminish 'eldoc-mode)
            (highlight-symbol-mode 1)
            (highlight-defined-mode 1)
            (slime-mode 1)
            (define-keys ;; lisp-interaction-mode-map
              lisp-mode-map
              '(("TAB" dhl-lisp-indent-and-complete)
                ("<C-return>" dhl-lisp-eval-print-defun)
                ("C-c C-e" mb-eval-and-execute)))))


;;;;_ , ielm

(use-package ielm
  :bind ("C-c :" . ielm)
  :commands ielm
  :config
  (progn
    (setq ielm-prompt "elisp> ")
    (add-hook 'ielm-mode-hook
              (lambda ()
                (eldoc-mode 1)
                (diminish 'eldoc-mode)
                (setq comint-dynamic-complete-functions
                      '(ielm-tab
                        comint-replace-by-expanded-history
                        ielm-complete-filename
                        ielm-complete-symbol))))))


;;;;_ , eshell

(use-package eshell
  :defer t
  :config
  (progn
    (add-hook 'eshell-mode-hook
              (lambda ()
                (define-keys eshell-mode-map
                  '(("C-a" eshell-maybe-bol)))))

    (defun eshell-maybe-bol ()
      "Moves point behind the eshell prompt, or
at the beginning of line, if already there."
      (interactive)
      (let ((p (point)))
        (eshell-bol)
        (when (= p (point))
          (beginning-of-line))))

    (defun eshell-clear ()
      "Clears the eshell buffer."
      (interactive)
      (let ((inhibit-read-only t))
        (erase-buffer)))))


;;;;_ , slime

(add-to-list 'load-path "~/emacs-config/site-lisp/slime")
(add-to-list 'load-path "~/emacs-config/site-lisp/slime/contrib")
(require 'slime)
(slime-setup)

(use-package slime
  :load-path ("slime"
              "slime/contrib")
  :commands (slime
             slime-mode
             slime-connect
             define-slime-contrib)
  :init
  (add-hook 'slime-mode-hook
            #'(lambda ()
                (slime-setup
                 '(slime-asdf
                   slime-fancy ;; problems with swank-js
                   slime-references
                   slime-indentation
                   ;; slime-js
                   ;; slime-repl
                   ))

                (define-keys slime-mode-map
                  '(("<return>" paredit-newline)
                    ;; ("C-<return>" other-window)
                    ("C-h F" info-lookup-symbol)))))

  :config
  (progn
    (setq slime-net-coding-system 'utf-8-unix
          slime-enable-evaluate-in-emacs t

          slime-lisp-implementations
          '((ccl ("ccl" "-K utf-8"))
            (abcl ("abcl"))
            (sbcl ("sbcl" "--core") :coding-system utf-8-unix)
            (clisp ("clisp" "-E utf-8" "-modern")))

          slime-default-lisp 'ccl)))

;; (setq inferior-lisp-program "ccl")

;;;;_ , hyperspec

(use-package hyperspec
  :bind ("C-. h l" . hyperspec-lookup)
  :init
  (defadvice hyperspec-lookup (around hyperspec-lookup-with-emacs-w3m first () activate)
    "Use emacs-w3m to lookup hyperspec."
    (let ((browse-url-browser-function 'w3m-browse-url)
          (common-lisp-hyperspec-root (expand-file-name "~/HyperSpec/HyperSpec/"))
          (common-lisp-hyperspec-symbol-table (expand-file-name "~/HyperSpec/HyperSpec/Data/Map_Sym.txt")))
      ad-do-it)))

;;;;_ , dash.el

(use-package dash
  :bind ("C-c d" . dash-at-point)
  :load-path "dash.el")


;;;;_ , s.el

(use-package s
  :load-path "s.el")


;;;;_ , clojure

(use-package clojure-mode
  :load-path "clojure-mode"
  :mode (("\\.cljs$" . clojure-mode)
         ("\\.clj$" . clojure-mode)))

;;;;_ , cider
(use-package queue
  :load-path "elpa/queue-0.1.1")

(use-package cider
  :load-path "cider"
  :commands (cider cider-mode cider-jack-in)
  :requires dash
  :config
  (progn
    (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
    (add-hook 'cider-repl-mode-hook 'paredit-mode)
    ;; (setq nrepl-buffer-name-show-port t)
    (setq cider-repl-result-prefix ";; => ")
    ))


;;;;_ , haskell
(use-package haskell-mode-autoloads
  :load-path "haskell-mode"
  :init
  (progn
    (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
    (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
    (setq haskell-process-process-cabal "~/.cabal/bin/cabal"
          haskell-process-path-ghci "/opt/homebrew-cask/Caskroom/ghc/7.8.3-r0/ghc-7.8.3.app/Contents/bin/ghci")))
(add-to-list 'Info-default-directory-list (expand-file-name "haskell-mode" user-emacs-directory))

;;;;_ , f.el

(use-package f
  :load-path "f.el")


;;;;_ , pkg-info

(use-package epl
  :load-path "epl")

(use-package pkg-info
  :load-path "pkg-info.el")


;;;;_ , nrepl

(use-package nrepl
  :load-path "nrepl"
  :disabled t
  :init
  (progn
    (add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)))


;;;;_ , ido

(use-package ido
  :init
  (ido-mode t))


;;;;_ , smex

(use-package smex
  :load-path "smex"
  :bind ("M-X" . dhl-invoke-smex)
  :requires ido
  :config
  (progn
    (smex-initialize)
    (setq smex-save-file "~/.smex")
    (smex-auto-update)

    (defun dhl-invoke-smex (x)
      "Invokes smex, if called without a prefix argument,
smex-major-mode-commands otherwise. Note that this
prevents using commands with prefix arguments."
      (interactive "p")
      (if (= x 1)
          (smex)
        (smex-major-mode-commands)))))


;;;;_ , ibuffer

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  (progn
    (setq ibuffer-show-empty-filter-groups nil
          ibuffer-expert nil
          ibuffer-saved-filter-groups '(("default"
                                         ("org" (or (name . "\\.org$")
                                                    (mode . org-mode)))
                                         ("elisp" (or (name . "\\.el$")
                                                      (mode . emacs-lisp-mode)))
                                         ("common lisp" (or (name . "\\.lisp$")
                                                            (name . "\\.cl$")
                                                            (name . "\\.lsp$")
                                                            (mode . lisp-mode)))
                                         ("clojure" (or (name . "\\.clj$")
                                                        (name . "\\.cljs$")
                                                        (mode . clojure-mode)))
                                         ("javascript" (or (name . "\\.js$")
                                                           (mode . javascript-mode)
                                                           (mode . js2-mode)
                                                           (mode . espresso-mode)
                                                           (name . "\\.ls$")
                                                           (mode . lispyscript-mode)))
                                         ("html" (or (name . "\\.html$")
                                                     (name . "\\.erb$")
                                                     (name . "\\.jade$")
                                                     (name . "\\.slim$")
                                                     (name . "\\.haml$")
                                                     (mode . html-mode)
                                                     (mode . web-mode)
                                                     (mode . slim-mode)
                                                     (mode . haml-mode)))
                                         ("css" (or (name . "\\.css$")
                                                    (mode . css-mode)))
                                         ("scss" (or (name . "\\.scss$")
                                                     (name . "\\.sass$")
                                                     (mode . scss-mode)
                                                     (mode . sass-mode)))
                                         ("less" (name . "\\.less$"))
                                         ("snippet" (or (name . "\\.yasnippet$")
                                                        (mode . snippet-mode)))
                                         ("pdf" (name . "\\.pdf$"))
                                         ("markdown" (or (name . "\\.md$")
                                                         (mode . markdown-mode)))

                                         ("yml" (or (name . "\\.yml$")
                                                    (mode . yaml-mode)))
                                         ("erc" (mode . erc-mode))
                                         ("twitter" (mode . twittering-mode))
                                         ("dired" (mode . dired-mode))
                                         ("gnus" (or
                                                  (mode . message-mode)
                                                  (mode . bbdb-mode)
                                                  (mode . mail-mode)
                                                  (mode . gnus-group-mode)
                                                  (mode . gnus-summary-mode)
                                                  (mode . gnus-article-mode)
                                                  (name . "^\\.bbdb$")
                                                  (name . "^\\.newsrc-dribble$")))
                                         ("special" (name . "^\\*.*\\*")))))
    (add-hook 'ibuffer-mode-hook
              (lambda ()
                (ibuffer-switch-to-saved-filter-groups "default")
                (ibuffer-auto-mode 1)
                (hl-line-mode 1)))

    (defadvice ibuffer
      (around ibuffer-point-to-most-recent first () activate)
      "Open ibuffer with cursor pointed to most recent buffer name."
      (let ((recent-buffer-name (buffer-name)))
        ad-do-it
        (ibuffer-jump-to-buffer recent-buffer-name)))))

;;;;_ , erc
;;;; get erc-extras from http://ftp.gnu.org/old-gnu/erc/

(use-package dot-erc
  :load-path "erc-5.3-extras"
  :commands erc)


;;;;_ , twittering-mode

(use-package twittering-mode
  :load-path "twittering-mode"
  :commands twit
  :config
  (progn
    (setq twittering-icon-mode t
          twittering-timer-interval 150
          twittering-number-of-tweets-on-retrieval 100
          Twittering-use-ssl t
          twittering-use-master-password nil
          twittering-scroll-mode t
          twittering-initial-timeline-spec-string '(":home"
                                                    ":replies"
                                                    ":favorites"
                                                    ":direct_messages"))
    (twittering-enable-unread-status-notifier)

    (define-keys twittering-mode-map
      '(("n" twittering-goto-next-status)
        ("p" twittering-goto-previous-status)
        ("j" twittering-goto-next-status-of-user)
        ("k" twittering-goto-previous-status-of-user)))))


;;;;_ , auto-complete

(use-package auto-complete-config
  :commands auto-complete-mode
  :load-path ("popup-el"
              "auto-complete"
              "auto-complete/dict")
  :init
  (progn
    (mapc (lambda (hook) (add-hook hook (lambda () (auto-complete-mode 1))))
          '(css-mode-hook
            html-mode-hook
            js2-mode-hook
            emacs-lisp-mode-hook
            lisp-mode-hook
            slime-mode-hook
            slime-repl-mode-hook
            ruby-mode-hook
            haml-mode-hook
            org-mode-hook
            latex-mode-hook
            LaTeX-mode-hook
            slim-mode-hook))

    (add-hook 'emacs-lisp-mode-hook
              (lambda ()
                (setq ac-sources '(ac-source-yasnippet
                                   ac-source-features
                                   ac-source-functions
                                   ac-source-variables
                                   ac-source-symbols
                                   ac-source-abbrev
                                   ac-source-dictionary
                                   ac-source-words-in-same-mode-buffers)))))
  :config
  (progn
    ;; (ac-set-trigger-key "TAB")
    (setq ;; ac-auto-start nil
     ac-use-quick-help t
     ac-use-comphist t
     ac-ignore-case 'smart
     ;; ac-fuzzy-enable t
     )

    (setq-default ac-sources '(ac-source-yasnippet
                               ac-source-abbrev
                               ac-source-dictionary
                               ac-source-words-in-same-mode-buffers))

    (add-to-list 'ac-modes 'slime-repl-mode)))

(use-package ac-slime
  :load-path "ac-slime"
  :commands (set-up-slime-ac
             ac-source-slime-simple
             ac-slime-fuzzy)
  :init
  (progn
    (add-hook 'slime-mode-hook 'set-up-slime-ac)
    (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
    (eval-after-load "auto-complete"
      '(add-to-list 'ac-modes 'slime-repl-mode))))


;;;;_ , org-mode
(use-package org
  :load-path ("org-mode/lisp"
              "org-mode/contrib/lisp")
  :mode ("\\.org$" . org-mode)
  :bind (("C-c a" . org-agenda)
         ("C-. c" . org-capture))
  :init
  (progn
    (add-to-list 'Info-default-directory-list (expand-file-name "org-mode/info" user-emacs-directory))
    (add-hook 'org-agenda-cleanup-fancy-diary-hook
              (lambda ()
                (goto-char (point-min))
                (save-excursion
                  (while (re-search-forward "^[a-z]" nil t)
                    (goto-char (match-beginning 0))
                    (insert "0:00-24:00 ")))
                (while (re-search-forward "^ [a-z]" nil t)
                  (goto-char (match-beginning 0))
                  (save-excursion
                    (re-search-backward "^[0-9]+:[0-9]+-[0-9]+:[0-9]+ " nil t))
                  (insert (match-string 0)))))
    (add-hook 'org-mode-hook
              (lambda ()
                (auto-fill-mode)
                (setq org-hide-leading-stars t
                      adaptive-fill-mode t
                      org-log-done t)))
    (add-hook 'org-agenda-mode-hook
              (lambda ()
                (hl-line-mode 1))))
  :config
  (progn
    (require 'org-mime)
    (require 'org-capture)
    (require 'org-toc)
    (require 'org-special-blocks)
    (require 'org-latex)
    (require 'ox-latex)
    (require 'ox-freemind)

    (require 'org-mac-iCal)

    (unless (boundp 'org-export-latex-classes)
      (setq org-export-latex-classes nil))

    ;; (setq org-agenda-files (file-expand-wildcards "~/Dropbox/notes/*.org"))
    (setq org-agenda-files '("~/Dropbox/notes/todox.org"))
    (setq org-default-notes-file "~/Dropbox/notes/backlogx.org")

    (setq org-capture-templates
          '(("T" "Todo" entry (file+headline "backlogx.org" "Tasks")
             "* TODO %^{Brief Description} %^g\n  %?\n   Added: %U\n  %i\n  [[%F]]")
            ("t" "Todo" entry (file+headline "backlogx.org" "Tasks")
             "* %^{Brief Description} %^g\n   %?\n  Added: %U\n  %i\n  [[%F]]")
            ("n" "Note" entry (file+headline "backlogx.org" "Notes")
             "* %^{Brief Description} %^g\n   %?\n  Added: %U\n  %i\n  [[%F]]")))


    (add-to-list 'org-agenda-custom-commands
                 '("H" "Office and Home Lists"
                   ((agenda)
                    (tags-todo "office")
                    (tags-todo "phone")
                    (tags-todo "email")
                    (tags-todo "lunchtime")
                    (tags-todo "time")
                    (tags-todo "project")
                    (tags-todo "reading"))))

    (add-to-list 'org-agenda-custom-commands
                 '("D" "Daily Action List"
                   ((agenda "" ((org-agenda-ndays 1)
                                (org-agenda-sorting-strategy
                                 (quote ((agenda time-up priority-down tag-up))))
                                (org-deadline-warning-days 0))))))

    ;; (add-to-list 'org-agenda-custom-commands
    ;;              '("p" "Priorities"
    ;;                ((agenda)
    ;;                 ("pa" "A items" tags-todo "+PRIORITY=\"A\"")
    ;;                 ("pb" "B items" tags-todo "+PRIORITY=\"B\"")
    ;;                 ("pc" "C items" tags-todo "+PRIORITY=\"C\""))))

    (setq org-directory (expand-file-name "~/Dropbox/notes/")
          org-refile-targets '(("todox.org" :maxlevel . 2)
                               ("backlogx.org" :level . 1)))

    (setq org-log-reschedule 'time
          org-log-redeadline 'time
          org-clock-persist 'history
          org-clock-modeline-total 'current
          org-clock-persist-file (expand-file-name "org-clock-save.el" user-tmp-directory)
          org-clock-idle-time 10
          org-hierarchical-todo-statistics nil
          org-table-export-default-format "orgtbl-to-csv"
          org-agenda-columns-add-appointments-to-effort-sum t
          org-agenda-default-appointment-duration 60
          org-startup-with-inline-images t
          org-startup-with-latex-preview t)

    (setq org-agenda-include-diary t)

    (setq org-use-speed-commands t)

    ;; (org-clock-persistence-insinuate)

    (setq org-todo-keywords
          '((type "TODO" "STARTED" "WAITING" "APPT" "|" "DONE" "CANCELED"  "DEFERRED")
            ;; (sequence "PROJECT" "SUB-PROJECT" "MILESTONE" "|" "FINISHED")
            ;; (sequence "INVOICE" "SENT" "|" "RCVD")
            ;; (sequence "BUG" "ISSUE" "FEATURE" "|" "FIXED")
            ))

    (setq org-stuck-projects
          '("+PROJECT/+SUB-PROJECT/+MILESTONE/-FINISHED"
            ("TODO" "WAITING" "WIP" "TESTING")
            ()
            "\\<IGNORE\\>"))

    (setq org-mobile-directory "~/Dropbox/MobileOrg")

    (setq org-mime-library 'semi
          org-src-fontify-natively t)

    ;; reset clocksum-format to old version
    ;; (setq org-time-clocksum-format '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

    (setq org-latex-pdf-process (quote ("texi2dvi --pdf --clean --verbose --batch %f")))

    ;; Custom HEADER
    (add-to-list 'org-latex-classes
                 '("ieee"
                   "\\documentclass{IEEEtran}"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")))))


;;;;_ , outline-mode

(use-package outline
  :commands (outline-mode
             outline-minor-mode)
  :init
  (add-hook 'outline-minor-mode-hook
	    (lambda ()
	      (define-key outline-minor-mode-map [(control tab)] 'org-cycle)
	      (define-key outline-minor-mode-map [(shift tab)] 'org-global-cycle))))


;;;;_ , mu4e

(setq smtpmail-default-smtp-server "smtp.gmail.com")
(require 'smtpmail)
(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'message-send-mail-with-sendmail
      message-signature t
      message-signature-directory (expand-file-name "~/Documents/signatures/")
      message-signature-file "default.txt"
      message-signature-insert-empty-line t

      smtpmail-stream-type 'starttls
      ;; smtpmail-local-domain "gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-debug-info t
      smptmail-debug-verb t
      smtpmail-warn-about-unknown-extensions t
      smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg")

      starttls-use-gnutls t
      starttls-extra-arguments '("--no-ca-verification")

      sendmail-program "/usr/local/bin/msmtp")

(use-package mu4e
  :load-path "mu/mu4e"
  :commands mu4e
  :config
  (progn
    (require 'org-mu4e)
    (setq org-auto-fill-function nil)
    (setq mu4e-attachment-dir (expand-file-name "~/Downloads")
          mu4e-confirm-quit nil

          mu4e-user-mail-address-list '("martin.balfanz@gmail.com"
                                        "me@martinbalfanz.com"
                                        "info@martinbalfanz.com"
                                        "martin.balfanz@minglabs.com"
                                        "info@mailme.io")

          mu4e-use-fancy-chars nil
          mu4e-compose-signature t
          org-mu4e-convert-to-html t

          mu4e-get-mail-command "offlineimap -u ttyui"
          ;; mu4e-get-mail-command "~/bin/mailrun.sh"
          ;; mu4e-get-mail-command "true"
          mu4e-maildir (expand-file-name "~/mail")
          mu4e-mu-binary "/usr/local/bin/mu"
          mu4e-mu-home nil ;; default dir
          mu4e-my-email-addresses user-mail-addresses ;; defined in .emacs
          mu4e-update-interval 180
          mu4e-headers-results-limit 1000)

    ;;_ folders
    (setq mu4e-drafts-folder "/drafts"
          mu4e-sent-folder "/sent"
          mu4e-trash-folder "/trash"
          mu4e-sent-messages-behavior (lambda ()
                                        (if (string= (message-sendmail-envelope-from) "info@mailme.io")
                                            'sent
                                          'delete))
          ;; mu4e-maildir-shortcuts
          ;; '(("/INBOX"                     . ?i)
          ;;   ("/[Google Mail].Sent Mail"   . ?s)
          ;;   ("/[Google Mail].Trash"       . ?t)
          ;;   ("/[Google Mail].All Mail"    . ?a))
          )

    (defun mb-mu4e-archive-gmail (msg)
      "Remove label '\Inbox' to archive MSG."
      (mu4e-action-retag-message msg "\"-\\\\Inbox\""))

    (defun mb-mu4e-delete-gmail (msg)
      "Add label '\Trash' to delete MSG."
      (mu4e-action-retag-message msg "\"+\\\\Trash\""))

    (defun mb-mu4e-delete-marked-message ()
      (interactive)
      (mu4e-headers-for-each
       (lambda (msg)
         (when (mu4e-mark-docid-marked-p (mu4e-message-field msg :docid))
           (mb-mu4e-delete-gmail msg)
           (mb-mu4e-archive-gmail msg)
           (mu4e-mark-set 'unmark)))))

    (defun mb-mu4e-archive-marked-message ()
      (interactive)
      (mu4e-headers-for-each
       (lambda (msg)
         (when (mu4e-mark-docid-marked-p (mu4e-message-field msg :docid))
           (mb-mu4e-archive-gmail msg)
           (mu4e-mark-set 'unmark)))))

    (setq mu4e-view-actions '(("archive message" . mb-mu4e-archive-gmail)
                              ("delete message" . mb-mu4e-delete-gmail)
                              ;; ("view as pdf" . mu4e-action-view-as-pdf)
                              ("view in browser" . mu4e-action-view-in-browser)
                              ("retag message" . mu4e-action-retag-message)
                              ("capture message" . mu4e-action-capture-message)))

    (setq mu4e-headers-actions '(("archive message" . mb-mu4e-archive-gmail)
                                 ("delete message" . mb-mu4e-delete-gmail)
                                 ("retag message" . mu4e-action-retag-message)
                                 ("capture message" . mu4e-action-capture-message)))

    (defvar my-mu4e-account-alist
      '(("gmail"
         (user-mail-address "martin.balfanz@gmail.com")
         (message-signature-file nil))
        ("info"
         (user-mail-address "info@martinbalfanz.com")
         (smtpmail-local-domain "martinbalfanz.com")
         (message-signature-file "info.txt"))
        ("mailme"
         (user-mail-address "info@mailme.io")
         (smtpmail-local-domain "mailme.io")
         (smtpmail-default-smtp-server "mail.gandi.net")
         (smtpmail-smtp-server "mail.gandi.net")
         (smtpmail-smtp-user "info@mailme.io")
         (message-signature-file nil)
         (mu4e-sent-folder "/mailme/Sent")
         (mu4e-drafts-folder "/mailme/Drafts")
         (mu4e-trash-folder "/mailme/Trash"))
        ("me"
         (user-mail-address "me@martinbalfanz.com")
         (message-signature-file nil)
         (smtpmail-local-domain "martinbalfanz.com"))
        ("ming"
         (user-mail-address "martin.balfanz@minglabs.com")
         (smtpmail-local-domain "minglabs.com")
         (message-signature-file "ming.txt"))))

    (add-hook 'mu4e-index-updated-hook
              (lambda ()
                (shell-command (concat "terminal-notifier"
                                       " -title mu4e"
                                       " -message 'Mail-Index update.'"
                                       ;" -sound Ping"
                                       ))))
    ;; (add-hook 'mu4e-view-mode-hook
    ;;           (lambda ()
    ;;             (longlines-mode 1)))

    (defun my-mu4e-set-account ()
      "Set the account for composing a message."
      (let* ((account
              (if mu4e-compose-parent-message
                  (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                    (string-match "/\\(.*?\\)/" maildir)
                    (match-string 1 maildir))
                (completing-read (format "Compose with account: (%s) "
                                         (mapconcat #'(lambda (var) (car var)) my-mu4e-account-alist "/"))
                                 (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                                 nil t nil nil (caar my-mu4e-account-alist))))
             (account-vars (cdr (assoc account my-mu4e-account-alist))))
        (if account-vars
            (mapc #'(lambda (var)
                      (set (car var) (cadr var)))
                  account-vars)
          (error "No email account found"))))

    (add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)

    (setq mu4e-bookmarks '(("tag:\\\\Inbox" "Inbox" ?i)
                           ("maildir:/ming/all and (tag:\\\\Inbox or tag:\\\\Sent)" "MING Inbox" ?m)
                           ("flag:flagged" "Todos" ?b)
                           ("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
                           ("date:today..now" "Today's messages" ?t)
                           ("date:7d..now" "Last 7 days" ?w)
                           ("mime:image/*" "Messages with images" ?p)))

    ;;_ headers
    (setq mu4e-headers-date-format "%Y-%m-%d [%H:%M]"
          mu4e-headers-fields '((:date . 20) (:flags . 6) (:from . 22) (:subject))
          ;; mu4e-headers-fields '((:from . 22) (:subject))
          mu4e-headers-leave-behavior 'ask
          mu4e-headers-visible-columns 60
          mu4e-headers-visible-lines 15
          mu4e-split-view 'horizontal
          mu4e-headers-skip-duplicates t)

    (add-hook 'mu4e-headers-mode-hook
              (lambda ()
                (setq cursor-type nil)) ;; hide cursor in headers view
              t)

    ;;_ message view
    (setq ;; mu4e-html2text-command "html2text -utf8 -nobs -style compact -width 72"
     mu4e-html2text-command "w3m -dump -cols 80 -T text/html"
     ;; mu4e-html2text-command "pandoc -f html -t latex"
     mu4e-show-images t
     mu4e-view-image-max-width 800
     mu4e-view-date-format "%c" ;; locale's date and time format
     mu4e-view-fields '(:from :to :cc :subject :tags :flags :date :maildir :attachments :signature)
     mu4e-view-hide-cited nil
     mu4e-view-prefer-html nil
     mu4e-view-show-addresses t)

    ;;     (defun mu4e~view-construct-attachments-header (msg)
    ;;       "This function overrides the default behaviour.
    ;; Since programs like Apple Mail can inline more attachement types than
    ;; just images (e.g. pdf documents), I needed a way to access them."
    ;;       (setq mu4e~view-attach-map ;; buffer local
    ;;             (make-hash-table :size 64 :weakness nil))
    ;;       (let* ((id 0)
    ;;              (attachments
    ;;               (remove-if
    ;;                (lambda (part)
    ;;                  (and (string-match "^[0-9]+\.part$" (plist-get part :name))
    ;;                       (not (member 'attachment (plist-get part :type)))
    ;;                       (not (string-match "^image" (plist-get part :mime-type)))))
    ;;                (plist-get msg :parts)))
    ;;              (attstr
    ;;               (mapconcat
    ;;                (lambda (part)
    ;;                  (let ((index (plist-get part :index))
    ;;                        (name (plist-get part :name))
    ;;                        (size (plist-get part :size))
    ;;                        (map (make-sparse-keymap)))
    ;;                    (incf id)
    ;;                    (puthash id index mu4e~view-attach-map)
    ;;                    (define-key map [mouse-2]
    ;;                      (mu4e~view-open-save-attach-func msg id nil))
    ;;                    (define-key map [?\M-\r]
    ;;                      (mu4e~view-open-save-attach-func msg id nil))
    ;;                    (define-key map [S-mouse-2]
    ;;                      (mu4e~view-open-save-attach-func msg id t))
    ;;                    (define-key map (kbd "<S-return>")
    ;;                      (mu4e~view-open-save-attach-func msg id t))
    ;;                    (concat
    ;;                     (propertize (format "[%d]" id)
    ;;                                 'face 'mu4e-view-attach-number-face)
    ;;                     (propertize name 'face 'mu4e-view-link-face
    ;;                                 'keymap map 'mouse-face 'highlight)
    ;;                     (when (and size (> size 0))
    ;;                       (concat (format "(%s)"
    ;;                                       (propertize (mu4e-display-size size)
    ;;                                                   'face 'mu4e-view-header-key-face)))))))
    ;;                attachments ", ")))
    ;;         (when attachments
    ;;           (mu4e~view-construct-header :attachments attstr t))))



    ;; ;; don't keep message buffers around
    (setq message-kill-buffer-on-exit t)))


;;;;_ , markdown
;;;; http://jblevins.org/projects/markdown-mode/

(use-package markdown-mode
  :load-path "markdown-mode"
  :mode ("\\.md$" . markdown-mode))


;;;;_ , sibilant

(use-package sibilant-mode
  :load-path "sibilantjs/misc"
  :mode ("\\.sibilant$" . sibilant-mode))


;;;;_ , magit / magithub / mo-git-blame / gist

(use-package git-commit-mode
  :load-path "git-modes")
(use-package git-rebase-mode)
(use-package gitattributes-mode)
(use-package gitconfig-mode)
(use-package gitignore-mode)

(use-package magit
  :load-path "magit"
  :commands (magit-status
             magit-log
             magit-branch-manager)
  :bind (("C-. m s" . magit-status)
         ("C-. m b" . magit-branch-manager)
         ("C-. m l" . magit-log))
  :config
  (progn
    (use-package magithub
      :load-path "magitub")))

(use-package mo-git-blame
  :load-path "mo-git-blame"
  :commands (mo-git-blame-current
             mo-git-blame-file))

(use-package gist
  :load-path ("gist" "gh.el" "pcache" "logito")
  :bind (("C-. g l" . gist-list)
         ("C-. g p" . gist-region-or-buffer)
         ("C-. g s" . gist-region-or-buffer-private))
  :config
  (hl-line-mode 1))


;;;;_ , mark-multiple

(use-package mark-multiple
  :load-path "mark-multiple")


;;;;_ , multiple-cursors

(use-package multiple-cursors
  :load-path "multiple-cursors"
  :init
  (progn
    (global-set-keys '(("C-S-c C-S-c" mc/edit-lines)
                       ("C->" mc/mark-next-like-this)
                       ("C-<" mc/mark-previous-like-this)
                       ("C-c C-<" mc/mark-all-like-this)))))


;;;;_ , flycheck

(use-package flycheck
  :load-path "flycheck")


;;;;_ , yasnippet

(use-package yasnippet
  ;; :if (not noninteractive)
  :load-path "yasnippet"
  :commands (yas/minor-mode
             yas/expand)
  ;; :mode ("~/emacs-config/snippets/" . snippet-mode)
  :init
  (progn
    (mapc (lambda (hook) (add-hook hook (lambda () (yas/minor-mode 1))))
          '(css-mode-hook
            html-mode-hook
            js2-mode-hook
            emacs-lisp-mode-hook
            lisp-mode-hook
            haml-mode-hook
            ruby-mode-hook
            slim-mode-hook)))
  :config
  (progn
    (yas-load-directory (expand-file-name "yasnippet/snippets" user-emacs-directory))
    ;; (yas-load-directory (expand-file-name "snippets/" user-init-directory))

    (setq yas/prompt-functions '(yas/dropdown-prompt yas/ido-prompt))

    (bind-key "<tab>" 'yas/next-field-or-maybe-expand yas/keymap)
    (bind-key "C-c y TAB" 'yas/expand)
    (bind-key "C-c y f" 'yas/find-snippets)
    (bind-key "C-c y r" 'yas/reload-all)
    (bind-key "C-c y v" 'yas/visit-snippet-file)))


;;;;_ , javascript

(use-package js2-mode
  :load-path "js2-mode"
  :mode ("\\.js$" . js2-mode)
  :config
  (progn
    (setq js2-pretty-multiline-declarations t)
    (use-package js2-refactor ;; js2-refactor-mode
      :load-path "js2-refactor"
      :requires (mark-multiple yasnippet))

    (add-hook 'js2-mode-hook
              (lambda ()
                ;; (paredit-mode 1)
                (highlight-symbol-mode 1)
                (slime-js-minor-mode 1)
                (local-set-key (kbd "<return>") 'newline-and-indent))
              t)))

;; js2-hightlight-vars-mode
;; see http://mihai.bazon.net/projects/editing-javascript-with-emacs-js2-mode/js2-highlight-vars-mode
;; M-n js2-highlight-vars-next
;; M-p js2-highlight-vars-prev
;; M-r js2-highlight-vars-rename
(use-package js2-highlight-vars
  :requires js2-mode
  :commands (js2-highlight-vars-mode)
  :init (add-hook 'js2-mode-hook 'js2-highlight-vars-mode t))


;;;;_ , nodejs-mode

(use-package nodejs-mode
  :disabled t
  :load-path "nodejs-mode"
  :commands (nodejs))


;;;;_ , swank-js

(use-package slime-js
  :load-path "swank-js"
  :commands (slime-js-minor-mode))


;;;;_ , skewer-mode
(use-package simple-httpd
  :load-path "emacs-web-server"
  :defer t)

(use-package skewer-mode
  :load-path "skewer-mode"
  :requires (js2-mode
             simple-httpd)
  :commands (run-skewer
             skewer-setup
             skewer-repl
             skewer-mode
             skewer-css-mode
             skewer-html-mode
             skewer-bower-load
             skewer-bower-refresh)
  :init
  (progn
    (require 'skewer-html)
    (require 'skewer-css)
    (require 'skewer-repl)
    (require 'skewer-bower)

    (define-keys skewer-html-mode-map
      '(("C-<return>" skewer-html-eval-tag)))


    ;; Bookmarklet to load skewer:
    ;; javascript:(function(){var d=document ;var s=d.createElement('script');s.src='http://localhost:8023/skewer';d.body.appendChild(s);})()
    (defun skewer-start ()
      (interactive)
      (let ((httpd-port 8023))
        (httpd-start)
        (message "Ready to skewer the browser. Now jack in with the bookmarklet.")))))


;;;;_ , coffeescript

(use-package coffee-mode
  :load-path "coffee-mode"
  :mode ("\\.coffee" . coffee-mode)
  :commands coffee-mode
  :config
  (progn
    (setq coffee-tab-width 2)))


;;;;_ , haml

(use-package haml-mode
  :load-path "haml-mode"
  :commands haml-mode
  :mode ("\\.haml$" . haml-mode))


;;;;_ , slim

(use-package slim-mode
  :load-path "emacs-slim"
  :commands slim-mode
  :mode ("\\.slim$" . slim-mode)
  :init
  (progn
    (add-hook 'slim-mode-hook 'highlight-indentation-current-column-mode)))


;;;;_ , css

(use-package css-mode
  :mode ("\\.css$" . css-mode)
  :config
  (progn
    (setq css-indent-offset 2
          cssm-indent-level '2)
    (define-keys css-mode-map
      '(("<return>" newline-and-indent)))
    (add-hook 'css-mode-hook
              (lambda ()
                ;; (paredit-mode 1)
                (rainbow-mode 1)))))


;;;;_ , sass

(use-package sass-mode
  :load-path "sass-mode"
  :commands sass-mode
  :mode ("\\.sass$" . sass-mode)
  :config
  (progn
    (add-hook 'sass-mode-hook
              (lambda ()
                (rainbow-mode 1)
                (highlight-indentation-current-column-mode)))))


;;;;_ , scss

(use-package scss-mode
  :load-path "scss-mode"
  :commands scss-mode
  :mode ("\\.scss$" . scss-mode)
  :config
  (progn
    (add-hook 'scss-mode-hook
              (lambda ()
                (rainbow-mode 1)
                (setq scss-compile-at-save nil)))))


;;;;_ , html

;; (add-to-list 'auto-mode-alist '("\\.html$" . html-mode))
;; (add-to-list 'auto-mode-alist '("\\.htm$" . html-mode))

(setq sgml-basic-offset 2)

(defun mb-html-mode-hook ()
  (setq truncate-lines 1
        auto-fill-mode -1)
  (turn-off-auto-fill)
  (define-keys html-mode-map
    '(("C->" sgml-close-tag)
      ("<return>" newline-and-indent))))

(add-hook 'html-mode-hook 'mb-html-mode-hook)


;;;;_ , zencoding

(use-package zencoding-mode
  :load-path "zencoding"
  :init
  (progn
    (add-hook 'nxml-mode-hook 'zencoding-mode)
    (add-hook 'html-mode-hook 'zencoding-mode)))


;;;;_ , web-mode

(use-package web-mode
  :load-path "web-mode"
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.htm$" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))

    (add-hook 'web-mode-hook
              (lambda () (setq web-mode-markup-indent-offset 2
                               web-mode-css-indent-offset 2
                               web-mode-code-indent-offset 2)))))


;;;;_ , time-stamp

(setq time-stamp-active t
      time-stamp-line-limit 10
      time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S (%U)")
(add-hook 'write-file-hooks 'time-stamp t)


;;;;_ , helm

(use-package helm
  ;; :disabled t
  :load-path "helm"
  :commands (helm-mini
             helm-mode
             helm-imenu)
  :config
  (progn
    (require 'helm-config)))

(use-package helm-mu
  :disabled t
  :load-path "helm-mu"
  :commands (helm-mu))


;;;;_ , fuzzy-el

(use-package fuzzy
  :load-path "fuzzy-el"
  :init
  (turn-on-fuzzy-isearch))


;;;;_ , annot

(use-package annot
  :load-path "annot/src"
  :commands (annot-edit/add
             annot-remove
             annot-edit
             annot-add-image)
  :bind (("C-x a" . annot-edit/add)
         ("C-x C-a" . annot-edit/add)
         ("C-x r" . annot-remove)
         ("C-x w" . annot-add-image)))


;;;;_ , deft

(use-package deft
  :load-path "deft"
  :init
  (progn
    (add-hook 'deft-mode-hook
              (lambda ()
                (hl-line-mode 1))))
  :config
  (progn
    (defun deft-default-filename ()
      "Default filename for new files without SLUG."
      (format-time-string "%Y-%m-%d-%H%M%S"))

    (defvar deft-export-directory (expand-file-name "~/Dropbox/notes")
      "Directory where readable format is exported.")

    (setq deft-extension "org"
          deft-text-mode 'org-mode
          deft-strip-title-regexp "^#.TITLE:[ ]*"
          deft-auto-save-interval 15.0
          deft-directory "~/Dropbox/notes")

    (defun deft-next-line ()
      "Move cursor to next line and open file in other window."
      (interactive)
      (next-line 1)
      (deft-open-file-other-window))

    (defun deft-previous-line ()
      "Move cursor to previous line and open file in other window."
      (interactive)
      (previous-line 1)
      (deft-open-file-other-window))

    (defun deft-new-file-default-name ()
      "Create file with default file name."
      (interactive)
      (deft-new-file-named (deft-default-filename)))

    (defun deft-parse-summary (contents title)
      "Parse the file CONTENTS and extract a summary.
The summary is a string extracted from the contents following the
title without comments."
      (let* ((summary-without-comments (replace-regexp-in-string "^#.*$" "" contents))
             (summary (replace-regexp-in-string "[\n\t ]+" " " summary-without-comments)))
        summary))

    (defadvice deft-new-file-named
      (after mb-deft-insert-org-template last () activate)
      "Create new file and insert org-mode template."
      (let* ((timestamp (concat "[" (format-time-string "%Y-%m-%d %H:%M") "]"))
             (tmpl (concat "#+TITLE:
#+DATE: " timestamp "
#+KEYWORDS:

")))
        (goto-char (point-min))
        (insert tmpl)
        (goto-char 9)))

    ;; (defadvice deft-open-file
    ;;   (after mb-deft-generate-readable-after-save last () activate)
    ;;   "After saving, generate readable file and save it to DEFT-EXPORT-DIRECTORY."
    ;;   (add-hook 'after-save-hook
    ;;             (lambda ()
    ;;               (let ((my-file (org-ascii-export-to-ascii)))
    ;;                 (copy-file my-file (format "%s/%s" deft-export-directory my-file) t t)
    ;;                 (delete-file my-file)))
    ;;             t t))

    (define-keys deft-mode-map
      '(("C-n" deft-next-line)
        ("C-p" deft-previous-line)
        ("C-u C-<return>" deft-new-file-default-name)))))


;;;;_ , sr-speedbar

(use-package sr-speedbar
  :load-path "sr-speedbar"
  :commands (sr-speedbar-open
             sr-speedbar-close
             sr-speedbar-toggle)
  :bind ("C-. s" . sr-speedbar-toggle)
  :config
  (progn
    (setq speedbar-use-images nil)))


;;;;_ , rainbow-delimiters

(use-package rainbow-delimiters
  :load-path "rainbow-delimiters"
  :commands (rainbow-delimiters-mode))


;;;;_ , rainbow-mode

(use-package rainbow-mode
  :diminish rainbow-mode
  :load-path "rainbow-mode"
  :commands (rainbow-mode))


;;;;_ , emms

(use-package emms-setup
  :disabled t
  :load-path "emms/lisp"
  :commands (emms
             emms-play-directory
             emms-play-directory-tree)
  :config
  (progn
    (require 'emms-player-mplayer)
    (emms-standard)
    (emms-default-players)

    (setq emms-player-list '(emms-player-vlc
                             emms-player-mplayer
                             emms-player-mplayer-playlist
                             ;;emms-player-mpg123
                             emms-player-mpg321)
          emms-source-file-default-directory "~/Music/iTunes/iTunes Media/Music")

    (bind-key "C-. e n" 'emms-next)
    (bind-key "C-. e p" 'emms-previous)
    (bind-key "C-. e SPC" 'emms-pause)))



;;;;_ , sunrise-commander

(use-package sunrise-commander
  :load-path "sunrise-commander"
  :commands sunrise)


;;;;_ , w3m

(use-package w3m
  :load-path "w3m"
  :commands (w3m
             w3m-search
             w3m-browse-url
             w3m-browse-url-new-session)
  :init
  (progn
    (setq w3m-command "/usr/local/bin/w3m"
          w3m-coding-system 'utf-8
          w3m-file-coding-system 'utf-8
          w3m-file-name-coding-system 'utf-8
          w3m-input-coding-system 'utf-8
          w3m-output-coding-system 'utf-8
          w3m-terminal-coding-system 'utf-8
          w3m-use-cookies t
          w3m-home-page "http://news.ycombinator.com"))
  :config
  (progn
    (require 'w3m-lnum)
    (w3m-lnum-mode 1)

    (define-keys w3m-mode-map
      '(("C-<return>" w3m-external-view-this-url)
        ("C-u C-<return>" w3m-external-view-current-url)
        ("f" w3m-lnum-goto)))))


;;;;_ , color-theme

(add-to-list 'custom-theme-load-path (expand-file-name "theme-solarized" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "theme-tomorrow/GNU Emacs" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "zenburn-emacs" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-init-directory))
;; (load-theme 'zenburn t)


;;;;_ , font
;;;; Seems like default font changed

;; (set-face-font 'default "DejaVu Sans Mono-12")
(when window-system
  (set-face-font 'variable-pitch "Georgia-16"))
(set-face-attribute 'mode-line nil :inherit 'unspecified)

;; (setq-default line-spacing 0)


;;;;_ , dim parentheses

(defface paren-face
  '((((class color) (background dark)) (:foreground "grey45"))
    (((class color) (background light)) (:foreground "grey85")))
  "Face used to dim parentheses.")

(mapc (lambda (hook) (add-hook hook (lambda () (font-lock-add-keywords nil '(("(\\|)" . 'paren-face))))))
      '(slime-mode-hook
        slime-repl-mode-hook
        emacs-lisp-mode-hook
        lisp-mode-hook
        ielm-mode-hook
        scheme-mode-hook
        inferior-scheme-mode-hook
        inferior-qi-mode-hook
        qi-mode-hook))

;; (mapc (lambda (hook)
;;         (add-hook hook (lambda () (font-lock-add-keywords nil '(("\\[\\|\\]" . 'paren-face)))))
;;         (add-hook hook (lambda () (font-lock-add-keywords nil '(("{\\|}" . 'paren-face))))))
;;       '(clojure-mode
;;         nrepl-mode))


;;;;_ , growl

(defvar growl-program "growlnotify")

(defun growl (title message)
  (start-process "growl" " growl"
                 growl-program
                 title
                 "-a" "Emacs")
  (process-send-string " growl" message)
  (process-send-string " growl" "\n")
  (process-send-eof " growl"))


;;;;_ , est.el

(use-package est
  :load-path "est.el")


;;;;_ , powerline

(use-package powerline
  :load-path "powerline"
  :disabled t)


;;;;_ , upword mode
(use-package upword-mode
  :load-path "upword-mode"
  :init
  (progn
    ;; (add-to-list 'auto-mode-alist
    ;;              '("UpWord\\ Notes\\/.*\\.txt$" . upword-mode)
                 ))


;;;;_. LABS

;;;;_ , org-reveal
(use-package ox-reveal
  :load-path "org-reveal"
  :config
  (progn
    (setq org-reveal-root "file:///Users/martin/code/reveal.js")))

;;;;_ , highlight-numbers
;; (use-package highlight-numbers
;;   :load-path "highlight-numbers")

;;;;_ , highlight-escape-sequences
(use-package highlight-escape-sequences
  :load-path "highlight-escape-sequences")

;;;;_ , paren-face
(use-package paren-face
  :load-path "paren-face"
  ;; :init
  ;; (progn
  ;;   (global-paren-face-mode 1))
  )

;;;;_ , rainbow-blocks
(use-package rainbow-blocks
  :load-path "rainbow-blocks")

;;;;_ , highlight-stages
(use-package highlight-stages
  :load-path "highlight-stages")

;;;;_ , hl-sexp
(use-package hl-sexp
  :load-path "hl-sexp")

;;;;_ , highlight-parentheses
(use-package highlight-parentheses
  :load-path "highlight-parentheses"
  :init
  (progn
    (global-highlight-parentheses-mode 1)
    ;; (add-hook 'after-init-hook 'global-highlight-parentheses-mode)
    ))

;;;;_ , color-identifiers-mode
(use-package color-identifiers-mode
  :load-path "color-identifiers-mode"
  :init
  (progn
    ;; (global-color-identifiers-mode 1)
    ;; (add-hook 'after-init-hook 'global-color-identifiers-mode)
    ))

;;;;_ , highlight-defined
(use-package highlight-defined
  :load-path "highlight-defined")

;;;;_ , highlight-symbol
(use-package highlight-symbol
  :load-path "highlight-symbol"
  :init
  (progn
    (setq highlight-symbol-idle-delay 0)))

;;;;_ , org-ehtml
(use-package web-server
  :load-path "eschulte-web-server")

(use-package org-ehtml
  :load-path "org-ehtml/src"
  :init
  (progn
    (setq org-ehtml-docroot (expand-file-name "~/notes")
          org-ehtml-everything-editable t
          org-ehtml-allow-agenda t)
    ;; (ws-start org-ehtml-handler 9123)
    ))

;;;;_ , git-timemachine
(use-package git-timemachine
  :load-path "git-timemachine")

;;;;_ , wanderlust
(add-to-list 'load-path (expand-file-name "apel" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "flim" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "semi" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "wanderlust/elmo" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "wanderlust/utils" user-emacs-directory))
(use-package wl
  :load-path "wanderlust/wl"
  :disabled t)

;;;;_ , tern
(use-package tern
  :load-path "tern/emacs")

(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))

;;;;_ , god-mode
(use-package god-mode
  :load-path "god-mode"
  :commands (god-local-mode god-mode-all)
  :bind ("<escape>" . god-local-mode)
  :disabled t
  :init
  (progn
    (defun mb-god-mode-indicate ()
      (let ((limited-colors-p (> 257 (length (defined-colors)))))
        (cond (god-local-mode (progn
                                (set-face-background 'mode-line (if limited-colors-p "white" "#e9e2cb"))
                                (set-face-background 'mode-line-inactive (if limited-colors-p "white" "#e9e2cb"))))
              (t (progn
                   (set-face-background 'mode-line (if limited-colors-p "black" "#0a2832"))
                   (set-face-background 'mode-line-inactive (if limited-colors-p "black" "#0a2832"))))))))

  (add-hook 'god-mode-enabled-hook 'c/god-mode-update-cursor)
  (add-hook 'god-mode-disabled-hook 'c/god-mode-update-cursor))

;;;;_ , evernote
(use-package evernote-mode
  :load-path "evernote-mode")

(use-package org-evernote
  :load-path "evernote-mode")

;;;;_ , todotxt
(use-package todotxt
  :load-path "todotxt.el"
  :init
  (progn
    (setq todotxt-file "~/Dropbox/todo/todo.txt")))

(use-package todotxt-mode
  :load-path "todotxt-mode"
  :init
  (progn
    (setq todotxt-default-file (expand-file-name "~/org/todo.txt")
          todotxt-default-archive-file (expand-file-name "~/org/done.txt"))))

;;;;_ , editorconfig
(use-package editorconfig
  :load-path "editorconfig")

;;;;_ , scala-mode2
;; (use-package scala-mode2
;;   :load-path "scala-mode2"
;;   :init
;;   (progn
;;     (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)))

;; ;;;;_ , ensime
;; (use-package ensime
;;   :load-path "ensime/src/main/elisp")

;;;;_ , clj-refactor
(use-package clj-refactor
  :load-path "clj-refactor.el")

;;;;_ , company-mode
(use-package company
  :load-path "company-mode")

;;;;_ , http repl
(use-package httprepl
  :load-path "httprepl.el")

;;;;_ , emacs-maildir
;; (use-package kv :load-path "emacs-kv")
;; (use-package maildir
;;   :load-path "emacs-maildir"
;;   :requires (kv)
;;   :config
;;   (progn
;;     (setq maildir-mail-dir "~/mail/gmail/all")))

;;;;_ , notmuch
(use-package notmuch
  :disabled t
  :load-path "notmuch/emacs")

;;;;_ , offlineimap-el
(use-package offlineimap
  :load-path "offlineimap-el")

;;;;_ , google-contacts.el
(use-package google-contacts
  :load-path "google-contacts.el"
  :disabled t)

;;;;_ , projectile
(use-package projectile
  :load-path "projectile"
  :init
  (progn
    (projectile-global-mode)))

;;;;_ , project-explorer
(use-package es-lib
  :load-path "es-lib"
  :defer t)

(use-package project-explorer
  :load-path "project-explorer"
  :requires (es-lib)
  :commands (project-explorer-mode
             project-explorer-open))

;;;;_ , dash
(use-package dash-at-point
  :load-path "dash-at-point"
  :commands (dash-at-point)
  :bind ("C-c d" . dash-at-point))


;;;;_ , anzu
(use-package anzu
  :load-path "emacs-anzu"
  :diminish anzu-mode
  :commands (anzu-mode
             global-anzu-mode)
  :init
  (progn
    (global-anzu-mode +1))
  :config
  (progn
    (setq anzu-search-threshold 1000)))

;;;;_ , elfeed
;; (use-package elfeed
;;   :load-path "elfeed"
;;   :config
;;   (progn
;;     (setq elfeed-feeds '("http://feeds.feedburner.com/paul-irish"
;;                          "https://github.com/blog/all.atom"
;;                          "http://planet.clojure.in/atom.xml"))

    ;; (use-package elfeed-web
    ;;   :load-path "elfeed/web")
    ;; ))

;;;;_ , fill-column-indicator
(use-package fill-column-indicator
  :load-path "fill-column-indicator"
  :commands fci-mode)

;;;;_ , highlight-indentation
(use-package highlight-indentation
  :load-path "highlight-indentation"
  :commands (highlight-indentation-mode
             highlight-indentation-current-column-mode))



(fset 'latex-fix-labels
      ;; label fix
      (lambda (&optional arg)
        "Keyboard macro."
        (interactive "p")
        (kmacro-exec-ring-item
         (quote ([19 108 97 98 101 108 123 102 105 103 58 13 134217826 134217826 2 67108896 67108896 19 125 13 23 5 return 25] 0 "%d")) arg)))


;;;;_ , doc-view
(setq-default doc-view-resolution 300)

;;;;_ , sentence-highlight
(use-package sentence-highlight
  :load-path "sentence-highlight")

;;;;_ , json
(use-package json)

;;;;_ , request
(use-package request
  :load-path "emacs-request")

;;;;_ , noflet
(use-package noflet
  :load-path "emacs-noflet")

;;;;_ , elnode
;; (use-package fakir
;;   :load-path "emacs-fakir"
;;   :requires (noflet kv))
;; (use-package web :load-path "emacs-web")
;; (use-package db :load-path "emacs-db")
;; (use-package esxml :load-path "esxml")
;; (use-package creole :load-path "elwikicreole")
;; (use-package uuid :load-path "emacs-uuid")
;; (use-package oauth :load-path "emacs-oauth")

;; (use-package elnode
;;   :load-path "elnode"
;;   :disabled t
;;   :requires (fakir
;;              kv
;;              web
;;              db))

;;;;_ , org-trello
(use-package org-trello
  :load-path "org-trello"
  :requires (request dash json elnode))

;;;;_ , ruby-mode
(use-package ruby-mode
  :mode (("\\.rb$" . ruby-mode)
         ("\\.ru$" . ruby-mode)
         ("Gemfile$" . ruby-mode)
         ("Rakefile$" . ruby-mode)))

;;;;_ , inf-ruby
(use-package inf-ruby
  :load-path "inf-ruby")

;;;;_ , jump
(use-package jump
  :load-path "jump.el"
  :requires (findr inflections))

(use-package ert
  :load-path "ert")

;;;;_ , rinari
(use-package rinari
  :load-path "rinari"
  :requires (inf-ruby jump ert))

;;;;_ , ack-and-a-half
(use-package ack-and-a-half
  :load-path "ack-and-a-half")

;;;;_ , ack-el
(use-package ack
  :load-path "ack-el")

;;;;_ , pomodoro
(use-package pomodoro)

;;;;_ , tomatinho
(use-package tomatinho
  :load-path "tomatinho"
  :disabled t)

;;;;_ , Saaxy
(use-package saaxy
  :load-path "Saaxy")

;;;;_ , auctex
(use-package tex-site
  :load-path "auctex"
  :mode ("\\.tex\\'" . latex-mode)
  :config
  (progn
    (use-package latex-mode
      :defer t
      :config
      (progn
        (use-package preview
          :load-path "auctex/preview")))

    (setq TeX-PDF-mode t)))

;;;;_ , lispyscript-mode
(use-package lispyscript-mode
  :load-path "lispyscript-mode"
  :mode (("\\.ls$" . lispyscript-mode)))

;;;;_ , litable
(use-package litable
  :load-path "litable")

;;;;_ , kanban.el
;;; http://draketo.de/light/english/free-software/el-kanban-org-table
(use-package kanban
  :load-path "kanban.el")

;;;;_ , visual regex
(use-package visual-regexp
  :load-path "visual-regexp.el")

(use-package visual-regexp-steroids
  :load-path "visual-regexp-steroids.el"
  :disabled t)

;;;;_ , smartparens
(use-package smartparens
  :load-path "smartparens")

;;;;_ , keyinfo
;;; keyinfo wirkt unnötig
;;; unbound.el gibt das nötige, keyinfo stopft es scheinbar "nur" in
;;; ein org-file
;; (require 'unbind)
;; M-x describe-unbound-keys
;; M-x describe-bindings
(use-package keyinfo
  :load-path "keyinfo")

;;;;_ , outshine
(use-package outshine
  :load-path "outshine"
  :disabled t
  :init
  (progn
    (add-hook 'outline-minor-mode-hook 'outshine-hook-function))

  :config
  (progn
    (define-keys outline-minor-mode-map
      '(("<tab>" outline-cycle)
        ("<M-left>" outline-promote)
        ("<M-right" outline-demote)
        ("<M-up>" outline-move-subtree-up)
        ("<M-down>" outline-move-subtree-down)))))

;;;;_ , hl-line+
(use-package hl-line+
  :init
  (progn
    (setq global-hl-line-mode nil)
    (hl-line-when-idle-interval 30)

    (toggle-hl-line-when-idle 1)))

;;;;_ , yaml-mode
(use-package yaml-mode
  :load-path "yaml-mode"
  :mode ("\\.yaml$" . yaml-mode))

;;;;_ , dart-mode
(use-package dart-mode
  :load-path "dart-mode"
  :mode ("\\.dart$" . dart-mode))

;;;;_ , anaphora
(use-package anaphora
  :load-path "anaphora")

;;;;_ , git-gutter
(use-package git-gutter
  :load-path "emacs-git-gutter")


;;;;_ , calfw
(use-package calfw
  :load-path "emacs-calfw"
  :init
  (progn
    (require 'calfw-org)
    (require 'calfw-ical)

    (defun mb-open-calendar ()
      (interactive)
      (cfw:open-calendar-buffer
       :contents-sources (list
                          (cfw:org-create-source "Green")
                          ;; (cfw:ical-create-source "business" "" "Red")
                          ;; (cfw:ical-create-source "MING" "" "Pink")
                          )))))

;;;;_ , howm
(use-package howm
  :load-path "howm"
  :disabled t
  :init
  (progn
    (setq howm-template "#+TITLE: %title%cursor
#+DATE: %date
# FILE: %file
#+TAGS:
"
          howm-file-name-format "%Y/%m/%Y-%m-%d-%H%M%S.org"
          howm-directory "~/howm/"))

  :config
  (progn
    (defun howm-push-to-repo ()
      "Git add all files and push it to origin."
      (interactive)
      (let* ((now (format-time-string "%Y-%m-%d @ %H:%M:%S"))
             (command (concat "cd " howm-directory
                              " && git add ."
                              " && git commit -m \"Update: " now "\""
                              " && git push origin master")))
        (shell-command command "*howm-git*" "*howm-git*"))
      ;; (kill-buffer "*howm-sync*")
      (message "Done."))))


;;;;_ , CEDET
(use-package cedet
  :load-path "cedet")

;;;;_ , golden-ratio
(use-package golden-ratio
  :load-path "golden-ratio.el"
  :commands (golden-ratio
             golden-ratio-enable
             golden-ratio-disable))


;;;;_ , ESS

(use-package ess-site
  :load-path "ess/lisp"
  :mode ("\\.r$" . r-mode)
  :commands (R)
  :config
  (progn ))


;;;;_ , bitlbee

(use-package bitlbee
  :load-path "bitlbee/lisp"
  :commands (bitlbee-start bitlbee-stop)
  :requires (erc-autoaway)
  :config
  (progn
    (setq erc-autoaway-idle-seconds 300
          erc-autoaway-idle-method 'emacs
          erc-autoaway-message "I'm idle (autoaway)."
          erc-auto-discard-away t
          erc-auto-set-away t)

    (defun i-wanna-be-social ()
      (interactive)
      (erc :server "localhost" :port 6667 :nick "me"))))


;;;;_ , gnus

(use-package nnir)
(use-package gnus
  :init
  (progn
    ;; (setq gnus-select-method '(nntp "news.gmane.org"))

    (setq gnus-select-method
          '(nnimap "gmail"
                   (nnimap-address "imap.gmail.com")
                   (nnimap-server-port 993)
                   (nnimap-stream ssl)
                   (nnir-search-engine imap)
                   (nnimap-authinfo-file "~/.authinfo.gpg")
                   ;; (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")
                   ;; (nnmail-expiry-wait 90)
                   ))

    (setq epa-file-cache-passphrase-for-symmetric-encryption t)

    (setq-default
     gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15f  %B%s%)\n"
     gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
     gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
     gnus-sum-thread-tree-false-root ""
     gnus-sum-thread-tree-indent ""
     gnus-sum-thread-tree-leaf-with-other "-> "
     gnus-sum-thread-tree-root ""
     gnus-sum-thread-tree-single-leaf "|_ "
     gnus-sum-thread-tree-vertical "|")

    (setq gnus-thread-sort-functions
          '((not gnus-thread-sort-by-date)
            (not gnus-thread-sort-by-number)))


    ; NO 'passive
    (setq gnus-use-cache t
          ;; gnus-use-adaptive-scoring t
          ;; gnus-save-score t
          )

    ;; (add-hook 'mail-citation-hook 'sc-cite-original)
    ;; (add-hook 'message-sent-hook 'gnus-score-followup-article)
    ;; (add-hook 'message-sent-hook 'gnus-score-followup-thread)
    ;; @see http://stackoverflow.com/questions/945419/how-dont-use-gnus-adaptive-scoring-in-some-newsgroups
    (setq gnus-parameters
          '(("nnimap.*"
             (gnus-use-scoring nil))))

    (defvar gnus-default-adaptive-score-alist
      '((gnus-kill-file-mark (from -10))
        (gnus-unread-mark)
        (gnus-read-mark (from 10) (subject 30))
        (gnus-catchup-mark (subject -10))
        (gnus-killed-mark (from -1) (subject -30))
        (gnus-del-mark (from -2) (subject -15))
        (gnus-ticked-mark (from 10))
        (gnus-dormant-mark (from 5))))

    (setq  gnus-score-find-score-files-function
           '(gnus-score-find-hierarchical gnus-score-find-bnews bbdb/gnus-score))

    ;; BBDB: Address list
    (when (file-exists-p "/usr/share/emacs/site-lisp/bbdb")
      (add-to-list 'load-path "/usr/share/emacs/site-lisp/bbdb")
      (require 'bbdb)
      (bbdb-initialize 'message 'gnus 'sendmail)
      (setq bbdb-file "~/bbdb.db")
      (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
      (setq bbdb/mail-auto-create-p t
            bbdb/news-auto-create-p t)
      (defvar bbdb-time-internal-format "%Y-%m-%d"
        "The internal date format.")
  ;;;###autoload
      (defun bbdb-timestamp-hook (record)
        "For use as a `bbdb-change-hook'; maintains a notes-field called `timestamp'
    for the given record which contains the time when it was last modified.  If
    there is such a field there already, it is changed, otherwise it is added."
        (bbdb-record-putprop record 'timestamp (format-time-string
                                                bbdb-time-internal-format
                                                (current-time)))))

    (add-hook 'message-mode-hook
              '(lambda ()
                 (flyspell-mode t)
                 (local-set-key "<TAB>" 'bbdb-complete-name)))

    ;; Fetch only part of the article if we can.  I saw this in someone
    ;; else's .gnus
    (setq gnus-read-active-file 'some)

    ;; Tree view for groups.  I like the organisational feel this has.
    (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

    ;; Threads!  I hate reading un-threaded email -- especially mailing
    ;; lists.  This helps a ton!
    (setq gnus-summary-thread-gathering-function
          'gnus-gather-threads-by-subject)

    ;; Also, I prefer to see only the top level message.  If a message has
    ;; several replies or is part of a thread, only show the first
    ;; message.  'gnus-thread-ignore-subject' will ignore the subject and
    ;; look at 'In-Reply-To:' and 'References:' headers.
    (setq gnus-thread-hide-subtree t)
    (setq gnus-thread-ignore-subject t)


    ;; Change email address for work folder.  This is one of the most
    ;; interesting features of Gnus.  I plan on adding custom .sigs soon
    ;; for different mailing lists.
    ;; Usage, FROM: My Name <work>
    ;; (setq gnus-posting-styles
    ;;       '((".*"
    ;;          (name "My Name"
    ;;                (address "username@gmail.com"
    ;;                         (organization "")
    ;;                         (signature-file "~/.signature")
    ;;                         ("X-Troll" "Emacs is better than Vi"))))))

    ;; You need install the command line brower 'w3m' and Emacs plugin 'w3m'
    (setq mm-text-html-renderer 'w3m)

    ;; (setq message-send-mail-function 'smtpmail-send-it
    ;;       smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
    ;;       smtpmail-auth-credentials '(("smtp.gmail.com" 587 "username@gmail.com" nil))
    ;;       smtpmail-default-smtp-server "smtp.gmail.com"
    ;;       smtpmail-smtp-server "smtp.gmail.com"
    ;;       smtpmail-smtp-service 587
    ;;       smtpmail-local-domain "homepc")
    ;;http://www.gnu.org/software/emacs/manual/html_node/gnus/_005b9_002e2_005d.html
    (setq gnus-use-correct-string-widths nil)
    ;; (gnus-compile)

    ;; (setq message-send-mail-function 'smtpmail-send-it
    ;;       smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
    ;;       smtpmail-auth-credentials '(("smtp.gmail.com" 587
    ;;                                    "user@gmail.com" nil))
    ;;       smtpmail-default-smtp-server "smtp.gmail.com"
    ;;       smtpmail-smtp-server "smtp.gmail.com"
    ;;       smtpmail-smtp-service 587
    ;;       gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

    ))

;; (setq gnus-select-method
;;       '(nnmaildir "GMail"
;;                   (directory "~/Maildir/mrtnblfnz")
;;                   (directory-files nnheader-directory-files-safe)
;;                   (get-new-mail nil)))


;;;;_ , Epilogue

(setq custom-file (expand-file-name "settings.el" user-init-directory))
(load custom-file t)

(setq source-directory (expand-file-name "~/Documents/code/emacs")
      find-function-C-source-directory (concat source-directory "/src")
      find-function-source-path (cons source-directory load-path))

(load "~/.emacs-auth" t)

(server-start)

;; Local Variables:
;;   mode: emacs-lisp
;;   mode: outline-minor
;;   outline-regexp: "^;;;;_\\([,. ]+\\)"
;; End:

;;;; .emacs ends here
