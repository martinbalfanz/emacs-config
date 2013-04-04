;;;;; -*- mode: emacs-lisp -*-
;;;;;
;;;;; Emacs Configuration File (.emacs)
;;;;;
;;;;;

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
      backup-directory-alist `(("." . ,(expand-file-name
                                        "~/.emacs-backups/")))
      vc-make-backup-files t)

(setq-default cursor-type 'bar          ;; cursor style
              indent-tabs-mode nil      ;; indentation never inserts tabs
              )

(fset 'yes-or-no-p 'y-or-n-p)           ;; allow y or n as answers

(put 'narrow-to-region 'disabled nil)   ;; enable narrowing
(put 'erase-buffer 'disabled nil)

(auto-compression-mode 1)       ;; file editing inside archives
(auto-image-file-mode 1)        ;; display image file as image

;;; basic appearance
(column-number-mode 1)          ;; show column in modeline
(display-battery-mode -1)
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

;;; coding system
(prefer-coding-system        'utf-8-unix)
(set-default-coding-systems  'utf-8-unix)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-language-environment    'utf-8)
(set-file-name-coding-system 'utf-8)
(set-locale-environment      "UTF-8")


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


;;;;_ , winner-mode
(winner-mode 1)
(windmove-default-keybindings)


;;;;_ , global key bindings

(global-set-keys '(("C-k" mb-kill-and-join-forward)
                   ("C-o" mb-newline-beneath)
                   ("C-S-o" mb-newline-above)
                   ("C-a" mb-beginning-of-line)
                   ;; ("C-. d" insert-date)
                   ))


;;;;_ , session management
;;;; Note: may not work in aquamacs.

(desktop-save-mode 1)

(setq desktop-dirname user-tmp-directory
      desktop-base-file-name ".emacs.desktop"
      savehist-file (expand-file-name "history" user-tmp-directory)
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
        ("M-a" slime-beginning-of-defun)
        ("M-e" slime-end-of-defun)
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
            (define-keys emacs-lisp-mode-map
              '(("TAB" dhl-lisp-indent-and-complete)
                ("<C-return>" dhl-lisp-eval-print-defun)
                ("C-c C-e" mb-eval-and-execute)))))

(add-hook 'lisp-mode-hook ;; 'lisp-interaction-mode-hook
          (lambda ()
            (eldoc-mode 1)
            (diminish 'eldoc-mode)
            (slime-mode 1)
            (define-keys ;; lisp-interaction-mode-map
              lisp-mode-map
              '(("TAB" dhl-lisp-indent-and-execute)
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

(use-package slime
  :load-path ("slime"
              "slime/contrib")
  :commands (slime
             slime-mode
             slime-connect)
  :init
  (add-hook 'slime-mode-hook
            #'(lambda ()
                (slime-setup
                 '(slime-asdf
                   slime-fancy
                   slime-references
                   slime-indentation))

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

(setq inferior-lisp-program "ccl")

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


;;;;_ , clojure

(use-package clojure-mode
  :load-path "clojure-mode")


;;;;_ , nrepl

(use-package nrepl
  :load-path "nrepl"
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
                                         ("common lisp" (or (name . "\\.lisp")
                                                            (name . "\\.cl")
                                                            (name . "\\.lsp")
                                                            (mode . lisp-mode)))
                                         ("clojure" (or (name . "\\.clj$")
                                                        (mode . clojure-mode)))
                                         ("javascript" (or (name . "\\.js")
                                                           (mode . javascript-mode)
                                                           (mode . js2-mode)
                                                           (mode . espresso-mode)))
                                         ("html" (or (name . "\\.html$")
                                                     (mode . html-mode)))
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
                                                  (name . "^\\.newsrc-dribble")))
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
            slime-repl-mode-hook))

    (add-hook 'emacs-lisp-mode-hook
              (lambda ()
                (setq ac-sources '(ac-source-features
                                   ac-source-functions
                                   ac-source-yasnippet
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
  :bind ("C-c a" . org-agenda)
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
    (require 'org-toc)

    (defvar mb-org-file-path-prefix "~/org/"
      "Pikeath to my org-files.")

    (defvar mb-org-file-list
      '("business.org"
        "personal.org"
        "university.org"
        "research.org"
        "todo.org")
      "List of files to be included in org-agenda.")

    (setq org-agenda-files
          (mapcar (lambda (s) (concat mb-org-file-path-prefix s))
                  mb-org-file-list))

    (setq org-default-notes-file
          (concat mb-org-file-path-prefix "notes.org"))

    (setq org-log-reschedule 'time
          org-log-redeadline 'time
          org-clock-persist 'history
          org-clock-modeline-total 'current
          org-clock-persist-file (expand-file-name "org-clock-save.el" user-tmp-directory)
          org-clock-idle-time 10
          org-hierarchical-todo-statistics nil
          org-table-export-default-format "orgtbl-to-csv")

    (setq org-agenda-include-diary t)

    (org-clock-persistence-insinuate)

    (setq org-todo-keywords
          '((type "TODO" "WAITING" "WIP" "TESTING" "|" "DONE" "DELEGATED" "CANCELED" "VOID")
            (sequence "PROJECT" "|" "FINISHED")
            (sequence "INVOICE" "SENT" "|" "RCVD")
            (sequence "BUG" "ISSUE" "FEATURE" "|" "FIXED")))

    (setq org-mime-library 'semi)
    (setq org-src-fontify-natively t)

    ;; reset clocksum-format to old version
    (setq org-time-clocksum-format '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))))


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

(use-package mu4e
  :load-path "mu/mu4e"
  :commands mu4e
  :config
  (progn
    (setq mu4e-attachment-dir (expand-file-name "~/Downloads")
          mu4e-confirm-quit nil

          ;; offlineimap runs in the background, so I don't need
          ;; mu4e to run it on update.
          ;; mu4e-get-mail-command "offlineimap -u ttyui"
          mu4e-get-mail-command "true"
          mu4e-maildir (expand-file-name "~/Maildir")
          mu4e-mu-binary "/usr/local/bin/mu"
          mu4e-mu-home nil ;; default dir
          mu4e-my-email-addresses user-mail-addresses ;; defined in .emacs
          mu4e-update-interval 300
          mu4e-search-results-limit 5000)

    ;;_ compse
    ;; mu4e-sent-messages-behavior 'delete
    ;; message-signature (concat
    ;;                    "Foo X. Bar\n"
    ;;                    "http://www.example.com\n")

    ;;_ folders
    (setq mu4e-drafts-folder "/drafts"
          mu4e-sent-folder "/sent"
          mu4e-trash-folder "/trash"
          )

    ;;_ headers
    (setq mu4e-headers-date-format "%Y-%m-%d [%H:%M]"
          mu4e-headers-fields '((:date . 20) (:flags . 6) (:from . 22) (:subject))
          ;; mu4e-headers-fields '((:from . 22) (:subject))
          mu4e-headers-leave-behavior 'ask
          mu4e-headers-visible-columns 60
          mu4e-headers-visible-lines 15
          mu4e-split-view 'horizontal)

    (add-hook 'mu4e-headers-mode-hook
              (lambda ()
                (setq cursor-type nil)) ;; hide cursor in headers view
              t)

    ;;_ message view
    (setq ;; mu4e-html2text-command "html2text -utf8 -nobs -style compact -width 72"
          mu4e-html2text-command "w3m -dump -cols 80 -T text/html"
          mu4e-show-images t
          mu4e-view-image-max-width 800
          mu4e-view-date-format "%c" ;; locale's date and time format
          mu4e-view-fields '(:from :to :cc :subject :flags :date :maildir :attachments :signature)
          mu4e-view-hide-cited nil
          mu4e-view-prefer-html nil
          mu4e-view-show-addresses t)

    (defun mu4e~view-construct-attachments-header (msg)
      "This function overrides the default behaviour.
Since programs like Apple Mail can inline more attachement types than
just images (e.g. pdf documents), I needed a way to access them."
      (setq mu4e~view-attach-map ;; buffer local
            (make-hash-table :size 64 :weakness nil))
      (let* ((id 0)
             (attachments
              (remove-if
               (lambda (part)
                 (and (string-match "^[0-9]+\.part$" (plist-get part :name))
                      (not (member 'attachment (plist-get part :type)))
                      (not (string-match "^image" (plist-get part :mime-type)))))
               (plist-get msg :parts)))
             (attstr
              (mapconcat
               (lambda (part)
                 (let ((index (plist-get part :index))
                       (name (plist-get part :name))
                       (size (plist-get part :size))
                       (map (make-sparse-keymap)))
                   (incf id)
                   (puthash id index mu4e~view-attach-map)
                   (define-key map [mouse-2]
                     (mu4e~view-open-save-attach-func msg id nil))
                   (define-key map [?\M-\r]
                     (mu4e~view-open-save-attach-func msg id nil))
                   (define-key map [S-mouse-2]
                     (mu4e~view-open-save-attach-func msg id t))
                   (define-key map (kbd "<S-return>")
                     (mu4e~view-open-save-attach-func msg id t))
                   (concat
                    (propertize (format "[%d]" id)
                                'face 'mu4e-view-attach-number-face)
                    (propertize name 'face 'mu4e-view-link-face
                                'keymap map 'mouse-face 'highlight)
                    (when (and size (> size 0))
                      (concat (format "(%s)"
                                      (propertize (mu4e-display-size size)
                                                  'face 'mu4e-view-header-key-face)))))))
               attachments ", ")))
        (when attachments
          (mu4e~view-construct-header :attachments attstr t))))
    ))


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
  :load-path "multiple-cursors")

;;;;_ , dash.el

(use-package dash
  :load-path "dash.el")


;;;;_ , javascript

(use-package js2-mode
  :load-path "js2-mode"
  :mode ("\\.js$" . js2-mode)
  :config
  (progn
    (setq js2-pretty-multiline-declarations t)
    (use-package js2-refactor ;; js2-refactor-mode
      :load-path "js2-refactor"
      :requires mark-multiple)

    (add-hook 'js2-mode-hook
              (lambda ()
                ;; (paredit-mode 1)
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


;;;;_ , coffeescript

(use-package coffee-mode
  :load-path "coffee-mode"
  :mode ("\\.coffee" . coffee-mode)
  :commands coffee-mode
  :config
  (progn
    (setq coffee-tab-width 2)))


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
                (rainbow-mode 1)))))


;;;;_ , scss

(use-package scss-mode
  :load-path "scss-mode"
  :commands scss-mode
  :mode ("\\.scss$" . scss-mode)
  :config
  (progn
    (add-hook 'scss-mode-hook
              (lambda ()
                (rainbow-mode 1)))))


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


;;;;_ , haml

(use-package haml-mode
  :load-path "haml-mode"
  :commands haml-mode
  :mode ("\\.haml$" . haml-mode))


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
            lisp-mode-hook)))
  :config
  (progn
    (yas/load-directory (expand-file-name "yasnippet/snippets" user-emacs-directory))
    ;; (yas/load-directory (expand-file-name "snippets/" user-init-directory))

    (setq yas/prompt-functions '(yas/dropdown-prompt yas/ido-prompt))

    (bind-key "<tab>" 'yas/next-field-or-maybe-expand yas/keymap)
    (bind-key "C-c y TAB" 'yas/expand)
    (bind-key "C-c y f" 'yas/find-snippets)
    (bind-key "C-c y r" 'yas/reload-all)
    (bind-key "C-c y v" 'yas/visit-snippet-file)))


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
    (add-to-list 'auto-mode-alist '("\\.htm$" . web-mode))))


;;;;_ , time-stamp

(setq time-stamp-active t
      time-stamp-line-limit 10
      time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S (%U)")
(add-hook 'write-file-hooks 'time-stamp t)


;;;;_ , helm

(use-package helm
  :disabled t
  :load-path "helm"
  :commands (helm-mini
             helm-mode)
  :config
  (progn
    (require 'helm-config)))


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
    (defvar deft-default-filename (format-time-string "%Y-%m-%d-%H%M%S")
      "Default filename for new files without SLUG.")

    (defvar deft-export-directory (expand-file-name "~/Google Drive/deft/public/")
      "Directory where readable format is exported.")

    (setq deft-extension "org"
          deft-text-mode 'org-mode
          deft-strip-title-regexp "^#.TITLE:[ ]*"
          deft-auto-save-interval 15.0
          deft-directory "~/Google Drive/deft")

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
      (deft-new-file-named deft-default-filename))

    (defun deft-parse-summary (contents title)
      "Parse the file CONTENTS and extract a summary.
The summary is a string extracted from the contents following the
title without comments."
      (let* ((summary (replace-regexp-in-string "^#.*$" "" contents))
             (summary (replace-regexp-in-string "[\n\t ]+" " " summary)))
        summary))

    (defadvice deft-new-file-named
      (after mb-deft-insert-org-template last () activate)
      "Create new file and insert org-mode template."
      (let* ((timestamp (concat "[" (format-time-string "%Y-%m-%d %H:%M") "]"))
            (tmpl (concat "#+TITLE: 
#+DATE: " timestamp "
#+TAGS:

")))
        (goto-char (point-min))
        (insert tmpl)
        (goto-char 10)))

    (defadvice deft-open-file
      (after mb-deft-generate-html-after-save last () activate)
      "After saving, generate readable file and save it to DEFT-EXPORT-DIRECTORY."
      (add-hook 'after-save-hook
                (lambda ()
                  (org-export-as-ascii 3 nil nil nil nil deft-export-directory))
                t t))

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
;;;; Emacs 24 has build-in theme support, so I removed
;;;; the color-theme package.

(add-to-list 'custom-theme-load-path (expand-file-name "theme-solarized" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "theme-tomorrow/GNU Emacs" user-emacs-directory))
(load-theme 'tomorrow-night t)
;;;;_ , font
(set-face-font 'variable-pitch "Georgia-16")



;;;;_ , dim parentheses

(defface paren-face
  '((((class color) (background dark)) (:foreground "grey25"))
    (((class color) (background light)) (:foreground "grey80")))
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


;;;;_ , est.el

(use-package est
  :load-path "est.el")


;;;;_ , powerline

(use-package powerline
  :load-path "powerline"
  :disabled t)


;;;;_ , golden-ratio
(use-package golden-ratio
  :load-path "golden-ratio.el"
  :commands (golden-ratio
             golden-ratio-enable
             golden-ratio-disable))


;;;;_. Epilogue

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
