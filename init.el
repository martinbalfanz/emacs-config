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
      )

(setq-default cursor-type 'bar          ;; cursor style
	      indent-tabs-mode nil      ;; indentation never inserts tabs
              )

(fset 'yes-or-no-p 'y-or-n-p)   ;; allow y or n as answers

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


;;;;_ , global key bindings

(global-set-keys '(("C-k" mb-kill-and-join-forward)
                   ("C-o" mb-newline-beneath)
                   ("C-S-o" mb-newline-above)
                   ("C-a" mb-beginning-of-line)))


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
            slime-repl-mode-hook
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
             slime-mode)
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
            (sbcl ("sbcl" "--core"))
            (clisp ("clisp" "-E utf-8" "-modern")))

          slime-default-lisp 'ccl)))


;;;;_ , ido + smex

(use-package ido
  :init
  (ido-mode t)

  :config
  (progn
    (use-package smex
      :load-path "smex"
      :bind ("M-X" . dhl-invoke-smex)
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
            (smex-major-mode-commands)))))))


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
  :config
  (progn
    ;; (ac-set-trigger-key "TAB")
    (setq ;; ac-auto-start nil
     ac-use-quick-help t
     ac-use-comphist t
     ac-ignore-case 'smart
     ;; ac-fuzzy-enable t
     )))


;;;;_ , org-mode

(use-package dot-org
  :mode ("\\.org$" . org-mode)
  :bind ("C-c a" . org-agenda))


;;;;_ , outline-mode

(use-package outline
  :commands (outline-mode
             outline-minor-mode)
  :init
  (add-hook 'outline-minor-mode-hook
	    (lambda ()
	      (define-key outline-minor-mode-map [(control tab)] 'org-cycle)
	      (define-key outline-minor-mode-map [(shift tab)] 'org-global-cycle))))

;;;;_ , markdown
;;;; http://jblevins.org/projects/markdown-mode/

(use-package markdown-mode
  :load-path "markdown-mode"
  :mode ("\\.md$" . markdown-mode))


;;;;_ , sibilant

(use-package sibilant-mode
  :load-path "sibilantjs/misc"
  :mode ("\\.sibilant$" . sibilant-mode))


;;;;_ , magit / magithub / mo-git-blame

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


;;;;_ , mark-multiple

(use-package mark-multiple
  :load-path "mark-multiple")


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
                ))))


;;;;_ , html

(add-to-list 'auto-mode-alist '("\\.html$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.htm$" . html-mode))

(setq sgml-basic-offset 2)

(defun mb-html-mode-hook ()
  (setq truncate-lines 1
        auto-fill-mode -1)
  (turn-off-auto-fill)
  (define-keys html-mode-map
    '(("C->" sgml-close-tag)
      ("<return>" newline-and-indent))))

(add-hook 'html-mode-hook 'mb-html-mode-hook)


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
            js2-mode-hook)))
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
          w3m-home-page "http://news.ycombinator.com")
    (add-hook 'w3m-mode-hook
              (lambda ()
                (local-set-key (kbd "C-<return>") 'w3m-external-view-this-url)
                (local-set-key (kbd "C-u C-<return>" 'w3m-external-view-current-url))))))


;;;;_ , color-theme
;;;; Emacs 24 has build-in theme support, so I removed
;;;; the color-theme package.

(add-to-list 'custom-theme-load-path (expand-file-name "theme-solarized" user-emacs-directory))
(load-theme 'solarized-dark t)


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
