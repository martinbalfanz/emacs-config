;;; This file is the entry point.  It bootstraps the configuration

(when (version< emacs-version "24")
  (error "This configuration is intended to work with Emacs 24+. Please upgrade."))

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/purcell/emacs.d/lisp/" user-emacs-directory))

(let ((benchmarking (expand-file-name "site-lisp/purcell/emacs.d/lisp/init-benchmarking.el" user-emacs-directory)))
  (when (file-exists-p benchmarking)
    (require 'init-benchmarking benchmarking)))

(defconst *spell-check-support-enabled* nil)
(defconst *is-a-mac* (eq system-type 'darwin))

;; bootstrap
;;--------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(require 'init-utils)
(require 'init-site-lisp)
(require 'init-elpa)
(require 'init-exec-path)

;; specific features and modes
;;--------------------------------------------------------------------------
(require-package 'diminish)

(require 'init-git)

;; emacsclient config
;;--------------------------------------------------------------------------
(require 'server)
(unless (server-running-p)
  (server-start))


;; customization
;;--------------------------------------------------------------------------
(when (file-exists-p custom-file)
  (load custom-file))


(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
