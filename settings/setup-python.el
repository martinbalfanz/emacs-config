(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
                ("SConscript\\'" . python-mode))
              auto-mode-alist))


;; requires flake8, jedi and autopep8 to be installed

(require-package 'elpy)
(require-package 'flycheck)
(require-package 'py-autopep8)
(require-package 'ein) ;; emacs ipython notebook

(elpy-enable)

(setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
(add-hook 'elpy-mode-hook 'flycheck-mode)

;; (elpy-use-ipython)

(when (executable-find "autopep8")
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))

(provide 'setup-python)
