(require-package 'ivy)
(require-package 'flx)
(ivy-mode 1)
(diminish 'ivy-mode)
(setq ivy-use-virtual-buffers t
      ivy-count-format ""
      projectile-completion-system 'ivy
      ivy-initial-inputs-alist '((counsel-M-x . "^")
                                 (man . "^")
                                 (woman . "^")))
(setq-default ivy-re-builders-alist
              '((t . ivy--regex-fuzzy)))

(add-hook 'after-init-hook
          (lambda ()
            (when (bound-and-true-p ido-ubiquitous-mode)
              (ido-ubiquitous-mode -1)
              (ido-mode -1))
            (ivy-mode 1)))

(require-package 'counsel)
(require-package 'counsel-projectile)
(setq-default counsel-mode-override-describe-bindings t)
(diminish 'counsel-mode)
(global-set-keys '(("M-x" counsel-M-x)
                   ("C-x C-f" counsel-find-file)
                   ("C-h f" counsel-describe-function)
                   ("C-h v" counsel-describe-variable)
                   ("<f1> l" counsel-load-library)
                   ("<f2> i" counsel-info-lookup-symbol)
                   ("<f2> u" counsel-unicode-char)))

(require-package 'swiper)
(global-set-key (kbd "C-s") 'swiper)


(provide 'setup-ivy)
