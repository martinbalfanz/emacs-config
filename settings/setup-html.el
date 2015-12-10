(require-package 'web-mode)
(require-package 'tagedit)

(add-hook 'html-mode-hook (lambda () (tidy-build-menu html-mode-map)))

(after-load 'web-mode
  (tagedit-add-paredit-like-keybindings)
  (add-hook 'web-mode-hook (lambda ()
                             (tagedit-mode 1)
                             (setq web-mode-markup-indent-offset 2
                                   web-mode-css-indent-offset 2
                                   web-mode-code-indent-offset 2))))

(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.htm$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp$" . web-mode))

(provide 'setup-html)
