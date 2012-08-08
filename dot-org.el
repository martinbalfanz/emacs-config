;;;;
;;;; org-mode
;;;;

(add-to-list 'load-path (expand-file-name "org-mode/lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "org-mode/contrib/lisp" user-emacs-directory))

(require 'org-install)
(require 'org-mime)
(require 'org-toc)

(defvar mb-org-file-path-prefix "~/org/"
  "Path to my org-files.")

(defvar mb-org-file-list
  '("business.org"
    "personal.org"
    "university.org"
    "research.org")
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
      org-clock-persist-file (expand-file-name "org-clock-save.el" mb-tmp-dir)
      org-clock-idle-time 10
      org-hierarchical-todo-statistics nil
      org-table-export-default-format "orgtbl-to-csv")

(setq org-agenda-include-diary t)

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

(org-clock-persistence-insinuate)

(setq org-todo-keywords
      '((type "TODO" "WAITING" "WIP" "TESTING" "|" "DONE" "DELEGATED" "CANCELED" "VOID")
        (sequence "PROJECT" "|" "FINISHED")
        (sequence "INVOICE" "SENT" "|" "RCVD")))

(add-hook 'org-mode-hook
          (lambda ()
            (auto-fill-mode)
            (setq org-hide-leading-stars t
                  adaptive-fill-mode t
                  org-log-done t)))

(add-hook 'org-agenda-mode-hook
          (lambda ()
            (hl-line-mode 1)))

(setq org-mime-library 'semi)
(setq org-src-fontify-natively t)
