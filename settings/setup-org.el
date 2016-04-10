;;;;_ , org-mode

;; (add-to-list 'Info-default-directory-list (expand-file-name "org-mode/info" user-emacs-directory))

(require-package 'org-plus-contrib)

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

;; (add-hook 'org-agenda-mode-hook
;;           (lambda ()
;;             (hl-line-mode 1)))

;; (require 'org-capture)
;; (require 'org-toc)
;; (require 'org-special-blocks) ;; TODO check this
;; (require 'org-latex)          ;; TODO check this
(require 'ox-latex)
;; (require 'ox-freemind)
;; (require 'org-habit)
;; (require 'org-mobile)

;; (require 'org-mac-iCal)

(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))



;; (setq org-agenda-files (file-expand-wildcards "~/Dropbox/notes/*.org"))
;; (setq org-agenda-files (expand-file-name "~/Dropbox/notes/todox.org"))
(setq org-agenda-files '("~/Dropbox/notes/todox.org"
                         "~/Dropbox/notes/inbox.org"
                         "~/Dropbox/notes/backlogx.org"
                         ;; "~/Dropbox/notes/mingx-google-cal.org"
                         ))

(setq org-default-notes-file "~/Dropbox/notes/backlogx.org")

(setq org-capture-templates
      '(("T" "Todo" entry (file+headline "backlogx.org" "Tasks")
         "* TODO %^{Brief Description} %^g\n  %?\n   Added: %U\n  %i\n  [[%F]]" :clock-resume t)
        ("t" "Todo" entry (file+headline "backlogx.org" "Tasks")
         "* %^{Brief Description} %^g\n   %?\n  Added: %U\n  %i\n  [[%F]]" :clock-resume t)
        ("n" "Note" entry (file+headline "backlogx.org" "Notes")
         "* %^{Brief Description} %^g\n   %?\n  Added: %U\n  %i\n  [[%F]]" :clock-resume t)))


;; (unless (boundp 'org-agenda-custom-commands)
;; (setq org-agenda-custom-commands '()))

(add-to-list 'org-agenda-custom-commands
             '("H" "Office and Home Lists"
               ((agenda)
                (tags-todo "office")
                (tags-todo "home")
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

(add-to-list 'org-agenda-custom-commands
             '("p" "Priorities"
               ((agenda)
                ("pa" "A items" tags-todo "+PRIORITY=\"A\"")
                ("pb" "B items" tags-todo "+PRIORITY=\"B\"")
                ("pc" "C items" tags-todo "+PRIORITY=\"C\""))))

(setq org-directory (expand-file-name "~/Dropbox/notes/")
      ;; org-refile-targets '(("todox.org" :maxlevel . 2)
      ;;                      ("backlogx.org" :level . 1))
      )

(setq org-log-reschedule 'time
      org-log-redeadline 'time
      ;; org-clock-persist 'history
      org-clock-modeline-total 'current
      ;; org-clock-persist-file (expand-file-name "org-clock-save.el" user-tmp-directory)
      org-clock-idle-time 10
      org-hierarchical-todo-statistics nil
      org-table-export-default-format "orgtbl-to-csv"
      org-agenda-columns-add-appointments-to-effort-sum t
      org-agenda-default-appointment-duration 60
      org-agenda-start-on-weekday 1
      org-startup-with-inline-images t
      org-startup-with-latex-preview t
      org-speed-command t)

;; (setq org-agenda-include-diary t)

(setq org-use-speed-commands t)

;; (org-clock-persistence-insinuate)

(setq org-todo-keywords
      '((sequence "TODO" "STARTED" "WAITING" "APPT" "|" "DONE" "CANCELED"  "DEFERRED")
        (sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
        (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
        (sequence "WAITING(w@/!)" "HOLD(h)" "|" "CANCELLED(c@/!)")
        ;; (sequence "PROJECT" "SUB-PROJECT" "MILESTONE" "|" "FINISHED")
        ;; (sequence "INVOICE" "SENT" "|" "RCVD")
        ;; (sequence "BUG" "ISSUE" "FEATURE" "|" "FIXED")
        ))


;; TODO Check on this
;; (setq org-stuck-projects
;;       '("+PROJECT/+SUB-PROJECT/+MILESTONE/-FINISHED"
;;         ("TODO" "WAITING" "WIP" "TESTING")
;;         ()
;;         "\\<IGNORE\\>"))

(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")


(require-package 'semi)
(require 'org-mime)

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
               ("\\paragraph{%s}" . "\\paragraph*{%s}")))

(provide 'setup-org)
