#!/usr/bin/env emacs --script

(require 'cl)

(defvar cline-dir nil "Directory passed as command-line-argument.")

(when (= 1 (length command-line-args-left))
  (setq cline-dir (car command-line-args-left)))

(defun git-repo-p (dir)
  "Check if DIR is a git repository."
  (let* ((dir (expand-file-name dir))
         (git-status (shell-command-to-string
                      (concat "cd " dir
                              " && git status"))))
    (not (string-match ".*Not a git repository.*" git-status))))

(defun dot-git-exists-p (dir)
  "Check if there is a `.git` directory or file in DIR.
If t, this is likely to be the top-level dir of a repository."
  (let ((dir-list (directory-files (expand-file-name dir))))
    (member ".git" dir-list)))

(defun first-level-dirs (dir)
  "Find all first-level directories in DIR."
  (unless (file-directory-p (expand-file-name dir))
    (error "not a directory '%s'" dir))
  (let ((file-list (directory-files (expand-file-name dir) t nil t)))
    (remove-if (lambda (file)
                 (or (not (file-directory-p file))
                     (string-match "\/\.$" file)
                     (string-match "\/\.\.$" file)))
               file-list)))

(defun get-current-branch ()
  "Get current branch."
  (let* ((branch-string (shell-command-to-string "git branch"))
         (branch-list (split-string branch-string "\n")))
    (dolist (branch branch-list)
      (when (string-match "^\*\ " branch)
        (return (substring branch 2))))))

(defun update-submodules (&optional dir)
  "Update all submodules in DIR."
  (let ((submodules (if dir (first-level-dirs dir)
                      (first-level-dirs "~/emacs-config/site-lisp"))))
    (dolist (submodule submodules)
      (if (and (git-repo-p submodule)
               (dot-git-exists-p submodule))
          (progn
            (message (concat "Updating " submodule))
            (shell-command (concat "cd " submodule))
            (shell-command (concat "git pull origin " (get-current-branch))))
        (message (concat "=======> " submodule " is no submodule.")))))
  (message "Done."))

(update-submodules cline-dir)

;; Local Variables:
;;   mode: emacs-lisp
;; End:
