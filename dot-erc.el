(when (require 'erc "erc" t)
  (require 'erc-match)
  ;;  (require 'erc-list-old)
  (require 'erc-dcc)
  (require 'erc-join)
  (require 'erc-nicklist)
  (require 'erc-imenu)
  (require 'erc-services)
  (erc-autojoin-mode 0) ;; needed, http://edward.oconnor.cx/2007/09/freenode-cloaking-and-erc
  (erc-spelling-mode -1)
  (erc-list-mode 1)
  (erc-timestamp-mode t)
  (erc-smiley-mode -1)
  (erc-scrolltobottom-mode 1)
  (erc-truncate-mode 1)
  (erc-services-mode 1))

(setq erc-keywords '()
                                        ;erc-pals '()
                                        ;erc-fools '()
      erc-current-nick-highlight-type 'nick-or-keyword
      erc-notice-highlight-type 'prefix
      erc-auto-query 'window-noselect
      erc-user-full-name "Martin Balfanz"
      erc-prompt-for-nickserv-password nil ;; auto identifiy
      erc-notice-highlight-type 'all
      erc-track-exclude-server-buffer nil
      erc-fill-function 'erc-fill-static
      erc-fill-static-center 16
      erc-fill-column 96
      erc-kill-buffer-on-part t
      erc-kill-queries-on-quit t
      erc-kill-server-buffer-on-quit t
      erc-max-buffer-size 500000
                                        ;erc-timestamp-format "[%H:%M] "
                                        ;erc-timestamp-format "[%R-%m/%d] "
                                        ;erc-fill-prefix "      + "
      erc-input-line-position -2
      erc-server-coding-system '(utf-8 . utf-8)
      erc-header-line-format "%a -- %s (%m, %l) %o"
      erc-mode-line-format "%t (%m, %l)"
      erc-hide-list '("JOIN" "QUIT" "PART") ;; "NICK"
      erc-autojoin-channels-alist '((".*freenode.net" "#emacs" "#orgmode"
                                     "#css"
                                     "#html" "#html5" "#whatwg"
                                     "##javascript" "#jquery" "#angularjs" "#node.js" "#grunt" "#three.js" "#&yet"
                                     "#lisp" "#clnoobs" "#lispcafe" "#lispweb" "#quicklisp" "#clojure"
                                     "#immutant")))

(setq erc-button-url-regexp
      "\\([-a-zA-Z0-9_=!?#$@~`%&*+\\/:;,]+\\.\\)+[-a-zA-Z0-9_=!?#$@~`%&*+\\/:;,]*[-a-zA-Z0-9\\/]")

(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))


;; logging
(setq erc-log-insert-log-on-open t
      erc-log-channels t
      erc-log-channels-directory "~/.irclogs/"
      erc-save-buffer-on-part t
      erc-save-quries-on-quit t
      erc-hide-timestamps nil)

;; erc functions
(defun erc-cmd-INFO (&rest ignore)
  "Send current info node."
  (unless (get-buffer "*info*")
    (error "No *info* buffer"))
  (let (output)
    (with-current-buffer "*info*"
      (let* ((file (file-name-nondirectory Info-current-file))
             (node Info-current-node))
        (setq output (format "(info \"(%s)%s\") <-- hit C-x C-e to evaluate"
                             file node))))
    (erc-send-message output)))

(defun erc-cmd-HOWMANY (&rest ignore)
  "Display how many users (and ops) the current channel has."
  (erc-display-message nil 'notice (current-buffer)
                       (let ((hash-table
                              (with-current-buffer
                                  (erc-server-buffer)
                                erc-server-users))
                             (users 0)
                             (ops 0))
                         (maphash (lambda (k v)
                                    (when (member (current-buffer)
                                                  (erc-server-user-buffers v))
                                      (incf users))
                                    (when (erc-channel-user-op-p k)
                                      (incf ops)))
                                  hash-table)
                         (format
                          "There are %s users (%s ops) on the current channel"
                          users ops))))


;; ercbar
;; http://www.emacswiki.org/emacs/ErcBar

(eval-after-load 'erc-track
  '(progn
     (defun erc-bar-move-back (n)
       "Moves back n message lines. Ignores wrapping, and server messages."
       (interactive "nHow many lines ? ")
       (re-search-backward "^.*<.*>" nil t n))

     (defun erc-bar-update-overlay ()
       "Update the overlay for current buffer, based on the content of
erc-modified-channels-alist. Should be executed on window change."
       (interactive)
       (let* ((info (assq (current-buffer) erc-modified-channels-alist))
              (count (cadr info)))
         (if (and info (> count erc-bar-threshold))
             (save-excursion
               (end-of-buffer)
               (when (erc-bar-move-back count)
                 (let ((inhibit-field-text-motion t))
                   (move-overlay erc-bar-overlay
                                 (line-beginning-position)
                                 (line-end-position)
                                 (current-buffer)))))
           (delete-overlay erc-bar-overlay))))

     (defvar erc-bar-threshold 1
       "Display bar when there are more than erc-bar-threshold unread messages.")
     (defvar erc-bar-overlay nil
       "Overlay used to set bar")
     (setq erc-bar-overlay (make-overlay 0 0))
     (overlay-put erc-bar-overlay 'face '(:underline "#FF5E99"))
     ;;put the hook before erc-modified-channels-update
     (defadvice erc-track-mode (after erc-bar-setup-hook
                                      (&rest args) activate)
       ;;remove and add, so we know it's in the first place
       (remove-hook 'window-configuration-change-hook 'erc-bar-update-overlay)
       (add-hook 'window-configuration-change-hook 'erc-bar-update-overlay))
     (add-hook 'erc-send-completed-hook
               (lambda (str) (erc-bar-update-overlay)))))

;; growlnotify
;; http://www.emacswiki.org/emacs/ErcGrowl
;; (defvar growlnotify-command (executable-find "growlnotify") "/usr/local/bin/growlnotify")

;; (defun growl (title message)
;;   "Shows a message through the growl notification system using
;;  `growlnotify-command` as the program."
;;   (flet ((encfn (s) (encode-coding-string s (keyboard-coding-system))) )
;;     (let* ((process (start-process "growlnotify" nil
;;                                    growlnotify-command
;;                                    (encfn title)
;;                                    "-a" "Emacs"
;;                                    "-n" "Emacs")))
;;       (process-send-string process (encfn message))
;;       (process-send-string process "\n")
;;       (process-send-eof process)))
;;   t)

;; (defun mb-growl-erc-hook (match-type nick message)
;;   "Shows a growl notification, when user's nick was mentioned.
;; If the buffer is currently not visible, makes it sticky."
;;   (unless (posix-string-match "^\\** *Users on #" message)
;;     (growl
;;      (concat "ERC > " (buffer-name (current-buffer)))
;;      message
;;      )))

;; ;; (add-hook 'erc-text-matched-hook 'mb-growl-erc-hook)
;; (defvar mb-erc-growl nil
;;   "Status of growl in erc-mode.")

;; (defun mb-erc-enable-growl ()
;;   "Enable growl for erc."
;;   (add-hook 'erc-text-matched-hook 'mb-growl-erc-hook)
;;   (setq mb-erc-growl t)
;;   (message "Enabled growl for erc-mode."))

;; (defun mb-erc-disable-growl ()
;;   "Disable growl for erc."
;;   (remove-hook 'erc-text-matched-hook 'mb-growl-erc-hook)
;;   (setq mb-erc-growl nil)
;;   (message "Disabled growl for erc-mode."))

;; (defun mb-erc-toggle-growl ()
;;   "Toggle growl for erc."
;;   (interactive)
;;   (if (not mb-erc-growl)
;;       (mb-erc-enable-growl)
;;     (mb-erc-disable-growl)))

;; (add-hook 'erc-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "C-c g") 'mb-erc-toggle-growl)))

;; cloak hook's
;; http://edward.oconnor.cx/2007/09/freenode-cloaking-and-erc
;; http://edward.oconnor.cx/config/.ercrc.el
(defvar ted-erc-autojoin t
  "Whether or not ERC should autojoin on connect.")

(defun ted-post-cloak-autojoin (network nick)
  "Autojoin when NickServ tells us to."
  (interactive)
  (when ted-erc-autojoin ; Defined in .emacs
    (erc-autojoin-channels erc-session-server (erc-current-nick)))
  nil)

(add-hook 'erc-nickserv-identified-hook 'ted-post-cloak-autojoin)

;; ;; znc
;; (add-to-list 'load-path "~/.emacs.d/znc")
;; (require 'znc)
;; (setq znc-erc-ssl-connector 'erc-tls)
;; ;; znc-servers defined in .emacs-auth


;; connecting
(load "~/.emacs-auth" t)

(defun erc-nick (server)
  (getf (getf erc-auth server) :name))

(defun erc-password (server)
  (getf (getf erc-auth server) :password))

(add-hook 'erc-after-connect
          (lambda (SERVER NICK)
            (cond ((string-match "freenode\\.net" SERVER)
                   (erc-message "NICK" (erc-nick 'freenode))
                   (erc-message "PRIVMSG" (concat "NickServ identify "
                                                  (erc-password 'freenode)))
                   (erc-message "PRIVMSG" (concat "NickServ ghost "
                                                  (erc-nick 'freenode)))))))

(provide 'dot-erc)
