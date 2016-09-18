(require-package 'circe)
(require-package 'circe-notifications)

(setq circe-network-options
      '(("Freenode"
         :tls t
         :nick my-irc-nick
         :sasl-username my-irc-nick
         ;; :sasl-password ""
         :channels ("#emacs" "#orgmode"
                    "#clojure" "#clojure-beginners" "#clojurescript" "#cljs-dev"
                    "#immutant" "#leiningen" ;; "#hoplon" "#compojure" "#bootclj"
                    ;; "#clojure-emacs"
                    "#lisp" "#clnoobs" "#lispcafe" "#lispweb" "#quicklisp"
                    "#html" "#html5" "#whatwg"
                    "#css"
                    "##javascript" "#reactjs"

                    ;; "#ansible" "#ubuntu" "#haskell" "#go-nuts" "#postgresql"
                    "#docker" "##programming"))))

(setq circe-use-cycle-completion t
      circe-reduce-lurker-spam t)

(add-hook 'circe-chat-mode-hook 'my-circe-prompt)
(defun my-circe-prompt ()
  (lui-set-prompt
   (concat (propertize (concat (buffer-name) ">")
                       'face 'circe-prompt-face)
           " ")))

(setq lui-track-bar-behavior 'before-switch-to-buffer)
;; (setq tracking-ignored-buffers '(("#emacs" circe-highlight-nick-face)))

(enable-lui-track-bar)

;; desktop notifications
(eval-after-load "circe-notifications"
  '(setq circe-notifications-watch-strings
         '("anthracite")
         circe-notifications-alert-style 'osx-notifier)) ;; see alert-styles for more!

(add-hook 'circe-server-connected-hook 'enable-circe-notifications)

(provide 'setup-circe)
