;;; starter-kit-elpa.el --- Install a base set of packages automatically.
;;
;; Part of the Emacs Starter Kit

(defvar starter-kit-packages (list 'idle-highlight
                                   'ruby-mode
                                   'inf-ruby
                                   'js2-mode
                                   'css-mode
                                   'nxml
                                   'gist
                                   'rinari
                               ;; To submit
;;;                                "magit"
;;;                                "paredit"
;;;                                "clojure-mode"
;;;                                "yaml"
;;;                                "haml"
;;;                                "sass"
;;;                                "cheat"
;;;                                "html-fontify"
;;;                                "color-theme"
;;;                                "color-theme-zenburn"
;;;                                "color-theme-vivid-chalk"
                               ;; Complicated ones
;;;                                "nxhtml"
;;;                                "jabber"
;;;                                "slime"
;;;                                "swank-clojure"
                                   )
  "Libraries that should be installed by default.")

(defun starter-kit-elpa-install ()
  "Install all starter-kit packages that aren't installed."
  (interactive)
  (dolist (package starter-kit-packages)
    (unless (or (member package package-activated-list)
                (functionp package))
      (message "Installing %s" (symbol-name package))
      (package-install package))))

(defun esk-online? ()
  "See if we're online.

Windows does not have the network-interface-list function, so we
just have to assume it's online."
  ;; TODO how could this work on Windows?
  (if (and (functionp 'network-interface-list)
           (network-interface-list))
      (some (lambda (iface) (unless (equal "lo" (car iface))
                         (member 'up (first (last (network-interface-info
                                                   (car iface)))))))
            (network-interface-list))
    t))

;; On your first run, this should pull in all the base packages.
(when (esk-online?) (ignore-errors (with-timeout (15)
                                     (starter-kit-elpa-install))))

(unless (functionp 'idle-highlight)
  ;; TODO: Quick workaround for a problem folks are reporting until I
  ;; get a chance to investigate further.
  (defun idle-highlight () (interactive)))

(provide 'starter-kit-elpa)