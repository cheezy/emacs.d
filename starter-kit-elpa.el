;;; starter-kit-elpa.el --- Install a base set of packages automatically.
;;
;; Part of the Emacs Starter Kit

(defvar starter-kit-packages (list 'idle-highlight
                                   'ruby-mode
                                   'inf-ruby
                                   'css-mode
                                   'elixir-mode
                                   'yasnippet
                                   'rinari
                                   'rvm
                                   'haml-mode
                                   'sass-mode
                                   'gist
                                   'textmate
                                   'rspec-mode
;;                                   'groovy-mode
                                   'jump
                                   'coffee-mode
                                   'feature-mode
;;                                   'applescript-mode
                                   'markdown-mode
                                   'yaml-mode
                                   'unbound
;;                                   'php-mode
;;                                   'jade-mode
                                   'sws-mode
                                   'flymake-easy
                                   'flymake-ruby
                                   'magit
                                   'clojure-mode
                                   'mark-multiple
                                   'expand-region
                                   'dash
                                   'ecukes
                                   'espuds
                                   )
  "Libraries that should be installed by default.")

(defun starter-kit-elpa-install ()
  "Install all starter-kit packages that aren't installed."
  (interactive)
  (dolist (package starter-kit-packages)
    (unless (functionp package)
      (package-install package))))

;; On your first run, this should pull in all the base packages.
;; But you might not be online, so ignore errors.
(ignore-errors
  (message "Checking base list of packages...")
  (starter-kit-elpa-install))

(provide 'starter-kit-elpa)
