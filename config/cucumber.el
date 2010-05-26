;; DESCRIPTION:
;;   Useful patterns for using the ido menu with Cucumber files.
;;
;; AUTHOR:
;;   Geoffrey Grosenbach http://peepcode.com
;;
;; Matches the major Feature/Scenario blocks.
;;
;; USAGE:
;;   (require 'config/cucumber)

(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

;; Cucumber menu
(setq feature-mode-imenu-generic-expression
      '(("Group" "\\s-*\\(\\(Feature\\|Scenario\\): .+\\)" 1)
        ))
(add-hook 'feature-mode-hook
          (lambda ()
            (setq imenu-generic-expression feature-mode-imenu-generic-expression)))

(provide 'config/cucumber)

