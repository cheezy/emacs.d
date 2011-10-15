
;; Python
;; Live cyclomatic complexity script from @garybernhardt
;; http://blog.extracheese.org/2009/11/refactoring_a_cyclomatic_complexity_script.html
;; (add-to-list 'load-path (concat dotfiles-dir "/vendor/pycomplexity"))
;; (require 'linum)
;; (require 'pycomplexity)
;; (add-hook 'python-mode-hook
;;           (function (lambda ()
;;                       (flymake-mode)
;;                       (linum-mode)
;;                       (pycomplexity-mode))))

(provide 'cheezy/python)
