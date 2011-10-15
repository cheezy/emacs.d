;; DESCRIPTION:
;;   Useful patterns for using the ido menu with Javascript files.
;;
;; AUTHOR:
;;   Geoffrey Grosenbach http://peepcode.com
;;
;; Matches things like:
;;
;;   function bacon() {}        // Standard function
;;   getJSON: function () {}    // Function as a key in a hash
;;   this.post = function () {} // Instance method in a function
;;   var MyObj = { ...          // Capitalized variable object 
;;
;; USAGE:
;;   (require 'cheezy/js)

(setq js2-basic-offset 2)
(setq js2-auto-indent-flag nil)
(setq javascript-indent-level 2)

(add-to-list 'auto-mode-alist '("Jimfile\\'" . javascript-mode))

(setq cheezy-js-imenu-generic-expression
      '(("Named Function" "function\\s-+\\(\\w+\\)\\s-*(" 1)
        ("Hash Method"  "^\\s-*\\(\\w+\\):\\s-*function\\s-*(" 1)
        ("Instance Method" "this\.\\(\\w+\\)\\s-*=\\s-*function\\s-*(" 1)
        ("Variable as Class" "var \\([A-Z]+\\w+\\) = {" 1)
        ("Assigned Function" "^\\s-*\\([A-z.]+\\w\\) = function\\s-*(.*) {" 1)
        ))

(add-hook 'javascript-mode-hook
          (lambda ()
            (setq imenu-generic-expression cheezy-js-imenu-generic-expression)
            (setq indent-region-function cheezy-js-beautify)))

;; Run jslint on a file to check syntax and coding conventions.
(add-hook 'javascript-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (let ((file (file-name-nondirectory buffer-file-name)))
                   (concat "node ~/src/reid-node-jslint/bin/jslint.js " file)))))

(defun tf-beautify-js ()
  "Run source through JavaScript beautifier."
  (interactive)
  ;; TODO: Doesn't save cursor location
  (save-excursion
    (shell-command-on-region
     (point-min) (point-max)
     "~/bin/beautify-js" t t)))

(add-hook 'javascript-mode-hook
          (lambda()
            (local-set-key [(meta shift n)] 'tf-beautify-js)
            ))

(provide 'cheezy/js)
