(use-package web-mode)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))

(use-package prettier-js)
(add-hook 'web-mode-hook 'prettier-js-mode)
;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)

(use-package typescript-mode)

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point-max)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
