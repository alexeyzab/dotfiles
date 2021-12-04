;; Configure package.el to include MELPA.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Ensure that use-package is installed.
;;
;; If use-package isn't already installed, it's extremely likely that this is a
;; fresh installation! So we'll want to update the package repository and
;; install use-package before loading the literate configuration.
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(org-babel-load-file "~/.emacs.d/configuration.org")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elfeed-feeds
   (quote
    ("https://www.barbellmedicine.com/blog/feed/"
     ("https://www.barbellmedicine.com/blog/feed/ \"~Barbell Medicine\"" fitness)
     ("https://feeds.feedburner.com/marginalrevolution/feed" economics)
     ("http://sachachua.com/blog/category/emacs/feed/" software emacs))))
 '(org-noter-notes-search-path (quote ("~/Dropbox/org/notes")))
 '(package-selected-packages
   (quote
    (json-mode restclient org-noter ansi package-build shut-up epl git commander f dash s cask md4rd perspective shx company-prescient ivy-prescient prescient pdf-tools deadgrep w3m tide engine-mode wgrep smex flx counsel which-key helpful dired-open dired-hide-dotfiles deft synosaurus elfeed-org elfeed bbdb evil-mu4e instapaper graphviz-dot-mode gnuplot org-bullets multi-term yaml-mode web-mode sbt-mode scala-mode projectile-rails ruby-end yard-mode rspec-mode chruby company-jedi py-autopep8 elpy python-mode flycheck-package rainbow-delimiters paredit coffee-mode haskell-mode slim-mode haml-mode company-go go-errcheck go-mode scss-mode company-coq projectile evil-magit forge ghub magit flycheck dumb-jump company ag diff-hl minions moody solarized-theme evil-org evil-surround evil auto-compile use-package)))
 '(safe-local-variable-values
   (quote
    ((haskell-process-use-ghci . t)
     (haskell-indent-spaces . 4)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-code-face ((t nil))))
