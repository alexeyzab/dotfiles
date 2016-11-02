(defvar *emacs-load-start* (current-time))

(setq dotfiles-dir (file-name-directory
                    (or load-file-name (buffer-file-name))))

(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(global-linum-mode t)
(setq inhibit-startup-screen t)

(prefer-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; Use package management!
(require 'package)
(add-to-list
  'package-archives
  '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)

;; Intero
(package-install 'intero)
(add-hook 'haskell-mode-hook 'intero-mode)

;; My-site-start config
(autoload 'my-site-start "~/.emacs.d/my-site-start/my-site-start" nil t)
(my-site-start "~/.emacs.d/site-start.d/")

 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(scroll-bar ((t (:background "dark slate gray")))))

(set-default-font "Fantasque Sans Mono-13")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ace-isearch-function (quote avy-goto-char))
 '(ace-isearch-input-length 7)
 '(ace-isearch-jump-delay 0.25)
 '(ace-isearch-use-jump (quote printing-char))
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes
   (quote
    ("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default)))
 '(package-selected-packages
   (quote
    (smex eyebrowse helm-company helm-hoogle spacemacs-theme shell-pop solarized-theme simpleclip powerline magit intero hindent ace-isearch)))
 '(shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda nil (eshell)))))
 '(shell-pop-universal-key "C-t"))
