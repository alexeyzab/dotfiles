(defvar *emacs-load-start* (current-time))

(setq dotfiles-dir (file-name-directory
                    (or load-file-name (buffer-file-name))))

(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)

(prefer-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; Use package management!
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "https://stable.melpa.org/packages/"))

(package-initialize)

;; Load packages
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-verbose t)
(setq use-package-always-ensure t)
(require 'use-package)

;; Always compile packages, and use the newest version available.
(setenv "GNUPGHOME" "~/.config/gnupg")
(use-package auto-compile
  :config (auto-compile-on-load-mode))
(setq load-prefer-newer t)


;; Sensible defaults
(load-file "~/.emacs.d/sensible-defaults.el")
(sensible-defaults/open-files-from-home-directory)
(sensible-defaults/increase-gc-threshold)
(sensible-defaults/delete-trailing-whitespace)
(sensible-defaults/treat-camelcase-as-separate-words)
(sensible-defaults/automatically-follow-symlinks)
(sensible-defaults/make-scripts-executable)
(sensible-defaults/single-space-after-periods)
(sensible-defaults/offer-to-create-parent-directories-on-save)
(sensible-defaults/apply-changes-to-highlighted-region)
(sensible-defaults/overwrite-selected-text)
(sensible-defaults/ensure-that-files-end-with-newline)
(sensible-defaults/confirm-closing-emacs)
(sensible-defaults/quiet-startup)
(sensible-defaults/make-dired-file-sizes-human-readable)
(sensible-defaults/shorten-yes-or-no)
(sensible-defaults/always-highlight-code)
(sensible-defaults/refresh-buffers-when-files-change)
(sensible-defaults/show-matching-parens)
(sensible-defaults/open-clicked-files-in-same-frame-on-mac)
(sensible-defaults/bind-keys-to-change-text-size)

;; My-site-start config
(autoload 'my-site-start "~/.emacs.d/my-site-start/my-site-start" nil t)
(my-site-start "~/.emacs.d/site-start.d/")
(setenv "INSIDE_EMACS" (format "%s,comint" emacs-version))

 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#282C34" :foreground "#bbc2cf" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :foundry "nil" :family "Fantasque Sans Mono"))))
 '(scroll-bar ((t (:background "dark slate gray")))))

(set-default-font "Fantasque Sans Mono-12")
; (set-default-font "Hack-12")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ace-isearch-function (quote avy-goto-char))
 '(ace-isearch-input-length 7)
 '(ace-isearch-jump-delay 0.25)
 '(ace-isearch-use-jump (quote printing-char))
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#1B2229" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#DFDFDF"])
 '(company-backends
   (quote
    (company-etags company-bbdb company-nxml company-css company-eclim company-semantic company-clang company-xcode company-cmake company-capf company-files
		   (company-dabbrev-code company-gtags company-etags company-keywords)
		   company-oddmuse company-dabbrev)))
 '(custom-enabled-themes (quote (doom-one)))
 '(custom-safe-themes
   (quote
    ("4182c491b5cc235ba5f27d3c1804fc9f11f51bf56fb6d961f94788be034179ad" "a4c9e536d86666d4494ef7f43c84807162d9bd29b0dfd39bdf2c3d845dcc7b2e" "83db918b06f0b1df1153f21c0d47250556c7ffb5b5e6906d21749f41737babb7" "6007e03860ebfafc4675fe7f420a259b5aea3b6fbc77e88218b531972d0a363d" "d29231b2550e0d30b7d0d7fc54a7fb2aa7f47d1b110ee625c1a56b30fea3be0f" "14c62607fb78d5fcfc03f23430423e9a3910f2beada81be6e0deb051129e8cca" "77b450ea9ee4dd9b0b5094a6eaac7fb13976b2dbb25d789a65f8ae53f5d42207" "5dc0ae2d193460de979a463b907b4b2c6d2c9c4657b2e9e66b8898d2592e3de5" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "10e231624707d46f7b2059cc9280c332f7c7a530ebc17dba7e506df34c5332c4" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "eb0a314ac9f75a2bf6ed53563b5d28b563eeba938f8433f6d1db781a47da1366" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default)))
 '(dumb-jump-rg-cmd "/usr/local/bin/rg")
 '(electric-pair-mode t)
 '(epg-gpg-program "/usr/bin/gpg")
 '(exec-path-from-shell-check-startup-files nil)
 '(fci-rule-color "#ECEFF1")
 '(hl-paren-colors
   (quote
    ("#B9F" "#B8D" "#B7B" "#B69" "#B57" "#B45" "#B33" "#B11")))
 '(hl-sexp-background-color "#efebe9")
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(org-fontify-done-headline t)
 '(org-fontify-quote-and-verse-blocks t)
 '(org-fontify-whole-heading-line t)
 '(package-selected-packages
   (quote
    (atom-one-dark-theme rustfmt company-nixos-options markdown-preview-mode pdf-tools w3m org-projectile paredit multi-term dumb-jump async dash with-editor diminish ivy swiper spinner seq s f queue popup epl pkg-info org-bullets magit-popup git-commit flycheck haskell-mode deferred expand-region hungry-delete counsel try use-package cider clojure-mode minitest enh-ruby-mode undo-tree shm dash-at-point lsp-mode lsp-haskell lsp-rust exec-path-from-shell column-marker fzf company-racer company smooth-scrolling ace-window smartparens wrap-region purescript-mode gruvbox-theme rainbow-delimiters quack which-key tabbar hamlet-mode markdown-mode langtool flyspell-correct-helm flyspell-correct flycheck-rust cargo racer company-flx blank-mode toml-mode rust-mode spacemacs-theme dracula-theme pinentry intero company-ghc eyebrowse shell-pop solarized-theme simpleclip powerline magit hindent)))
 '(purescript-mode-hook
   (quote
    (turn-on-purescript-indent turn-on-purescript-indentation)))
 '(racer-rust-src-path
   "~/.multirust/toolchains/nightly-x86_64-apple-darwin/lib/rustlib/src/rust/src")
 '(safe-local-variable-values
   (quote
    ((haskell-process-use-ghci . t)
     (haskell-indent-spaces . 4))))
 '(shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda nil (eshell)))))
 '(shell-pop-universal-key "C-t")
 '(tabbar-separator (quote (0.5)))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#B71C1C")
     (40 . "#FF5722")
     (60 . "#FFA000")
     (80 . "#558b2f")
     (100 . "#00796b")
     (120 . "#2196f3")
     (140 . "#4527A0")
     (160 . "#B71C1C")
     (180 . "#FF5722")
     (200 . "#FFA000")
     (220 . "#558b2f")
     (240 . "#00796b")
     (260 . "#2196f3")
     (280 . "#4527A0")
     (300 . "#B71C1C")
     (320 . "#FF5722")
     (340 . "#FFA000")
     (360 . "#558b2f"))))
 '(vc-annotate-very-old-color nil))
