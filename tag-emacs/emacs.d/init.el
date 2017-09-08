;; Use package management!
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "https://stable.melpa.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Always compile packages, and use the newest version available.
(use-package auto-compile
  :config (auto-compile-on-load-mode))
(setq load-prefer-newer t)

;; Load main configuration with Org-mode
(org-babel-load-file "~/.emacs.d/configuration.org")

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
 '(company-backends
   (quote
    (company-etags company-bbdb company-nxml company-css company-eclim company-semantic company-clang company-xcode company-cmake company-capf company-files
                   (company-dabbrev-code company-gtags company-etags company-keywords)
                   company-oddmuse company-dabbrev)))
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (doom-vibrant)))
 '(custom-safe-themes
   (quote
    ("365d9553de0e0d658af60cff7b8f891ca185a2d7ba3fc6d29aadba69f5194c7f" "611e38c2deae6dcda8c5ac9dd903a356c5de5b62477469133c89b2785eb7a14d" "2e973a84accb627f603f309fc13938daa227055d2a6528c80a9b7a9e2dd8da6b" "b81bfd85aed18e4341dbf4d461ed42d75ec78820a60ce86730fc17fc949389b2" "6f11ad991da959fa8de046f7f8271b22d3a97ee7b6eca62c81d5a917790a45d9" "4182c491b5cc235ba5f27d3c1804fc9f11f51bf56fb6d961f94788be034179ad" "6c35ffc17f8288be4c7866deb7437e8af33cd09930e195738cdfef911ab77274" "67e998c3c23fe24ed0fb92b9de75011b92f35d3e89344157ae0d544d50a63a72" "ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" "83db918b06f0b1df1153f21c0d47250556c7ffb5b5e6906d21749f41737babb7" "6007e03860ebfafc4675fe7f420a259b5aea3b6fbc77e88218b531972d0a363d" "d29231b2550e0d30b7d0d7fc54a7fb2aa7f47d1b110ee625c1a56b30fea3be0f" "14c62607fb78d5fcfc03f23430423e9a3910f2beada81be6e0deb051129e8cca" "77b450ea9ee4dd9b0b5094a6eaac7fb13976b2dbb25d789a65f8ae53f5d42207" "5dc0ae2d193460de979a463b907b4b2c6d2c9c4657b2e9e66b8898d2592e3de5" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "10e231624707d46f7b2059cc9280c332f7c7a530ebc17dba7e506df34c5332c4" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "eb0a314ac9f75a2bf6ed53563b5d28b563eeba938f8433f6d1db781a47da1366" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default)))
 '(dumb-jump-rg-cmd "/usr/local/bin/rg")
 '(electric-pair-mode t)
 '(engine-mode t)
 '(epg-gpg-program "/usr/local/bin/gpg")
 '(exec-path-from-shell-check-startup-files nil)
 '(fci-rule-color "#ECEFF1")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(hl-paren-colors
   (quote
    ("#B9F" "#B8D" "#B7B" "#B69" "#B57" "#B45" "#B33" "#B11")))
 '(hl-sexp-background-color "#efebe9")
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(package-selected-packages
   (quote
    (cargo-mode solaire-mode nord-theme zenburn-theme shakespeare-mode typing-game typing projectile-ripgrep company-mode git-timemachine avy multiple-cursors hasky-stack mu4e-alert dired-open dired-details dired+ paredit synosaurus 4clojure rg ag counsel-projectile doom-themes company-ycmd flycheck-package engine-mode projectile yasnippet web-mode diff-hl auto-compile multi-term dumb-jump async with-editor diminish ivy swiper spinner seq s f queue popup epl pkg-info org-bullets magit-popup git-commit flycheck haskell-mode deferred expand-region hungry-delete counsel try use-package cider clojure-mode minitest enh-ruby-mode undo-tree shm dash-at-point lsp-mode lsp-haskell lsp-rust exec-path-from-shell column-marker fzf company-racer company smooth-scrolling ace-window smartparens wrap-region purescript-mode gruvbox-theme rainbow-delimiters quack which-key tabbar hamlet-mode markdown-mode langtool flyspell-correct-helm flyspell-correct flycheck-rust cargo racer company-flx blank-mode toml-mode rust-mode spacemacs-theme dracula-theme pinentry intero company-ghc eyebrowse shell-pop solarized-theme simpleclip powerline magit hindent)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(purescript-mode-hook
   (quote
    (turn-on-purescript-indent turn-on-purescript-indentation)))
 '(racer-rust-src-path
   "~/.multirust/toolchains/nightly-x86_64-apple-darwin/lib/rustlib/src/rust/src")
 '(safe-local-variable-values
   (quote
    ((haskell-indent-spaces . 2)
     (haskell-process-use-ghci . t)
     (haskell-indent-spaces . 4))))
 '(send-mail-function (quote smtpmail-send-it))
 '(shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda nil (eshell)))))
 '(shell-pop-universal-key "C-t")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(tabbar-separator (quote (0.5)))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
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
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#282c34" :foreground "#bbc2cf" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :foundry "nil" :family "Fantasque Sans Mono"))))
 '(markdown-code-face ((t nil)))
 '(scroll-bar ((t (:background "dark slate gray")))))
