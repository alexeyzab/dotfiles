(setq comp-speed 2)

(when (boundp 'comp-eln-load-path)
  (let ((eln-cache-dir (expand-file-name "cache/eln-cache/" user-emacs-directory))
        (find-exec (executable-find "find")))
    (setcar comp-eln-load-path eln-cache-dir)
    ;; Quitting emacs while native compilation in progress can leave zero byte
    ;; sized *.eln files behind. Hence delete such files during startup.
    (when find-exec
      (call-process find-exec nil nil nil eln-cache-dir
                    "-name" "*.eln" "-size" "0" "-delete"))))

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))

(package-initialize)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(eval-when-compile
  (add-to-list 'load-path "~/.emacs.d/use-package")
  (require 'use-package))
(use-package bind-key)
(defvar *emacs-load-start* (current-time))

(setq dotfiles-dir (file-name-directory
                    (or load-file-name (buffer-file-name))))

(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)

(prefer-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; (package-initialize)
(setq use-package-verbose t)
(setq use-package-always-ensure t)

;; Always compile packages, and use the newest version available.
(setenv "GNUPGHOME" "~/.config/gnupg")

;; Sensible defaults
(load-file "~/.emacs.d/sensible-defaults/sensible-defaults.el")
(sensible-defaults/open-files-from-home-directory)
(sensible-defaults/increase-gc-threshold)
;; (sensible-defaults/delete-trailing-whitespace)
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
 '(default ((t (:background "#2E3440"))))
 '(minibuffer-prompt ((t (:background "#2E3440" :foreground "#88C0D0" :weight bold))))
 '(scroll-bar ((t (:background "dark slate gray"))))
 '(solaire-minibuffer-face ((t (:inherit solaire-default-face :background "#2E3440")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ace-isearch-function 'avy-goto-char)
 '(ace-isearch-input-length 7)
 '(ace-isearch-jump-delay 0.25)
 '(ace-isearch-use-jump 'printing-char)
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#1B2229" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#DFDFDF"])
 '(company-backends
   '(company-etags company-bbdb company-nxml company-css company-eclim company-semantic company-clang company-xcode company-cmake company-capf company-files
		   (company-dabbrev-code company-gtags company-etags company-keywords)
		   company-oddmuse company-dabbrev))
 '(custom-safe-themes
   '("281904c0e4ef7e2e36d0433c2d28b9b46bcc173f00b563a163256ae3e543c33c" "f0eb51d80f73b247eb03ab216f94e9f86177863fb7e48b44aacaddbfe3357cf1" "db5b906ccc66db25ccd23fc531a213a1afb500d717125d526d8ff67df768f2fc" "7a242b3b88c299501f92aa94da0c242911d1c1cec851072f100daf7fc4aeca2d" "14de8f58ad656af5be374086ae7ab663811633fc1483a02add92f7a1ff1a8455" "8e0c6a96a17a5b45979c31265821053aff9beea9fb5ac5e41130e0c27a89214e" "9d9fda57c476672acd8c6efeb9dc801abea906634575ad2c7688d055878e69d6" "b35a14c7d94c1f411890d45edfb9dc1bd61c5becd5c326790b51df6ebf60f402" "9240e71034689655a6c05c04063af2c90d0a831aa4e7ca24c8b6e29b5a2da946" "4182c491b5cc235ba5f27d3c1804fc9f11f51bf56fb6d961f94788be034179ad" "a4c9e536d86666d4494ef7f43c84807162d9bd29b0dfd39bdf2c3d845dcc7b2e" "83db918b06f0b1df1153f21c0d47250556c7ffb5b5e6906d21749f41737babb7" "6007e03860ebfafc4675fe7f420a259b5aea3b6fbc77e88218b531972d0a363d" "d29231b2550e0d30b7d0d7fc54a7fb2aa7f47d1b110ee625c1a56b30fea3be0f" "14c62607fb78d5fcfc03f23430423e9a3910f2beada81be6e0deb051129e8cca" "77b450ea9ee4dd9b0b5094a6eaac7fb13976b2dbb25d789a65f8ae53f5d42207" "5dc0ae2d193460de979a463b907b4b2c6d2c9c4657b2e9e66b8898d2592e3de5" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "10e231624707d46f7b2059cc9280c332f7c7a530ebc17dba7e506df34c5332c4" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "eb0a314ac9f75a2bf6ed53563b5d28b563eeba938f8433f6d1db781a47da1366" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default))
 '(dante-repl-command-line nil)
 '(dante-repl-command-line-methods-alist
   '((styx .
	   #[257 "\300\301\302#\207"
		 [dante-repl-by-file
		  ("styx.yaml")
		  ("styx" "repl" dante-target)]
		 5 "

(fn ROOT)"])
     (nix .
	  #[257 "\300\301\302#\207"
		[dante-repl-by-file
		 ("shell.nix" "default.nix")
		 ("nix-shell" "--run"
		  (if dante-target
		      (concat "cabal new-repl " dante-target)
		    "cabal new-repl"))]
		5 "

(fn ROOT)"])
     (stack .
	    #[257 "\300\301\302#\207"
		  [dante-repl-by-file
		   ("stack.yaml")
		   ("stack" "repl" dante-target)]
		  5 "

(fn ROOT)"])
     (mafia .
	    #[257 "\300\301\302#\207"
		  [dante-repl-by-file
		   ("mafia")
		   ("mafia" "repl" dante-target)]
		  5 "

(fn ROOT)"])
     (new-build .
		#[257 "\300\301\302#\204 \303\304!\205 \305\207"
		      [directory-files nil ".*\\.cabal$" file-exists-p "cabal.project"
				       ("cabal" "new-repl" dante-target)]
		      5 "

(fn ROOT)"])
     (bare .
	   #[257 "\300\207"
		 [("cabal" "repl" dante-target)]
		 2 "

(fn _)"])))
 '(dumb-jump-rg-cmd "/usr/local/bin/rg")
 '(electric-pair-mode t)
 '(epg-gpg-program "/usr/local/bin/gpg")
 '(exec-path-from-shell-check-startup-files nil)
 '(fci-rule-color "#ECEFF1")
 '(global-flycheck-mode t)
 '(haskell-process-args-stack-ghci '(""))
 '(haskell-process-type 'stack-ghci)
 '(hl-paren-colors '("#B9F" "#B8D" "#B7B" "#B69" "#B57" "#B45" "#B33" "#B11"))
 '(hl-sexp-background-color "#efebe9")
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(lsp-haskell-process-wrapper-function
   '(lambda
      (argv)
      (append
       (append
	(list "nix-shell" "-I" "." "--command")
	(list
	 (mapconcat ÃÂÃÂÃÂÃÂÃÂÃÂÃÂÃÂÃÂÃÂÃÂÃÂÃÂÃÂÃÂÃÂ¢ÃÂÃÂÃÂÃÂÃÂÃÂÃÂÃÂÃÂÃÂÃÂÃÂÃÂÃÂÃÂÃÂÃÂÃÂÃÂÃÂÃÂÃÂÃÂÃÂÃÂÃÂÃÂÃÂÃÂÃÂÃÂÃÂidentity argv " ")))
       (list
	(concat
	 (lsp-haskell--get-root)
	 "/shell.nix")))))
 '(lsp-log-io t)
 '(org-agenda-files
   '("~/.dotfiles/tag-emacs/emacs.d/site-start.d/90org-mode-config.el" "~/Dropbox/org/index.org"))
 '(org-fontify-done-headline t)
 '(org-fontify-quote-and-verse-blocks t)
 '(org-fontify-whole-heading-line t)
 '(package-selected-packages
   '(nano-theme quelpa-use-package quelpa lsp-treemacs lsp-ivy frog-jump-buffer emojify emoji-fontset vterm ssh flymake-proselint json-mode helm-ag doom-modeline gnu-elpa-keyring-update flymake-puppet nix-update nix-haskell-mode direnv elfeed command-log-mode instapaper minions moody forge prettier-js rainbow-delimeters org-plus-contrib org-mobile-sync tide xref-js2 js2-refactor rustic eglot solaire-mode nord-theme god-mode twittering-mode deadgrep mew popup-complete ac-haskell-process auto-complete docker system-packages symon browse-kill-ring zoom ace-popup-menu wttrin bind-key rainbow-delimeters-mode rainbow-mode ghc-mod company-ghci git-timemachine php-mode web-mode js-mode js-format rjsx-mode jsx-mode restclient ranger spaceline-config spaceline helpful nix-sandbox nixos-sandbox nixos-options flycheck-haskell yasnippet-snippets yasnippet dante atom-one-dark-theme rustfmt company-nixos-options markdown-preview-mode pdf-tools w3m org-projectile paredit multi-term dumb-jump async dash with-editor diminish ivy swiper spinner seq s f queue popup epl pkg-info org-bullets magit-popup git-commit flycheck haskell-mode deferred expand-region hungry-delete counsel try straight-use-package cider clojure-mode minitest enh-ruby-mode undo-tree shm dash-at-point lsp-mode lsp-haskell lsp-rust exec-path-from-shell column-marker fzf company-racer company smooth-scrolling ace-window smartparens wrap-region purescript-mode gruvbox-theme rainbow-delimiters quack which-key tabbar hamlet-mode markdown-mode langtool flyspell-correct-helm flyspell-correct flycheck-rust cargo racer company-flx blank-mode toml-mode rust-mode spacemacs-theme dracula-theme pinentry intero company-ghc eyebrowse shell-pop solarized-theme simpleclip powerline magit hindent))
 '(purescript-mode-hook '(turn-on-purescript-indent turn-on-purescript-indentation))
 '(racer-rust-src-path "~/code/rust/src")
 '(rustic-rustfmt-bin "rustfmt")
 '(safe-local-variable-values
   '((haskell-process-use-ghci . t)
     (haskell-indent-spaces . 4)))
 '(send-mail-function 'smtpmail-send-it)
 '(shell-pop-shell-type '("eshell" "*eshell*" (lambda nil (eshell))))
 '(shell-pop-universal-key "C-t")
 '(sql-postgres-login-params '((database)))
 '(tabbar-separator '(0.5))
 '(typescript-auto-indent-flag nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#B71C1C")
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
     (360 . "#558b2f")))
 '(vc-annotate-very-old-color nil)
 '(warning-suppress-types
   '((comp)
     (comp)
     (comp)
     (comp)
     (comp)
     (comp)
     (comp)
     (comp)
     (comp)
     (comp)
     (comp)
     (comp)
     (comp)
     (comp)
     (comp)
     (comp)
     (comp)
     (comp)
     (comp)
     (comp)
     (comp)
     (comp)
     (comp)
     (comp)
     (comp)))
 '(widget-image-enable nil)
 '(x-underline-at-descent-line t)
 '(zoom-ignore-predicates
   '((lambda nil
       (>
	(count-lines
	 (point-min)
	 (point-max))
	20))))
 '(zoom-ignored-buffer-name-regexps '("^*calc"))
 '(zoom-ignored-buffer-names '("zoom.el" "init.el"))
 '(zoom-ignored-major-modes '(dired-mode markdown-mode))
 '(zoom-mode t nil (zoom))
 '(zoom-size '(0.618 . 0.618)))
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
