;; When 'package-quickstart' is non-nil, package.el precomputes a big autoloads
;; file so that activation of packages can be done much faster, which can speed up
;; your startup significantly.
;; It also causes variables like package-user-dir and package-load-list to be
;; consulted when 'package-quickstart-refresh' is run rather than at startup so
;; you don't need to set them in your early init file.
(setq package-quickstart t)
