(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   [monokai-bg monokai-red monokai-green monokai-orange monokai-blue monokai-purple monokai-cyan monokai-fg])
 '(custom-enabled-themes (quote (molokai)))
 '(custom-safe-themes
   (quote
    ("d293542c9d4be8a9e9ec8afd6938c7304ac3d0d39110344908706614ed5861c9" "e26780280b5248eb9b2d02a237d9941956fc94972443b0f7aeec12b5c15db9f3" "99cbc2aaa2b77374c2c06091494bd9d2ebfe6dc5f64c7ccdb36c083aff892f7d" "47583b577fb062aeb89d3c45689a4f2646b7ebcb02e6cb2d5f6e2790afb91a18" "60e70079a187df634db25db4bb778255eaace1ef4309e56389459fb9418b4840" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(fci-rule-color monokai-bg-1)
 '(global-font-lock-mode t)
 '(global-linum-mode t)
 '(ido-mode (quote both) nil (ido))
 '(line-number-mode nil)
 '(menu-bar-mode nil)
 '(nyan-mode t)
 (require 'package)
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.milkbox.net/packages/"))))
 (add-to-list 'package-archives
	      '("marmalade" .
		"http://marmalade-repo.org/packages/"))
(package-refresh-contents)

(defun install-if-needed (package)
  (unless (package-installed-p package)
    (package-install package)))

;; make more packages available with the package installer
(setq to-install
      '(python-mode magit yasnippet jedi auto-complete autopair find-file-in-repository))

(mapc 'install-if-needed to-install)


(require 'magit)
(global-set-key "\C-xg" 'magit-status)

(require 'auto-complete)
(require 'autopair)
(require 'flymake)
(require 'yasnippet)

(global-set-key [f7] 'find-file-in-repository)

;; auto-complete mode extra settings
(setq
 ac-auto-start 2
 ac-override-local-map nil
 ac-use-menu-map t
 ac-candidate-limit 20)


;; Python mode settings
(require 'python-mode)
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(setq py-electric-colon-active t)
(add-hook 'python-mode-hook 'autopair-mode)
(add-hook 'python-mode-hook 'yas-minor-mode)

;; Jedi Settings
(require 'jedi)

 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(vc-annotate-background monokai-bg)
 '(vc-annotate-color-map
   (quote
    ((20 . monokai-fg-1)
     (40 . monokai-bg+2)
     (60 . monokai-red)
     (80 . monokai-red+1)
     (100 . monokai-orange)
     (120 . monokai-orange+1)
     (140 . monokai-green)
     (160 . monokai-green+1)
     (180 . monokai-yellow)
     (200 . monokai-yellow+1)
     (220 . monokai-blue)
     (240 . monokai-blue+1)
     (260 . monokai-purple)
     (280 . monokai-purple+1)
     (300 . monokai-cyan)
     (320 . monokai-cyan+1)
     (340 . monokai-magenta)
     (360 . monokai-magenta+1))))
 '(vc-annotate-very-old-color monokai-magenta))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))
