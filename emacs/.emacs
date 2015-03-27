(require 'package)
(setq my-packages
      '(
	auto-complete
	auto-complete-config
        ac-js2
	ac-php
	autopair
	emmet-mode
	editor-config
	fill-column-indicator
	flymake
	js2-mode
	less-css-mode
        magit
	markdown-mode
	monokai-theme
	phpunit
	phpdocumentor
        tronesque
	twittering-mode
	web-mode
        yasnipet
))

;;;;;;;;;;;;;;;;;;;;; Package management ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Repositiories list
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
;;; initialize the packages and create the packages list if not exists
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;;; install packages if not exists
(dolist (pkg my-packages)
  (when (and (not (package-installed-p pkg))
           (assoc pkg package-archive-contents))
    (package-install pkg)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;; Global config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make sure that UTF-8 is used everywhere.

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-input-method nil)

;; I like to keep all of the temporal files and dirs (cache, backups,
;; ...) in an unique directory. If this directory does not exists, then
;; create it

(unless (file-exists-p "~/.emacs.d/tmp")
  (make-directory "~/.emacs.d/tmp"))
(defvar voylinux-emacs-temporal-directory (concat user-emacs-directory "tmp/"))

;; I prefer to use a undo-tree with branches that store auto-save files.

(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)

;;;;;;;;;;;;;;;;;;;;;;;; GUI and visual configs ;;;;;;;;;;;;;;;;

;; Set font
(set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 128)
(require 'fill-column-indicator)
(fci-mode)
(setq fci-rule-column 80)

;; Select theme
(load-theme 'tronesque t)

;; Highlight current line
(global-hl-line-mode 1)

;; No GUI stuff, please
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq redisplay-dont-pause t)

;; Splash screen settings
(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)



;; No backup files
(setq make-backup-files nil)

;; Ido mode
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)


;; Special keybindings
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-x g") 'magit-status)

;; colum number
(setq column-number-mode t)

;; line numbers
(global-linum-mode t)


;; Autopair
(require 'autopair)

;;; yasnippet
;;; should be loaded before auto complete so that they can work together
(require 'yasnippet)
(yas-global-mode 1)

;; Autocomplete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
;; Autocomplete key --
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")



;; ============= MODES ========================================================
(add-to-list 'auto-mode-alist '("\\.hbs$"  . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$"  . web-mode))
(add-to-list 'auto-mode-alist '("\\.php$"  . web-mode))
(add-to-list 'auto-mode-alist '("\\.less$" . less-css-mode))

;; Javscript and relateds
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)
(add-hook 'js-mode-hook 'my-paredit-nonlisp) ;use with the above function
(add-hook 'js-mode-hook 'esk-paredit-nonlisp) ;for emacs starter kit
(setq js2-basic-offset 2)

(setq js2-use-font-lock-faces t)
(setq js2-highlight-level 3)


;; Emmet mode
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.

;; Edi global configs
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;;(define-key text-mode-map (kdb "TAB") 'tab-to-tab-stop)
(setq-default indent-tabs-mode nil)
(setq-default tab-always-indent 'complete)



;; Web mode defaults
(defun my-web-mode-defaults()
  (require 'fill-column-indicator)
  (local-set-key (kbd "RET") 'newline-and-indent)
  (fci-mode)
  (setq fci-rule-column 80)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)


  (setq emmet-mode t)
  (setq web-mode-code-indent-offset 2))

;; Modes and hooks
(add-hook 'web-mode-hook 'my-web-mode-defaults)
(add-hook 'less-css-mode 'my-web-mode-defaults)

;; Org mode

(add-to-list 'auto-mode-alist '("\.org\'" . org-mode))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-log-done t)
(setq org-agenda-files (list "~/org/blog.org"
"~/org/personal.org"
"~/org/hb.org"
"~/org/mh.org"
"~/org/learn.org"
))

;; Custom functions

(defun open-dot-emacs ()
  "opening dot emacs"
  (interactive) ; Now it's also a command
  (find-file "~/.emacs")
)

;; kill all buffes
(defun close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))
