(require 'package)
(setq my-packages
      '(
	auto-complete
	auto-complete-config
	autopair
	emmet-mode
	editor-config
	fill-column-indicator
	less-css-mode
	markdown-mode
	monokai-theme
	twittering-mode
	web-mode	
))

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

;; Set font
(set-face-attribute 'default nil :family "Monaco" :height 128)


(require 'fill-column-indicator)
(fci-mode)
(setq fci-rule-column 80)

;; Select theme
(load-theme 'monokai t)

;; Highlight current line
(global-hl-line-mode 1)

;; No GUI stuff, please
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

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

;; Autocomplete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
;; Autocomplete key --
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")


;; ============= MODES ========================================================
(add-to-list 'auto-mode-alist '("\\.hbs$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.less$" . less-css-mode))

;; Web mode
(add-hook 'web-mode-hook
	  '(lambda()
    (setq emmet-mode t)
	  (setq indent-tabs-mode nil)
	  (setq tab-width 2)
	  (setq c-basic-indent 4)))
