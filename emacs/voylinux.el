;;;
;;    Emacs configuration
;; author: @voylinux
;; email: voylinux@gmail.com
;; updated_at: 2015-05-29
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)

;;; Code:
(setq my-packages
      '(
;;	autopair
;;	company
	emmet-mode
;;	editor-config
;;      alead
	fill-column-indicator
;;	flymake
;;      flymake-php
;;      helm
	helm-git
        helm-git-grep
        helm-ls-git
;;	js2-mode
	less-css-mode
	markdown-mode
	monokai-theme
        nyam-mode
        php-mode
;;	php-extras
;;	phpunit
;;	phpdocumentor
;;      solarized-theme
;;      sublime-themes
;;      tronesque
;;	twittering-mode
	web-mode
;;      yasnippet
))

;;; Repositiories list
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

;;; initialize the packages and create the packages list if not exists
(require 'eieio)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;;; install packages if not exists
(dolist (pkg my-packages)
  (when (and (not (package-installed-p pkg))
             (assoc pkg package-archive-contents))
    (package-install pkg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global config
;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; All temp files in a single directory

(unless (file-exists-p "~/.emacs.d/tmp")
  (make-directory "~/.emacs.d/tmp"))
(defvar voylinux-emacs-temporal-directory (concat user-emacs-directory "tmp/"))

;;; No autosaves

(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)

;; No backupfiles
(setq make-backup-files nil)
(setq auto-save-default nil)

;;; No tabs
(setq-default indent-tabs-mode nil)


;;;;;;;;;;;;;;;;;;;;;;;;;
;; GUI and visual configs
;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Some fun in the scroll
(nyan-mode t)

;;; No scroll bar
(scroll-bar-mode -1)

;;; Set font
(set-face-attribute 'default nil :family "Droid Sans Mono" :height 140)

;;; 80 colums indicator
(require 'fill-column-indicator)
(fci-mode)
(setq fci-rule-width 1)
(setq-default fci-rule-column 80)
;; Enable it globally
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)

;;; Select theme
(load-theme 'monokai t)

;;; Highlight current line
(global-hl-line-mode 1)

;;; colum number
(setq column-number-mode t)

;;; Delete trailling spaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; Line numbers
(require 'linum)
(global-linum-mode 1)





;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  MODES
;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'php-mode)
(add-to-list 'auto-mode-alist '("\\.hbs$"  . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$"  . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
(add-to-list 'auto-mode-alist '("/\\(views\\|html\\|templates\\)/.*\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.less$" . less-css-mode))

(defun php-coding-style ()
  (setq c-basic-indent 2)
  (setq c-basic-offset 2)
  (c-set-offset 'case-label '+)
  (c-set-offset 'arglist-intro '+) ; for FAPI arrays and DBTNG
  (c-set-offset 'arglist-cont-nonempty 'c-lineup-math)
  )

(defun web-coding-style ()
  (setq c-basic-indent 2)
  (setq c-basic-offset 2)
  (c-set-offset 'case-label '+)
  (c-set-offset 'arglist-intro '+) ; for FAPI arrays and DBTNG
  (c-set-offset 'arglist-cont-nonempty 'c-lineup-math)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-script-padding 1)
  (setq web-mode-block-padding 0)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-comment-keywords t)
  )


(add-hook 'php-mode-hook 'php-coding-style)
(add-hook 'web-mode-hook 'web-coding-style)
(add-hook 'less-css-mode-hook 'web-coding-style)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Custom functions
;;;;;;;;;;;;;;;;;;;;;;;;

(defun open-dot-emacs ()
  "opening dot emacs"
  (interactive) ; Now it's also a commandç
  (find-file "~/.emacs.d/personal/voylinux.el")
  )

;;; kill all buffers
(defun close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))




;; Make all this available
(provide 'voylinux)

;;; voylinux.el ends here
