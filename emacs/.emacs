(require 'package)
(setq my-packages
      '(
	autopair
	company
	emmet-mode
	editor-config
        elfeed
	fill-column-indicator
	flymake
        flymake-php
        helm
	helm-git
        helm-git-grep
        helm-ls-git
	js2-mode
	less-css-mode
        magit
	markdown-mode
	monokai-theme
	php-extras
	phpunit
	phpdocumentor
        solarized-theme
        sublime-themes
        tronesque
	twittering-mode
	web-mode
        yasnippet
))

;;;;;;;;;;;;;;;;;;;;; Package management ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Repositiories list
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;; Global config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make sure that UTF-8 is used everywhere.

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-input-method nil)

;; Linum mode
(global-linum-mode t)

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

;; Some fun in the scroll
(nyan-mode)
;;(global-linum-mode 1)

;; Set font
(set-face-attribute 'default nil :family "Droid Sans Mono" :height 140)
(require 'fill-column-indicator)
(fci-mode)
(setq fci-rule-width 1)
(setq-default fci-rule-column 80)
;; Enable it globally
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)

;; Select theme
(load-theme 'solarized-dark t)

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



;; No backupfiles
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Ido mode
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers nil)


;; Special keybindings
;(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; =========== Magit
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "<f10>") 'eshell)
(global-set-key (kbd "M-o") 'other-window)

;; colum number
(setq column-number-mode t)

(require 'autopair)

;;; yasnippet
;;; should be loaded before auto complete so that they can work together
(require 'yasnippet)
(yas-global-mode 1)




;; Autocomplete
(add-hook 'after-init-hook 'global-company-mode)
(setq company-auto-complete t)
(setq company-dabbrev-downcase nil)




;; ============= MODES ========================================================

(add-to-list 'auto-mode-alist '("\\.hbs$"  . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$"  . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
(add-to-list 'auto-mode-alist '("/\\(views\\|html\\|templates\\)/.*\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.less$" . less-css-mode))

;; Javscript and relateds
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)
(add-hook 'js-mode-hook 'my-paredit-nonlisp) ;use with the above function
(add-hook 'js-mode-hook 'esk-paredit-nonlisp) ;for emacs starter kit

;;(add-hook 'less-css-mode-hook 'my-code-style-setup)

(add-hook 'php-mode-hook 'php-enable-psr2-coding-style)
(add-hook 'php-mode-hook 'flymake-php-load)
(add-hook 'php-mode-hook 'my-setup-php)


(add-hook 'web-mode-hook 'my-code-style-setup)

(defun my-code-style-setup ()
  ;; enable web mode
  (web-mode)
  ;; enable emmet-mode
  (emmet-mode)
  ;; Linue numbers
  (linum-mode)

  ;; make these variables local
  (make-local-variable 'web-mode-code-indent-offset)
  (make-local-variable 'web-mode-markup-indent-offset)
  (make-local-variable 'web-mode-css-indent-offset)

  ;; set indentation, can set different indentation level for different code type
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)

  ;; Force indentation for html attrs
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-style-padding 1)
  (setq web-mode-script-padding 1)
  (setq web-mode-block-padding 0)

  (setq web-mode-block-padding 0)
  (setq web-mode-block-padding 0)  ;; Auto pairing
  (setq web-mode-enable-css-colorization t) ;; Css colorization

  (setq web-mode-enable-auto-pairing nil) ;; Auto pairing

)

;; PHP code style configuration
(defun my-setup-php ()
  (emmet-mode)
  (fci-mode)
  (setq indent-tabs-mode nil)
  ;; line numbers
  (linum-mode)

(c-add-style
 "php"
 '((c-basic-offset . 2)
   (c-doc-comment-style . javadoc)
   (c-offsets-alist . ((arglist-close . php-lineup-arglist-close)
                       (arglist-cont . (first php-lineup-cascaded-calls 0))
                       (arglist-cont-nonempty . (first php-lineup-cascaded-calls c-lineup-arglist))
                       (arglist-intro . php-lineup-arglist-intro)
                       (case-label . +)
                       (class-open . -)
                       (comment-intro . 0)
                       (inlambda . 0)
                       (inline-open . 0)
                       (label . +)
                       (statement-cont . (first php-lineup-cascaded-calls php-lineup-string-cont +))
                       (substatement-open . 0)
                       (topmost-intro-cont . (first php-lineup-cascaded-calls +))))))



)

(setq js2-basic-offset 2)

(setq js2-use-font-lock-faces t)
(setq js2-highlight-level 3)


;; Emmet mode
;;(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.

;; Edit global configs
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; ;;(define-key text-mode-map (kdb "TAB") 'tab-to-tab-stop)
(setq-default indent-tabs-mode nil)
(setq-default tab-always-indent 'complete)
(global-set-key "\C-x z" 'repeat) ;; Create a "Redo"
(global-set-key (kbd "C-x C-k") 'kill-this-buffer) ;; Kill current buffer


;; Web mode defaults
;(defun my-web-mode-defaults()
 ; (require 'fill-column-indicator)
;;  (local-set-key (kbd "RET") 'newline-and-indent)
 ; (fci-mode)
 ; (setq fci-rule-column 80)
 ; (setq web-mode-css-indent-offset 2)
 ; (setq web-mode-markup-indent-offset 2)
 ; (setq emmet-mode t)
 ; (setq indent-tabs-mode nil)
 ; (setq tab-width 2)
 ; (setq c-basic-offset 2)
 ; (setq web-mode-code-indent-offset 2)
 ; (setq linum-mode t)
;)


;(defun my-php-mode-defaults ()
 ; "My PHP mode configuration."
 ; (require 'fill-column-indicator)
 ; (local-set-key (kbd "RET") 'newline-and-indent)
 ; (fci-mode)
 ; (setq fci-rule-column 80)
 ; (setq emmet-mode t)
 ; (setq indent-tabs-mode nil)
 ; (setq tab-width 2)
 ; (setq c-basic-offset 2)
 ; (setq linum-mode t)
;)

;; ;; Indent setup
;; ;; idea from : http://blog.binchen.org/posts/easy-indentation-setup-in-emacs-for-web-development.html
;; (defun my-setup-indent (n)
;;   (require 'fill-column-indicator)
;;   (local-set-key (kbd "RET") 'newline-and-indent)
;;   (fci-mode)
;;   (setq fci-rule-column 80)
;;   (setq emmet-mode t)
;;   ;; web development
;;   (setq coffee-tab-width n) ; coffeescript
;;   (setq javascript-indent-level n) ; javascript-mode
;;   (setq js-indent-level n) ; js-mode
;;   (setq js2-basic-offset n) ; js2-mode
;;   (setq web-mode-markup-indent-offset n) ; web-mode, html tag in html file
;;   (setq web-mode-css-indent-offset n) ; web-mode, css in html file
;;   (setq web-mode-code-indent-offset n) ; web-mode, js code in html file
;;   (setq css-indent-offset n) ; css-mode
;;   (setq tab-width n) ;; tab size
;;   (setq c-basic-offset n)
;;   (setq indent-tabs-mode nil)
;;   (setq linum-mode t)
;; )


;; (defun my-office-code-style ()
;;   (interactive)
;;   (message "Office code style!")
;;   (setq indent-tabs-mode nil) ; use space instead of tabs
;;   (my-setup-indent 2))



;; Modes and hooks
;; (add-hook 'web-mode-hook 'my-office-code-style)
;; (add-hook 'less-css-mode 'my-office-code-style)
;; (add-hook 'php-mode-hook 'my-office-code-style)

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

;; kill all buffers

(defun close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

;; Helm
;;; Enable Modes (This is loading nearly everything).
;;
(setq helm-mode 1)
(setq helm-adaptative-mode 1)
(setq helm-autoresize-mode 1)


(setq helm-locate-fuzzy-match t)   ;; Fuzzy locate files
(setq helm-M-x-fuzzy-match t)      ;; Fuzzy match
(setq helm-recentf-fuzzy-match t)  ;; Helm mini
(setq helm-semantic-fuzzy-match t) ;; Helm semantic



;;; Global-map

(global-set-key (kbd "M-x")                          'undefined)
(global-set-key (kbd "M-x")                          'helm-M-x)
(global-set-key (kbd "M-y")                          'helm-show-kill-ring)
(global-set-key (kbd "C-c f")                        'helm-recentf)
(global-set-key (kbd "C-x C-f")                      'helm-find-files)
(global-set-key (kbd "C-c <SPC>")                    'helm-all-mark-rings)
(global-set-key (kbd "C-x r b")                      'helm-filtered-bookmarks)
(global-set-key (kbd "C-h r")                        'helm-info-emacs)
(global-set-key (kbd "C-:")                          'helm-eval-expression-with-eldoc)
(global-set-key (kbd "C-,")                          'helm-calcul-expression)
(global-set-key (kbd "C-h d")                        'helm-info-at-point)
(global-set-key (kbd "C-c g")                        'helm-google-suggest)
(global-set-key (kbd "C-x C-d")                      'helm-browse-project)
(global-set-key (kbd "<f1>")                         'helm-resume)
(global-set-key (kbd "C-h C-f")                      'helm-apropos)
(global-set-key (kbd "<f5> s")                       'helm-find)
(global-set-key (kbd "<f2>")                         'helm-execute-kmacro)
(define-key global-map [remap jump-to-register]      'helm-register)
(define-key global-map [remap list-buffers]          'helm-buffers-list)
(define-key global-map [remap dabbrev-expand]        'helm-dabbrev)
(define-key global-map [remap find-tag]              'helm-etags-select)
(define-key global-map [remap xref-find-definitions] 'helm-etags-select)

;; Helm GIT
(global-set-key (kbd "C-x C-d") 'helm-browse-project)
(global-set-key (kbd "C-x C-g") 'helm-git-grep)


;; Etags
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (eshell-command
   (format "find %s -type f -name \"*.[ch]\" | etags -" dir-name)))

;; Refresh etags
  ;;;  Jonas.Jarnestrom<at>ki.ericsson.se A smarter
  ;;;  find-tag that automagically reruns etags when it cant find a
  ;;;  requested item and then makes a new try to locate it.
  ;;;  Fri Mar 15 09:52:14 2002

(defadvice find-tag (around refresh-etags activate)
   "Rerun etags and reload tags if tag not found and redo find-tag.
   If buffer is modified, ask about save before running etags."
  (let ((extension (file-name-extension (buffer-file-name))))
    (condition-case err
    ad-do-it
      (error (and (buffer-modified-p)
          (not (ding))
          (y-or-n-p "Buffer is modified, save it? ")
          (save-buffer))
         (er-refresh-etags extension)
         ad-do-it))))

(defun er-refresh-etags (&optional extension)
  "Run etags on all peer files in current dir and reload them silently."
  (interactive)
  (shell-command (format "etags *.%s" (or extension "el")))
  (let ((tags-revert-without-query t))  ; don't query, revert silently
    (visit-tags-table default-directory nil)))

;; Keybindings
(global-set-key (kbd "C-x w") 'elfeed)

;; RSS elfeed list
(setq elfeed-feeds
      '("http://nullprogram.com/feed/"
        "http://www.terminally-incoherent.com/blog/feed/"))
