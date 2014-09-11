;; Requisites: Emacs >= 24
(require 'package)
(package-initialize)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)

(package-refresh-contents)

(defun install-if-needed (package)
  (unless (package-installed-p package)
    (package-install package)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GENERAL CUSTOMIZATIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Line numbers
(setq column-number-mode t)
(global-linum-mode t)

;; No auto-backup
(setq make-backup-files nil)


;; make more packages available with the package installer
(setq to-install
      '(python-mode magit pep8 linum hlinum yasnippet jedi jinja2-mode auto-complete autopair find-file-in-repository))

(mapc 'install-if-needed to-install)

(require 'magit)
(global-set-key "\C-xg" 'magit-status)

(require 'auto-complete)
(require 'autopair)
(require 'flymake)
(require 'yasnippet)


;; Special keys mapping
(global-set-key [f7] 'find-file-in-repository) ;; Finds in repository
(global-set-key [f8] 'pep8)  ;; Check PEP8 rules in Python files

; auto-complete mode extra settings
(setq
 ac-auto-start 2
 ac-override-local-map nil
 ac-use-menu-map t
 ac-candidate-limit 20)

;; ;; Python mode settings
(require 'python-mode)
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(setq py-electric-colon-active t)
(add-hook 'python-mode-hook '(lambda() (require 'virtualenv)))
(add-hook 'python-mode-hook 'autopair-mode)
(add-hook 'python-mode-hook 'yas-minor-mode)
(add-hook 'python-mode-hook 'pylint-flymake-mode)
(add-hook 'python-mode-hook 'pep8-flymake-mode)

;; ;; Jedi settings
(require 'jedi)
;; It's also required to run "pip install --user jedi" and "pip
;; install --user epc" to get the Python side of the library work
;; correctly.
;; With the same interpreter you're using.

;; if you need to change your python intepreter, if you want to change it
;; (setq jedi:server-command
;;       '("python2" "/home/andrea/.emacs.d/elpa/jedi-0.1.2/jediepcserver.py"))

(add-hook 'python-mode-hook
	  (lambda ()
	    (jedi:setup)
	    (jedi:ac-setup)
            (local-set-key "\C-cd" 'jedi:show-doc)
            (local-set-key (kbd "M-SPC") 'jedi:complete)
            (local-set-key (kbd "M-.") 'jedi:goto-definition)))

;; Flymake settings for Python
(defun flymake-python-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "epylint" (list local-file))))

(defun flymake-activate ()
  "Activates flymake when real buffer and you have write access"
  (if (and
       (buffer-file-name)
       (file-writable-p buffer-file-name))
      (progn
        (flymake-mode t)
        ;; this is necessary since there is no flymake-mode-hook...
        (local-set-key (kbd "C-c n") 'flymake-goto-next-error)
        (local-set-key (kbd "C-c p") 'flymake-goto-prev-error))))

(defun ca-flymake-show-help ()
  (when (get-char-property (point) 'flymake-overlay)
    (let ((help (get-char-property (point) 'help-echo)))
      (if help (message "%s" help)))))

(add-hook 'post-command-hook 'ca-flymake-show-help)


(add-to-list 'flymake-allowed-file-name-masks
             '("\\.py\\'" flymake-python-init))

(add-hook 'python-mode-hook 'flymake-activate)
(add-hook 'python-mode-hook 'auto-complete-mode)
(add-hook 'python-mode-hook 'pep8-flymake-mode)

(ido-mode t)
(linum-mode t)
;; -------------------- extra nice things --------------------
;; use shift to move around windows
(windmove-default-keybindings 'shift)
(show-paren-mode t)
 ; Turn beep off
(setq visible-bell nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (molokai)))
 '(custom-safe-themes (quote ("0e121ff9bef6937edad8dfcff7d88ac9219b5b4f1570fd1702e546a80dba0832" "47583b577fb062aeb89d3c45689a4f2646b7ebcb02e6cb2d5f6e2790afb91a18" default)))
 '(hlinum-activate t)
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PHP STUFF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.phtml$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HTML
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(require 'web-mode)
;;(require 'emmet-mode)
;;(defun my-web-mode-hook()
;;  "Hooks for the web mode."
;;  (setq web-mode-markup-indent-offset 2)
;;  (setq web-mode-css-indent-offset 2)
;;  (setq web-mode-code-indent-offset 2)
  
;;)
;;(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
;;(add-hook 'web-mode-hook 'emmet-mode)
;;(add-hook 'web-mode-hook 'my-web-mode-hook)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;JS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;JAVASCRIPT CONFIGURATION 
;; (http://truongtx.me/2014/02/23/set-up-javascript-development-environment-in-emacs/)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)
(setq js2-highlight-level 3)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(defun my-paredit-nonlisp ()
  "Turn on paredit mode for non-lisps."
  (interactive)
  (set (make-local-variable 'paredit-space-for-delimiter-predicates)
       '((lambda (endp delimiter) nil)))
  (paredit-mode 1))
(add-hook 'js2-mode-hook 'my-paredit-nonlisp) ;use with the above function
(add-hook 'js2-mode-hook
	  '(lambda()
	     (setq indent-tabs-mode t)
	     (setq tab-width 2)
	     (setq js-indent-level 2)))
;;npm install -g jshint
(require 'flycheck)
(add-hook 'js2-mode-hook
          (lambda () (flycheck-mode t)))
;;npm install -g js-beatufiy
(require 'web-beautify)
(require 'js2-refactor)
(js2r-add-keybindings-with-prefix "C-c C-m")
;;(define-key js2-mode-map "{" 'paredit-open-curly)
;;(define-key js2-mode-map "}" 'paredit-close-curly-and-newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SASS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'scss-mode "scss-mode" "Major mode for editing css code." t)
(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))

;; Compilation mode
;; Compilation ;;;;;;;;;;;;;;;;;;;;;;;;;
(setq compilation-scroll-output t)
;;(setq compilation-window-height nil)

(setq compilation-ask-about-save nil)
(setq compilation-save-buffers-predicate '(lambda () nil))

(defvar aj-compilation-saved-window-configuration nil
  "Previous window conf from before a compilation")

(defvar aj-compile-command ""
  "The compile command used by compilation-start since
  `compile-command' is only saved by `compile' command.")

;; Hide *compilation* buffer if compile didn't give erros
(defadvice compilation-start (before aj-compilation-save-window-configuration(command comint))
  "Save window configuration before compilation in
`aj-compilation-saved-window-configuration'"

  ;; compile command is not saved in compilation-start function only in
  ;; compile function (rgrep only uses compilation-start)
  (setq aj-compile-command command)
  ;; Save window configuration
  (setq aj-compilation-saved-window-configuration
        (current-window-configuration)))
(ad-activate 'compilation-start)

;; compilation-handle-exit returns (run-hook-with-args
;; 'compilation-finish-functions cur-buffer msg) Could use but it only
;; got a string describing status
(defadvice compilation-handle-exit
  (after aj-compilation-exit-function(process-status exit-status msg))
  "Hack to restore window conf"
  (let ((hide (string-match "find" aj-compile-command)))
    (when (and (eq process-status 'exit)
               (zerop exit-status)
               ;; Not nil and not 0 means that command was "find" at
               ;; pos 0 which means that I don't want to restore the
               ;; layout
               (not (and (integerp hide) (zerop hide))))
      (set-window-configuration aj-compilation-saved-window-configuration))))
(ad-activate 'compilation-handle-exit)

(provide 'aj-compilation)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOM STUFF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Functions
(defun open-dot-emacs() 
  "opening-dot-emacs"
  (interactive) ;; This makes the function a command too
  (find-file "~/.emacs")
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COLUMN WARNING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default fci-rule-column 80)
(setq fci-handle-truncate-lines nil)
(add-hook 'after-change-major-mode-hook 'auto-fci-mode)
(add-hook 'window-size-change-functions 'auto-fci-mode)
(defun auto-fci-mode (&optional unused)
  (if (> (frame-width) 80)
      (fci-mode 1)
    (fci-mode 0))
)
