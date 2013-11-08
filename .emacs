(require 'package)
(package-initialize)
(add-to-list 'package-archives
         '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
         '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives
         '("marmalade" . "http://marmalade-repo.org/packages/") t)

(package-refresh-contents)

(defun install-if-needed (package)
  (unless (package-installed-p package)
    (package-install package)))

;; make more packages available with the package installer
(setq to-install
      '(python-mode magit yasnippet jedi circe nyan-mode sublime-themes auto-complete autopair find-file-in-repository))

(mapc 'install-if-needed to-install)

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
 

;; make more packages available with the package installer

(require 'magit)
(global-set-key "\C-xg" 'magit-status)

(require 'auto-complete)
(require 'autopair)
(require 'flymake)
(require 'yasnippet)

(global-set-key [f7] 'find-file-in-repository)

; auto-complete mode extra settings
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

;; ;; Jedi settings
(require 'jedi)
;; It's also required to run "pip install --user jedi" and "pip
;; install --user epc" to get the Python side of the library work
;; correctly.
;; With the same interpreter you're using.

;; if you need to change your python intepreter, if you want to change it
;; (setq jedi:server-command
;; '("python2" "/home/andrea/.emacs.d/elpa/jedi-0.1.2/jediepcserver.py"))

(add-hook 'python-mode-hook
         (lambda ()
         (jedi:setup)
         (jedi:ac-setup)
            (local-set-key "\C-cd" 'jedi:show-doc)
            (local-set-key (kbd "M-SPC") 'jedi:complete)
            (local-set-key (kbd "M-.") 'jedi:goto-definition)))
;; Jedi Settings
(require 'jedi)

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

(ido-mode t)

;; -------------------- extra nice things --------------------
;; use shift to move around windows
(windmove-default-keybindings 'shift)
(show-paren-mode t)
 ; Turn beep off
(setq visible-bell nil)
(setq column-number-mode t)
(setq make-backup-files nil);;eliminar backup automático
(setq tool-bar-mode nil)
(global-linum-mode t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (fogus)))
 '(custom-safe-themes (quote ("d293542c9d4be8a9e9ec8afd6938c7304ac3d0d39110344908706614ed5861c9" "61d1a82d5eaafffbdd3cab1ac843da873304d1f05f66ab5a981f833a3aec3fc0" default)))
 '(linum-format " %7i "))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
