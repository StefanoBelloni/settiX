;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EMACS - INIT.EL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; -*- Mode: Emacs-Lisp -*-

;;; This is a sample .emacs file.
;;;
;;; The .emacs file, which should reside in your home directory, allows you to
;;; customize the behavior of Emacs.  In general, changes to your .emacs file
;;; will not take effect until the next time you start up Emacs.  You can load
;;; it explicitly with `M-x load-file RET ~/.emacs RET'.
;;;
;;; There is a great deal of documentation on customization in the Emacs
;;; manual.  You can read this manual with the online Info browser: type
;;; `C-h i' or select "Emacs Info" from the "Help" menu.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      Basic Customization                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives
         '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

(when (not package-archive-contents)
    (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Open .h in C++ mode!
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(setq mode-line-format
	  (list
	   ;; value of `mode-name'
	   "%m: "
	   ;; value of current buffer name
	   "buffer %b, "
	   ;; value of current line number
	   "line %l "
	   "-- user: "
	   ;; value of user
	   (getenv "USER")
	   ;; viper mode
	   "  Viper mode:" 'viper-mode-string
	   )
	  )

;(diminish 'projectile-mode)

(setq backup-directory-alist `(("." . "~/.saves")))


(add-to-list 'load-path "~/.emacs.d/custom")


(require 'linum)
(global-linum-mode t)
;; use customized linum-format: add a addition space after the line number
;; (setq linum-format (lambda (line) (propertize (format (let ((w (length (number-to-string (count-lines (point-min) (point-max)))))) (concat "%" \
;; (number-to-string w) "d   ")) line) 'face 'linum))

(setq linum-format "%4d \u2502 ")
;; Required linum off
(use-package linum-off)
(require 'linum-off)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UNDO TREE
(use-package undo-tree)
(require 'undo-tree)
(global-undo-tree-mode)

(defun goto-match-paren (arg)
    "Go to the matching parenthesis if on parenthesis, otherwise insert %.
vi style of % jumping to matching brace."
    (interactive "p")
    (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	  ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	  (t (self-insert-command (or arg 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;      THEMES
;;;;;;;;;;;;;;;;;;;;;;;;;
;; Now you can load the theme with the interactive function load-theme like this:
;;M-x load-theme RET paganini

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; Load automatically a thema.
;;(load-theme 'aanila t)
(load-theme 'tango-dark t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TAGS 25.x.x issue M-* does not work  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key global-map (kbd "M-*") 'pop-tag-mark)

;;;;;;;;;;;;;;;;;;;;
;; RESIZE WINDOW
;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;;;;;;;;;;;;;;;;;;;;;;
;; WALK AROUND
;; walk around the windows with S-<Arrow>

(windmove-default-keybindings)

(add-to-list 'exec-path "/usr/local/bin/")

;;;;;;;;;;;;;;;;;;;;;
;; VIPER
;;;;;;;;;;;;;;;;;;;;

(setq viper-mode t)
(require 'viper)

;; COLOR according to state

(setq-default c-basic-offset 4
              tab-width 4
              indent-tabs-mode nil)
;;(setq-default c-basic-offset 3
;;              tab-width 3
;;              indent-tabs-mode nil)
;; (setq-default indent-tabs-mode t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (paganini)))
 '(custom-safe-themes
   (quote
    ("8448455d8b1563e27f8ec811ddf195bf92fa79dce7bdd46c6e228cb05bc74743" "5c64430cb8e12e2486cd9f74d4ce5172e00f8e633095d27edd212787a4225245" "f146cf0feba4fed38730de65e924e26140b470a4d503287e9ddcf7cca0b5b3f0" default)))
 '(flymake-cppcheck-checks (list "warning" "performance" "information" "style"))
 '(flymake-cppcheck-location (quote tempdir))
 '(package-selected-packages
   (quote
    (emms flymake-cppcheck flymake-easy flycheck magit undo-tree linum-off company-irony-c-headers flycheck-irony irony-eldoc company-irony irony company sr-speedbar ggtags use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; FONTS
;;;;;;;;;;;;;;;;;;;;;;;;;

;;(set-default-font "Monospace 8")
(set-frame-font "Monospace 8")
;;(set-face-attribute 'default nil :height 77)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  GGTAGS SET UP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ggtags)
(require 'ggtags)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 1))))

(define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
(define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
(define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
(define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
(define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
(define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)

(define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)

;; The Imenu facility offers a way to find the major definitions, such as function definitions, variable definitions in a file by name. ggtags can integrate Imenu:
(setq-local imenu-create-index-function #'ggtags-build-imenu-index)

;; sr-speedbar configuration
(setq speedbar-show-unknown-files t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  COMPANY-MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package company)
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IRONY-MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

;; (optional) adds CC special commands to `company-begin-commands' in order to
;; trigger completion at interesting places, such as after scope operator
;;     std::|
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  LIGHT / DARK TOOGLE COLOR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flymake cppcheck
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'after-init-hook #'global-flycheck-mode)
(use-package flymake-cppcheck)
(require 'flymake-cppcheck)
(add-hook 'c-mode-hook 'flymake-cppcheck-load)
(add-hook 'c++-mode-hook 'flymake-cppcheck-load)

(add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11")))


;; add clang executable
(setq-default flycheck-c/c++-clang-executable "/usr/bin/clang++-3.8")
;;(setq-default flycheck-c/c++-clang-executable "/usr/bin/clang")
;;(setq-default flycheck-c/c++-clang-executable "/usr/lib/llvm-3.8/bin/clang++")

;; (provide 'init)\n;;; init.el ends here

;; magit package 
(use-package magit)
(require 'magit)

;;** EMMS
 ;; Autoload the id3-browser and bind it to F7.
 ;; You can change this to your favorite EMMS interface.
 (autoload 'emms-smart-browse "emms-browser.el" "Browse with EMMS" t)
 (global-set-key [(f7)] 'emms-smart-browse)

 (with-eval-after-load 'emms
   (emms-standard) ;; or (emms-devel) if you want all features
   (setq emms-source-file-default-directory "~/music"
         emms-info-asynchronously t
         emms-show-format "♪ %s")

   ;; Might want to check `emms-info-functions',
   ;; `emms-info-libtag-program-name',
   ;; `emms-source-file-directory-tree-function'
   ;; as well.

   ;; Determine which player to use.
   ;; If you don't have strong preferences or don't have
   ;; exotic files from the past (wma) `emms-default-players`
   ;; is probably all you need.
   (if (executable-find "mpg123")
       (setq emms-player-list '(emms-player-mplayer))
     (emms-default-players))

   ;; For libre.fm see `emms-librefm-scrobbler-username' and
   ;; `emms-librefm-scrobbler-password'.
   ;; Future versions will use .authoinfo.gpg.
   )
