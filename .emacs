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


;; Global lines numbering

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(global-linum-mode t)
(require 'linum)
(global-linum-mode t)
;; use customized linum-format: add a addition space after the line number                                                                      
;; (setq linum-format (lambda (line) (propertize (format (let ((w (length (number-to-string (count-lines (point-min) (point-max)))))) (concat "%" \
;; (number-to-string w) "d   ")) line) 'face 'linum))

(setq linum-format "%4d \u2502 ")

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
(load-theme 'manoj-dark t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("63b7b8a45190b2e7362a975067bd76b55ae548c00e9088d12b4133eb0525a604" "5c64430cb8e12e2486cd9f74d4ce5172e00f8e633095d27edd212787a4225245" "f146cf0feba4fed38730de65e924e26140b470a4d503287e9ddcf7cca0b5b3f0" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TAGS 25.x.x issue M-* does not work  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key global-map "\M-*" 'pop-tag-mark)

;;;;;;;;;;;;;;;;;;;;;;
;; WALK AROUND
;; walk around the windows with S-<Arrow>

(windmove-default-keybindings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; M-f like in Vim
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun next-word (p)
   "Move point to the beginning of the next word, past any spaces"
   (interactive "d")
   (forward-word)
   (forward-word)
   (backward-word))
(global-set-key "\M-f" 'next-word)
