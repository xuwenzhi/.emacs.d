;; -*- coding: utf-8 -*-
;(defvar best-gc-cons-threshold gc-cons-threshold "Best default gc threshold value. Should't be too big.")
;(defvar best-gc-cons-threshold 4000000 "Best default gc threshold value. Should't be too big.")
;; don't GC during startup to save time
(setq gc-cons-threshold most-positive-fixnum)
(setq emacs-load-start-time (current-time))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

;; Turn off sound alarms completely
(setq ring-bell-function 'ignore)
;;
;; ;; disable welcome page
(setq inhibit-startup-message t)

;; 显示行号
(global-linum-mode 1) ; always show line numbers                              
(setq linum-format "%2d|")  ;set format

(display-time)
(setq calendar-week-start-day 1)


;;----------------------------------------------------------------------------
;; Which functionality to enable (use t or nil for true and false)
;;----------------------------------------------------------------------------
(setq *is-a-mac* (eq system-type 'darwin))
(setq *win64* (eq system-type 'windows-nt) )
(setq *cygwin* (eq system-type 'cygwin) )
(setq *linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)) )
(setq *unix* (or *linux* (eq system-type 'usg-unix-v) (eq system-type 'berkeley-unix)) )
(setq *emacs24* (and (not (featurep 'xemacs)) (or (>= emacs-major-version 24))) )
(setq *no-memory* (cond
                   (*is-a-mac*
                    (< (string-to-number (nth 1 (split-string (shell-command-to-string "sysctl hw.physmem")))) 4000000000))
                   (*linux* nil)
                   (t nil)))


(setq emacs-load-start-time (current-time))

;;为了org-mode
(setq ispell-program-name "/usr/bin/aspell")

(require 'init-lisp)
(require 'init-elpa)

;; If you want to use latest version
(add-to-list 'package-archives '("melpa" . "http://elpa.emacs-china.org/melpa/"))
(add-to-list 'package-archives '("marmalade" . "http://elpa.emacs-china.org/marmalade/")) 
;;
;; ;; If you want to use last tagged version
(add-to-list 'package-archives '("melpa-stable" . "http://elpa.emacs-china.org/melpa-stable/"))

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.

Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     ;; (package-installed-p 'evil)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages))

;; make sure to have downloaded archive description.
;; Or use package-archive-contents as suggested by Nicolas Dudebout
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(ensure-package-installed 'iedit 'magit) ;  --> (nil nil) if iedit and magit are already installed

;; activate installed packages
(package-initialize)

;; ibuffer做一个漂亮的buffer列表界面
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;switch-window
(require 'switch-window)
(global-set-key (kbd "C-x o") 'switch-window)
(global-set-key (kbd "C-x 1") 'switch-window-then-maximize)
(global-set-key (kbd "C-x 0") 'switch-window-then-delete)
(setq switch-window-increase 6) ;Increase or decrease this number.
(setq switch-window-threshold 2)
(setq switch-window-auto-resize-window t)
(setq switch-window-default-window-size 0.8) ;80% of frame size

;;ace-jump
(autoload
    'ace-jump-mode
      "ace-jump-mode" t)
(eval-after-load "ace-jump-mode"
                   '(ace-jump-mode-enable-mark-sync))

(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

;; 表格库
(autoload 'table-insert "table" "WYGIWYS table editor")

(require 'color-theme)
(color-theme-initialize)
;(color-theme-euphoria)
;(color-theme-tty-dark)
;(color-theme-oswald)
(load-file "~/.emacs.d/elpa/cherry-blossom-theme-20150621.2042/cherry-blossom-theme.el")

;;复制粘贴
(defun copy-from-osx () 
  (shell-command-to-string "pbpaste")) 
(defun paste-to-osx (text &optional push) 
  (let ((process-connection-type nil)) 
    (let ((proc (start-process"pbcopy" "*Messages*" "pbcopy"))) 
      (process-send-string proc text) 
      (process-send-eof proc)))) 
(setq interprogram-cut-function 'paste-to-osx) 
(setq interprogram-paste-function 'copy-from-osx)

(global-set-key (kbd "C-t") 'goto-line)

;;php.
(require 'php-mode)
(php-mode)

(require 'web-mode)
(defun bs-web-mode-hook ()
    (local-set-key '[backtab] 'indent-relative)
      (setq indent-tabs-mode nil)
        (setq web-mode-markup-indent-offset 2
                      web-mode-css-indent-offset 2
                              web-mode-code-indent-offset 2))

(add-hook 'web-mode-hook 'bs-web-mode-hook)

;;javascript
(require 'init-javascript)


(load "init-gtags")
;(require 'auto-complete)
;;自动补全界面
(require 'auto-complete-config)

;;设定默认补全库来源——自动设定，而不是每次更新都要重新写
(setq auto-complete-directory
(if load-file-name
(file-name-directory load-file-name)
(expand-file-name "~/.emacs.d/elpa/auto-complete")))

(add-to-list 'ac-dictionary-directories (concat auto-complete-directory
"/dict"))

(ac-config-default)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
(setq web-mode-engines-alist
      '(("php"    . "\\.phtml\\'")
        ("blade"  . "\\.blade\\."))
      )
(setq web-mode-ac-sources-alist
      '(("php" . (ac-source-yasnippet ac-source-php-auto-yasnippets))
        ("html" . (ac-source-emmet-html-aliases ac-source-emmet-html-snippets))
        ("css" . (ac-source-css-property ac-source-emmet-css-snippets))))

(add-hook 'web-mode-before-auto-complete-hooks
          '(lambda ()
             (let ((web-mode-cur-language
                    (web-mode-language-at-pos)))
               (if (string= web-mode-cur-language "php")
                   (yas-activate-extra-mode 'php-mode)
                 (yas-deactivate-extra-mode 'php-mode))
               (if (string= web-mode-cur-language "css")
                   (setq emmet-use-css-transform t)
                                  (setq emmet-use-css-transform nil)))))


(require 'init-company)

;; 当前行高亮,其实是隐藏掉高亮
(require 'hl-line)
(or (facep 'my-hl-line-face) (make-face 'my-hl-line-face))
(setq hl-line-face 'my-hl-line-face)
(face-spec-set 'my-hl-line-face '((t (
                                      :background "DodgerBlue3"
                                                  ;;:bold
                                                  ;;:weight nil
                                      :inverse-video nil
                                      ))))
(defun wcy-color-theme-adjust-hl-mode-face()
  "interactive"
  (let* ((color  (x-color-values (face-attribute 'default :background))))
    (if (null color)
        (let ((my-color (mapcar
                       (lambda (x)
                         (let ((y (/ #XFFFF 4))
                               (delta #X18FF))
                           (cond
                            ((< x (* y 1))
                             (+ x delta))
                            ((< x (* y 2))
                             (+ x delta))
                            ((< x (* y 3))
                             (- x delta))
                            (t
                             (- x delta)))))
                       color)))
        (message "%S %S" color my-color)
        (set-face-attribute
         'my-hl-line-face nil
         :background
         (concat "#"
                 (mapconcat
                  (lambda (c) (format "%04X" c))
                  my-color
                  "")))))))
(wcy-color-theme-adjust-hl-mode-face)

;;ruby mode
(autoload 'ruby-mode "ruby-mode" "Major mode for ruby files" t)
   (add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
   (add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(custom-safe-themes
   (quote
    ("bdc90f305ecd4008fd39174adebfcdaf729e38aac1222a872b1f054d97adbc3d" "cc60d17db31a53adf93ec6fad5a9cfff6e177664994a52346f81f62840fe8e23" default)))
 '(fringe-mode 6)
 '(linum-format (quote dynamic))
 '(send-mail-function (quote mailclient-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
