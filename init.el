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

;;Mail
(setq send-mail-function (quote smtpmail-send-it))
(setq smtpmail-smtp-server "smtp.163.com")
(setq smtpmail-smtp-service 25)
(setq user-full-name "xuwenzhi")
(setq user-mail-address "azxuwen701@163.com")

;; 浏览器


;; 需要在home下新建.authinfo文件，内容如下（都是你作为发件人的信息，你的邮箱地址，你的邮箱密码）：
;;   ;; machine smtp.qq.com login 邮箱地址 port 26 password 密码明文

;; If you want to use latest version
(add-to-list 'package-archives '("melpa" . "http://elpa.emacs.org/packages"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/")) 
;;
;; ;; If you want to use last tagged version
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)


;;快捷键专区


;;php
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
 '(send-mail-function (quote mailclient-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
