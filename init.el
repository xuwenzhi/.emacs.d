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
(setq linum-format "%3d| ")  ;set format


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

(require 'init-lisp)
(require 'init-elpa)

;; If you want to use latest version
(add-to-list 'package-archives '("melpa" . "http://elpa.emacs.org/packages"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/")) 
;;
;; ;; If you want to use last tagged version
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)


;;快捷键专区



(require 'php-mode)
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

(require 'init-web-mode)
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
