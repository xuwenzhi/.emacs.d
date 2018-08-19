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


(require 'init-lisp)
(require 'init-elpa)


(require 'package)

;; If you want to use latest version
;(add-to-list 'package-archives '("elpa" . "http://elpa.emacs-china.org/gnu/"))
(add-to-list 'package-archives
                          '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;(add-to-list 'package-archives '("marmalade" . "http://elpa.emacs-china.org/marmalade/")) 

;;
;; ;; If you want to use last tagged version
;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))

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

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(require 'el-get)
(el-get 'sync)
(package-initialize)

;;disable menu bar.
(menu-bar-mode -1)

(require 'switch-window)
(global-set-key (kbd "C-x o") 'switch-window)
(global-set-key (kbd "C-x 1") 'switch-window-then-maximize)
(global-set-key (kbd "C-x 0") 'switch-window-then-delete)
(setq switch-window-increase 6) ;Increase or decrease this number.
(setq switch-window-threshold 2)
(setq switch-window-auto-resize-window t)
(setq switch-window-default-window-size 0.55) ;80% of frame size

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

;;copy without cursor
(defun get-point (symbol &optional arg)
      "get the point"
      (funcall symbol arg)
      (point)
     )
     
     (defun copy-thing (begin-of-thing end-of-thing &optional arg)
       "copy thing between beg & end into kill ring"
        (save-excursion
          (let ((beg (get-point begin-of-thing 1))
         	 (end (get-point end-of-thing arg)))
            (copy-region-as-kill beg end)))
     )
     
     (defun paste-to-mark(&optional arg)
       "Paste things to mark, or to the prompt in shell-mode"
       (let ((pasteMe 
     	 (lambda()
     	   (if (string= "shell-mode" major-mode)
     	     (progn (comint-next-prompt 25535) (yank))
     	   (progn (goto-char (mark)) (yank) )))))
     	(if arg
     	    (if (= arg 1)
     		nil
     	      (funcall pasteMe))
     	  (funcall pasteMe))
     	))
(defun copy-word (&optional arg)
      "Copy words at point into kill-ring"
       (interactive "P")
       (copy-thing 'backward-word 'forward-word arg)
       ;;(paste-to-mark arg)
     )
(global-set-key (kbd "C-c w") (quote copy-word))
(defun copy-line (&optional arg)
      "Save current line into Kill-Ring without mark the line "
       (interactive "P")
       (copy-thing 'beginning-of-line 'end-of-line arg)
       (paste-to-mark arg)
     )
(global-set-key (kbd "C-c l") (quote copy-line))


;;php.
(require 'php-mode)
(php-mode)
(php-enable-psr2-coding-style)

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


;;Auto Complete
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(ac-set-trigger-key "TAB")
(setq ac-auto-start nil)
(set-face-background 'ac-candidate-face "lightgray")
(set-face-underline 'ac-candidate-face "darkgray")

;;flycheck
(package-install 'exec-path-from-shell)
(exec-path-from-shell-initialize)
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(flycheck-define-checker my-php
                           "A PHP syntax checker using the PHP command line interpreter.
                         
                         See URL `http://php.net/manual/en/features.commandline.php'."
                           :command ("php" "-l" "-d" "error_reporting=E_ALL" "-d" "display_errors=1"
                                                 "-d" "log_errors=0" source)
                             :error-patterns
                               ((error line-start (or "Parse" "Fatal" "syntax") " error" (any ":" ",") " "
                                                 (message) " in " (file-name) " on line " line line-end))
                                 :modes (php-mode php+-mode web-mode))
(flycheck-select-checker 'my-php)
(flycheck-mode t)
;;语法检测
(setq ispell-program-name "/usr/local/bin/aspell")

;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)
(require 'php-auto-yasnippets)
(payas/ac-setup)

;; 自动匹配括号和引号 
(require 'autopair)
(autopair-global-mode) ;; to enable in all buffers')

(require 'multiple-cursors)
(global-set-key (kbd "C-c m") 'mc/edit-lines)
(global-set-key (kbd "C-c f") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c b") 'mc/mark-previous-like-this)

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


(require 'init-company)

;;markdown
(autoload 'markdown-mode "markdown-mode.el"
             "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;; org-mode
;; The following lines are always needed. Choose your own keys.
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-hook 'org-mode-hook 'turn-on-font-lock) ; not needed when
                                        ; global-font-lock-mode is on
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-todo-keywords
      '((sequence "TODO" "DOING" "VERIFYING" "|" "DONE")))
(setq org-log-done 'time)
(setq org-log-done 'note)
;; child work
(defun org-summary-todo (n-done n-not-done)
      "Switch entry to DONE when all subentries are done, to TODO otherwise."
      (let (org-log-done org-log-states)   ; turn off logging
        (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
    
    (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
;; global todo list
(setq org-agenda-files (list "~/.todos/work.org"
                             "~/.todos/projects.org"
                             "~/.todos/home.org"
                             "~/.todos/records.org"))

(require 'org-alert)
;(setq alert-default-style 'libnotify)
(setq alert-default-style 'notifier)


;; 当前行高亮,其实是隐藏掉高亮
(require 'hl-line)
(or (facep 'my-hl-line-face) (make-face 'my-hl-line-face))
(setq hl-line-face 'my-hl-line-face)
(face-spec-set 'my-hl-line-face '((t (
                                      :background "DodgerBlue3"
                                                  :bold
                                                  :weight nil
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

(require 'projectile)

;; 默认全局使用
(projectile-global-mode)
;; 默认打开缓存
(setq projectile-enable-caching t)
;; helm-projectile 使用f5键打开默认文件搜索
(global-set-key [f5] 'helm-projectile)
;; Grep in a projectile
(global-set-key (kbd "C-c p s g") 'helm-projectile-grep)
;; projectile magit
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

(require 'helm-projectile)
(helm-projectile-on)

(require 'neotree)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(setq projectile-switch-project-action 'neotree-projectile-action)
(defun neotree-ffip-project-dir ()
      "Open NeoTree using the git root."
          (interactive)
              (let ((project-dir (ffip-project-root))
                              (file-name (buffer-file-name)))
                      (if project-dir
                                  (progn
                                                (neotree-dir project-dir)
                                                            (neotree-find file-name))
                                          (message "Could not find git project root."))))
  
(global-set-key [f8] 'neotree-toggle)
(define-key global-map (kbd "C-c C-p") 'neotree-ffip-project-dir)

(define-key global-map (kbd "C-x C-b") 'projectile-ibuffer)
(setq helm-buffers-list-cache (cdr (projectile-project-buffer-names)))
(global-set-key [f1] 'helm-projectile-switch-project)
(global-set-key [f2] 'helm-projectile-find-file)
(global-set-key [f3] 'helm-M-x)
(global-set-key [f4] 'projectile-find-tag)
(global-set-key [f5] 'helm-projectile-grep)
(global-set-key [f6] 'align-regexp)
(global-set-key [f7] 'revert-buffer)
(global-set-key [f8] 'goto-line)
(global-set-key (kbd "C-x b") 'helm-buffers-list)


;; eshell
(with-eval-after-load "esh-opt"
  (autoload 'epe-theme-lambda "eshell-prompt-extras")
  (setq eshell-highlight-prompt nil
                eshell-prompt-function 'epe-theme-lambda))

;; w3m
(add-to-list 'exec-path "/usr/local/bin/w3m")
(require 'w3m)
(setq w3m-use-favicon nil)
(setq w3m-command-arguments '("-cookie" "-F"))
(setq w3m-use-cookies t)


(require 'color-theme)
(color-theme-initialize)
;(add-hook 'after-init-hook 
;	  (lambda () (load-theme 'cyberpunk t)))
;(color-theme-euphoria)
;(color-theme-tty-dark)
;(color-theme-oswald)
(load-file "~/.emacs.d/elpa/cherry-blossom-theme-20150621.2042/cherry-blossom-theme.el")
(custom-set-faces
 '(magit-diff-added ((t (:background "black" :foreground "green3"))))
 '(magit-diff-removed ((t (:background "black" :foreground "red3")))))


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
