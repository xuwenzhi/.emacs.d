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

;;Fun
;(require 'zone)
;(zone-when-idle 600)


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
(require 'use-package)

;; If you want to use latest version
;(add-to-list 'package-archives '("elpa" . "http://elpa.emacs-china.org/gnu/"))

;(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;(add-to-list 'package-archives
;                          '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;;(add-to-list 'package-archives '("marmalade" . "http://elpa.emacs-china.org/marmalade/"))
;;
;; ;; If you want to use last tagged version
(add-to-list 'package-archives '("melpa" . "http://elpa.emacs-china.org/melpa/"))
;(add-to-list 'package-archives '("melpa" . "http://melpa.org"))

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

(global-wakatime-mode 1)

(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)
;; remove trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; remove begine whitespace
(global-set-key (kbd "C-x w d") 'delete-whitespace-rectangle)

(require 'switch-window)
(global-set-key (kbd "C-x o") 'switch-window)
(global-set-key (kbd "C-x 1") 'switch-window-then-maximize)
(global-set-key (kbd "C-x 0") 'switch-window-then-delete)
(setq switch-window-increase 6) ;Increase or decrease this number.
(setq switch-window-threshold 2)
(setq switch-window-auto-resize-window t)
(setq switch-window-default-window-size 0.55) ;80% of frame size
(setq switch-window-input-style 'minibuffer)
(setq switch-window-auto-resize-window
      (lambda ()
	(equal (buffer-name) "*scratch*"))) ;when return t, run auto switch
(setq switch-window-default-window-size '(0.8 . 0.6)) ;80% width and 60% height of frame
(setq switch-window-minibuffer-shortcut ?z)

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

(require 'helm-gtags)
(helm-gtags-mode t)
(setq helm-gtags-ignore-case t
      helm-gtags-auto-update t
      helm-gtags-use-input-at-cursor t
      helm-gtags-pulse-at-cursor t
      helm-gtags-prefix-key "\C-cg"
      helm-gtags-suggested-key-mapping t)
(global-set-key (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
(global-set-key (kbd "C-c g s") 'helm-gtags-select)
(global-set-key (kbd "M-."    ) 'helm-gtags-dwim)
(global-set-key (kbd "M-,"    ) 'helm-gtags-pop-stack)
(global-set-key (kbd "C-c g p") 'helm-gtags-previous-history)
(global-set-key (kbd "C-c g n") 'helm-gtags-next-history)

;;Auto Complete
(use-package auto-complete
  :ensure t
  :init
  (progn
    (ac-config-default)
    (global-auto-complete-mode t)
    ))
(defun semantic-and-gtags-complete ()
  (interactive)
  (auto-complete '(ac-source-semantic ac-source-gtags)))
(setq ac-auto-start t)
(set-face-background 'ac-candidate-face "pink")
(set-face-underline 'ac-candidate-face "black")

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
;(require 'yasnippet)
;(yas-global-mode 1)
;(yas-reload-all)
;(add-hook 'prog-mode-hook #'yas-minor-mode)
;(require 'php-auto-yasnippets)
;(payas/ac-setup)

;; 自动匹配括号和引号
(require 'autopair)
(autopair-global-mode) ;; to enable in all buffers')

(require 'multiple-cursors)
(global-set-key (kbd "C-c m s") 'mc/edit-lines)
(global-set-key (kbd "C-c m n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c m p") 'mc/mark-previous-like-this)

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
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(require 'org-alert)
;(setq alert-default-style 'libnotify)
(setq alert-default-style 'notifier)

;; highlight
(require 'highlight-symbol)
(global-set-key (kbd "C-c h n") 'highlight-symbol-next)
(global-set-key (kbd "C-c h p") 'highlight-symbol-prev)

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
;; Grep in a projectile
(global-set-key (kbd "C-c p s g") 'helm-projectile-grep)
;; projectile magit
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

(require 'helm)
(require 'helm-config)
(require 'helm-projectile)
(helm-projectile-on)

(require 'helm-config)
(require 'helm-ack)

(custom-set-variables
 ;; Does not insert '--type' option
 '(helm-ack-auto-set-filetype nil)
 ;; Insert "thing-at-point 'symbol" as search pattern
 '(helm-ack-thing-at-point 'symbol))

(require 'neotree)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(setq projectile-switch-project-action 'neotree-projectile-action)
;; 选中一个文件，关闭neotree
(defun neo-open-file-hide (full-path &optional arg)
    "Open a file node and hides tree."
      (neo-global--select-mru-window arg)
        (find-file full-path)
          (neotree-hide))

(defun neotree-enter-hide (&optional arg)
    "Enters file and hides neotree directly"
      (interactive "P")
        (neo-buffer--execute arg 'neo-open-file-hide 'neo-open-dir))
(add-hook
   'neotree-mode-hook
    (lambda ()
         (define-key neotree-mode-map (kbd "RET") 'neotree-enter-hide)))

;; kill not current project buffer.
(defun kill-other-buffers ()
    "Kill all buffers but current buffer and special buffers"
      (interactive)
        (dolist (buffer (delq (current-buffer) (buffer-list)))
              (let ((name (buffer-name buffer)))
                      (when (and name (not (string-equal name ""))
                                              (/= (aref name 0) ?\s)
                                                           (string-match "^[^\*]" name))
                                (funcall 'kill-buffer buffer)))))
(define-key global-map (kbd "C-c k") 'kill-other-buffers)


(define-key global-map (kbd "C-x C-b") 'projectile-ibuffer)
(setq helm-buffers-list-cache (cdr (projectile-project-buffer-names)))
(global-set-key [f1] 'helm-projectile-switch-project)
(global-set-key [f2] 'helm-projectile-find-file)
(global-set-key [f3] 'helm-projectile-grep)
(global-set-key [f4] 'projectile-find-tag)
(global-set-key [f5] 'helm-projectile-switch-to-buffer)
(global-set-key [f6] 'align-regexp)
(global-set-key [f7] 'revert-buffer)
(global-set-key [f8] 'goto-line)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "M-x") 'helm-M-x)

;;rest client 
(require 'restclient)
(require 'restclient-helm)
(add-to-list 'auto-mode-alist '("\\.rc\\'" . restclient-mode))

;; eshell
(with-eval-after-load "esh-opt"
  (autoload 'epe-theme-lambda "eshell-prompt-extras")
  (setq eshell-highlight-prompt nil
                eshell-prompt-function 'epe-theme-lambda))

(require 'init-eshell)

(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 5))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "* Eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))

(global-set-key (kbd "C-!") 'eshell-here)
(defun eshell/x ()
  (insert "exit")
  (eshell-send-input)
  ;(delete-window)
  (switch-window-then-delete))


;; w3m
(require 'w3m)
(add-to-list 'exec-path "/usr/local/bin/w3m")
(setq w3m-use-favicon nil)
(setq w3m-command-arguments '("-cookie" "-F"))
(setq w3m-use-cookies t)
(setq w3m-home-page "https://www.google.com")


(require 'color-theme)
(color-theme-initialize)
;(add-hook 'after-init-hook
;	  (lambda () (load-theme 'cyberpunk t)))
;(color-theme-euphoria)
;(color-theme-tty-dark)
;(color-theme-oswald)
;(load-file "~/.emacs.d/elpa/cherry-blossom-theme-20150621.2042/cherry-blossom-theme.el")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-diff-added ((t (:background "black" :foreground "green3"))))
 '(magit-diff-removed ((t (:background "black" :foreground "red3")))))

(load-theme 'grandshell t)

;; feed
(global-set-key (kbd "C-x w") 'elfeed)
(setq elfeed-feeds
      '("https://www.zhihu.com/rss"
	"http://www.read.org.cn/feed"
	"http://news.163.com/special/00011K6L/rss_newsattitude.xml"
	"https://36kr.com/feed"
	"http://www.ifanr.com/feed"
	"http://www.write.org.cn/feed"
	"http://blog.sina.com.cn/rss/1286528122.xml"
	"http://feeds.feedburner.com/ruanyifeng"
        "http://planet.emacsen.org/atom.xml"))

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
    ("3860a842e0bf585df9e5785e06d600a86e8b605e5cc0b74320dfe667bcbe816c" "f27c3fcfb19bf38892bc6e72d0046af7a1ded81f54435f9d4d09b3bff9c52fc1" "bdc90f305ecd4008fd39174adebfcdaf729e38aac1222a872b1f054d97adbc3d" "cc60d17db31a53adf93ec6fad5a9cfff6e177664994a52346f81f62840fe8e23" default)))
 '(fringe-mode 6 nil (fringe))
 '(linum-format (quote dynamic))
 '(send-mail-function (quote mailclient-send-it))
 '(wakatime-cli-path "/usr/local/bin/wakatime")
 '(wakatime-python-bin nil))
