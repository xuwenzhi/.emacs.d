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

;; Show line numbers (linum was removed in Emacs 29; use built-in display-line-numbers)
(global-display-line-numbers-mode 1) ; always show line numbers
(setq display-line-numbers-width 3)  ;set min width

;; all backups goto ~/.backups instead in the current directory
(setq backup-directory-alist (quote (("." . "~/.backups"))))

(display-time)
(setq calendar-week-start-day 1)

(defvar efs/default-font-size 180)
(defvar efs/default-variable-font-size 180)

;; Make frame transparency overridable
(defvar efs/frame-transparency '(90 . 90))

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(set-face-attribute 'default nil :font "Fira Code Retina" :height efs/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height efs/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height efs/default-variable-font-size :weight 'regular)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


;; Known issues Failed to download ‘MELPA’ archive
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

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


;; Initialize package sources
(require 'package)


(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                                                  ("org" . "https://orgmode.org/elpa/")
                                                                           ("elpa" . "https://elpa.gnu.org/packages/")))

;(package-initialize)
(unless package-archive-contents
    (package-refresh-contents))

  ;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
    (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package auto-package-update
               :custom
                 (auto-package-update-interval 7)
                   (auto-package-update-prompt-before-update t)
                     (auto-package-update-hide-results t)
                       :config
                         (auto-package-update-maybe)
                           (auto-package-update-at-time "09:00"))

;; activate installed packages
;(package-initialize)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(require 'el-get)
(el-get 'sync)

;;disable menu bar.
(menu-bar-mode -1)

;;(global-wakatime-mode 1)

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

;; Move between windows with M-<arrow> (overrides M-left/M-right word motion)
(windmove-default-keybindings 'meta)

;;rust
;(require 'rust-mode)
;(setq rust-format-on-save t) ;; for rust-format
;(add-hook 'rust-mode-hook 'cargo-minor-mode) ;; for cargo
;(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;;ace-jump
;(autoload
;    'ace-jump-mode
;      "ace-jump-mode" t)
;(eval-after-load "ace-jump-mode"
;                   '(ace-jump-mode-enable-mark-sync))
;(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
;(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

;; Table library
;(autoload 'table-insert "table" "WYGIWYS table editor")


;; Copy & paste (macOS clipboard integration)
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


;; C++
(require 'cpputils-cmake)

;; C/C++ LSP via built-in eglot + clangd (Apple clangd from Xcode CLT).
;; Gives API/function completion (through company), signature hints (eldoc),
;; live diagnostics (flymake), and go-to-definition (xref).
(add-hook 'c-mode-hook    #'eglot-ensure)
(add-hook 'c++-mode-hook  #'eglot-ensure)
(add-hook 'c-ts-mode-hook #'eglot-ensure)
(add-hook 'c++-ts-mode-hook #'eglot-ensure)

;; Auto-format C/C++ on save via clangd (uses a .clang-format file if present,
;; otherwise LLVM style). No-op unless eglot is connected.
(defun my/c-format-buffer ()
  (when (and (bound-and-true-p eglot--managed-mode) (eglot-current-server))
    (ignore-errors (eglot-format-buffer))))
(dolist (h '(c-mode-hook c++-mode-hook c-ts-mode-hook c++-ts-mode-hook))
  (add-hook h (lambda () (add-hook 'before-save-hook #'my/c-format-buffer nil t))))

;; In LSP (eglot) buffers, use xref/clangd for jump-to-def instead of helm-gtags.
;; Scoped to eglot-mode-map, so helm-gtags' M-. still applies everywhere else.
(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "M-.") #'xref-find-definitions)
  (define-key eglot-mode-map (kbd "M-,") #'xref-go-back))

;;php.
(require 'flymake-proc) ; php-mode references flymake-proc-allowed-file-name-masks at load time
(require 'php-mode)


;(require 'web-mode)
;(defun bs-web-mode-hook ()
;    (local-set-key '[backtab] 'indent-relative)
;      (setq indent-tabs-mode nil)
;        (setq web-mode-markup-indent-offset 2
;                      web-mode-css-indent-offset 2
;                              web-mode-code-indent-offset 2))

;(add-hook 'web-mode-hook 'bs-web-mode-hook)

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

;;Auto Complete -- DISABLED.
;; auto-complete is unmaintained and crashes on Emacs 30 (the obarray type change
;; makes `ac-show-menu' fail with "wrong-type-argument sequencep #<obarray>"),
;; which broke opening the Claude Code vterm buffer. `company' (see
;; lisp/init-company.el, global-company-mode) is the maintained replacement.
;; (use-package auto-complete
;;   :ensure t
;;   :init
;;   (progn
;;     (ac-config-default)
;;     (global-auto-complete-mode t)))
;; (defun semantic-and-gtags-complete ()
;;   (interactive)
;;   (auto-complete '(ac-source-semantic ac-source-gtags)))
;; (setq ac-auto-start t)
;; (set-face-background 'ac-candidate-face "pink")
;; (set-face-underline 'ac-candidate-face "black")

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
;; my-php is registered above; global-flycheck-mode auto-selects it in PHP buffers.
;; (flycheck-select-checker / flycheck-mode must run in a real buffer, not at load time)
;; Syntax checking
(setq ispell-program-name (or (executable-find "aspell") "/opt/homebrew/bin/aspell"))

;; Auto-pair brackets and quotes
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
(add-to-list 'auto-mode-alist '("\\.twig\\'" . web-mode))
(setq web-mode-engines-alist
      '(("php"    . "\\.phtml\\'")
        ("blade"  . "\\.blade\\."))
      )


(require 'init-company)

;;markdown
(autoload 'markdown-mode "markdown-mode.el"
             "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;; org-mode

(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :pin org
  :commands (org-capture org-agenda)
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-agenda-files
        '("~/Projects/Code/emacs-from-scratch/OrgFiles/Tasks.org"
          "~/Projects/Code/emacs-from-scratch/OrgFiles/Habits.org"
          "~/Projects/Code/emacs-from-scratch/OrgFiles/Birthdays.org"))

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  (setq org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
      (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  (setq org-refile-targets
    '(("Archive.org" :maxlevel . 1)
      ("Tasks.org" :maxlevel . 1)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-tag-alist
    '((:startgroup)
       ; Put mutually exclusive tags here
       (:endgroup)
       ("@errand" . ?E)
       ("@home" . ?H)
       ("@work" . ?W)
       ("agenda" . ?a)
       ("planning" . ?p)
       ("publish" . ?P)
       ("batch" . ?b)
       ("note" . ?n)
       ("idea" . ?i)))

  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
   '(("d" "Dashboard"
     ((agenda "" ((org-deadline-warning-days 7)))
      (todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))
      (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

    ("n" "Next Tasks"
     ((todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))))

    ("W" "Work Tasks" tags-todo "+work-email")

    ;; Low-effort next actions
    ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
     ((org-agenda-overriding-header "Low Effort Tasks")
      (org-agenda-max-todos 20)
      (org-agenda-files org-agenda-files)))

    ("w" "Workflow Status"
     ((todo "WAIT"
            ((org-agenda-overriding-header "Waiting on External")
             (org-agenda-files org-agenda-files)))
      (todo "REVIEW"
            ((org-agenda-overriding-header "In Review")
             (org-agenda-files org-agenda-files)))
      (todo "PLAN"
            ((org-agenda-overriding-header "In Planning")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "BACKLOG"
            ((org-agenda-overriding-header "Project Backlog")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "READY"
            ((org-agenda-overriding-header "Ready for Work")
             (org-agenda-files org-agenda-files)))
      (todo "ACTIVE"
            ((org-agenda-overriding-header "Active Projects")
             (org-agenda-files org-agenda-files)))
      (todo "COMPLETED"
            ((org-agenda-overriding-header "Completed Projects")
             (org-agenda-files org-agenda-files)))
      (todo "CANC"
            ((org-agenda-overriding-header "Cancelled Projects")
             (org-agenda-files org-agenda-files)))))))

  (setq org-capture-templates
    `(("t" "Tasks / Projects")
      ("tt" "Task" entry (file+olp "~/.todo/task.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

      ("j" "Journal Entries")
      ("jj" "Journal" entry
           (file+olp+datetree "~/.todo/Journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :clock-in :clock-resume
           :empty-lines 1)
      ("jm" "Meeting" entry
           (file+olp+datetree "~/.todo/Journal.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)

      ("w" "Workflows")
      ("we" "Checking Email" entry (file+olp+datetree "~/.todo/Journal.org")
           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

      ("m" "Metrics Capture")
      ("mw" "Weight" table-line (file+headline "~/.todo/Metrics.org" "Weight")
       "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

  (define-key global-map (kbd "C-c j")
    (lambda () (interactive) (org-capture nil "jj")))

  (efs/org-font-setup))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

(with-eval-after-load 'org
  (org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t)
      (python . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes))

(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))

;; Automatically tangle our Emacs.org config file when we save it
(defun efs/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))



;; emojify
(add-hook 'after-init-hook #'global-emojify-mode)

;; highlight
(require 'highlight-symbol)
(global-set-key (kbd "C-c h n") 'highlight-symbol-next)
(global-set-key (kbd "C-c h p") 'highlight-symbol-prev)

;; Current-line highlight (configured here to keep it subtle/hidden)
(require 'hl-line)
(or (facep 'my-hl-line-face) (make-face 'my-hl-line-face))
(setq hl-line-face 'my-hl-line-face)
;; No background on the current-line highlight (keeps iTerm2 transparency).
(face-spec-set 'my-hl-line-face '((t (:background unspecified :inverse-video nil))))

(require 'projectile)

;; Enable globally by default
(projectile-global-mode)
;; Enable caching by default
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
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-ack-auto-set-filetype nil)
 '(helm-ack-thing-at-point 'symbol)
 '(package-selected-packages nil))

(require 'neotree)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(setq projectile-switch-project-action 'neotree-projectile-action)
;; Select a file, then close neotree
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
;; Removed: (setq helm-buffers-list-cache (cdr (projectile-project-buffer-names)))
;; It ran at startup outside any project, forcing a "Switch to project:" prompt on every launch.
(global-set-key [f1] 'goto-line)
(global-set-key [f2] 'helm-projectile-switch-project)
(global-set-key [f3] 'helm-projectile-find-file)
(global-set-key [f4] 'helm-projectile-grep)
(global-set-key [f5] 'projectile-find-tag)
(global-set-key [f6] 'helm-projectile-switch-to-buffer)
(global-set-key [f7] 'align-regexp)
(global-set-key [f8] 'revert-buffer)
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

;; w3m
(require 'w3m)
(setq w3m-command (or (executable-find "w3m") "/opt/homebrew/bin/w3m"))
(setq w3m-use-favicon nil)
(setq w3m-command-arguments '("-cookie" "-F"))
(setq w3m-use-cookies t)
(setq w3m-home-page "https://www.google.com")


;(require 'color-theme)
;(color-theme-initialize)
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

;; Let iTerm2 transparency show through: clear the theme's opaque default
;; background on terminal frames. Must run late (window-setup-hook) because
;; Emacs re-applies frame settings after init; after-make-frame-functions
;; covers later emacsclient -nw frames.
(defun my/terminal-transparency (&optional frame)
  (unless (display-graphic-p frame)
    (set-face-background 'default "unspecified-bg" frame)))
(add-hook 'after-make-frame-functions #'my/terminal-transparency)
(add-hook 'window-setup-hook #'my/terminal-transparency)

(use-package all-the-icons)

;; Mode line
(use-package nerd-icons :ensure t)
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 25)
  (doom-modeline-bar-width 3)
  (doom-modeline-icon nil)) ; text-only: works in -nw without a Nerd Font in iTerm2

(use-package term
               :commands term
                 :config
                   (setq explicit-shell-file-name "bash") ;; Change this to zsh, etc
                     ;;(setq explicit-zsh-args '())         ;; Use 'explicit-<shell>-args for shell-specific args

                       ;; Match the default Bash shell prompt.  Update this if you have a custom prompt
                         (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

;(use-package eterm-256color
;               :hook (term-mode . eterm-256color-mode))


(defun efs/configure-eshell ()
    ;; Save command history when commands are entered
      (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

        ;; Truncate buffer for performance
          (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

            ;; Bind some useful keys for evil-mode
              (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
                (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
                  (evil-normalize-keymaps)

                    (setq eshell-history-size         10000
                                  eshell-buffer-maximum-lines 10000
                                          eshell-hist-ignoredups t
                                                  eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt
               :after eshell)

(use-package eshell
               :hook (eshell-first-time-mode . efs/configure-eshell)
                 :config

                   (with-eval-after-load 'esh-opt
                                             (setq eshell-destroy-buffer-when-process-dies t)
                                                 (setq eshell-visual-commands '("htop" "zsh" "vim")))

                     (eshell-git-prompt-use-theme 'powerline))



(use-package all-the-icons-dired
               :hook (dired-mode . all-the-icons-dired-mode))


;;----------------------------------------------------------------------------
;; Claude Code (claude-code.el, vterm backend)
;;   Start a session: M-x claude-code-run   |   Menu: C-c c
;;----------------------------------------------------------------------------
(setq vterm-always-compile-module t) ; rebuild vterm module silently after updates
;; Lighten vterm's dim "bright black" color (used for Claude Code's input hint).
(with-eval-after-load 'vterm
  (set-face-foreground 'vterm-color-bright-black "gray65"))

;; vterm is a terminal: turn off line numbers / hl-line. The line-number margin
;; changes vterm's reported width mid-render (see vterm--get-margin-width), which
;; garbles the Claude Code TUI on startup.
(add-hook 'vterm-mode-hook
          (lambda ()
            (display-line-numbers-mode -1)
            (setq-local global-hl-line-mode nil)
            (when (bound-and-true-p hl-line-mode) (hl-line-mode -1))
            (setq-local truncate-lines t)))

;; Force a clean redraw shortly after the Claude session opens, so the TUI
;; repaints at the final side-window width (same effect as resizing manually).
(defun my/claude-code-redraw (&rest _)
  (run-at-time
   0.5 nil
   (lambda ()
     (let* ((buf (ignore-errors (get-buffer (claude-code-buffer-name))))
            (win (and buf (get-buffer-window buf t))))
       (when (and (window-live-p win)
                  (buffer-local-value 'vterm--process buf)
                  (process-live-p (buffer-local-value 'vterm--process buf)))
         (with-current-buffer buf
           (vterm--window-adjust-process-window-size vterm--process (list win))))))))
(advice-add 'claude-code-run :after #'my/claude-code-redraw)

;; Manual redraw command, bound to C-c r inside the Claude Code buffer.
;; Nudges the window width to force the TUI to repaint (like a manual resize).
(defun my/claude-code-redraw-buffer ()
  "Force the Claude Code TUI to repaint (fixes occasional garbled rendering)."
  (interactive)
  (let ((win (get-buffer-window (current-buffer))))
    (when (window-live-p win)
      (ignore-errors
        (window-resize win -1 t)
        (window-resize win 1 t))
      (when (and (boundp 'vterm--process) (process-live-p vterm--process))
        (vterm--window-adjust-process-window-size vterm--process (list win)))))
  (redraw-display))
(with-eval-after-load 'claude-code-ui
  (define-key claude-code-vterm-mode-map (kbd "C-c r") #'my/claude-code-redraw-buffer))

(use-package claude-code
  :ensure t
  :custom
  ;; Always launch with permission checks bypassed
  (claude-code-executable "claude --dangerously-skip-permissions")
  :init
  (defun my/claude-code-send-region-with-context (context)
    "Send the active region to Claude Code, prefixed with additional CONTEXT."
    (interactive
     (list (if (use-region-p)
               (read-string "Additional context: ")
             (user-error "No region selected"))))
    (require 'claude-code)
    (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
      (claude-code-send-string (concat context "\n\n" text))))
  ;; Friendlier command names: `claude-code' opens a session, `claude-code-kill'
  ;; quits it and kills the buffer.
  (defalias 'claude-code #'claude-code-run)
  (defalias 'claude-code-kill #'claude-code-quit)
  (defun claude-code-resume ()
    "Start Claude Code and resume a previous conversation (session picker)."
    (interactive)
    (require 'claude-code)
    (let ((claude-code-executable (concat claude-code-executable " --resume")))
      (call-interactively #'claude-code-run)))
  ;; claude-code-run autoloads from claude-code-core, which does NOT load the UI
  ;; module that defines `claude-code-vterm-mode'.  Pull in the full feature once
  ;; core loads so all sub-modules are available.
  (with-eval-after-load 'claude-code-core (require 'claude-code))
  ;; :bind* (override map) so these win over major-mode C-c C-c bindings
  ;; (e.g. php-mode/CC-mode bind C-c C-c to comment-region, which otherwise
  ;; shadows the 3-key sequence and makes the final `c` self-insert).
  :bind* (("C-c C-c c" . claude-code)                             ; open / start session
          ("C-c C-c C" . claude-code-resume)                      ; resume a previous session
          ("C-c C-c k" . claude-code-kill)                        ; quit / kill session
          ("C-c C-c x" . my/claude-code-send-region-with-context) ; send region + context
          ("C-c C-c s" . claude-code-send-string)))               ; send a prompt directly
;; (Transient menu still available via M-x claude-code-transient)

;; Always show the Claude Code buffer in a side window on the LEFT
(add-to-list 'display-buffer-alist
             '("^\\*claude:"
               (display-buffer-in-side-window)
               (side . left)
               (slot . 0)
               (window-width . 0.33)
               (window-parameters . ((no-delete-other-windows . t)))))


;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
