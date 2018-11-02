;; https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org

(use-package eshell
  :init
  (setq eshell-scroll-to-bottom-on-input 'all
        eshell-error-if-no-glob t
        eshell-hist-ignoredups t
        eshell-save-history-on-exit t
        eshell-prefer-lisp-functions nil
        eshell-destroy-buffer-when-process-dies t))

(use-package eshell
	     :init
	     (add-hook 'eshell-mode-hook
		       (lambda ()
			 (add-to-list 'eshell-visual-commands "ssh")
			 (add-to-list 'eshell-visual-commands "top"))))

(add-hook 'eshell-mode-hook (lambda ()
			      (eshell/alias "e" "find-file $1")
			      (eshell/alias "ff" "find-file $1")
			      (eshell/alias "emacs" "find-file $1")
			      (eshell/alias "ee" "find-file-other-window $1")

			      (eshell/alias "gd" "magit-diff-unstaged")
			      (eshell/alias "gds" "magit-diff-staged")
			      (eshell/alias "d" "dired $1")

			      ;; The 'ls' executable requires the Gnu version on the Mac
			      (let ((ls (if (file-exists-p "/usr/local/bin/gls")
					    "/usr/local/bin/gls"
					  "/bin/ls")))
				(eshell/alias "ll" (concat ls " -AlohG --color=always")))))


(defun eshell/gst (&rest args)
  (magit-status (pop args) nil)
  (eshell/echo))   ;; The echo command suppresses output


(defun eshell/f (filename &optional dir try-count)
    "Searches for files matching FILENAME in either DIR or the
current directory. Just a typical wrapper around the standard
`find' executable.

Since any wildcards in FILENAME need to be escaped, this wraps the shell command.

If not results were found, it calls the `find' executable up to
two more times, wrapping the FILENAME pattern in wildcat
matches. This seems to be more helpful to me."
    (let* ((cmd (concat
		 (executable-find "find")
		 " " (or dir ".")
		 "      -not -path '*/.git*'"
		 " -and -not -path '*node_modules*'"
		 " -and -not -path '*classes*'"
		 " -and "
		 " -type f -and "
		 "-iname '" filename "'"))
	   (results (shell-command-to-string cmd)))

      (if (not (s-blank-str? results))
	  results
	(cond
	 ((or (null try-count) (= 0 try-count))
	  (eshell/f (concat filename "*") dir 1))
	 ((or (null try-count) (= 1 try-count))
	  (eshell/f (concat "*" filename) dir 2))
	 (t "")))))

(defun eshell/ef (filename &optional dir)
    "Searches for the first matching filename and loads it into a
file to edit."
    (let* ((files (eshell/f filename dir))
	   (file (car (s-split "\n" files))))
      (find-file file)))


(defun eshell/find (&rest args)
  "Wrapper around the ‘find’ executable."
  (let ((cmd (concat "find " (string-join args))))
    (shell-command-to-string cmd)))


(defun eshell/clear ()
  "Clear the eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))


(defun curr-dir-git-branch-string (pwd)
    "Returns current git branch as a string, or the empty string if
PWD is not in a git repo (or the git command is not found)."
    (interactive)
    (when (and (not (file-remote-p pwd))
	       (eshell-search-path "git")
	       (locate-dominating-file pwd ".git"))
      (let* ((git-url (shell-command-to-string "git config --get remote.origin.url"))
	     (git-repo (file-name-base (s-trim git-url)))
	     (git-output (shell-command-to-string (concat "git rev-parse --abbrev-ref HEAD")))
	     (git-branch (s-trim git-output))
	     (git-icon  "\xe0a0")
	     (git-icon2 (propertize "\xf020" 'face `(:family "octicons"))))
	(concat git-repo " " git-icon2 " " git-branch))))


(defun pwd-replace-home (pwd)
  "Replace home in PWD with tilde (~) character."
  (interactive)
  (let* ((home (expand-file-name (getenv "HOME")))
	 (home-len (length home)))
    (if (and
	 (>= (length pwd) home-len)
	 (equal home (substring pwd 0 home-len)))
	(concat "~" (substring pwd home-len))
      pwd)))


(defun pwd-shorten-dirs (pwd)
  "Shorten all directory names in PWD except the last two."
  (let ((p-lst (split-string pwd "/")))
    (if (> (length p-lst) 2)
	(concat
	 (mapconcat (lambda (elm) (if (zerop (length elm)) ""
				    (substring elm 0 1)))
		    (butlast p-lst 2)
		    "/")
	 "/"
	 (mapconcat (lambda (elm) elm)
		    (last p-lst 2)
		    "/"))
      pwd)))  ;; Otherwise, we just return the PWD


(defun split-directory-prompt (directory)
  (if (string-match-p ".*/.*" directory)
      (list (file-name-directory directory) (file-name-base directory))
    (list "" directory)))


(defun ruby-prompt ()
  "Returns a string (may be empty) based on the current Ruby Virtual Environment."
  (let* ((executable "~/.rvm/bin/rvm-prompt")
	 (command    (concat executable "v g")))
    (when (file-exists-p executable)
      (let* ((results (shell-command-to-string executable))
	     (cleaned (string-trim results))
	     (gem     (propertize "\xe92b" 'face `(:family "alltheicons"))))
	(when (and cleaned (not (equal cleaned "")))
	  (s-replace "ruby-" gem cleaned))))))



(defun eshell/eshell-local-prompt-function ()
    "A prompt for eshell that works locally (in that is assumes
that it could run certain commands) in order to make a prettier,
more-helpful local prompt."
    (interactive)
    (let* ((pwd        (eshell/pwd))
	   (directory (split-directory-prompt
		       (pwd-shorten-dirs
			(pwd-replace-home pwd))))
	   (parent (car directory))
	   (name   (cadr directory))
	   (branch (curr-dir-git-branch-string pwd))
	   (ruby   (when (not (file-remote-p pwd)) (ruby-prompt)))

	   (dark-env (eq 'dark (frame-parameter nil 'background-mode)))
	   (for-bars                 `(:weight bold))
	   (for-parent  (if dark-env `(:foreground "dark orange") `(:foreground "blue")))
	   (for-dir     (if dark-env `(:foreground "orange" :weight bold)
			  `(:foreground "blue" :weight bold)))
	   (for-git                  `(:foreground "green"))
	   (for-ruby                 `(:foreground "red")))
	   

      (concat
       (propertize "⟣─ "    'face for-bars)
       (propertize parent   'face for-parent)
       (propertize name     'face for-dir)
       (when branch
	 (concat (propertize " ── "    'face for-bars)
		 (propertize branch   'face for-git)))
       (when ruby
	 (concat (propertize " ── " 'face for-bars)
		 (propertize ruby   'face for-ruby)))
       (propertize "\n"     'face for-bars)
       (propertize (if (= (user-uid) 0) " #" " $") 'face `(:weight ultra-bold))
       ;; (propertize " └→" 'face (if (= (user-uid) 0) `(:weight ultra-bold :foreground "red") `(:weight ultra-bold)))
       (propertize " "    'face `(:weight bold)))))

(setq-default eshell-prompt-function #'eshell/eshell-local-prompt-function)


(setq eshell-highlight-prompt nil)

(defun eshell-here ()
    "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
    (interactive)
    (let* ((parent (if (buffer-file-name)
		       (file-name-directory (buffer-file-name))
		     default-directory))
	   (height (/ (window-total-height) 3))
	   (name   (car (last (split-string parent "/" t)))))
      (split-window-vertically (- height))
      (other-window 1)
      (eshell "new")
      (rename-buffer (concat "*eshell: " name "*"))

      (insert (concat "ls"))
      (eshell-send-input)))

(bind-key "C-!" 'eshell-here)


(defun eshell-here ()
    "Opens up a new shell in the directory associated with the
    current buffer's file. The eshell is renamed to match that
    directory to make multiple eshell windows easier."
    (interactive)
    (let* ((height (/ (window-total-height) 3)))
      (split-window-vertically (- height))
      (other-window 1)
      (eshell "new")
      (insert (concat "ls"))
      (eshell-send-input)))

(bind-key "C-!" 'eshell-here)

(use-package eshell
	     :config
	     (defun ha/eshell-quit-or-delete-char (arg)
	       (interactive "p")
	       (if (and (eolp) (looking-back eshell-prompt-regexp))
		   (progn
		     (eshell-life-is-too-much) ; Why not? (eshell/exit)
		     (ignore-errors
		       (delete-window)))
		 (delete-forward-char arg)))
	     :init
	     (add-hook 'eshell-mode-hook
		       (lambda ()
			 (bind-keys :map eshell-mode-map
				    ("C-d" . ha/eshell-quit-or-delete-char)))))


(defun eshell-there (host)
    "Creates an eshell session that uses Tramp to automatically
connect to a remote system, HOST.  The hostname can be either the
IP address, or FQDN, and can specify the user account, as in
root@blah.com. HOST can also be a complete Tramp reference."
    (interactive "sHost: ")

    (let* ((default-directory
	     (cond
	      ((string-match-p "^/" host) host)

	      ((string-match-p (ha/eshell-host-regexp 'full) host)
	       (string-match (ha/eshell-host-regexp 'full) host) ;; Why!?
	       (let* ((user1 (match-string 2 host))
		      (host1 (match-string 3 host))
		      (user2 (match-string 6 host))
		      (host2 (match-string 7 host)))
		 (if host1
		     (ha/eshell-host->tramp user1 host1)
		   (ha/eshell-host->tramp user2 host2))))

	      (t (format "/%s:" host)))))
      (eshell-here)))


(defun ha/eshell-host-regexp (regexp)
  "Returns a particular regular expression based on symbol, REGEXP"
  (let* ((user-regexp      "\\(\\([[:alpha:].]+\\)@\\)?")
	 (tramp-regexp     "\\b/ssh:[:graph:]+")
	 (ip-char          "[[:digit:]]")
	 (ip-plus-period   (concat ip-char "+" "\\."))
	 (ip-regexp        (concat "\\(\\(" ip-plus-period "\\)\\{3\\}" ip-char "+\\)"))
	 (host-char        "[[:alpha:][:digit:]-]")
	 (host-plus-period (concat host-char "+" "\\."))
	 (host-regexp      (concat "\\(\\(" host-plus-period "\\)+" host-char "+\\)"))
	 (horrific-regexp  (concat "\\b"
				   user-regexp ip-regexp
				   "\\|"
				   user-regexp host-regexp
				   "\\b")))
    (cond
     ((eq regexp 'tramp) tramp-regexp)
     ((eq regexp 'host)  host-regexp)
     ((eq regexp 'full)  horrific-regexp))))



(defun ha/eshell-host-regexp (regexp)
  "Returns a particular regular expression based on symbol, REGEXP"
  (let* ((user-regexp      "\\(\\([[:alpha:].]+\\)@\\)?")
         (tramp-regexp     "\\b/ssh:[:graph:]+")
         (ip-char          "[[:digit:]]")
         (ip-plus-period   (concat ip-char "+" "\\."))
         (ip-regexp        (concat "\\(\\(" ip-plus-period "\\)\\{3\\}" ip-char "+\\)"))
         (host-char        "[[:alpha:][:digit:]-]")
         (host-plus-period (concat host-char "+" "\\."))
         (host-regexp      (concat "\\(\\(" host-plus-period "\\)+" host-char "+\\)"))
         (horrific-regexp  (concat "\\b"
                                   user-regexp ip-regexp
                                   "\\|"
                                   user-regexp host-regexp
                                   "\\b")))
    (cond
     ((eq regexp 'tramp) tramp-regexp)
     ((eq regexp 'host)  host-regexp)
     ((eq regexp 'full)  horrific-regexp))))



(defun ha/eshell-scan-for-hostnames ()
  "Helper function to scan the current line for any hostnames, IP
or Tramp references.  This returns a tuple of the username (if
found) and the hostname.

If a Tramp reference is found, the username part of the tuple
will be `nil'."
  (save-excursion
    (goto-char (line-beginning-position))
    (if (search-forward-regexp (ha/eshell-host-regexp 'tramp) (line-end-position) t)
        (cons nil (buffer-substring-no-properties (match-beginning 0) (match-end 0)))

      ;; Returns the text associated with match expression, NUM or `nil' if no match was found.
      (cl-flet ((ha/eshell-get-expression (num) (if-let ((first (match-beginning num))
                                                         (end   (match-end num)))
                                                    (buffer-substring-no-properties first end))))

        (search-forward-regexp (ha/eshell-host-regexp 'full) (line-end-position))

        ;; Until this is completely robust, let's keep this debugging code here:
        ;; (message (mapconcat (lambda (tup) (if-let ((s (car tup))
        ;;                                       (e (cadr tup)))
        ;;                                  (buffer-substring-no-properties s e)
        ;;                                "null"))
        ;;             (-partition 2 (match-data t)) " -- "))

        (let ((user1 (ha/eshell-get-expression 2))
              (host1 (ha/eshell-get-expression 3))
              (user2 (ha/eshell-get-expression 6))
              (host2 (ha/eshell-get-expression 7)))
          (if host1
              (cons user1 host1)
            (cons user2 host2)))))))


(defun ha/eshell-host->tramp (username hostname &optional prefer-root)
  "Returns a TRAMP reference based on a USERNAME and HOSTNAME
that refers to any host or IP address."
  (cond ((string-match-p "^/" host)
           host)
        ((or (and prefer-root (not username)) (equal username "root"))
           (format "/ssh:%s|sudo:%s:" hostname hostname))
        ((or (null username) (equal username user-login-name))
           (format "/ssh:%s:" hostname))
        (t
           (format "/ssh:%s|sudo:%s|sudo@%s:%s:" hostname hostname username hostname))))

(defun eshell-here-on-line (p)
  "Search the current line for an IP address or hostname, and call the `eshell-here' function.

Call with PREFIX to connect with the `root' useraccount, via
`sudo'."
  (interactive "p")
  (destructuring-bind (user host) (ha/eshell-scan-for-hostnames)
    (let ((default-directory (ha/eshell-host->tramp user host (> p 1))))
      (message "Connecting to: %s" default-directory)
      ;; With the `default-directory' set to a Tramp reference, rock on!
      (eshell-here))))

(bind-key "M-s-1" #'eshell-here-on-line)


(defun remote-shell-tramp-connection (hostname &optional root directory)
  "Return a TRAMP connection string to HOSTNAME. If ROOT is
non-nil, returns an sudo compatible string."
  (when (null directory)
    (setq directory "")))


(defun remote-shell-buffer-name (hostname &optional command-str default-name)
  "Returns a standard format for our remote shell command buffer
windows based on the HOSTNAME and the COMMAND-STR. Uses
DEFAULT-NAME if specified."
  (cond
   (default-name     default-name)
   (command-str      (let ((command (car (split-string command-str))))
                        (format "*%s:%s*" hostname command)))
   (t                (format "*%s*" hostname))))


(defun remote-shell (hostname &optional root)
  "Start an shell experience on HOSTNAME, that can be an alias to
a virtual machine from my 'cloud' server. With prefix command, opens
the shell as the root user account."
  (interactive
   (list (if (fboundp #'ido-completing-read)
             (ido-completing-read "Hostname: " (remote-shell-fav-hosts-list))
           (completing-read "Hostname: " (remote-shell-fav-hosts-list)))))
  (when (equal current-prefix-arg '(4))
    (setq root t))
  (let ((default-directory (remote-shell-tramp-connection hostname root)))
    (shell (remote-shell-buffer-name hostname))))

(defun eshell-favorite (hostname &optional root)
  "Start an shell experience on HOSTNAME, that can be an alias to
a virtual machine from my 'cloud' server. With prefix command, opens
the shell as the root user account."
  (interactive
   (list (if (fboundp #'ido-completing-read)
             (ido-completing-read "Hostname: " (remote-shell-fav-hosts-list))
           (completing-read "Hostname: " (remote-shell-fav-hosts-list)))))
  (when (equal current-prefix-arg '(4))
    (setq root t))
  (let ((default-directory (remote-shell-tramp-connection hostname root)))
    (eshell (remote-shell-buffer-name hostname))))

(defun remote-shell-command (hostname command
                                      &optional root bufname directory)
  "On HOSTNAME, run COMMAND (if the command ends with &, run
asynchronously). With a `C-u' prefix, run the command as ROOT.
When non-interactive, you can specify BUFNAME for the buffer's
name, and DIRECTORY where the command should run."
  (interactive
   (list (if #'ido-completing-read
             (ido-completing-read "Hostname: " (remote-shell-fav-hosts-list))
           (completing-read "Hostname: " (remote-shell-fav-hosts-list)))
         (read-string "Command: ")))
  (when (equal current-prefix-arg '(4))
    (setq root t))
  (let ((default-directory (remote-shell-tramp-connection hostname root directory)))
    (shell-command command (remote-shell-buffer-name hostname command bufname))))

(defun remote-shell-commands (clients command
                                      &optional root async directory)
  "On each host entry in CLIENTS, run the shell COMMAND,
optionally as ROOT. If ASYNC is non-nil, appends the `&' to the
shell command in order to run it asynchronously. Runs the command
in the default home directory unless DIRECTORY is specified."
  (if async
      (setq command (concat command " &")))
  (dolist (host clients)
    (remote-shell-command host command root nil directory)))


(defun remote-shell-commands-show (clients command)
  "Shows each buffer of a previously executed command. For example:

        (let ((my-favs '(\"os-controller\" \"contrail-controller\"
                         \"compute\" \"nagios\" \"elk\"))
              (command \"chef-client\"))
          (remote-shell-commands my-favs command t t)
          (remote-shell-commands-show my-favs command))"

  (delete-other-windows)
  (let ((first-time t))
    (dolist (host clients)
      (if (not first-time)
          (split-window-vertically)
        (split-window-horizontally)
        (setq first-time nil))

      (other-window 1)
      (switch-to-buffer (remote-shell-buffer-name host command))
      (balance-windows)
      (sit-for 0.5))))

(setq tramp-default-method "ssh")

(defun eshell-next-prompt (n)
  "Move to end of Nth next prompt in the buffer. See `eshell-prompt-regexp'."
  (interactive "p")
  (re-search-forward eshell-prompt-regexp nil t n)
  (when eshell-highlight-prompt
    (while (not (get-text-property (line-beginning-position) 'read-only) )
      (re-search-forward eshell-prompt-regexp nil t n)))
  (eshell-skip-prompt))

(defun eshell-previous-prompt (n)
  "Move to end of Nth previous prompt in the buffer. See `eshell-prompt-regexp'."
  (interactive "p")
  (backward-char)
  (eshell-next-prompt (- n)))

(defun eshell-insert-history ()
  "Displays the eshell history to select and insert back into your eshell."
  (interactive)
  (insert (ido-completing-read "Eshell history: "
                               (delete-dups
                                (ring-elements eshell-history-ring)))))

(add-hook 'eshell-mode-hook (lambda ()
    (define-key eshell-mode-map (kbd "M-S-P") 'eshell-previous-prompt)
    (define-key eshell-mode-map (kbd "M-S-N") 'eshell-next-prompt)
    (define-key eshell-mode-map (kbd "M-r") 'eshell-insert-history)))

(defun execute-command-on-file-buffer (cmd)
  (interactive "sCommand to execute: ")
  (let* ((file-name (buffer-file-name))
         (full-cmd (concat cmd " " file-name)))
    (shell-command full-cmd)))

(bind-key "A-1" #'execute-command-on-file-buffer)

(defun execute-command-on-file-directory (cmd)
  (interactive "sCommand to execute: ")
  (let* ((dir-name (file-name-directory (buffer-file-name)))
         (full-cmd (concat "cd " dir-name "; " cmd)))
    (shell-command full-cmd)))

(bind-key "A-!" #'execute-command-on-file-directory)
(bind-key "s-!" #'execute-command-on-file-directory)

(provide 'init-eshell)
