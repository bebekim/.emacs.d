;;; package --- Summary
;;; Commentary:

;;; Code:
;; ----------------------------
;; BASIC CUSTOMIZATION
;; global variables

;; global variables
(setq
 inhibit-startup-screen t
 create-lockfiles nil
 make-backup-files nil
 column-number-mode t
 scroll-error-top-bottom t
 show-paren-delay 0.5
 use-package-always-ensure t
 sentence-end-double-space nil
 inhibit-startup-message t
 inhibit-splash-creen t
 global-linum-mode t
 global-visual-line-mode 1)


;; buffer local variables
(setq-default
 indent-tabs-mode nil
 tab-width 4
 c-basic-offset 4)

;; themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; modes
(electric-indent-mode 0)
(transient-mark-mode 1)

;; meta key from alt to cmd on Mac
;; (setq mac-option-modifier 'super)
;; (setq mac-command-modifier 'meta)

(global-set-key (kbd "C-?") 'help-command)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-unset-key (kbd "C-z"))


(require 'package)
(package-initialize)
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/"))
 package-archive-priorities '(("melpa-stable" . 1)))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(use-package scala-mode
:interpreter
("scala" . scala-mode))

(add-to-list 'load-path "~/.emacs.d/lisp/")
(load "swift-mode")

;; set $MATHPATH, $PATH, exec-path from shell on linux and os X
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))


;; ;; Your path from shell needs to be seen by Emacs, and to make that easier, use the following elisp package.
;; (add-to-list 'load-path "~/.emacs.d/packages")
;; (unless (require 'exec-path-from-shell nil 'noerror)
;;   (exec-path-from-shell-initialize))


;; Some convenient variables
(defvar dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name))
  "The root directory of the Emacs initialization process.")
(defvar modules-dir (concat dotfiles-dir "modules/")
  "This directory contains most of the customizations.  These files cover editing,  UI, and mode specific changes.")
(defvar vendor-dir (concat dotfiles-dir "vendor/")
  "This directory house Emacs Lisp packages that are not yet available in ELPA (or Marmalade).")


;; add directories to Emacs's `load-path'
(add-to-list 'load-path modules-dir)
(add-to-list 'load-path vendor-dir)

;; Keep customizations in a separate file
(setq custom-file (concat dotfiles-dir "custom.el"))
(load custom-file 'noerror)

(setq user-full-name "Youngha Kim")
(setq user-mail-address "yh.kim@outlook.com")


;;------------------------

(when (eq system-type 'gnu/linux)

  ;; default font size (point * 10)
  
  ;; (set-face-attribute 'default nil :family "Monaco")
  (set-fontset-font t 'hangul (font-spec :name "NanumGothicCoding"))
  
  
  )


;; -------
(defun create-shell ()
    "Create a shell with a given name."
    (interactive);; "Prompt\n shell name:")
    (let ((shell-name (read-string "shell name: " nil)))
    (shell (concat "*" shell-name "*"))))


;; Displaying compilation error messages in the echo area
(setq help-at-pt-display-when-idle t)
;; (setq help-at-pt-timer-delay 0.1)
;; (help-at-pt-set-timer)


;; create windmove bindings
(global-set-key "\M-j" 'windmove-left)
(global-set-key "\M-l" 'windmove-right)
(global-set-key "\M-i" 'windmove-up)
(global-set-key "\M-k" 'windmove-down)
;; ;; -------

(projectile-mode)
(global-company-mode t)

(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode t)

 ;; Display ido results vertically, rather than horizontally
  (setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
  (defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
  (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
  (defun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
    (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
    (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
  (add-hook 'ido-setup-hook 'ido-define-keys)
;;
(put 'upcase-region 'disabled nil)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

(require 'flycheck)

;; turn on flychecking globally
(add-hook 'after-init-hook #'global-flycheck-mode)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))


;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(json-jsonlist)))

;; https://github.com/purcell/exec-path-from-shell
;; only need exec-path-from-shell on OSX
;; this hopefully sets up path and other vars better
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
)
(add-hook 'web-mode-hook  'my-web-mode-hook)

;; for better jsx syntax-highlighting in web-mode
;; - courtesy of Patrick @halbtuerke
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
    (let ((web-mode-enable-part-face nil))
      ad-do-it)
    ad-do-it))

(require 'emmet-mode)

(defun bs-web-mode-hook ()
  (local-set-key '[backtab] 'indent-relative)
  (setq indent-tabs-mode nil)
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

(add-hook 'web-mode-hook 'bs-web-mode-hook)



(defun toggle-php-flavor-mode ()
  (interactive)
  "Toggle mode between PHP & Web-Mode Helper modes"
  (cond ((string= mode-name "PHP")
         (web-mode))
        ((string= mode-name "Web")
         (php-mode))))

(global-set-key [f5] 'toggle-php-flavor-mode)

;; highlight indentation
(add-to-list 'load-path "~/.emacs.d/packages/highlight-indents/")
(require 'highlight-indentation)
(add-hook 'python-mode-hook 'highlight-indentation-mode)
(add-hook 'js2-mode-hook 'highlight-indentation-mode)

(defun aj-toggle-fold ()
  "Toggle fold all lines larger than indentation on current line."
  (interactive)
  (let ((col 1))
    (save-excursion
      (back-to-indentation)
      (setq col (+ 1 (current-column)))
      (set-selective-display
       (if selective-display nil (or col 1))))))
(global-set-key [(M C i)] 'aj-toggle-fold)


;; ;; ;; ---------------------------------------------
 

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-done ((t (:foreground "PaleGreen" :weight normal :strike-through t))))
 '(org-headline-done ((t (:foreground "LightSalmon" :strike-through t)))))

;; -------
;; disable vc-mode so that git doesn't slow down Emacs opening
'(vc-handled-backends nil)


;; -------
;; ;; nerd tree configuration
;; (setq neo-theme (if (display-graphic-p) 'icons 'arrow))

;; ;; Every time when the neotree window is opened, let it find current file and jump to node.
;; (setq neo-smart-open t)

;; ;; When running 'projectile-switch-project' (C-c p p), 'neotree' will change root automatically.
;; (setq projectile-switch-project-action 'neotree-projectile-action)


;; (add-to-list 'load-path "/opt/ess-17.11/lisp")
(load "ess-site")
;; (setq ess-smart-S-assign-key ":")
;; (ess-toggle-S-assign nil)
;; (ess-toggle-S-assign nil)
(ess-toggle-underscore nil) ; leave underscore key alone!


;; python set up
(require 'virtualenvwrapper)
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell) ;; if you want eshell support

(add-hook 'python-mode-hook 'anaconda-mode)

(require 'conda)
(conda-env-initialize-interactive-shells)
(conda-env-initialize-eshell)
(conda-env-autoactivate-mode t)

(custom-set-variables
 '(conda-anaconda-home "~/anaconda3"))

;;;;org-mode configuration
;; Enable org-mode
(add-to-list 'load-path "~/.emacs.d/org-9.1.6/lisp")
(add-to-list 'load-path "~/.emacs.d/org-9.1.6/contrib/lisp" t)

;; location of local org files
(setq org-directory "/home/yhk/Dropbox/gtd")
;; ;; name of the file new notes will be stored
;; (setq org-mobile-inbox-for-pull "/home/yhk/Dropbox/gtd/mobile.org")
;; ;; Dropbox root directory for MobileOrg
;; (setq org-mobile-directory "/home/yhk/Dropbox/Apps/MobileOrg")

;; ;; (load "ob-julia.el")
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (emacs-lisp . t)
   (python . t)
   (shell . t)
;;    (swift . t)
   ))

;; (setq org-confirm-babel-evaluate nil)
;; (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)   
;; (add-hook 'org-mode-hook 'org-display-inline-images)

(setq org-log-done t)
(setq org-agenda-files '("/home/yhk/Dropbox/gtd"))
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-capture-templates
      '(
	("c" "Capture" entry (file+headline "~/Dropbox/gtd/inbox.org" "Tasks")
	 "* CAPTURE %i%?")
	("j" "Journal" entry (file+olp+datetree "~/Dropbox/gtd/journal.org")
	 "** %^{Heading}")
	("l" "Log Time" entry (file+datetree "~/Dropbox/gtd/timelog.org")
	 "** %U - %^{Activity}  :TIMELOG:")
	))

(setq org-tag-alist '((:startgroup . nil)
                      ("@work" . ?w) ("@home" . ?h) ("@school" . ?s) ("@sideproject" . ?p)
                      (:endgroup . nil)
                      ("dev" . ?d) ("bizdev" . ?b) ("admin" . ?m)))


(setq org-refile-targets '((nil :maxlevel . 9)
                                (org-agenda-files :maxlevel . 9)))
(setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
(setq org-refile-use-outline-path t)                  ; Show full paths for refiling

;; The following lines are always needed.  Choose your own keys.
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(define-key global-map "\C-c c" 'org-capture)
(global-set-key "\C-cc" 'org-capture)
(global-set-key (kbd "<f6>") 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key (kbd "C-c o") 
                (lambda () (interactive) (find-file "~/Dropbox/org/organizer.org")))

;; include entries from the Emacs diary into Org mode agenda
(setq org-agenda-include-diary t)

;; show sum of efforts for a day in org-agenda day title
;; https://emacs.stackexchange.com/questions/21380/show-sum-of-efforts-for-a-day-in-org-agenda-day-title
(setq org-columns-default-format "%60ITEM(Task) %6Effort(Estim){:}")

;; load markdown exporter automatically with org-mode
(eval-after-load "org"
  '(require 'ox-md nil t))

;; archive done tasks
(defun org-archive-done-tasks()
  (interactive)
  (org-map-entries
   (lambda gtd()
     (org-archive-subtree)
     (setq org-ap-continue-from (outline-previous-heading)))
   "/DONE" 'tree))

;; archive all done tasks
(defun org-archive-all-done-tasks ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))
(eval-after-load "org"
  '(require 'ox-md nil t))

;; org-mode to wordpress blog
;; (setq org2blog/wp-blog-alist
;;       `(("wordpress"
;; 	 :url "https://younghak.wordpress.com/xmlrpc.php"
;; 	 :username "younghak"
;; 	 :default-title "Hello World"
;; 	 :default-categories "Uncategorized"
;; 	 :tags-as-categories nil)
;; 	)
;;       )

;; (setq org2blog/wp-buffer-template
;; "--------------------------
;; #+TITLE: %s
;; #+DATE: %s
;; --------------------------\n")
;; (defun my-format-function (format-string)
;;   (format format-string
;; 	  org2blog/wp-default-title
;; 	  (format-time-string "%d-%m-%Y" (current-time))))
;; (setq org2blog/wp-buffer-format-function 'my-format-function)

;; ;; credential
;; (let (credentials)
;;   (add-to-list 'auth-sources "~/.authinfo.gpg")
;;   (setq credentials (auth-source-user-and-password "wordpressblog"))
;;   (setq org2blog/wp-blog-alist
;; 	`(("wordpress"
;; 	   :url "https://younghak.wordpress.com/xmlrpc.php"
;; 	   :username ,(car (auth-source-user-and-password "wordpressblog"))
;; 	   :password ,(cadr (auth-source-user-and-password "wordpressblog"))
;; 	   :default-title "Hello World"
;; 	   :default-categories ("Uncategorized")
;; 	   :tags-as-categories nil))))
;; (setq org2blog/wp-use-sourcecode-shortcode 't)
;; (setq org2blog/wp-sourcecode-default-params nil)
;; (setq org2blog/wp-sourcecode-langs
;;       '("bash" "java" "sql" "pytyhon" "emacs-lisp" "lisp"))
;; (setq org-src-fontify-natively t)


;; 

;; (el-get-bundle slack)
(use-package slack
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
  (setq slack-prefer-current-team t)
  :config
  (slack-register-team
   :name "signalnco"
   :default t
   :client-id "200312814944.380470304402"
   :client-secret "604e5ee6dbfcba0b27eeba791d4e3588"
   :token "xoxp-200312814944-310111957252-379922802273-ab89c11d70ac2f9dc4b6a656a0ee2f61"
   :subscribed-channels '(test-rename rrrrr)
   :full-and-display-names t
   )

  (slack-register-team
   :name "test"
   :client-id "200312814944.380470304402"
   :client-secret "604e5ee6dbfcba0b27eeba791d4e3588"
   :token "xoxp-200312814944-310111957252-379922802273-ab89c11d70ac2f9dc4b6a656a0ee2f61"
   :subscribed-channels '(hoge fuga))

  (evil-define-key 'normal slack-info-mode-map
    ",u" 'slack-room-update-messages)
  (evil-define-key 'normal slack-mode-map
    ",c" 'slack-buffer-kill
    ",ra" 'slack-message-add-reaction
    ",rr" 'slack-message-remove-reaction
    ",rs" 'slack-message-show-reaction-users
    ",pl" 'slack-room-pins-list
    ",pa" 'slack-message-pins-add
    ",pr" 'slack-message-pins-remove
    ",mm" 'slack-message-write-another-buffer
    ",me" 'slack-message-edit
    ",md" 'slack-message-delete
    ",u" 'slack-room-update-messages
    ",2" 'slack-message-embed-mention
    ",3" 'slack-message-embed-channel
    "\C-n" 'slack-buffer-goto-next-message
    "\C-p" 'slack-buffer-goto-prev-message)
   (evil-define-key 'normal slack-edit-message-mode-map
    ",k" 'slack-message-cancel-edit
    ",s" 'slack-message-send-from-buffer
    ",2" 'slack-message-embed-mention
    ",3" 'slack-message-embed-channel))

(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'notifier))


;; ;; org-jira setting
(setq jiralib-url "http://perforce.signalnco.com:8080/")
;; ;; ;; (define-key org-jira-map (kbd "C-c pg") 'org-jira-get-projects)
;; ;; ;; (define-key org-jira-map (kbd "C-c ib") 'org-jira-browse-issue)
;; ;; ;; (define-key org-jira-map (kbd "C-c ig") 'org-jira-get-issues)
;; ;; ;; (define-key org-jira-map (kbd "C-c ih") 'org-jira-get-issues-headonly)
;; ;; ;; (define-key org-jira-map (kbd "C-c iu") 'org-jira-update-issue)
;; ;; ;; (define-key org-jira-map (kbd "C-c iw") 'org-jira-progress-issue)
;; ;; ;; (define-key org-jira-map (kbd "C-c in") 'org-jira-progress-issue-next)
;; ;; ;; (define-key org-jira-map (kbd "C-c ia") 'org-jira-assign-issue)
;; ;; ;; (define-key org-jira-map (kbd "C-c ir") 'org-jira-refresh-issue)
;; ;; ;; (define-key org-jira-map (kbd "C-c iR") 'org-jira-refresh-issues-in-buffer)
;; ;; ;; (define-key org-jira-map (kbd "C-c ic") 'org-jira-create-issue)
;; ;; ;; (define-key org-jira-map (kbd "C-c ik") 'org-jira-copy-current-issue-key)
;; ;; ;; (define-key org-jira-map (kbd "C-c sc") 'org-jira-create-subtask)
;; ;; ;; (define-key org-jira-map (kbd "C-c sg") 'org-jira-get-subtasks)
;; ;; ;; (define-key org-jira-map (kbd "C-c cu") 'org-jira-update-comment)
;; ;; ;; (define-key org-jira-map (kbd "C-c wu") 'org-jira-update-worklogs-from-org-clocks)
;; ;; ;; (define-key org-jira-map (kbd "C-c tj") 'org-jira-todo-to-jira)
;; ;; ;; (define-key org-jira-map (kbd "C-c if") 'org-jira-get-issues-by-fixversion)


;; change font for done task
(setq org-fontify-done-headline t)

;; mu4e configuration
;; mail mu4e
(add-to-list 'load-path "/opt/mu/mu4e")
(require 'mu4e)
(setq mail-user-agent 'mu4e-user-agent)
(setq mu4e-maildir "~/Maildir")

(setq mu4e-drafts-folder "/[Gmail].Drafts")
(setq mu4e-sent-folder   "/[Gmail].Sent Mail")
(setq mu4e-trash-folder  "/[Gmail].Trash")


;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

;; (See the documentation for `mu4e-sent-messages-behavior' if you have
;; additional non-Gmail addresses and want assign them different
;; behavior.)

;; setup some handy shortcuts
;; you can quickly switch to your Inbox -- press ``ji''
;; then, when you want archive some messages, move them to
;; the 'All Mail' folder by pressing ``ma''.

(setq mu4e-maildir-shortcuts
    '( ("/INBOX"               . ?i)
       ("/[Gmail].Sent Mail"   . ?s)
       ("/[Gmail].Trash"       . ?t)
       ("/[Gmail].All Mail"    . ?a)))


(setq mu4e-get-mail-command "offlineimap -o")

;; something about ourselves
(setq
   user-mail-address "goldenfermi@gmail.com"
   user-full-name  "Youngha Kim"
   mu4e-compose-signature
    (concat
     ""
     ))

(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
   starttls-use-gnutls t
   smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
   smtpmail-auth-credentials
        '(("smtp.gmail.com" 587 "marcus.yh.kim@gmail.com" nil))
   smtpmail-default-smtp-server "smtp.gmail.com"
   smtpmail-smtp-server "smtp.gmail.com"
   smtpmail-smtp-service 587)

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

;; ;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)


(setq mu4e-contexts
 `( ,(make-mu4e-context
     :name "personal"
     :match-func (lambda (msg) (when msg
       (string-prefix-p "/personal" (mu4e-message-field msg :maildir))))
     :vars '(
	     (mu4e-drafts-folder . "/[Gmail].Drafts")
	     (mu4e-trash-folder . "/[Gmail].Trash")
	     (mu4e-refile-folder . "/[Gmail].Archive")
	     ))
   ,(make-mu4e-context
     :name "school"
     :match-func (lambda (msg) (when msg
       (string-prefix-p "/school" (mu4e-message-field msg :maildir))))
     :vars '(
   	     (mu4e-drafts-folder . "/[Gmail.Drafts")
   	     (mu4e-trash-folder . "/[Gmail.Trash")
   	     (mu4e-refile-folder . "/[Gmail].Archive")
   	     ))
   ))

;; ;; refile messages according to the date
;; (defun refile-to-date-folder (prefix msg)
;;   "Refiles the message to the prefix.year.month folder according to 
;; the message date. Creates the folder if necessary"
;;   (let* ((time (mu4e-message-field-raw msg :date))
;;          (mdir (if time (concat prefix (format-time-string ".%Y.%m" time)) prefix))
;;          (fullpath (concat mu4e-maildir mdir)))
;;     (if (mu4e-create-maildir-maybe fullpath) 
;;         mdir
;;       (mu4e-error "Folder does not exist"))))

;; (defun refile-to-old-date-folder (msg)
;;   "Refiles to old."
;;   (refile-to-date-folder "/old" msg))

;; ;; default
;; (setq
;;  mu4e-refile-folder 'refile-to-old-date-folder)


;; setup some handy shortcuts
;; you can quickly switch to your Inbox -- press ``ji''
;; then, when you want archive some messages, move them to
;; the 'All Mail' folder by pressing ``ma''.



;; also, make sure the gnutls command line utils are installed
;; package 'gnutls-bin' in Debian/Ubuntu


;; (defvar my-mu4e-account-alist
;;   '(("personal"
;;      (mu4e-sent-folder "/[Gmail].Sent Mail")
;;      (user-mail-address "goldenfermi@gmail.com")
;;      (smtpmail-smtp-user "goldenfermi")
;;      (smtpmail-local-domain "gmail.com")
;;      (smtpmail-default-smtp-server "smtp.gmail.com")
;;      (smtpmail-smtp-server "smtp.gmail.com")
;;      (smtpmail-smtp-service 587)
;;      )
;;     ;; ("school"
;;     ;;  (mu4e-sent-folder "/[Gmail].Sent Mail")
;;     ;;  (user-mail-address "younghak@student.unimelb.edu.au")
;;     ;;  (smtpmail-smtp-user "younghak")
;;     ;;  (smtpmail-local-domain "student.unimelb.edu.au")
;;     ;;  (smtpmail-default-smtp-server "smtp.gmail.com")
;;     ;;  (smtpmail-smtp-server "smtp.gmail.com")
;;     ;;  (smtpmail-smtp-service 587)
;;     ;;  )
     
;;      ;; Include any other accounts here ...
;;     ))

;; (defun my-mu4e-set-account ()
;;   "Set the account for composing a message.
;;    This function is taken from: 
;;      https://www.djcbsoftware.nl/code/mu/mu4e/Multiple-accounts.html"
;;   (let* ((account
;;     (if mu4e-compose-parent-message
;;         (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
;;     (string-match "/\\(.*?\\)/" maildir)
;;     (match-string 1 maildir))
;;       (completing-read (format "Compose with account: (%s) "
;;              (mapconcat #'(lambda (var) (car var))
;;             my-mu4e-account-alist "/"))
;;            (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
;;            nil t nil nil (caar my-mu4e-account-alist))))
;;    (account-vars (cdr (assoc account my-mu4e-account-alist))))
;;     (if account-vars
;;   (mapc #'(lambda (var)
;;       (set (car var) (cadr var)))
;;         account-vars)
;;       (error "No email account found"))))
;; (add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)

;; ;; don't save messages to Sent Messages, Gmail/IMAP takes care of this
;; (setq mu4e-sent-messages-behavior 'delete)

;; ----------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(custom-enabled-themes (quote (zenburn)))
 '(default-input-method "korean-hangul")
 '(fci-rule-color "#37474f")
 '(hl-sexp-background-color "#1c1f26")
 '(org-agenda-files
   (quote
    ("/home/yhk/Dropbox/gtd/gtd.org" "/home/yhk/Dropbox/gtd/inbox.org" "/home/yhk/Dropbox/gtd/someday.org" "/home/yhk/Dropbox/gtd/tickler.org" "/home/yhk/Dropbox/gtd/reference.org")))
 '(org-columns-default-format "%38ITEM(Details) %TAGS(Context) %7TODO(To Do) %17Effort(Estimated Effort){:} %6CLOCKSUM(Clock)")
 '(org-global-properties
   (quote
    (("Effort_ALL" . "00:05 00:10 00:20 00:30 00:45 01:00 02:00 03:00 04:00 05:00 06:00"))))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m)))
 '(org-todo-keywords
   (quote
    ((sequence "CAPTURE(c)" "CLARIFY(l)" "INCUBATE(i)" "|" "DELEGATE(g)" "CANCELLED(x)")
     (sequence "ORGANIZE(o)" "REFLECT(r)")
     (sequence "TODO(t)" "|" "DONE(d)" "FAIL(f)"))))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#f36c60")
     (40 . "#ff9800")
     (60 . "#fff59d")
     (80 . "#8bc34a")
     (100 . "#81d4fa")
     (120 . "#4dd0e1")
     (140 . "#b39ddb")
     (160 . "#f36c60")
     (180 . "#ff9800")
     (200 . "#fff59d")
     (220 . "#8bc34a")
     (240 . "#81d4fa")
     (260 . "#4dd0e1")
     (280 . "#b39ddb")
     (300 . "#f36c60")
     (320 . "#ff9800")
     (340 . "#fff59d")
     (360 . "#8bc34a"))))
 '(vc-annotate-very-old-color nil))

;; (put 'scroll-left 'disabled nil)

(provide 'init)
;;; init.el ends here
