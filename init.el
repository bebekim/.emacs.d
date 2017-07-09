;; Package system
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives
	       '("gnu" . "https://elpa.gnu.org/packages/") t)
  (add-to-list 'package-archives
	       '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives 
	       '("org" . "http://orgmode.org/elpa/") t) ;; ord-mode repository
  (add-to-list 'package-archives
	       '("elpy" . "http://jorgenschaefer.github.io/packages/") t) ;; elpy repository
  (package-initialize)
  )

;; line number present
(global-linum-mode t)

;; -*- mode: elisp -*-
;; Disable the splash screen (to enable it agin, replace the t with 0)
(setq inhibit-splash-screen t)

;; Enable transient mark mode
(transient-mark-mode 1)

;; meta key from alt to cmd on Mac
 (setq mac-option-modifier 'super)
 (setq mac-command-modifier 'meta)

;; Your path from shell needs to be seen by Emacs, and to make that easier, use the following elisp package.
(add-to-list 'load-path "~/.emacs.d/lisp")
(unless (require 'exec-path-from-shell nil 'noerror)
  (exec-path-from-shell-initialize))

;;------------------------

(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :family "Monaco")

  ;; default font size (point * 10)

  ;; default Latin font (e.g. Consolas)
  ;; but I use Monaco 
  (set-face-attribute 'default nil :family "Monaco")

  ;; default font size (point * 10)
  ;;
  ;; WARNING!  Depending on the default font,
  ;; if the size is not supported very well, the frame will be clipped
  ;; so that the beginning of the buffer may not be visible correctly. 
  (set-face-attribute 'default nil :height 130)

  ;; use specific font for Korean charset.
  ;; if you want to use different font size for specific charset,
  ;; add :size POINT-SIZE in the font-spec.
  (set-fontset-font t 'hangul (font-spec :name "NanumGothicCoding"))

  ;; you may want to add different for other charset in this way.
  )

;;---------------------------
;; korean font
(defun xftp (&optional frame)
  "Return t if FRAME support XFT font backend."
  (let ((xft-supported))
    (mapc (lambda (x) (if (eq x 'xft) (setq xft-supported t)))
          (frame-parameter frame 'font-backend))
    xft-supported))

(when (xftp)
  (let ((fontset "fontset-default"))
    (set-fontset-font fontset 'latin
                      '("DejaVu Sans Mono" . "unicode-bmp"))
    (set-fontset-font fontset 'hangul
                      '("NanumGothic" . "unicode-bmp"))
    (set-face-attribute 'default nil
                        :font fontset
                        :height 110)))


;;;;org-mode configuration
;; Enable org-mode
(require 'org)

;; The following lines are always needed.  Choose your own keys.
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key (kbd "<f6>") 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-log-done t)
(setq org-agenda-files (list "~/Dropbox/org/work.org"
                             "~/Dropbox/org/school.org" 
                             "~/Dropbox/org/home.org"))
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-capture-templates
      '(
	("a" "Appointment" entry (file+headline "~/Dropbox/org/gcal.org" "Calendar") "* APPT %^{Description} %^g
%?
Added: %U")
        ("j" "Journal" entry (file+datetree "~/Dropbox/org/journal.org")
	 "* %?\nEntered on %U\n  %i\n  %a")
	("l" "Link" entry (file+headline "~/Dropbox/org/link.org" "Links")
	 "* %? %^L %^g \n%T" :prepend t)
	("d" "Diet" entry (file+datetree "~/Dropbox/org/diet.org")
	 "* %?\nEntered on %U\n  %i\n  %a")))

;; Make org-mode work with files ending in .org
;; (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; The above is the default in recent emacsen
(global-set-key (kbd "C-c o") 
                (lambda () (interactive) (find-file "~/Dropbox/org/organizer.org")))


(setq org-default-notes-file (concat org-directory "/notes.org"))

;;;;org-mode configuration
;; Enable org-mode
(require 'org)
;; Make org-mode work with files ending in .org
;; (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; The above is the default in recent emacsen
(global-set-key (kbd "C-c o") 
                (lambda () (interactive) (find-file "~/Dropbox/org/organizer.org")))
(global-set-key "\C-c l" 'org-store-link)
(global-set-key "\C-c a" 'org-agenda)
(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map "\C-c c" 'org-capture)
(global-set-key "\C-c c" 'org-capture)
(global-set-key (kbd "<f6>") 'org-capture)
(global-set-key "\C-c b" 'org-iswitchb)
(setq org-log-done t)
(setq org-agenda-files (list "~/Dropbox/org/work.org"
                             "~/Dropbox/org/school.org" 
                             "~/Dropbox/org/home.org"))
(setq org-default-notes-file (concat org-directory "/notes.org"))

;; include entries from the Emacs diary into Org mode agenda
(setq org-agenda-include-diary t)

;; load markdown exporter automatically with org-mode
(eval-after-load "org"
  '(require 'ox-md nil t))

;; archive done tasks
(defun org-archive-done-tasks()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-ap-continue-from (outline-previous-heading)))
  "/DONE" 'tree))

;; archive all done tasks
(defun org-archive-all-done-tasks ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))
(eval-after-load "org"
  '(require 'ox-md nil t))

;; change font for done task
(setq org-fontify-done-headline t)

;; : becomes <-
(setq ess-smart-S-assign-key ":")
(ess-toggle-S-assign nil)
(ess-toggle-S-assign nil)
(ess-toggle-underscore nil) ; leave underscore key alone!

(projectile-global-mode)
;; (add-hook 'ruby-mode-hook 'projectile-on)
;; (require 'robe)
;; (add-hook 'ruby-mode-hook 'robe-mode)

;; ;; Configuring emacs for ruby
;; (require 'flymake-ruby)
;; (add-hook 'ruby-mode-hook 'flymake-ruby-load)
;; (setq ruby-deep-indent-paren nil)
;; (global-set-key (kbd "C-c r r") 'inf-ruby)

(require 'ido)
(ido-mode t)

;; ruby shell inside emacs
(global-set-key (kbd "C-c r r") 'inf-ruby)

;; project management utilities
(projectile-global-mode)
;; (add-hook 'ruby-mode-hook 'projectile-on)

;; code assistance tool 
;; (require 'robe)
;; (add-hook 'ruby-mode-hook 'robe-mode)

;; 
(global-company-mode t)
;; (push 'company-robe company-backends)

(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

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

;; http://baohaojun.github.io/org-jira.html
;; (add-to-list 'load-path "~/.emacs.d/org-jira/")
;; (setq jiralib-url "https://fastcampus.atlassian.net/jira")
;; (require 'org-jira)

;; org trello
;; (require 'org-install)
;; (require 'org-trello)

;; incomplete installation of mu4e
;; the exact path may differ -- check it
;; (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")


;; ---------------------------------------------
 (custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (misterioso)))
 '(default-input-method "korean-hangul")
 '(org-agenda-files
   (quote
    ("~/Dropbox/org/work.org" "~/Dropbox/org/home.org" "~/Dropbox/org/school.org")))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m org-checklist org-drill)))
 '(org-tag-alist nil)
 '(org-todo-keywords
   (quote
    ((sequence "CAPTURE(c)" "ACTIONABLE(a)" "INCUBATE(i)" "|" "DELEGATE(g)" "CANCELLED(x)")
     (sequence "ORGANIZE(o)" "REFLECT(r)" "|" "FOLLOWUP(f)" "WAITING(w)")
     (sequence "ENGAGE(e)" "|" "DONE(d)"))))
 '(package-selected-packages
   (quote
    (magit racket-mode org-pomodoro projectile org markdown-mode exec-path-from-shell company)))
 '(scheme-program-name "mit-scheme")
 '(org-agenda-files (quote ("~/Dropbox/org/work.org" "~/Dropbox/org/school.org" "~/Dropbox/org/home.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-done ((t (:foreground "PaleGreen" :weight normal :strike-through t))))
 '(org-headline-done ((t (:foreground "LightSalmon" :strike-through t)))))
