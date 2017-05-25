;; Package system
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
;; (add-to-list 'package-archives
;; 	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t) ;; ord-mode repository
(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/") t) ;; elpy repository
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

; activate all the packages (in particular autoloads)
(package-initialize)

;; Get rid of default Emacs splash screen and but an empty buffer.
;; Scratch buffer is in org-mode
(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)


(elpy-enable) ;; enable elpy by default
;; Your python-shell-interpreter doesn’t seem to support readline
;; https://emacs.stackexchange.com/questions/30082/your-python-shell-interpreter-doesn-t-seem-to-support-readline
(with-eval-after-load 'python
  (defun python-shell-completion-native-try ()
    "Return non-nil if can trigger native completion."
    (let ((python-shell-completion-native-enable t)
          (python-shell-completion-native-output-timeout
           python-shell-completion-native-try-output-timeout))
      (python-shell-completion-native-get-completions
       (get-buffer-process (current-buffer))
       nil "_"))))



;; basic setup for el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)

(require 'xscheme) ;;load xscheme 

;; key binding for mac-use
;; Rebinding Command in this way won’t work when running Emacs inside a terminal, as the terminal program will intercept the Command keybindings.x
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(global-set-key (kbd "M-`") 'other-frame)
(setq mac-option-modifier nil)

;; fixing key binding bug in elpy
(define-key yas-minor-mode-map (kbd "C-c k") 'yas-expand)


;;fixing another key binding bug in iedit mode
(define-key global-map (kbd "C-c o") 'iedit-mode)


;; line number present
(global-linum-mode t)

;; display the current git repository
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;; -*- mode: elisp -*-
;; Disable the splash screen (to enable it agin, replace the t with 0)
(setq inhibit-splash-screen t)

;; Enable transient mark mode
(transient-mark-mode 1)

;; meta key from alt to cmd on Mac
 (setq mac-option-modifier 'super)
 (setq mac-command-modifier 'meta)

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
	("i" "To Do Item" entry (file+headline "~/Dropbox/org/item.org" "To Do Items")
	 "* %?\n%T" :prepend t)
	("d" "DietJournal" entry (file+datetree "~/Dropbox/org/diet.org")
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

;; include entries from the Emacs diary into Org mode agenda
(setq org-agenda-include-diary t)

;; load markdown exporter automatically with org-mode
(eval-after-load "org"
  '(require 'ox-md nil t))

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
    (magit racket-mode org-pomodoro elpy robe projectile org markdown-mode flymake-ruby exec-path-from-shell company)))
 '(scheme-program-name "mit-scheme"))
 '(org-agenda-files (quote ("~/Dropbox/org/work.org" "~/Dropbox/org/school.org" "~/Dropbox/org/home.org")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-done ((t (:foreground "PaleGreen" :weight normal :strike-through t))))
 '(org-headline-done ((t (:foreground "LightSalmon" :strike-through t)))))

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

;;----------------------

;; ;; korean font
;; (defun xftp (&optional frame)
;;   "Return t if FRAME support XFT font backend."
;;   (let ((xft-supported))
;;     (mapc (lambda (x) (if (eq x 'xft) (setq xft-supported t)))
;;           (frame-parameter frame 'font-backend))
;;     xft-supported))

;; (when (xftp)
;;   (let ((fontset "fontset-default"))
;;     (set-fontset-font fontset 'latin
;;                       '("DejaVu Sans Mono" . "unicode-bmp"))
;;     (set-fontset-font fontset 'hangul
;;                       '("NanumGothic" . "unicode-bmp"))
;;     (set-face-attribute 'default nil
;;                         :font fontset
;;                         :height 110)))


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


;; Configuring emacs for ruby
;; (require 'flymake-ruby)
;; (add-hook 'ruby-mode-hook 'flymake-ruby-load)

;; ruby shell inside emacs
(global-set-key (kbd "C-c r r") 'inf-ruby)

;; project management utilities
(projectile-global-mode)
(add-hook 'ruby-mode-hook 'projectile-on)

;; code assistance tool 
(require 'robe)
(add-hook 'ruby-mode-hook 'robe-mode)

;; 
(global-company-mode t)
(push 'company-robe company-backends)

(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)


;; Archive All DONE entries
(defun my-org-archive-done-tasks ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))


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


;; Configuring emacs for ruby
(require 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)
(setq ruby-deep-indent-paren nil)
(global-set-key (kbd "C-c r r") 'inf-ruby)
(projectile-global-mode)
;; (add-hook 'ruby-mode-hook 'projectile-on)
(require 'robe)
(add-hook 'ruby-mode-hook 'robe-mode)


(require 'ido)
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

;; Your path from shell needs to be seen by Emacs, and to make that easier, use the following elisp package.
(add-to-list 'load-path "~/.emacs.d/lisp")
(unless (require 'exec-path-from-shell nil 'noerror)
  (exec-path-from-shell-initialize))

;; ;; Emacs package for managing Django projects
;; (add-to-list 'load-path "~/.emacs.d/lisp")
;; (require 'python-django)

(add-to-list 'load-path "/Users/yhk/.emacs.d/ESS/lisp/")
(load "ess-site")

(setq scheme-root "/Applications/MIT-Scheme.app/Contents/Resources")

(setq scheme-program-name
      (concat
       scheme-root "/mit-scheme "
       "--library " scheme-root " "
       "--band " scheme-root "/all.com "
       "-heap 10000"))

(require 'xscheme)
(load "xscheme")

(setenv "MITSCHEME_LIBRARY_PATH"
	"/Applications/MIT-Scheme.app/Contents/Resources")
;; add color to emacs shell
    (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
    (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
