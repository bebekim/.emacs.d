;; Package system
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives
	       '("gnu" . "https://elpa.gnu.org/packages/") t)
  (add-to-list 'package-archives
	       '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives 
	       '("org" . "http://orgmode.org/elpa/") t) ;; ord-mode repository
  )

; Use "package" to install "use-package", a better package management and config system
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(defvar myPackages
  '(better-defaults
    material-theme
    company
    projectile
    web-mode
    emmet-mode
    virtualenvwrapper
    elpy
    flycheck
    py-autopep8
    ein
    neotree))


(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)

(setq tramp-default-method "ssh")

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
 global-visual-line-mode 1
 transient-mark-mode 1
 global-font-lock-mode 1
 )


;; buffer local variables
(setq-default
 indent-tabs-mode nil
 tab-width 2
 c-basic-offset 4)

;; themes
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(global-set-key (kbd "C-?") 'help-command)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-unset-key (kbd "C-z"))




(setq inhibit-startup-message t) ;; hide the startup message
(load-theme 'material t) ;; load material theme

;; scheme
(require 'xscheme)

(setenv "MITSCHEME_LIBRARY_PATH"
        "/Applications/MIT-Scheme.app/Contents/Resources")

(setq show-paren-delay 0
      show-paren-style 'parentheses)
(show-paren-mode 1)
(setq scheme-program-name "/usr/local/bin/mit-scheme")

;; python
(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable))



;; ;; Use IPython for REPL
;; (setq python-shell-interpreter "jupyter"
;;       python-shell-interpreter-args "console --simple-prompt"
;;       python-shell-prompt-detect-failure-warning nil)
;; (add-to-list 'python-shell-completion-native-disabled-interpreters
;;              "jupyter")


;; ;; Enable Flycheck
;; (when (require 'flycheck nil t)
;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;   (add-hook 'elpy-mode-hook 'flycheck-mode))

;; ;; Enable autopep8
;; (require 'py-autopep8)
;; (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)


;; set $MATHPATH, $PATH, exec-path from shell on linux and os X
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; -------
(defun create-shell ()
    "creates a shell with a given name"
    (interactive);; "Prompt\n shell name:")
    (let ((shell-name (read-string "shell name: " nil)))
    (shell (concat "*" shell-name "*"))))

;;------------------------
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'alt)
;; meta key from alt to cmd on Mac
;;(setq mac-option-modifier 'super)
;;(setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'meta)
;;(setq mac-command-modifier 'control)
;;(setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char)
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

;; --------
;; Shift the selected region right if distance is postive, left if
;; negative


(setq user-full-name "Youngha Kim")
(setq user-mail-address "yh.kim@outlook.com")

;; create windmove bindings
(global-set-key "\M-j" 'windmove-left)
(global-set-key "\M-l" 'windmove-right)
(global-set-key "\M-i" 'windmove-up)
(global-set-key "\M-k" 'windmove-down)

(defun shift-region (distance)
  (let ((mark (mark)))
    (save-excursion
      (indent-rigidly (region-beginning) (region-end) distance)
      (push-mark mark t t)
      ;; Tell the command loop not to deactivate the mark
      ;; for transient mark mode
      (setq deactivate-mark nil))))

(defun shift-right ()
  (interactive)
  (shift-region 1))

(defun shift-left ()
  (interactive)
  (shift-region -1))

;; Bind (shift-right) and (shift-left) function to your favorite keys. I use
;; the following so that Ctrl-Shift-Right Arrow moves selected text one 
;; column to the right, Ctrl-Shift-Left Arrow moves selected text one
;; column to the left:

(global-set-key [C-S-right] 'shift-right)
(global-set-key [C-S-left] 'shift-left)


;; Duplicate a whole line in vim 
;; https://stackoverflow.com/questions/73319/duplicate-a-whole-line-in-vim#73357
(defun duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")

  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))

  ;; local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion

      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))

      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; insert the line arg times
        (while (> count 0)
          (newline)         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count)))
        )

      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    ) ; end-of-let

  ;; put the point in the lowest line and return
  (next-line arg))

(global-set-key (kbd "\C-c\C-d") 'duplicate-line)


;; ;; -----
;; ;; gherkin
;; (require 'feature-mode)
;; (add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))
;; (setq feature-default-language "fi")
;; (setq feature-default-i18n-file "~/.emacs.d/lisp/cucumber.el/i18n.yml")

;; (add-to-list 'load-path "~/.emacs.d/lisp/gherkin-mode")
;; (require 'gherkin-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(custom-enabled-themes (quote (misterioso)))
 '(default-input-method "korean-hangul")
 '(fci-rule-color "#37474f")
 '(hl-sexp-background-color "#1c1f26")
 '(ledger-clear-whole-transactions t t)
 '(org-global-properties
   (quote
    (("Effort_ALL" . "00:05 00:10 00:20 00:30 00:45 01:00 02:00 03:00 04:00 05:00 06:00"))))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m)))
 '(org-tag-alist nil)
 '(org-todo-keywords
   (quote
    ((sequence "CAPTURE(c)" "ACTIONABLE(a)" "INCUBATE(i)" "|" "DELEGATE(g)" "CANCELLED(x)")
     (sequence "ORGANIZE(o)" "FOLLOWUP(f)" "REFLECT(r)")
     (sequence "TODO(t)" "|" "DONE(d)"))))
 '(package-selected-packages
   (quote
    (elpy csharp-mode flycheck-ledger ledger-mode pipenv magit-todos magit-org-todos all-the-icons py-autopep8 conda ein highlight-indentation rjsx-mode indium json-mode use-package anaconda-mode yasnippet cl-lib s eclim meghanada feature-mode neotree material-theme better-defaults docker-compose-mode docker virtualenvwrapper auto-complete js2-mode web-mode php-mode eide list-packages-ext helm-dash flymake-jslint flymake-css web-beautify emmet-mode magit racket-mode org-pomodoro projectile org markdown-mode exec-path-from-shell company)))
 '(scheme-program-name "mit-scheme")
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

;; Displaying compilation error messages in the echo area
(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)

;; Configuring auto-complete-mode
;; regular auto-complete initialization
(require 'auto-complete-config)
(ac-config-default)

;; magit
(global-set-key (kbd "C-x g") 'magit-status)

;;;;org-mode configuration
;; Enable org-mode
(require 'org)
(add-to-list 'load-path "/Users/yhk/.emacs.d/lisp")
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-hook 'org-mode-hook 'turn-on-font-lock)

;; The above is the default in recent emacs
(setq org-directory "~/Documents/org")
(setq org-log-done t)
(setq org-agenda-files (list "~/Documents/org/inbox.org"
                             "~/Documents/org/gtd.org" 
                             "~/Documents/org/someday.org"
			     "~/Documents/org/tickler.org"))
(setq org-default-notes-file "~/Documents/org/journal.org")
(setq org-capture-templates
      '(
        ("j" "Journal" entry (file+datetree "~/Documents/org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("l" "Link" entry (file+headline "~/Documents/org/link.org" "Links")
         "* %? %^L %^g \n%T" :prepend t)
        ("c" "Capture" entry (file+headline "~/Documents/org/inbox.org" "Tasks")
         "* CAPTURE %i%?")
        ("T" "Tickler" entry (file+headline "~/Documents/org/tickler.org" "Tickler")
         "* %i%? \n %U")))

(setq org-refile-targets '(("~/Documents/org/gtd.org" :maxlevel . 3)
                           ("~/Documents/org/link.org" :maxlevel . 3)
                           ("~/Documents/org/someday.org" :level . 2)
                           ("~/Documents/org/tickler.org" :maxlevel . 2)))


;; The following lines are always needed.  Choose your own keys.
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "<f5>") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "<f6>") 'org-capture)
(global-set-key (kbd "\C-cb") 'org-iswitchb)

;; archive done tasks
(defun org-archive-done-tasks()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-ap-continue-from (outline-previous-heading)))
  "/DONE" 'tree))

;; change font for done task
(setq org-fontify-done-headline t)

;; load markdown exporter automatically with org-mode
(eval-after-load "org"
  '(require 'ox-md nil t))

;; Refresh org-agenda after rescheduling a task.
(defun org-agenda-refresh ()
  "Refresh all `org-agenda' buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'org-agenda-mode)
        (org-agenda-maybe-redo)))))

(defadvice org-schedule (after refresh-agenda activate)
  "Refresh org-agenda."
  (org-agenda-refresh))

;; Log time a task was set to Done.
(setq org-log-done (quote time))

;; Don't log the time a task was rescheduled or redeadlined.
(setq org-log-redeadline nil)
(setq org-log-reschedule nil)

(setq org-read-date-prefer-future 'time)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (emacs-lisp . t)
   (python . t)
   ))



(setq org-confirm-babel-evaluate nil)
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)   
(add-hook 'org-mode-hook 'org-display-inline-images)

(setq org-babel-python-command "ipython --pylab=osx --pdb --nosep --classic --no-banner --no-confirm-exit")
(setenv "PYTHONPATH"    "/usr/local/bin")


(defun org-babel-python-strip-session-chars ()
  "Remove >>> and ... from a Python session output."
  (when (and (string=
              "python"
              (org-element-property :language (org-element-at-point)))
             (string-match
              ":session"
              (org-element-property :parameters (org-element-at-point))))

    (save-excursion
      (when (org-babel-where-is-src-block-result)
        (goto-char (org-babel-where-is-src-block-result))
        (end-of-line 1)
        ;(while (looking-at "[\n\r\t\f ]") (forward-char 1))
        (while (re-search-forward
                "\\(>>> \\|\\.\\.\\. \\|: $\\|: >>>$\\)"
                (org-element-property :end (org-element-at-point))
                t)
          (replace-match "")
          ;; this enables us to get rid of blank lines and blank : >>>
          (beginning-of-line)
          (when (looking-at "^$")
            (kill-line)))))))

(add-hook 'org-babel-after-execute-hook 'org-babel-python-strip-session-chars)



;; ess
(setq ess-smart-S-assign-key ":")
(ess-toggle-S-assign nil)
(ess-toggle-S-assign nil)
(ess-toggle-underscore nil) ; leave underscore key alone!


;; emacs-ide
;; (eide-start)

(projectile-global-mode)
(global-company-mode t)

;; ledger
(use-package ledger-mode
    :mode ("\\.dat\\'"
           "\\.ledger\\'")
    :custom (ledger-clear-whole-transactions t))
       
  (use-package flycheck-ledger :after ledger-mode)

;; (push 'company-robe company-backends)

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

;; ------------------
;; editing web templates

;; use web-mode for .jsx files
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; require flycheck
(require 'flycheck)


;; highlight indentation
(add-to-list 'load-path "~/.emacs.d/packages/highlight-indents/")

;; (require 'highlight-indentation)
;; (add-hook 'js2-mode-hook 'highlight-indentation-mode)
;; (add-hook 'python-mode-hook 'highlight-indentation-mode)

;; 
(require 'rjsx-mode)
;; for all of .js files
;; (add-to-list 'auto-mode-alist '("\\.jsx$" . rjsx-mode))

;; only for the js files under the components/containers folder
(add-to-list 'auto-mode-alist '("components\/.*\.js\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("containers\/.*\.js\'" . rjsx-mode))


(add-hook 'rjsx-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil) ;;Use space instead of tab
            (setq js-indent-level 2) ;;space width is 2 (default is 4)
            (setq js2-strict-missing-semi-warning nil))) ;;disable the semicolon warning


;;----

(defun indent-region-custom(numSpaces)
    (progn 
        ; default to start and end of current line
        (setq regionStart (line-beginning-position))
        (setq regionEnd (line-end-position))

        ; if there's a selection, use that instead of the current line
        (when (use-region-p)
            (setq regionStart (region-beginning))
            (setq regionEnd (region-end))
        )

        (save-excursion ; restore the position afterwards            
            (goto-char regionStart) ; go to the start of region
            (setq start (line-beginning-position)) ; save the start of the line
            (goto-char regionEnd) ; go to the end of region
            (setq end (line-end-position)) ; save the end of the line

            (indent-rigidly start end numSpaces) ; indent between start and end
            (setq deactivate-mark nil) ; restore the selected region
        )
    )
)

;;----


;; --------------------------------------------

;; python set up
;; Virtualenv wrapper
;; works with python.el, default on emacs 24.3 and up
;; (require 'virtualenvwrapper)
;; (venv-initialize-interactive-shells) ;; if you want interactive shell support
;; (venv-initialize-eshell) ;; if you want eshell support
;; ;; note that setting `venv-location` is not necessary if you
;; use the default location (`~/.virtualenvs`), or if the
;; the environment variable `WORKON_HOME` points to the right place

;; ;; enable anaconda-mode
;; (require 'conda)
;; (conda-env-initialize-interactive-shells)
;; (conda-env-initialize-eshell)
;; (conda-env-autoactivate-mode t)

;; (custom-set-variables
;;  '(conda-anaconda-home "~/anaconda3"))

;; (add-hook 'python-mode-hook 'anaconda-mode)
;; (add-hook 'python-mode-hook 'anaconda-eldoc-mode)


;; ---------------------------------------------
 

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-done ((t (:foreground "PaleGreen" :weight normal :strike-through t))))
 '(org-headline-done ((t (:foreground "LightSalmon" :strike-through t)))))


;; disable vc-mode so that git doesn't slow down Emacs opening
'(vc-handled-backends nil)

;; --------
;; OSX/docker-machine configuration

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))
;; Use "docker-machine env box" command to find out your environment variables
(setenv "DOCKER_TLS_VERIFY" "1")
(setenv "DOCKER_HOST" "tcp://10.11.12.13:2376")
(setenv "DOCKER_CERT_PATH" "/Users/foo/.docker/machine/machines/box")
(setenv "DOCKER_MACHINE_NAME" "box")

;; -------
;; nerd tree configuration
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

;; Every time when the neotree window is opened, let it find current file and jump to node.
(setq neo-smart-open t)

;; When running 'projectile-switch-project' (C-c p p), 'neotree' will change root automatically.
(setq projectile-switch-project-action 'neotree-projectile-action)
