;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-

;; init file proper begins here
;; package manager
(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))
(setq package-archive-priorities
      '(("gnu" . 10)
        ("nongnu" . 9)
        ("melpa" . 8)))
(add-hook 'package-menu-mode-hook 'hl-line-mode)
(package-initialize)

(defvar tramp-default-method)
(defvar use-package-always-ensure)
(defvar ruby-deep-indent-paren)
(defvar org-capture-templates)
(defvar org-state)
(defvar ledger-clear-whole-transactions)
(defvar js-indent-level)
(defvar js-jsx-indent-level)
(defvar typescript-ts-mode-indent-offset)
(defvar js2-strict-missing-semi-warning)
(defvar markdown-command)
(defvar markdown-command-needs-filename)
(defvar markdown-mode-map)
(defvar jiralib-url)
(defvar neo-theme)
(defvar neo-smart-open)
(defvar projectile-switch-project-action)
(defvar exec-path-from-shell-variables)
(defvar racket-program)
(defvar racket-mode-map)
(defvar racket-repl-mode-map)
(defvar geiser-racket-binary)
(defvar geiser-active-implementations)
(defvar geiser-default-implementation)
(defvar paredit-mode-map)

(declare-function uv-mode-auto-activate-hook "uv-mode")
(declare-function exec-path-from-shell-initialize "exec-path-from-shell")
(declare-function ac-config-default "auto-complete-config")
(declare-function racket-run-and-switch-to-repl "racket-repl")
(declare-function racket-send-definition "racket-repl")
(declare-function racket-send-last-sexp "racket-repl")
(declare-function racket-send-region "racket-repl")
(declare-function paredit-open-curly "paredit")
(declare-function paredit-close-curly "paredit")
(declare-function paredit-wrap-square "paredit")
(declare-function paredit-wrap-curly "paredit")
(declare-function rainbow-delimiters-mode "rainbow-delimiters")
(declare-function org-bullets-mode "org-bullets")
(declare-function eglot "eglot")
(declare-function treesit-install-language-grammar "treesit")
(declare-function markdown-do "markdown-mode")

(setq package-selected-packages
  '(better-defaults
    material-theme
    company
    projectile
    web-mode
    uv-mode
    pythonic
    org-jira
    emmet-mode
    flycheck
    ein
    neotree
    flymake-ruby
    exec-path-from-shell
    racket-mode
    paredit
    rainbow-delimiters
    geiser
    geiser-racket
    auto-complete
    rjsx-mode
    ledger-mode
    flycheck-ledger
    visual-fill-column
    highlight-indent-guides
    markdown-mode
    magit))

(unless (require 'use-package nil t)
  (package-refresh-contents)
  (package-install 'use-package)
  (require 'use-package))

(require 'package-vc)

;; PostgreSQL browser/editor.
(unless (package-installed-p 'pg)
  (package-vc-install "https://github.com/emarsden/pg-el" nil nil 'pg))
(unless (package-installed-p 'pgmacs)
  (package-vc-install "https://github.com/emarsden/pgmacs" nil nil 'pgmacs))
(require 'pgmacs)
(setq pgmacs-use-worker-thread nil)
(setq pgmacs-large-database-threshold 0)
(setq pgmacs-use-header-line nil)
(setq pgmacs-table-list-buttons nil)

;; Avoid native macOS Emacs crashes in font/SVG rendering while PGmacs builds buffers.
(defun my-pgmacs-no-svg-icon (&rest _args)
  "")

(advice-add 'pgmacs--maybe-svg-icon :override #'my-pgmacs-no-svg-icon)

(cl-defun my-pgmacs-make-badge-text (label &key color help-echo)
  (pgmacs--make-badge/text label :color color :help-echo help-echo))

(advice-add 'pgmacs--make-badge :override #'my-pgmacs-make-badge-text)

(defun my-pgmacs-display-table-name-plain (name)
  (pgmacs--display-identifier name))

(advice-add 'pgmacs--display-table-name :override #'my-pgmacs-display-table-name-plain)

(defvar my-pgmacs-min-visible-rows 8
  "Minimum number of rows PGmacs should fetch when opening a table.")

(defvar my-pgmacs-row-screen-padding 9
  "Approximate non-data rows in a PGmacs table buffer.")

(defun my-pgmacs-visible-row-limit ()
  "Return a PGmacs row limit sized for the current window."
  (max my-pgmacs-min-visible-rows
       (- (window-body-height nil t) my-pgmacs-row-screen-padding)))

(defun my-pgmacs-limit-display-table (orig-fun table &rest args)
  "Open TABLE with a row limit sized to the current window."
  (let ((pgmacs-row-limit (my-pgmacs-visible-row-limit)))
    (setq-default pgmacs-row-limit pgmacs-row-limit)
    (apply orig-fun table args)))

(advice-add 'pgmacs--display-table :around #'my-pgmacs-limit-display-table)

(defun my-pgmacs-select-rows-offset-limited (con table-name-escaped offset row-count)
  "Fetch PGmacs rows with an explicit SQL LIMIT and OFFSET."
  (let* ((row-count (max 1 row-count))
         (offset (max 0 offset)))
    (pcase (pgcon-server-variant con)
      ('questdb
       (pg-exec con (format "SELECT * FROM %s LIMIT %s,%s"
                            table-name-escaped offset (+ offset row-count))))
      (_
       (pg-exec con (format "SELECT * FROM %s LIMIT %s OFFSET %s"
                            table-name-escaped row-count offset))))))

(advice-add 'pgmacs--select-rows-offset :override #'my-pgmacs-select-rows-offset-limited)

(defun my-pgmacs-select-rows-where-limited (con table-name-escaped where-filter row-count)
  "Fetch filtered PGmacs rows with an explicit SQL LIMIT."
  (when (cl-search ";" where-filter)
    (user-error "WHERE filter must not contain end-of-statement marker ';'"))
  (let ((case-fold-search t))
    (when (cl-search "WHERE" where-filter :test #'char-equal)
      (user-error "WHERE filter should be specified without the WHERE keyword")))
  (pg-exec con (format "SELECT * FROM %s WHERE %s LIMIT %s"
                       table-name-escaped where-filter (max 1 row-count))))

(advice-add 'pgmacs--select-rows-where :override #'my-pgmacs-select-rows-where-limited)

(defun my-pgmacs-fast-row-count (_orig-fun _table)
  "Skip PGmacs' per-table row-count pass for large Docker datasets."
  nil)

(advice-add 'pgmacs--estimate-row-count :around #'my-pgmacs-fast-row-count)

(defun my-pgmacs-skip-table-owner (_orig-fun _con _table)
  "Skip PGmacs' per-table owner lookup during table-list rendering."
  nil)

(advice-add 'pg-table-owner :around #'my-pgmacs-skip-table-owner)

(defvar my-pgmacs-schema-filter nil
  "Schema to display in PGmacs, loaded from the project .env.")

(defun my-pgmacs-display-table-name (table)
  "Return TABLE as a display string for PGmacs."
  (if (pg-qualified-name-p table)
      (format "%s.%s" (pg-qualified-name-schema table) (pg-qualified-name-name table))
    table))

(defun my-pgmacs-list-tables-bulk ()
  "Return PGmacs table-list rows using one metadata query."
  (let* ((con pgmacs--con)
         (default-schema "public")
         (schema-filter (or my-pgmacs-schema-filter (getenv "PGMACS_SCHEMA")))
         (sql "SELECT
                 n.nspname,
                 c.relname,
                 pg_catalog.pg_total_relation_size(c.oid),
                 pg_catalog.pg_get_userbyid(c.relowner),
                 obj_description(c.oid, 'pg_class')
               FROM pg_catalog.pg_class c
               JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
               WHERE c.relkind IN ('r', 'p')
                 AND n.nspname NOT IN ('pg_catalog', 'information_schema')
                 AND ($1::text IS NULL OR n.nspname = $1)
               ORDER BY n.nspname, c.relname")
         (rows (pg-result (pg-exec-prepared con sql `((,schema-filter . "text"))) :tuples)))
    (cl-loop
     for row in rows
     for schema = (nth 0 row)
     for name = (nth 1 row)
     collect (list
              (if (string= schema default-schema)
                  name
                (make-pg-qualified-name :schema schema :name name))
              nil
              (nth 2 row)
              (nth 3 row)
              (or (nth 4 row) "")))))

(advice-add 'pgmacs--list-tables-full :override #'my-pgmacs-list-tables-bulk)

(require 'project)
(require 'subr-x)
(require 'url-parse)

(defun my-dotenv-unquote (value)
  "Return VALUE with simple dotenv quoting removed."
  (let ((value (string-trim value)))
    (if (and (>= (length value) 2)
             (or (and (string-prefix-p "\"" value) (string-suffix-p "\"" value))
                 (and (string-prefix-p "'" value) (string-suffix-p "'" value))))
        (substring value 1 -1)
      value)))

(defun my-dotenv-read-file (file)
  "Read dotenv FILE and return an alist of environment variables."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (let (vars)
        (dolist (line (split-string (buffer-string) "\n"))
          (when (string-match
                 "\\`[[:space:]]*\\(?:export[[:space:]]+\\)?\\([A-Za-z_][A-Za-z0-9_]*\\)[[:space:]]*=[[:space:]]*\\(.*\\)\\'"
                 line)
            (let* ((key (match-string 1 line))
                   (raw (replace-regexp-in-string "[[:space:]]+#.*\\'" "" (match-string 2 line)))
                   (value (my-dotenv-unquote raw)))
              (push (cons key value) vars))))
        (nreverse vars)))))

(defun my-project-dotenv-file ()
  "Return the current project's .env file, if one is available."
  (let* ((project (project-current nil))
         (root (and project (project-root project)))
         (project-env (and root (expand-file-name ".env" root))))
    (cond
     ((and project-env (file-readable-p project-env)) project-env)
     ((locate-dominating-file default-directory ".env")
      (expand-file-name ".env" (locate-dominating-file default-directory ".env"))))))

(defun my-pgmacs-env-from-database-url (url)
  "Return PG environment variables parsed from PostgreSQL URL."
  (when (and url (string-match-p "\\`postgres\\(?:ql\\)?://" url))
    (let* ((parsed (url-generic-parse-url url))
           (user (url-user parsed))
           (password (url-password parsed))
           (host (url-host parsed))
           (port (url-port parsed))
           (path (url-filename parsed))
           (dbname (and path (string-remove-prefix "/" path))))
      (delq nil
            (list
             (and dbname (not (string-empty-p dbname))
                  (cons "PGDATABASE" (url-unhex-string dbname)))
             (and host (cons "PGHOST" host))
             (and port (cons "PGPORT" (number-to-string port)))
             (and user (cons "PGUSER" (url-unhex-string user)))
             (and password (cons "PGPASSWORD" (url-unhex-string password))))))))

(defun my-pgmacs-normalize-dotenv (vars)
  "Return PGmacs-friendly environment entries from dotenv VARS."
  (let ((database-url (cdr (assoc "DATABASE_URL" vars)))
        pg-vars)
    (dolist (var `(("PGDATABASE" . ,(or (cdr (assoc "PGDATABASE" vars))
                                       (cdr (assoc "PGMACS_PGDATABASE" vars))
                                       (cdr (assoc "PGMACS_POSTGRES_DB" vars))
                                       (cdr (assoc "POSTGRES_DB" vars))
                                       (cdr (assoc "POSTGRES_DATABASE" vars))
                                       (cdr (assoc "POSTGRESQL_DATABASE" vars))
                                       (cdr (assoc "DB_NAME" vars))
                                       (cdr (assoc "DATABASE_NAME" vars))))
                   ("PGHOST" . ,(or (cdr (assoc "PGHOST" vars))
                                    (cdr (assoc "PGMACS_PGHOST" vars))
                                    (cdr (assoc "PGMACS_POSTGRES_HOST" vars))
                                    (cdr (assoc "POSTGRES_HOST" vars))
                                    (cdr (assoc "POSTGRES_HOSTNAME" vars))
                                    (cdr (assoc "POSTGRESQL_HOST" vars))
                                    (cdr (assoc "DB_HOST" vars))
                                    (cdr (assoc "DATABASE_HOST" vars))))
                   ("PGPORT" . ,(or (cdr (assoc "PGPORT" vars))
                                    (cdr (assoc "PGMACS_PGPORT" vars))
                                    (cdr (assoc "PGMACS_POSTGRES_PORT" vars))
                                    (cdr (assoc "POSTGRES_PORT" vars))
                                    (cdr (assoc "POSTGRES_PORT_NUMBER" vars))
                                    (cdr (assoc "POSTGRESQL_PORT" vars))
                                    (cdr (assoc "POSTGRESQL_PORT_NUMBER" vars))
                                    (cdr (assoc "DB_PORT" vars))
                                    (cdr (assoc "DATABASE_PORT" vars))))
                   ("PGUSER" . ,(or (cdr (assoc "PGUSER" vars))
                                    (cdr (assoc "PGMACS_PGUSER" vars))
                                    (cdr (assoc "PGMACS_POSTGRES_USER" vars))
                                    (cdr (assoc "POSTGRES_USER" vars))
                                    (cdr (assoc "POSTGRESQL_USERNAME" vars))
                                    (cdr (assoc "DB_USER" vars))
                                    (cdr (assoc "DATABASE_USER" vars))))
                   ("PGPASSWORD" . ,(or (cdr (assoc "PGPASSWORD" vars))
                                        (cdr (assoc "PGMACS_PGPASSWORD" vars))
                                        (cdr (assoc "PGMACS_POSTGRES_PASSWORD" vars))
                                        (cdr (assoc "POSTGRES_PASSWORD" vars))
                                        (cdr (assoc "POSTGRESQL_PASSWORD" vars))
                                        (cdr (assoc "DB_PASSWORD" vars))
                                        (cdr (assoc "DATABASE_PASSWORD" vars))))))
      (when (cdr var)
        (push var pg-vars)))
    (append
     vars
     (my-pgmacs-env-from-database-url database-url)
     (nreverse pg-vars))))

(defun my-env-alist-to-process-environment (vars)
  "Convert environment alist VARS to entries for `process-environment'."
  (delq nil
        (mapcar (lambda (var)
                  (when (cdr var)
                    (format "%s=%s" (car var) (cdr var))))
                vars)))

(defun my-pgmacs-with-project-dotenv (orig-fun &rest args)
  "Run ORIG-FUN with PostgreSQL defaults loaded from the project .env."
  (let* ((dotenv (my-project-dotenv-file))
         (vars (and dotenv (my-dotenv-read-file dotenv)))
         (process-environment
          (append (my-env-alist-to-process-environment
                   (my-pgmacs-normalize-dotenv vars))
                  process-environment)))
    (setq my-pgmacs-schema-filter (cdr (assoc "PGMACS_SCHEMA" vars)))
    (apply orig-fun args)))

(advice-add 'pgmacs :around #'my-pgmacs-with-project-dotenv)

;; Native Codex session buffers inside Emacs.
(unless (package-installed-p 'cond-let)
  (package-vc-install "https://github.com/tarsius/cond-let" nil nil 'cond-let))
(unless (package-installed-p 'transient '(0 9 0))
  (package-vc-install
   '(transient . (:url "https://github.com/magit/transient"
                 :lisp-dir "lisp"
                 :main-file "lisp/transient.el"))))
(unless (package-installed-p 'codex-ide)
  (package-vc-install "https://github.com/dgillis/emacs-codex-ide" nil nil 'codex-ide))
(require 'codex-ide)
(global-set-key (kbd "C-c C-;") 'codex-ide-menu)

(global-set-key (kbd "C-c g") 'goto-line)

(setq use-package-always-ensure nil)

(setq tramp-default-method "ssh")

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Use Emacs' own Korean input method so control/navigation keys stay commands.
(setq default-input-method "korean-hangul")
(global-set-key (kbd "C-\\") #'toggle-input-method)
(global-set-key (kbd "C-c \\") #'toggle-input-method)
(global-set-key (kbd "C-SPC") #'set-mark-command)

(setq
 inhibit-startup-screen t
 create-lockfiles nil
 make-backup-files nil
 column-number-mode t
 scroll-error-top-bottom t
 show-paren-delay 0.5
 sentence-end-double-space nil
 inhibit-startup-message t
 )

(global-display-line-numbers-mode 1)
(setq-default fill-column 80)
(setq-default display-fill-column-indicator-column 80)

(when (require 'visual-fill-column nil t)
  (setq-default visual-fill-column-width 80)
  (add-hook 'visual-line-mode-hook #'visual-fill-column-mode))

(global-visual-line-mode 1)
(global-display-fill-column-indicator-mode 1)
(dolist (hook '(text-mode-hook org-mode-hook markdown-mode-hook gfm-mode-hook))
  (add-hook hook #'turn-on-auto-fill))
(when (fboundp 'visual-fill-column-mode)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (setq fill-column 80
            display-fill-column-indicator-column 80
            visual-fill-column-width 80)
      (display-fill-column-indicator-mode 1)
      (when visual-line-mode
        (visual-fill-column-mode 1))
      (when (derived-mode-p 'text-mode 'org-mode 'markdown-mode 'gfm-mode)
        (auto-fill-mode 1)))))
(transient-mark-mode 1)
(global-font-lock-mode 1)
(global-auto-revert-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)


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
(when (member 'material (custom-available-themes))
  (load-theme 'material t))

;; Scheme, Racket, and SICP
(require 'xscheme nil t)

(setenv "MITSCHEME_LIBRARY_PATH"
        "/Applications/MIT-Scheme.app/Contents/Resources")

(setq show-paren-delay 0
      show-paren-style 'parentheses)
(show-paren-mode 1)
(setq scheme-program-name "/usr/local/bin/mit-scheme")

;; Racket
;; The macOS DMG installer does not put Racket on the PATH used by GUI Emacs.
(defvar my-racket-bin-dir "/Applications/Racket v9.1/bin")
(defvar my-racket-program (expand-file-name "racket" my-racket-bin-dir))
(defvar my-raco-program (expand-file-name "raco" my-racket-bin-dir))

(defun my-lisp-editing-mode ()
  "Enable structural editing helpers for Lisp-family buffers."
  (when (require 'paredit nil t)
    (paredit-mode 1))
  (when (require 'rainbow-delimiters nil t)
    (rainbow-delimiters-mode 1)))

(when (file-executable-p my-racket-program)
  (add-to-list 'exec-path my-racket-bin-dir)
  (setenv "PATH" (concat my-racket-bin-dir path-separator (getenv "PATH")))
  (setq racket-program my-racket-program
        geiser-racket-binary my-racket-program))

(add-to-list 'auto-mode-alist '("\\.sicp\\'" . racket-mode))
(add-to-list 'magic-mode-alist '("^#lang \\(racket\\|sicp\\)" . racket-mode))
(add-hook 'scheme-mode-hook #'my-lisp-editing-mode)

(defun my-sicp-new-buffer ()
  "Create a new unsaved SICP buffer using Racket's sicp language."
  (interactive)
  (switch-to-buffer (generate-new-buffer "sicp-exercise.rkt"))
  (racket-mode)
  (insert "#lang sicp\n\n")
  (save-excursion
    (insert ";; Exercise \n\n"))
  (my-lisp-editing-mode))

(with-eval-after-load 'racket-mode
  (add-hook 'racket-mode-hook #'racket-xp-mode)
  (add-hook 'racket-mode-hook #'my-lisp-editing-mode)
  (add-hook 'racket-repl-mode-hook #'my-lisp-editing-mode)
  (define-key racket-mode-map (kbd "<f5>") #'racket-run-and-switch-to-repl)
  (define-key racket-mode-map (kbd "C-c C-k") #'racket-run-and-switch-to-repl)
  (define-key racket-mode-map (kbd "C-c C-c") #'racket-send-definition)
  (define-key racket-mode-map (kbd "C-c C-e") #'racket-send-last-sexp)
  (define-key racket-mode-map (kbd "C-c C-r") #'racket-send-region))

(with-eval-after-load 'paredit
  (define-key paredit-mode-map (kbd "{") #'paredit-open-curly)
  (define-key paredit-mode-map (kbd "}") #'paredit-close-curly)
  (define-key paredit-mode-map (kbd "M-[") #'paredit-wrap-square)
  (define-key paredit-mode-map (kbd "M-{") #'paredit-wrap-curly)
  ;; Keep interactive REPL modes from losing their submit key.
  (dolist (key '("RET" "C-m" "C-j"))
    (define-key paredit-mode-map (kbd key) nil)))

(with-eval-after-load 'geiser
  (setq geiser-active-implementations '(racket)
        geiser-default-implementation 'racket))

;; python
(defvar python-shell-virtualenv-root)

(defun my-python-project-venv ()
  "Return the nearest uv-created .venv directory, or nil."
  (when-let ((root (locate-dominating-file default-directory ".venv")))
    (expand-file-name ".venv" root)))

(defun my-python-use-uv-venv ()
  "Use a project-local uv .venv for Python buffers when present."
  (when-let* ((venv (my-python-project-venv))
              (python (expand-file-name "bin/python" venv)))
    (when (file-executable-p python)
      (setq-local python-shell-interpreter python)
      (setq-local python-shell-virtualenv-root venv)
      (setq-local exec-path (cons (expand-file-name "bin" venv) exec-path)))))

(if (require 'uv-mode nil t)
    (add-hook 'python-mode-hook #'uv-mode-auto-activate-hook)
  (add-hook 'python-mode-hook 'my-python-use-uv-venv))

(when (require 'flymake-ruby nil t)
  (add-hook 'ruby-mode-hook 'flymake-ruby-load))

(setq ruby-deep-indent-paren nil)

(global-set-key (kbd "C-c r r") 'inf-ruby)
(global-set-key (kbd "C-c r a") 'rvm-activate-corresponding-ruby)



;; Keep GUI/daemon Emacs in sync with the shell environment on macOS.
(when (and (or (memq window-system '(mac ns x pgtk))
               (daemonp))
           (require 'exec-path-from-shell nil t))
  (dolist (var '("SSH_AUTH_SOCK" "LANG" "LC_CTYPE" "UV_CACHE_DIR" "UV_PROJECT_ENVIRONMENT"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

;; -------
(defun create-shell ()
    "creates a shell with a given name"
    (interactive);; "Prompt\n shell name:")
    (let ((shell-name (read-string "shell name: " nil)))
    (shell (concat "*" shell-name "*"))))

;;------------------------
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'alt
        ns-option-modifier 'alt)
;; Use Command as Meta in macOS GUI builds.
;;(setq mac-option-modifier 'super)
;;(setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'meta)
  (setq ns-command-modifier 'meta)
;;(setq mac-command-modifier 'control)
;;(setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char)
  ;; Emacs 30.2 macOS builds can crash in the NS font backend while resolving
  ;; explicit fontsets at startup. Keep startup on the system font path.
  (setq-default face-font-rescale-alist nil))

;;---------------------------
;; korean font
(defun xftp (&optional frame)
  "Return t if FRAME support XFT font backend."
  (let ((xft-supported))
    (mapc (lambda (x) (if (eq x 'xft) (setq xft-supported t)))
          (frame-parameter frame 'font-backend))
    xft-supported))

(when (and (not (eq system-type 'darwin)) (xftp))
  (let ((fontset "fontset-default"))
    (when (find-font (font-spec :name "DejaVu Sans Mono"))
      (set-fontset-font fontset 'latin
                        '("DejaVu Sans Mono" . "unicode-bmp")))
    (when (find-font (font-spec :name "NanumGothic"))
      (set-fontset-font fontset 'hangul
                        '("NanumGothic" . "unicode-bmp")))
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
  (forward-line arg))

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
 '(custom-enabled-themes '(misterioso))
 '(default-input-method "korean-hangul")
 '(fci-rule-color "#37474f")
 '(hl-sexp-background-color "#1c1f26")
 '(ledger-clear-whole-transactions t)
 '(org-agenda-files
   '("/Users/marcus.kim/Sync/org/inbox.org"
     "/Users/marcus.kim/Sync/org/gtd.org"
     "/Users/marcus.kim/Sync/org/someday.org"))
 '(org-global-properties
   '(("Effort_ALL"
      . "00:05 00:10 00:20 00:30 00:45 01:00 02:00 03:00 04:00 05:00 06:00")))
 '(org-modules '(org-habit))
 '(package-selected-packages
   '(auto-complete better-defaults company ein emmet-mode
                   exec-path-from-shell flycheck flycheck-ledger
                   flymake-ruby geiser geiser-racket highlight-indent-guides
                   ledger-mode magit markdown-mode material-theme neotree org-jira
                   paredit projectile pythonic racket-mode
                   rainbow-delimiters rjsx-mode uv-mode visual-fill-column
                   web-mode))
 '(scheme-program-name "mit-scheme")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#f36c60") (40 . "#ff9800") (60 . "#fff59d") (80 . "#8bc34a")
     (100 . "#81d4fa") (120 . "#4dd0e1") (140 . "#b39ddb")
     (160 . "#f36c60") (180 . "#ff9800") (200 . "#fff59d")
     (220 . "#8bc34a") (240 . "#81d4fa") (260 . "#4dd0e1")
     (280 . "#b39ddb") (300 . "#f36c60") (320 . "#ff9800")
     (340 . "#fff59d") (360 . "#8bc34a")))
 '(vc-annotate-very-old-color nil))

;; Displaying compilation error messages in the echo area
(require 'help-at-pt)
(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)

;; Configuring auto-complete-mode
;; regular auto-complete initialization
(when (require 'auto-complete-config nil t)
  (ac-config-default))

;; magit
(global-set-key (kbd "C-x g") 'magit-status)

;; markdown
(setq markdown-command '("pandoc" "--from=gfm" "--to=html5" "--standalone")
      markdown-command-needs-filename nil)

(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

(with-eval-after-load 'markdown-mode
  (define-key markdown-mode-map (kbd "C-c C-e") #'markdown-do))

;;;;org-mode configuration
;; Enable org-mode
(require 'org)
(require 'org-agenda)
(require 'ob-python)
(require 'org-element)
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-hook 'org-mode-hook 'turn-on-font-lock)
(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))
(add-hook 'org-mode-hook #'org-indent-mode)

(setq org-ellipsis "..."
      org-hide-leading-stars t
      org-startup-indented t
      org-indent-indentation-per-level 4
      org-hide-emphasis-markers t
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-src-window-setup 'current-window
      org-export-with-smart-quotes t
      org-html-postamble nil)

(when (require 'org-bullets nil t)
  (add-hook 'org-mode-hook #'org-bullets-mode))

(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))

;; GTD in Org: capture quickly, clarify later, and review from agenda views.
(setq org-directory (expand-file-name "~/Sync/org"))
(setq org-log-done t)
(setq initial-buffer-choice (expand-file-name "gtd.org" org-directory))
(setq org-default-notes-file (expand-file-name "inbox.org" org-directory))
(setq org-agenda-files
      (mapcar (lambda (file) (expand-file-name file org-directory))
              '("inbox.org"
                "gtd.org"
                "someday.org")))

(setq org-todo-keywords
      '((sequence "CAPTURE(c)" "CLARIFY(f)" "WAITING(w@/!)" "SOMEDAY(s)" "|" "DONE(d!)" "CANCELLED(x@)")))

(setq org-tag-alist
      '((:startgroup)
        ("@work" . ?w)
        ("@home" . ?h)
        ("@errand" . ?e)
        (:endgroup)
        ("computer" . ?c)
        ("phone" . ?p)
        ("reading" . ?r)
        ("blocked" . ?b)
        ("meeting" . ?m)
        ("deepwork" . ?d)
        ("admin" . ?a)
        ("critical" . ?x)
        ("followup" . ?f)))

(setq org-todo-keyword-faces
      '(("CAPTURE" . (:foreground "GoldenRod" :weight bold))
        ("CLARIFY" . (:foreground "DeepSkyBlue" :weight bold))
        ("WAITING" . (:foreground "DarkOrange" :weight bold))
        ("SOMEDAY" . (:foreground "gray70" :weight bold))
        ("DONE" . (:foreground "PaleGreen" :weight normal :strike-through t))
        ("CANCELLED" . (:foreground "LightSalmon" :weight normal :strike-through t))))

(setq org-tag-faces
      '(("@work" . (:foreground "LightSteelBlue" :weight bold))
        ("@home" . (:foreground "PaleGreen" :weight bold))
        ("@errand" . (:foreground "khaki" :weight bold))
        ("blocked" . (:foreground "OrangeRed" :weight bold))
        ("meeting" . (:foreground "yellow1" :weight bold))
        ("critical" . (:foreground "red1" :weight bold))
        ("followup" . (:foreground "DarkOrange" :weight bold))
        ("deepwork" . (:foreground "MediumPurple1" :weight bold))))

(setq org-capture-templates
      `(("c" "Capture" entry
         (file ,(expand-file-name "inbox.org" org-directory))
         "* CAPTURE %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n%a")
        ("f" "Clarify" entry
         (file+headline ,(expand-file-name "gtd.org" org-directory) "Clarify")
         "* CLARIFY %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n%a")
        ("s" "Someday" entry
         (file ,(expand-file-name "someday.org" org-directory))
         "* SOMEDAY %?\n:PROPERTIES:\n:CREATED: %U\n:END:")
        ("j" "Journal" entry
         (file+datetree ,(expand-file-name "journal.org" org-directory))
         "* %?\nEntered on %U\n%i\n%a")
        ("w" "Work log" entry
         (file+datetree ,(expand-file-name "work-log.org" org-directory))
         "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:"
         :empty-lines 0)
        ("l" "Link" entry
         (file+headline ,(expand-file-name "link.org" org-directory) "Links")
         "* %? %^L %^g\n%T" :prepend t)))

(setq org-refile-targets
      `((,(expand-file-name "gtd.org" org-directory) :maxlevel . 3)
        (,(expand-file-name "someday.org" org-directory) :maxlevel . 3)
        (,(expand-file-name "link.org" org-directory) :maxlevel . 3)))

(defun my/org-agenda-skip-if-no-deadline ()
  "Skip current subtree unless it has an Org DEADLINE."
  (unless (org-entry-get nil "DEADLINE")
    (or (outline-next-heading) (point-max))))

(defun my/org-agenda-skip-if-deadline ()
  "Skip current subtree if it has an Org DEADLINE."
  (when (org-entry-get nil "DEADLINE")
    (or (outline-next-heading) (point-max))))

(setq org-agenda-custom-commands
      '(("g" "GTD"
         ((agenda ""
                  ((org-agenda-overriding-header "Due Dates / Calendar")
                   (org-agenda-span 14)
                   (org-deadline-warning-days 365)
                   (org-agenda-sorting-strategy '(deadline-up time-up priority-down))))
          (tags-todo "TODO=\"CAPTURE\"|TODO=\"CLARIFY\"|TODO=\"WAITING\"|TODO=\"SOMEDAY\""
                     ((org-agenda-overriding-header "Dated Work")
                      (org-agenda-skip-function #'my/org-agenda-skip-if-no-deadline)
                      (org-agenda-sorting-strategy '(deadline-up priority-down category-keep))))
          (tags-todo "TODO=\"CAPTURE\"|TODO=\"CLARIFY\"|TODO=\"WAITING\"|TODO=\"SOMEDAY\""
                     ((org-agenda-overriding-header "No Due Date")
                      (org-agenda-skip-function #'my/org-agenda-skip-if-deadline)
                      (org-agenda-sorting-strategy '(priority-down category-keep))))))
        ("f" "Clarify" todo "CLARIFY")
        ("i" "Inbox" todo "CAPTURE")
        ("w" "Waiting" todo "WAITING")))


;; The following lines are always needed.  Choose your own keys.
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "<f5>") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "<f6>") 'org-capture)
(global-set-key (kbd "\C-cb") 'org-switchb)

;; File inbox tasks out of the inbox as soon as they are clarified.
(defun my/org-state-target-file ()
  "Return the filing target for the current Org state."
  (cond
   ((member org-state '("CLARIFY" "WAITING"))
    (expand-file-name "gtd.org" org-directory))
   ((string= org-state "SOMEDAY")
    (expand-file-name "someday.org" org-directory))
   ((string= org-state "DONE")
    (expand-file-name "done.org" org-directory))
   ((string= org-state "CANCELLED")
    (expand-file-name "archive.org" org-directory))
   (t nil)))

(defun my/org-ensure-completed-task-file (target-file title)
  "Create TARGET-FILE with TITLE when it does not exist."
  (unless (file-exists-p target-file)
    (make-directory (file-name-directory target-file) t)
    (write-region (format "#+TITLE: %s\n\n" title) nil target-file nil 'silent)))

(defun my/org-file-title-for-state ()
  "Return a file title for the current Org state."
  (cond
   ((string= org-state "DONE") "Done")
   ((string= org-state "CANCELLED") "Archive")
   ((string= org-state "SOMEDAY") "Someday")
   (t "GTD")))

(defun my/org-heading-for-state ()
  "Return the target heading for the current Org state."
  (cond
   ((string= org-state "CLARIFY") "Clarify")
   ((string= org-state "WAITING") "Waiting For")
   ((string= org-state "SOMEDAY") "Someday / Maybe")
   (t nil)))

(defun my/org-shift-subtree-to-level (subtree target-level)
  "Return SUBTREE with its top heading shifted to TARGET-LEVEL."
  (with-temp-buffer
    (insert subtree)
    (goto-char (point-min))
    (when (looking-at "^\\(\\*+\\) ")
      (let* ((current-level (length (match-string 1)))
             (delta (- target-level current-level)))
        (while (re-search-forward "^\\(\\*+\\) " nil t)
          (let* ((stars (match-string 1))
                 (new-level (max 1 (+ (length stars) delta))))
            (replace-match (concat (make-string new-level ?*) " ") t t)))))
    (buffer-string)))

(defun my/org-ensure-target-heading (target-file title heading)
  "Ensure TARGET-FILE exists and has top-level HEADING."
  (my/org-ensure-completed-task-file target-file title)
  (with-current-buffer (find-file-noselect target-file)
    (goto-char (point-min))
    (unless (re-search-forward (format "^\\* %s$" (regexp-quote heading)) nil t)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert (format "* %s\n" heading)))
    (save-buffer)))

(defun my/org-append-subtree-under-heading (target-file title heading subtree)
  "Append SUBTREE as a child of HEADING in TARGET-FILE."
  (my/org-ensure-target-heading target-file title heading)
  (with-current-buffer (find-file-noselect target-file)
    (goto-char (point-min))
    (unless (re-search-forward (format "^\\* %s$" (regexp-quote heading)) nil t)
      (error "Could not find target heading: %s" heading))
    (org-back-to-heading t)
    (org-end-of-subtree t t)
    (unless (bolp) (insert "\n"))
    (insert "\n" (my/org-shift-subtree-to-level subtree 2))
    (unless (string-suffix-p "\n" subtree)
      (insert "\n"))
    (save-buffer)))

(defun my/org-completed-date-components ()
  "Return completion date as (YEAR MONTH DAY)."
  (let* ((closed (org-entry-get nil "CLOSED"))
         (time (if closed
                   (org-time-string-to-time closed)
                 (current-time))))
    (list (format-time-string "%Y" time)
          (format-time-string "%Y-%m %B" time)
          (format-time-string "%Y-%m-%d %A" time))))

(defun my/org-ensure-date-tree (target-file date-components)
  "Ensure TARGET-FILE has YEAR / MONTH / DAY headings for DATE-COMPONENTS."
  (with-current-buffer (find-file-noselect target-file)
    (let ((year (nth 0 date-components))
          (month (nth 1 date-components))
          (day (nth 2 date-components)))
      (goto-char (point-min))
      (unless (re-search-forward (format "^\\* %s$" (regexp-quote year)) nil t)
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert (format "* %s\n" year)))
      (org-back-to-heading t)
      (let ((year-end (save-excursion (org-end-of-subtree t t) (point))))
        (unless (re-search-forward (format "^\\*\\* %s$" (regexp-quote month)) year-end t)
          (goto-char year-end)
          (unless (bolp) (insert "\n"))
          (insert (format "** %s\n" month))))
      (org-back-to-heading t)
      (let ((month-end (save-excursion (org-end-of-subtree t t) (point))))
        (unless (re-search-forward (format "^\\*\\*\\* %s$" (regexp-quote day)) month-end t)
          (goto-char month-end)
          (unless (bolp) (insert "\n"))
          (insert (format "*** %s\n" day))))
      (save-buffer))))

(defun my/org-append-subtree-under-date (target-file date-components subtree)
  "Append SUBTREE under TARGET-FILE's DATE-COMPONENTS day heading."
  (my/org-ensure-date-tree target-file date-components)
  (with-current-buffer (find-file-noselect target-file)
    (let ((day (nth 2 date-components)))
      (goto-char (point-min))
      (unless (re-search-forward (format "^\\*\\*\\* %s$" (regexp-quote day)) nil t)
        (error "Could not find completed-task date heading: %s" day))
      (org-back-to-heading t)
      (org-end-of-subtree t t)
      (unless (bolp) (insert "\n"))
      (insert "\n" (my/org-shift-subtree-to-level subtree 4))
      (unless (string-suffix-p "\n" subtree)
        (insert "\n"))
      (save-buffer))))

(defun my/org-file-task-after-state-change ()
  "File tasks into the bucket or history file for their Org state."
  (let ((target-file (my/org-state-target-file)))
    (when (and target-file
               buffer-file-name)
      (let ((title (my/org-file-title-for-state)))
        (my/org-ensure-completed-task-file target-file title)
        (save-excursion
          (org-back-to-heading t)
          (let* ((start (point))
                 (end (save-excursion
                        (org-end-of-subtree t t)
                        (point)))
                 (date-components (my/org-completed-date-components))
                 (subtree (buffer-substring start end))
                 (target-heading (my/org-heading-for-state))
                 (same-file (string= (file-truename buffer-file-name)
                                     (file-truename target-file)))
                 (already-filed
                  (and same-file
                       target-heading
                       (save-excursion
                         (org-up-heading-safe)
                         (string= (org-get-heading t t t t) target-heading)))))
            (unless already-filed
              (if same-file
                  (progn
                    (delete-region start end)
                    (save-buffer)
                    (if (member org-state '("DONE" "CANCELLED"))
                        (my/org-append-subtree-under-date target-file date-components subtree)
                      (my/org-append-subtree-under-heading
                       target-file title target-heading subtree)))
                (if (member org-state '("DONE" "CANCELLED"))
                    (my/org-append-subtree-under-date target-file date-components subtree)
                  (my/org-append-subtree-under-heading
                   target-file title target-heading subtree))
                (delete-region start end))
              (when (fboundp 'org-agenda-refresh)
                (org-agenda-refresh)))))))))

(add-hook 'org-after-todo-state-change-hook #'my/org-file-task-after-state-change)

(defun my/org-current-jira-key ()
  "Return the Jira key for the current Org subtree when available."
  (or (org-entry-get nil "JIRA_KEY")
      (save-excursion
        (org-back-to-heading t)
        (when (looking-at ".*\\b\\([A-Z][A-Z0-9]+-[0-9]+\\)\\b")
          (match-string-no-properties 1)))))

(defun my/org-jira-key-from-string (value)
  "Return the first Jira key found in VALUE."
  (when (string-match "\\b\\([A-Z][A-Z0-9]+-[0-9]+\\)\\b" value)
    (match-string 1 value)))

(defun my/org-find-jira-task (jira-key)
  "Open the Org file containing JIRA-KEY and move point to its heading."
  (let ((files (directory-files org-directory t "\\.org\\'"))
        found)
    (while (and files (not found))
      (let ((file (pop files)))
        (with-current-buffer (find-file-noselect file)
          (goto-char (point-min))
          (when (re-search-forward
                 (format "^\\s-*:JIRA_KEY:\\s-+%s\\s-*$" (regexp-quote jira-key))
                 nil t)
            (org-back-to-heading t)
            (setq found (current-buffer))))))
    (unless found
      (error "Could not find Jira key in org files: %s" jira-key))
    (switch-to-buffer found)
    (org-back-to-heading t)))

(defun my/org-add-pr-waiting-note (pr-url)
  "Add PR metadata and waiting note to the current Org subtree."
  (org-set-property "PR_URL" pr-url)
  (org-set-property "WAITING_ON" "PR review / merge")
  (save-excursion
    (org-end-of-meta-data t)
    (unless (save-excursion
              (save-restriction
                (org-narrow-to-subtree)
                (goto-char (point-min))
                (search-forward pr-url nil t)))
      (insert
       (format
        "\n- PR: [[%s][Bitbucket PR]]\n- After merge, verify the next scheduled run or expected acceptance check succeeds.\n"
        pr-url)))))

(defun my/org-file-current-subtree-under-heading (target-file title heading)
  "Move current subtree to HEADING in TARGET-FILE, including same-file moves."
  (let (subtree)
    (save-excursion
      (org-back-to-heading t)
      (let ((start (point))
            (end (save-excursion
                   (org-end-of-subtree t t)
                   (point))))
        (setq subtree (buffer-substring start end))
        (delete-region start end)
        (save-buffer)))
    (my/org-append-subtree-under-heading target-file title heading subtree)))

(defun my/org-submit-pr-url (pr-url &optional jira-key)
  "Attach PR-URL to a Jira task, mark it WAITING, and file it under Waiting For."
  (interactive
   (let* ((url (read-string "PR URL: "))
          (detected (or (my/org-jira-key-from-string url)
                        (when (derived-mode-p 'org-mode)
                          (my/org-current-jira-key))))
          (key (read-string "Jira key: " detected)))
     (list url key)))
  (unless (and jira-key (not (string-empty-p jira-key)))
    (error "Jira key is required"))
  (my/org-find-jira-task jira-key)
  (my/org-add-pr-waiting-note pr-url)
  (let ((org-after-todo-state-change-hook nil))
    (org-todo "WAITING"))
  (my/org-file-current-subtree-under-heading
   (expand-file-name "gtd.org" org-directory) "GTD" "Waiting For")
  (message "Filed %s as WAITING on PR review: %s" jira-key pr-url))

(global-set-key (kbd "C-c p") #'my/org-submit-pr-url)

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

(advice-add 'org-schedule :after
            (lambda (&rest _args)
              (org-agenda-refresh)))

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
   (shell . t)
   ))




(defun my-org-babel-racket--body-with-lang (body params)
  "Return BODY with a #lang line when the block does not already have one."
  (if (string-match-p "\\`[[:space:]\n]*#lang[[:space:]]+" body)
      body
    (let ((lang (or (cdr (assq :lang params)) "sicp")))
      (concat "#lang " lang "\n" body))))

(defun org-babel-execute:racket (body params)
  "Execute a Racket Org Babel block by running a temporary .rkt file.
This is a small local alternative to ob-racket. It supports normal and
inline `src_racket' blocks, uses the block's :lang header when present,
and otherwise defaults to #lang sicp."
  (let* ((program (or (and (boundp 'racket-program) racket-program)
                      (executable-find "racket")
                      "racket"))
         (source (my-org-babel-racket--body-with-lang body params))
         (file (make-temp-file "org-babel-racket-" nil ".rkt"))
         (command (format "%s %s"
                          (shell-quote-argument program)
                          (shell-quote-argument file))))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert source)
            (unless (string-suffix-p "\n" source)
              (insert "\n")))
          (org-babel-eval command ""))
      (when (file-exists-p file)
        (delete-file file)))))

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
(add-to-list 'load-path "~/.emacs.d/ess/lisp")

;; emacs-ide
;; (eide-start)

(when (fboundp 'projectile-global-mode)
  (projectile-global-mode))
(when (fboundp 'global-company-mode)
  (global-company-mode t))

;; ledger
(when (require 'ledger-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.dat\\'" . ledger-mode))
  (add-to-list 'auto-mode-alist '("\\.ledger\\'" . ledger-mode))
  (setq ledger-clear-whole-transactions t)
  (require 'flycheck-ledger nil t))

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
(when (require 'web-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

;; require flycheck
(require 'flycheck nil t)

;; YAML
(use-package yaml-ts-mode
  :ensure nil
  :mode (("\\.yaml\\'" . yaml-ts-mode)
         ("\\.yml\\'" . yaml-ts-mode)))

(when (require 'flycheck nil t)
  (dolist (hook '(yaml-mode-hook yaml-ts-mode-hook))
    (add-hook hook #'flycheck-mode)))

(use-package highlight-indent-guides
  :ensure t
  :hook ((yaml-mode yaml-ts-mode) . highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-auto-enabled nil)
  :config
  (set-face-foreground 'highlight-indent-guides-character-face "#5f6875")
  (set-face-foreground 'highlight-indent-guides-top-character-face "#7f8895")
  (set-face-foreground 'highlight-indent-guides-stack-character-face "#7f8895"))

;; Small JavaScript/TypeScript setup. Use built-in tree-sitter modes on
;; Emacs 29+; install grammars with M-x my-install-js-ts-grammars.
(when (fboundp 'js-ts-mode)
  (add-to-list 'auto-mode-alist '("\\.m?js\\'" . js-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.cjs\\'" . js-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js-ts-mode)))

(when (fboundp 'typescript-ts-mode)
  (add-to-list 'auto-mode-alist '("\\.mts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.cts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode)))

(when (fboundp 'tsx-ts-mode)
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode)))

(setq-default js-indent-level 2
              js-jsx-indent-level 2
              typescript-ts-mode-indent-offset 2)

(with-eval-after-load 'treesit
  (dolist (grammar
           '((javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
             (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
             (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
             (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
    (add-to-list 'treesit-language-source-alist grammar)))

(defun my-install-js-ts-grammars ()
  "Install tree-sitter grammars for JavaScript, TypeScript, TSX, and YAML."
  (interactive)
  (require 'treesit)
  (dolist (lang '(javascript typescript tsx yaml))
    (unless (treesit-language-available-p lang)
      (treesit-install-language-grammar lang))))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((js-mode js-ts-mode js-jsx-mode typescript-ts-mode tsx-ts-mode)
                 . ("typescript-language-server" "--stdio"))))

(defun my-js-ts-eglot ()
  "Start TypeScript language support for the current project."
  (interactive)
  (if (executable-find "typescript-language-server")
      (eglot)
    (user-error "Install with: npm install -g typescript-language-server typescript")))

(defun my-js-ts-mode-setup ()
  "Small shared defaults for JavaScript and TypeScript buffers."
  (setq-local indent-tabs-mode nil)
  (local-set-key (kbd "C-c l") #'my-js-ts-eglot))

(dolist (hook '(js-mode-hook js-ts-mode-hook js-jsx-mode-hook
                typescript-ts-mode-hook tsx-ts-mode-hook))
  (add-hook hook #'my-js-ts-mode-setup))


;; highlight indentation
(add-to-list 'load-path "~/.emacs.d/packages/highlight-indents/")

;; (require 'highlight-indentation)
;; (add-hook 'js2-mode-hook 'highlight-indentation-mode)
;; (add-hook 'python-mode-hook 'highlight-indentation-mode)

;; 
(require 'rjsx-mode nil t)
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

(defun indent-region-custom (num-spaces)
  (let ((region-start (line-beginning-position))
        (region-end (line-end-position)))
    (when (use-region-p)
      (setq region-start (region-beginning)
            region-end (region-end)))
    (save-excursion
      (goto-char region-start)
      (let ((start (line-beginning-position)))
        (goto-char region-end)
        (indent-rigidly start (line-end-position) num-spaces)
        (setq deactivate-mark nil)))))

;;----


;; --------------------------------------------




;; ---------------------------------------------

(setq jiralib-url "https://yhakim.atlassian.net")

;; ------------------

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-done ((t (:foreground "PaleGreen" :weight normal :strike-through t))))
 '(org-headline-done ((t (:foreground "LightSalmon" :strike-through t)))))

(when (facep 'fill-column-indicator)
  (set-face-attribute 'fill-column-indicator nil :foreground "#7f8895"))


;; Keep Git available for package-vc/archive metadata. Disabling every VC
;; backend makes package archives warn that their Git backend is missing.
(setq vc-handled-backends '(Git))

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
(unless (package-installed-p 'neotree)
  (package-refresh-contents)
  (package-install 'neotree))

(when (require 'neotree nil t)
  (setq neo-window-position 'left)
  (setq neo-theme 'arrow)

  ;; Every time when the neotree window is opened, let it find current file and jump to node.
  (setq neo-smart-open t)

  ;; When running 'projectile-switch-project' (C-c p p), 'neotree' will change root automatically.
  (setq projectile-switch-project-action 'neotree-projectile-action)

  (global-set-key (kbd "<f8>") 'neotree-toggle))
