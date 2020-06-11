
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
