;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;

;; Whether display icons in the mode-line.
;; While using the server mode in GUI, should set the value explicitly.
(setq doom-modeline-icon t)

;; Whether display the icon for `major-mode'. It respects option `doom-modeline-icon'.
(setq doom-modeline-major-mode-icon t)

;; Whether display the colorful icon for `major-mode'.
;; It respects `nerd-icons-color-icons'.
(setq doom-modeline-major-mode-color-icon t)

;; Whether display the icon for the buffer state. It respects option `doom-modeline-icon'.
(setq doom-modeline-buffer-state-icon t)

;; Whether display the modification icon for the buffer.
;; It respects option `doom-modeline-icon' and option `doom-modeline-buffer-state-icon'.
(setq doom-modeline-buffer-modification-icon t)

;; Whether display the lsp icon. It respects option `doom-modeline-icon'.
(setq doom-modeline-lsp-icon t)


;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

;; my theme conf
(setq doom-theme 'catppuccin)
;; (setq doom-theme 'doom-badger)

(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 14 :weight 'light)
      doom-variable-pitch-font (font-spec :family "JetBrainsMono Nerd Font" :size 15)
      doom-big-font (font-spec :family "JetBrainsMono Nerd Font" :size 24))
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!

;; derek's config
(after! org
  (setq org-directory "~/Documents/org/"
        org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-ellipsis " ⤵ "
        org-superstar-headline-bullets-list '("◉" "●" "✸" "✿" "◆")
        ;; org-superstar-itembullet-alist '((?+ . ?➤) (?- . ?✦)) ; changes +/- symbols in item lists
        org-log-done 'time
        org-hide-emphasis-markers t
        ;; ex. of org-link-abbrev-alist in action
        ;; [[arch-wiki:Name_of_Page][Description]]
        org-link-abbrev-alist    ; This overwrites the default Doom org-link-abbrev-list
        '(("google" . "http://www.google.com/search?q=")
          ("arch-wiki" . "https://wiki.archlinux.org/index.php/")
          ("ddg" . "https://duckduckgo.com/?q=")
          ("wiki" . "https://en.wikipedia.org/wiki/"))
        ;; org-table-convert-region-max-lines 20000
        org-todo-keywords        ; This overwrites the default Doom org-todo-keywords
        '((sequence
           "TODO(t)"           ; A task that is ready to be tackled
           "BLOG(b)"           ; Blog writing assignments
           "GYM(g)"            ; Things to accomplish at the gym
           "PROJ(p)"           ; A project that contains other tasks
           "VIDEO(v)"          ; Video assignments
           "WAIT(w)"           ; Something is holding up this task
           "|"                 ; The pipe necessary to separate "active" states and "inactive" states
           "DONE(d)"           ; Task has been completed
           "CANCELLED(c)" )))) ; Task has been cancelled

(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.4))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.15))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.05))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.01))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.01))))
 )


;; (set-face-attribute 'org-list-dt nil :foreground "#ff00ff")
(custom-set-faces! '(org-list-dt :foreground "#F0A015"))

;; (doom/set-frame-opacity .95)
(set-frame-parameter (selected-frame) 'alpha '(88 96))
(add-to-list 'default-frame-alist '(alpha 88 96))

(setq left-margin-width 2)
(setq right-margin-width 2)

(custom-set-faces! '(cursor :background "#F0A015"))
(setq fancy-splash-image
      (concat doom-private-dir "images/"
              (nth (random 3) '("emacs.png"
                                "banner.png"
                                "doom-emacs-bw-light.svg"))))


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(map! :n "H" #'evil-first-non-blank)
(map! :n "L" #'evil-end-of-line)

;; Option 1: Per buffer
;; (after! org
;;   (add-hook 'org-mode-hook #'org-modern-mode)
;;   (add-hook 'org-agenda-finalize-hook #'org-modern-agenda))

;; org-roam
(after! org
  (setq org-roam-directory "~/Documents/org/roam/"
        org-roam-graph-viewer "brave"))

(map! :leader
      (:prefix ("n r" . "org-roam")
       :desc "Completion at point" "c" #'completion-at-point
       :desc "Find node"           "f" #'org-roam-node-find
       :desc "Show graph"          "g" #'org-roam-graph
       :desc "Insert node"         "i" #'org-roam-node-insert
       :desc "Capture to node"     "n" #'org-roam-capture
       :desc "Toggle roam buffer"  "r" #'org-roam-buffer-toggle))

(after! evil-escape
  (setq evil-escape-key-sequence "jj"))

(map! :n "C-f f" #'org-roam-node-find)
(map! :n "C-f r" #'org-roam-node-random)
(map! :n "C-f b" #'org-mark-ring-goto)

;; (add-to-list 'load-path "/usr/share/emacs/site-lisp/elpa-src/mu4e-1.8.14")
;; (set-email-account! "mail.ru"
;;                     '((mu4e-sent-folder       . "/mail.ru/sent")
;;                       (mu4e-drafts-folder     . "/mail.ru/drafts")
;;                       (mu4e-trash-folder      . "/mail.ru/trash")
;;                       (mu4e-refile-folder     . "/mail.ru/all")
;;                       (smtpmail-smtp-user     . "reptiloid666@mail.ru")
;;                       (user-mail-address      . "reptiloid666@mail.ru")    ;; only needed for mu < 1.4
;;                       (mu4e-compose-signature . "---\nKirill Konovalov"))
;;                     t)

(use-package! gptel
  :config
  (setq! gptel-api-key "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"))


;; Groq offers an OpenAI compatible API
(gptel-make-openai "Groq"               ;Any name you want
  :host "api.groq.com"
  :endpoint "/openai/v1/chat/completions"
  :stream t
  :key "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"
  :models '(llama-3.1-70b-versatile
            llama-3.1-8b-instant
            llama3-70b-8192
            llama3-8b-8192
            mixtral-8x7b-32768
            gemma-7b-it))

(use-package! visual-fill-column
  :custom
  (visual-fill-column-width 125)
  (visual-fill-column-center-text t)
  :hook (org-mode . visual-fill-column-mode))
