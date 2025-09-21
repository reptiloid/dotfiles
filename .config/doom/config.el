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
;; (setq doom-theme 'leuven)

(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 14 :weight 'light)
      doom-variable-pitch-font (font-spec :family "JetBrainsMono Nerd Font" :size 13)
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
        org-attach-directory "~/Documents/org/.attach"
        org-attach-id-dir "~/Documents/org/.attach"
        org-id-locations-file "~/Documents/org/.orgids"
        ;; org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-default-notes-file "~/Documents/org/Agenda/Inbox.org"
        ;; org-ellipsis " ⤵ "
        ;; org-ellipsis "⋮"
        org-ellipsis "…"
        org-superstar-headline-bullets-list '("⁖" "‣" "◉" "●" "∘" "⊙"  "❂" "○" "✸")
        ;; org-superstar-itembullet-alist '((?+ . ?➤) (?- . ?✦)) ; changes +/- symbols in item lists
        org-log-done 'time
        org-agenda-start-with-log-mode t
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
           "PROJ(p)"           ; A project that contains other tasks
           "WAIT(w)"           ; Something is holding up this task
           "|"                 ; The pipe necessary to separate "active" states and "inactive" states
           "DONE(d)"           ; Task has been completed
           ))))

(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.325))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.05))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.02 :foreground "#77D0E7"))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.01))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.01))))
 '(org-level-6 ((t (:inherit outline-6 :height 1.01))))
 '(org-level-7 ((t (:inherit outline-7 :height 1.01))))
 )

(setq calendar-week-start-day 1)

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
              (nth (random 2) '(
                                "emacs.svg"
                                ;; "emacs.png"
                                ;; "banner.png"
                                "logo-mini-doom.png"
                                ;; "doom-emacs-bw-light.svg"
                                ))))


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

(map! :n "M-z" #'+vterm/toggle)
(map! :n "M-q" #'kill-current-buffer)
(map! :n "M-n" #'next-buffer)
(map! :n "M-p" #'previous-buffer)

;; Option 1: Per buffer
;; (after! org
;;   (add-hook 'org-mode-hook #'org-modern-mode)
;;   (add-hook 'org-agenda-finalize-hook #'org-modern-agenda))
;; org-roam



(require 'org-download)

;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)

(after! org-download
   (setq org-download-method 'directory))

(after! org
  (setq-default org-download-image-dir "img/"
        org-download-heading-lvl nil))


(after! org
  (setq org-roam-directory "~/Documents/org/roam/"
        org-roam-db-location "~/Documents/org/roam/org-roam.db"
        org-roam-graph-viewer "brave")

;; (setq-default org-download-image-dir "~/Pictures/doom/downloads")

  (custom-set-faces!
        '(org-agenda-date :inherit outline-1 :height 1.15)
        '(org-agenda-date-today :inherit outline-2 :height 1.15)
        '(org-agenda-date-weekend :inherit outline-1 :height 1.15)
        '(org-agenda-date-weekend-today :inherit outline-2 :height 1.15)
        '(org-super-agenda-header :inherit custom-button :weight bold :height 1.05)
        '(org-link :foreground unspecified))


   (setq org-roam-capture-templates
         '(("d" "default" plain
             "%?"
             :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n<t")
             :unnarrowed t)

            ("b" "book" plain
             "\n* Source\n\nAuthor: %^{Author}\nTitle: ${title}\nYear: %^{Year}\n\n* Summary\n\n%?"
             :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
             :unnarrowed t)

            ("n" "notes" plain (file "~/Documents/org/roam/Templates/NoteTemplate.org")
             :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
             :unnarrowed t)
        )
    )

   (setq org-agenda-skip-function-global '(org-agenda-skip-entry-if 'todo 'done))

   (setq org-agenda-span 1
         org-agenda-start-day "+0d")

    (setq org-agenda-custom-commands
        '(("p" "Planning"
                ((tags-todo "+@planning"
                                ((org-agenda-overriding-header "Planning Tasks")))
                (tags-todo "-{.*}"
                                ((org-agenda-overriding-header "Untagged Tasks")))
                (todo ".*" ((org-agenda-files '("~/Documents/org/Agenda/Inbox.org"))
                                (org-agenda-overriding-header "Unprocessed Inbox Items")))))

          ("d" "Daily Agenda"
                ((agenda "" ((org-agenda-span 'day)
                        (org-deadline-warning-days 7)))
                (tags-todo "+PRIORITY=\"A\""
                        ((org-agenda-overriding-header "High Priority Tasks")))))

          ("w" "Weekly Review"
                ((agenda ""
                        ((org-agenda-overriding-header "Completed Tasks")
                        (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo 'done))
                        (org-agenda-span 'week)))

                (agenda ""
                        ((org-agenda-overriding-header "Unfinished Scheduled Tasks")
                        (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                        (org-agenda-span 'week)))))))
)



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

;; org agenda
(setq org-tag-alist
      '(;;Places
        ("@home" . ?H)
        ("@work" . ?W)

        ;; Activities
        ("@health" . ?h)
        ("@investing" . ?i)
        ("@learning" . ?l)
        ("@youtube" . ?y)
        ("@planing" . ?n)
        ("@programming" . ?p)
        ("@writing" . ?w)
        ("@shopping" . ?s)
        ("@message" . ?m)
        ("@call" . ?a)))

(setq org-agenda-files '("~/Documents/org/Agenda/Personal.org"
                         "~/Documents/org/Agenda/Projects.org"
                         "~/Documents/org/Agenda/Work.org"))

(setq olivetti-style 'fancy
      olivetti-margin-width 120)
(setq-default olivetti-body-width 120)

;; Function to be run when org-agenda is opened
(defun org-agenda-open-hook ()
  "Hook to be run when org-agenda is opened"
  (olivetti-mode))
;; Adds hook to org agenda mode, making follow mode active in org agenda
(add-hook 'org-agenda-mode-hook 'org-agenda-open-hook)


;; (setq org-agenda-category-icon-alist
;;       `(("Work" ,(list (all-the-icons-faicon "graduation-cap" :height 0.8)) nil nil :ascent center)
;;         ("Inbox" ,(list (all-the-icons-faicon "home" :v-adjust 0.005)) nil nil :ascent center)
;;         ("Personal" ,(list (all-the-icons-faicon "youtube-play" :height 0.9)) nil nil :ascent center)
;;         ("Projects" ,(list (all-the-icons-faicon "music" :height 0.9)) nil nil :ascent center)
;;         ;; ("Stories.s" ,(list (all-the-icons-faicon "book" :height 0.9)) nil nil :ascent center)
;;         ;; ("Author.p" ,(list (all-the-icons-faicon "pencil" :height 0.9)) nil nil :ascent center)
;;         ;; ("Gamedev.s" ,(list (all-the-icons-faicon "gamepad" :height 0.9)) nil nil :ascent center)
;;         ;; ("Knowledge.p" ,(list (all-the-icons-faicon "database" :height 0.8)) nil nil :ascent center)
;;         ;; ("Personal.p" ,(list (all-the-icons-material "person" :height 0.9)) nil nil :ascent center)
;; ))


;; llms

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

;; (use-package! tramp-yadm
;;   :defer t
;;   :init
;;   (defun yadm-status ()
;;     "Invoke magit on the yadm repo"
;;     (interactive)
;;     (magit-status "/yadm::~")
;;     (setq-local magit-git-executable (executable-find "yadm"))
;;     (setq-local magit-remote-git-executable (executable-find "yadm")))

;;   ;; (after! magit
;;   ;;   (tramp-yadm-register)
;;   ;;   (map! :leader :desc "Open yadm status" "g p" #'yadm-status))
;; )
;; (add-to-list 'projectile-known-projects "/yadm::~")

;; (use-package! magit-file-icons
;;   :after magit
;;   :init
;;   (magit-file-icons-mode 1))

;; (use-package! ox-chameleon
;;   :after ox)
;; (add-to-list 'org-latex-packages-alist '("" "scrextend" nil))
;; (add-to-list 'org-latex-packages-alist '("" "xcolor" nil))
