(use-package emacs
  :bind
  (("C-x C-r" . recentf-open-files)
   ("C-c e" . mac-opt-chars-mode))

  :init
  (defun mac-toggle-ns-alt-modifier ()
    (if (not mac-opt-chars-mode)
        (setq ns-alternate-modifier 'meta)
      (setq ns-alternate-modifier nil)))
  
  (define-minor-mode mac-opt-chars-mode
    "Type characters with option as in other Mac applications."
    :global t
    :lighter " é"
    (mac-toggle-ns-alt-modifier))
  
  :hook
  (text-mode . flyspell-mode)
  (prog-mode . flyspell-prog-mode)
  (org-mode . flyspell-mode)
  (org-mode . (lambda () (display-line-numbers-mode 0)))
  (org-mode . (lambda () (face-remap-add-relative 'default
						  :family "iA Writer Duo S"
						  :height 150)))
  
  :config
  (require 'modus-themes)
  (require 'ef-themes)
  
  (setq debug-on-error t)

  (setq modus-themes-italic-constructs t
	      modus-themes-bold-constructs nil)

  (mapc #'disable-theme custom-enabled-themes)
  ;; Light themes
  ;(load-theme 'ef-cyprus :no-confirm)
  (set-frame-parameter nil 'ns-appearance 'light)
  ;; Dark themes
  (load-theme 'ef-maris-dark :no-confirm)
  ;(load-theme 'modus-vivendi-tinted :no-confirm)
  (set-frame-parameter nil 'ns-appearance 'dark)

  (set-frame-parameter nil 'ns-transparent-titlebar t)
  
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  
  (electric-pair-mode t)
  (delete-selection-mode t)
  (recentf-mode t)

  (setq ns-alternate-modifier 'meta)

  (setq ispell-program-name "aspell")
  (setq ispell-list-command "list")
  
  (setq-default truncate-lines t
                left-margin-width 0
                right-margin-width 2)

  (setq c-basic-indent 2)
  (setq tab-width 2)
  (setq indent-tabs-mode nil)
  (setq inhibit-startup-screen t
        initial-buffer-choice nil)

  (defconst current-dir (file-name-directory (or load-file-name buffer-file-name)))
  
  (setq initial-scratch-message
        (with-temp-buffer
	  (insert-file-contents (expand-file-name "scratch-template" current-dir))
	  (buffer-string)))

  (set-frame-font "Berkeley Mono 14" nil t)

  (defvar emacs-autosave-directory
    (concat user-emacs-directory "autosaves/")
    "This variable dictates where to put auto saves. It is set to a
  directory called autosaves located wherever your .emacs.d/ is
  located.")
  
  ;; Sets all files to be backed up and auto saved in a single directory.
  (setq backup-directory-alist
        `((".*" . ,emacs-autosave-directory))
        auto-save-file-name-transforms
        `((".*" ,emacs-autosave-directory t)))
  
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  (defun jh/copy-file-path (&optional @dir-path-only-p)
  "Copy the current buffer's file path or dired path to `kill-ring'.
Result is full path.
If `universal-argument' is called first, copy only the dir path.

If in dired, copy the file/dir cursor is on, or marked files.

If a buffer is not file and not dired, copy value of `default-directory'
(which is usually the “current” dir when that buffer was created)

URL `http://ergoemacs.org/emacs/emacs_copy_file_path.html'
Version 2017-09-01"
  (interactive "P")
  (let (($fpath
         (if (string-equal major-mode 'dired-mode)
             (progn
               (let (($result (mapconcat 'identity (dired-get-marked-files) "\n")))
                 (if (equal (length $result) 0)
                     (progn default-directory )
                   (progn $result))))
           (if (buffer-file-name)
               (buffer-file-name)
             (expand-file-name default-directory)))))
    (kill-new
     (if @dir-path-only-p
         (progn
           (message "Directory path copied: 「%s」" (file-name-directory $fpath))
           (file-name-directory $fpath))
       (progn
         (message "File path copied: 「%s」" $fpath)
         $fpath )))))
  
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Support opening new minibuffers from inside existing minibuffers.
  (setq enable-recursive-minibuffers t)

  (global-display-line-numbers-mode)

  ;; Reload changed files from disk
  (global-auto-revert-mode t)

  ;; Emacs 28 and newer: Hide commands in M-x which do not work in the current
  ;; mode.  Vertico commands are hidden in normal buffers. This setting is
  ;; useful beyond Vertico.
  (setq read-extended-command-predicate #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)

  (setq package-enable-at-startup t)
  (setq package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("org" . "https://orgmode.org/elpa/")
                           ("gnu" . "https://elpa.gnu.org/packages/")))
  
  (require 'use-package-ensure)
  (setq use-package-always-ensure t))

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package visual-fill-column
  :hook
  (org-mode . visual-fill-column-mode)
  :custom
  (visual-fill-column-width 80)
  (visual-fill-column-center-text t))

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '((ruby-mode ruby-ts-mode) "ruby-lsp")))

(use-package rg
  :config
  (rg-enable-menu))

(use-package compat)

(use-package vertico
  :after (compat)
  :init
  (vertico-mode)
  (setq vertico-scroll-margin 100))

(use-package savehist
  :init
  (savehist-mode 1))

(use-package magit)

(use-package graphql-mode)

(use-package lua-mode)

(use-package mmm-mode
  :config
  (setq mmm-global-mode 'maybe)
  (setq mmm-parse-when-idle 't)
  (mmm-add-classes
   '((markdown-graphql
      :submode graphql-mode
      :front "^```graphql[\n\r]+"
      :back "^```$")))
  (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-graphql))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package corfu
  ;; Dropdown auto-complete
  :init
  (global-corfu-mode)
  (setq corfu-auto t)
  (setq corfu-quit-no-match 'separator))

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
)

(use-package markdown-mode)

(use-package marginalia
  :bind
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package ace-window
  :bind
  (("M-o" . ace-window))
  ;; x - delete window
  ;; m - swap windows
  ;; M - move window
  ;; c - copy window
  ;; j - select buffer
  ;; n - select the previous window
  ;; F - split window fairly, either vertically or horizontally
  ;; v - split window vertically
  ;; b - split window horizontally
  ;; o - maximize current window
  ;; ? - show these command bindings
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

  (defun jh/ace-tear-off-window ()
  "Select a window with ace-window and tear it off the frame.
   This displays the window in a new frame, see `tear-off-window'."
  (interactive)
  (when-let ((win (aw-select " ACE"))
             (buf (window-buffer win))
             (frame (make-frame)))
    (select-frame frame)
    (pop-to-buffer-same-window buf)
    (delete-window win)))

  (defun jh/ace-window-one-command ()
    (interactive)
    (let ((win (aw-select " ACE")))
      (when (windowp win)
	(with-selected-window win
          (let* ((command (key-binding
                           (read-key-sequence
                            (format "Run in %s..." (buffer-name)))))
		 (this-command command))
            (call-interactively command))))))

  (keymap-global-set "C-x O" 'jh/ace-window-one-command)
  
  (defun jh/ace-window-prefix ()
  "Use `ace-window' to display the buffer of the next command.
   The next buffer is the buffer displayed by the next command invoked
   immediately after this command (ignoring reading from the minibuffer).
   Creates a new window before displaying the buffer.
   When `switch-to-buffer-obey-display-actions' is non-nil,
   `switch-to-buffer' commands are also supported."
  (interactive)
  (display-buffer-override-next-command
   (lambda (buffer _)
     (let (window type)
       (setq
        window (aw-select (propertize " ACE" 'face 'mode-line-highlight))
        type 'reuse)
       (cons window type)))
   nil "[ace-window]")
  (message "Use `ace-window' to display next command buffer..."))

  (keymap-global-set "C-x 4 o" 'jh/ace-window-prefix))

(use-package mood-line
  :config
  (mood-line-mode)
  
  :custom
  (setq mood-line-glyph-alist mood-line-glyphs-unicode))

(use-package gptel
  :bind
  (("C-c g g" . gptel)
   ("C-c g ." . gptel-send)
   ("C-c g k" . gptel-abort))
  :config
  (setq-default gptel-org-branching-context t)
  (setq-default gptel-backend
		(gptel-make-openai "llama-cpp"
		  :stream t
		  :protocol "http"
		  :host "localhost:11311"
		  :key nil
		  :models '("dummy"))))

(use-package ts
  :init
  (defun this-monday ()
    (let* ((now (ts-now))
           (day-of-week (ts-dow now))
           (days-since-monday (- day-of-week 1)))
      (ts-adjust 'day (- days-since-monday) now)))
  :custom
  (ts-default-format "%Y-%m-%d %H:%M"))

(use-package org
  :after (ts)
  
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture))

  :custom
  (custom-set-faces
   '(org-level-1 ((t (:foreground "#3F4B3B" :weight bold :height 1.0))))
   '(org-level-2 ((t (:foreground "#44633F" :weight bold :height 1.0))))
   '(org-level-3 ((t (:foreground "#5A9367" :weight bold :height 1.0))))
   '(org-level-4 ((t (:foreground "#5CAB7D" :weight bold :height 1.0))))
   '(org-level-5 ((t (:foreground "#46494C" :weight bold :height 1.0))))
   '(org-level-6 ((t (:foreground "#4C5C68" :weight bold :height 1.0))))
   '(org-level-7 ((t (:foreground "#1985A1" :weight bold :height 1.0))))
   '(org-level-8 ((t (:foreground "#FFA69E" :weight bold :height 1.0)))))

  :init
  (require 'org-duration)
  
  (defun jh/org-agenda-to-appt ()
    (interactive)
    (setq appt-time-msg-list nil)
    (org-agenda-to-appt))

  (defun get-attr-quote-properties ()
    "Get the attr_quote properties of the first quote block in the current buffer."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^#\\+begin_quote" nil t)
	(let ((element (org-element-at-point)))
          (when (eq (org-element-type element) 'quote-block)
          (org-element-property :attr_quote element))))))
  
  (defun activate-capture-frame ()
    "run org-capture in capture frame"
    (select-frame-by-name "capture")
    (switch-to-buffer (get-buffer-create "*scratch*"))
    (org-capture))
  
  (defun jh/update-org-and-refresh-agenda ()
    "Revert org buffer without confirmation and refresh agenda."
    (interactive)
    (when (eq major-mode 'org-mode)
      (revert-buffer t t))  ; Reverts the buffer without confirmation
    (org-agenda nil "w"))   ; Refreshes the agenda view

  (defun jh/disable-electric-pair-for-org-mode ()
    "Disable electric pair mode for < in org-mode."
    (setq-local electric-pair-inhibit-predicate
		(lambda (c)
                  (if (char-equal c ?<) t (electric-pair-default-inhibit c)))))

  :hook
  (org-agenda-finalize . jh/org-agenda-to-appt)
  (text-mode . turn-on-visual-line-mode)
  (org-mode . jh/disable-electric-pair-for-org-mode)
  
  :config
  (require 'ox-md)
  (require 'org-tempo)
  (require 'ol)
  (require 'ob-ruby)
  
  (jh/org-agenda-to-appt)
  (appt-activate t)
  (run-at-time "24:01" nil 'jh/org-agenda-to-appt)

  (setq org-agenda-window-setup 'only-window)
  
  (setq org-directory "/Users/jahfer/Documents/Org/")
  
  (add-to-list 'org-src-lang-modes '("graphql" . graphql))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ruby . t)))
  
  (setq-default
   org-log-done 'time
   org-support-shift-select t
   org-agenda-compact-blocks nil
   org-outline-path-complete-in-steps nil
   org-refile-allow-creating-parent-nodes 'confirm
   org-refile-use-outline-path 'file
   org-priority-faces '((?A . (:foreground "red"))
			(?B . (:foreground "#c195dc"))
			(?C . (:foreground "grey")))
   org-todo-keyword-faces '(("CANCELED" . (:foreground "red")))
   org-agenda-prefix-format '(
			      (agenda  . " %i %?-12t% s") ;; file name + org-agenda-entry-type
			      ;;(agenda  . "  • ")
			      (timeline  . "  % s")
			      (todo  . " %-12:c")
			      ;;(tags  . "  ")
			      (tags . " %i %-16c")
			      (search . " %i %-12:c"))
   org-agenda-block-separator nil
   org-agenda-hide-tags-regexp "work"
   org-agenda-files (list
		     (concat org-directory "notes/daily/")
		     (concat org-directory "tasks.org")
		     (concat org-directory "events.org")
		     (concat org-directory "inbox.org"))
   org-default-notes-file (concat org-directory "inbox.org")
   org-refile-targets '((("/Users/jahfer/Documents/Org/tasks.org") . (:maxlevel . 3)))
   org-capture-templates '(("n" "Note" entry (file+headline "/Users/jahfer/Documents/Org/inbox.org" "Inbox Notes")
                            "* %?\n" :clock-in t :clock-resume t)
                           ("t" "Todo" entry (file+headline "/Users/jahfer/Documents/Org/inbox.org" "Inbox Tasks")
                            "* TODO %?\n"))
   org-agenda-custom-commands
   '(("w" "Work list"
      ((tags "unfiled/TODO"
	     ((org-agenda-overriding-header "🗄️ Unfiled")))
       (tags (concat "+CLOSED>=\"<" (ts-format "%Y-%m-%d" (this-monday)) ">\"+TODO=\"DONE\"")
	           (;(org-agenda-archives-mode t)
	            (org-agenda-overriding-header (concat "\n" (make-string (- (window-width) 4) ?-) "\n\n" "🏁 Week in Review"))))
       (tags "work/STARTED|REVIEWED"
	     ((org-agenda-overriding-header (concat "\n" "🛠️ In Progress"))))
       (tags-todo "work+PRIORITY=\"A\"+TODO=\"TODO\""
                  ((org-agenda-overriding-header (concat "\n" (make-string (- (window-width) 4) ?-) "\n\n" "↪️ Next"))))
       (tags "work/WAITING"
             ((org-agenda-overriding-header (concat "\n" "⚠️ Stalled"))))
       (tags-todo "work+PRIORITY=\"B\"+TODO=\"TODO\""
		              ((org-agenda-overriding-header (concat "\n" (make-string (- (window-width) 4) ?-) "\n\n" "📋 Todo"))))
       (agenda ""
	             ((org-agenda-overriding-header (concat "\n" (make-string (- (window-width) 4) ?-) "\n\n" "🗓️ Calendar"))
		            (org-agenda-span 'day)
		            (org-agenda-files (list
				                           (concat org-directory "tasks.org")
				                           (concat org-directory "events.org")
				                           (concat org-directory "notes/daily")))
		            (org-agenda-scheduled-leaders '("" "LATE (%2dx):"))))
       (tags "work+PRIORITY=\"C\"+TODO=\"TODO\""
             ((org-agenda-overriding-header (concat "\n" (make-string (- (window-width) 4) ?-) "\n\n" "↩️ Someday")))))
      ()
      ("/Users/jahfer/Documents/Org/public/agenda.html"))
     ("h" "Home list"
      ((tags "home/TODO"
             ((org-agenda-overriding-header "Todo:")))))))
  (org-add-link-type "gh" 'org-gh-open))

(use-package org-timeblock)

(use-package org-roam
  :hook
  (org-mode . jh/org-mode-setup)
  
  :custom
  (org-roam-directory (file-truename "/Users/jahfer/Documents/Org/notes/"))
  
  :bind (("C-c r b" . org-roam-buffer-toggle)
         ("C-c r f" . org-roam-node-find)
         ("C-c r g" . org-roam-graph)
         ("C-c r i" . org-roam-node-insert)
         ("C-c r c" . org-roam-capture)
	 ("C-c r a" . (lambda () (interactive) (org-roam-node-find nil "Index")))
         ;;	("C-c r x" . (lambda () (find-file "/Users/jahfer/Documents/Org/notes/20240412085436-index.org")))
         ("C-c r j" . org-roam-dailies-capture-today)
	 ("C-c r t" . org-roam-dailies-goto-today))
  
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  (setq org-id-track-globally t)
  (setq org-roam-capture-templates
	'(("d" "default" plain "* %?"
	   :target (file+head "%<Y%m%d%H%M%S>-${slug}.org" "#+startup: content indent hidestars\n#+title: ${title}")
	   :unnarrowed t)))
  ;; If using org-roam-protocol
  (require 'org-roam-protocol)

  (defun jh/org-roam-get-title-id-completions ()
    "Get a list of Org-roam node titles and their paths for completion."
    (mapcar (lambda (node)
              (cons (org-roam-node-title node)
                    (org-roam-node-id node)))
            (org-roam-node-list)))

  (defun jh/org-roam-completion-at-point ()
    (when (and (derived-mode-p 'org-mode)
               (looking-back "@\\(\\w*\\)" (line-beginning-position)))
      (let ((bounds (bounds-of-thing-at-point 'symbol)))
	(list (car bounds) (cdr bounds)
              (completion-table-dynamic
               (lambda (_)
		 (jh/org-roam-get-title-id-completions)))
              :exclusive 'no))))

  (defun jh/org-roam-insert-id-link (completion)
    "Insert Org-roam id link for the selected COMPLETION."
    (let* ((title (car completion))
           (id (cdr completion)))
      (delete-region (car (bounds-of-thing-at-point 'symbol))
                     (cdr (bounds-of-thing-at-point 'symbol)))
      (insert (format "[[id:%s][%s]]" id title))))
  
  (defun jh/org-mode-setup ()
    (setq-local completion-at-point-functions
		(list #'jh/org-roam-completion-at-point))
    (setq-local corfu-exit-function #'jh/org-roam-insert-id-link)))

(use-package consult-org-roam
   :after (consult org-roam)
   :init
   (require 'consult-org-roam)
   ;; Activate the minor mode
   (consult-org-roam-mode 1)
   :custom
   ;; Use `ripgrep' for searching with `consult-org-roam-search'
   (consult-org-roam-grep-func #'consult-ripgrep)
   ;; Configure a custom narrow key for `consult-buffer'
   (consult-org-roam-buffer-narrow-key ?r)
   ;; Display org-roam buffers right after non-org-roam buffers
   ;; in consult-buffer (and not down at the bottom)
   (consult-org-roam-buffer-after-buffers t)
   :config
   ;; Eventually suppress previewing for certain functions
   (consult-customize
    consult-org-roam-forward-links
    :preview-key "M-.")
   :bind
   ;; Define some convenient keybindings as an addition
   ("C-c r b" . consult-org-roam-backlinks)
   ("C-c r B" . consult-org-roam-backlinks-recursive)
   ("C-c r l" . consult-org-roam-forward-links)
   ("C-c r r" . consult-org-roam-search))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("1ea82e39d89b526e2266786886d1f0d3a3fa36c87480fad59d8fab3b03ef576e" default))
 '(package-selected-packages
   '(ob-ruby consult-org-roam org-tempo lua-mode visual-fill-column org-timeblock compat compatz gptel mmm-mode graphql-mode rg markdown-mode exec-path-from-shell vertico-directory spacious-padding casual auto-package-update magit minions mood-line all-the-icons-completion all-the-icons ef-themes modus-themes ace-window marginalia corfu orderless vertico)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(keycast-key ((t)))
 '(tab-line-tab ((t)))
 '(tab-line-tab-active ((t)))
 '(tab-line-tab-inactive ((t))))
