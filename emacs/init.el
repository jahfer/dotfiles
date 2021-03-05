;;    Managing extensions for Emacs is simplified using =package= which
;;    is built in to Emacs 24 and newer. To load downloaded packages we
;;    need to initialize =package=.
(require 'package)
(setq package-enable-at-startup nil)

(server-start t t)

(setq initial-frame-alist '((font . "-*-Input-normal-normal-semicondensed-*-14-*-*-*-m-0-iso10646-1")))
(setq default-frame-alist '((font . "-*-Input-normal-normal-semicondensed-*-14-*-*-*-m-0-iso10646-1")))

;;-*-Input-normal-normal-semicondensed-*-14-*-*-*-m-0-iso10646-1

;; set tab sanity
(setq c-basic-indent 2)
(setq tab-width 2)
(setq coffee-tab-width 2)
(setq indent-tabs-mode nil)
(setq inhibit-startup-screen t
      initial-buffer-choice nil)

(defconst current-dir (file-name-directory (or load-file-name buffer-file-name)))

(setq initial-scratch-message
      (with-temp-buffer
	(insert-file-contents (expand-file-name "scratch-template" current-dir))
	(buffer-string)))

(setq initial-major-mode 'org-mode)

;; Packages can be fetched from different mirrors, [[http://melpa.milkbox.net/#/][melpa]] is the largest
;;    archive and is well maintained.

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("MELPA" . "http://melpa.org/packages/")))

;; We can define a predicate that tells us whether or not the newest version
;;    of a package is installed.

(defun newest-package-installed-p (package)
  "Return true if the newest available PACKAGE is installed."
  (message "Installing package: %s" package)
  (when (package-installed-p package)
    (let* ((get-desc (if (version< emacs-version "24.4") 'cdr 'cadr))
	   (builtin-version (alist-get package package--builtin-versions))
	   (local-pkg-desc  (assq package package-alist))
	   (newest-pkg-desc (assq package package-archive-contents)))
      (cond ((and local-pkg-desc newest-pkg-desc)
	     (version-list-= (package-desc-version
			      (funcall get-desc local-pkg-desc))
			     (package-desc-version
			      (funcall get-desc newest-pkg-desc))))
	    ((and builtin-version newest-pkg-desc)
	     (version-list-= builtin-version
			     (package-desc-version
			      (funcall get-desc newest-pkg-desc))))))))

;; Let's write a function to install a package if it is not installed or
;;    upgrades it if a new version has been released. Here our predicate comes
;;    in handy.

(defun upgrade-or-install-package (package)
  "Unless the newest available version of PACKAGE is installed
PACKAGE is installed and the current version is deleted."
  (unless (newest-package-installed-p package)
    (let ((pkg-desc (assq package package-alist)))
      (when pkg-desc
	(package-delete (cadr pkg-desc)))
      (and (assq package package-archive-contents)
           (package-install package)))))

;; Also, we will need a function to find all dependencies from a given package.
(defun dependencies (package)
  "Returns a list of dependencies from a given PACKAGE."
  (let* ((pkg-desc (assq package package-alist))
;	 (message "Updating dependencies for %s" (symbol-name package))
         (reqs (and pkg-desc (package-desc-reqs (cadr pkg-desc)))))
    (mapcar 'car reqs)))

;; The =package-refresh-contents= function downloads archive descriptions,
;;    this is a major bottleneck in this configuration. To avoid this we can
;;    try to only check for updates once every day or so. Here are three
;;    variables. The first specifies how often we should check for update
;;    shown value does not take effect until you set or save it.
;;    her one should update during the initialization. The
;;    third is a path to a file where a time-stamp is stored in order to check
;;    when packages were updated last.

(defvar days-between-updates 7)
(defvar do-package-update-on-init t)
(defvar package-last-update-file
  (expand-file-name (concat user-emacs-directory ".package-last-update")))

;; The tricky part is figuring out when packages were last updated. Here is
;;    a hacky way of doing it, using [[http://www.gnu.org/software/emacs/manual/html_node/emacs/Time-Stamps.html][time-stamps]]. By adding a time-stamp to the
;;    a file, we can determine whether or not to do an update. After that we
;;    must run the =time-stamp=-function to update the time-stamp.

(require 'time-stamp)
;; Open the package-last-update-file
(with-temp-file package-last-update-file
  (if (file-exists-p package-last-update-file)
      (progn
        ;; Insert it's original content's.
        (insert-file-contents package-last-update-file)
        (let ((start (re-search-forward time-stamp-start nil t))
              (end (re-search-forward time-stamp-end nil t)))
          (when (and start end)
            ;; Assuming we have found a time-stamp, we check determine if it's
            ;; time to update.
            (setq do-package-update-on-init
                  (<= days-between-updates
                      (days-between
                       (current-time-string)
                       (buffer-substring-no-properties start end))))
            ;; Remember to update the time-stamp.
            (when do-package-update-on-init
              (time-stamp)))))
    ;; If no such file exists it is created with a time-stamp.
    (insert "Time-stamp: <>")
    (time-stamp)))


;; Now we can use the function above to make sure packages are installed and
;;    up to date. Here are some packages I find useful (some of these
;;    configurations are also dependent on them).

(when (and do-package-update-on-init
           (y-or-n-p "Update all packages?"))
  (package-refresh-contents)

  (let* ((packages
          '(
	    use-package
	    auto-complete      ; auto completion
	    use-package        ; package configuration
;;	    cider              ; clojure integrated development environment and REPL
;;	    clojure-mode       ; major mode for clojure code
            flx-ido            ; flx integration for ido
            ido-vertical-mode  ; Makes ido-mode display vertically.
            magit             ; control Git from Emacs
	    monokai-theme     ; A fruity color theme for Emacs.
            move-text         ; Move current line or region with M-up or M-down
            multiple-cursors  ; Multiple cursors for Emacs.
            paredit           ; minor mode for editing parentheses
            powerline         ; Rewrite of Powerline
            smex              ; M-x interface with Ido-style fuzzy matching.
	    projectile-rails  ; projectile support for rails projectsx
	    sublime-themes    ; Perty themes
	    color-theme-solarized
	    key-chord
	    markdown-mode
	    org-brain
      magit
	    polymode
	    htmlize
      hydra
      avy
      deft
      graphviz-dot-mode
      zetteldeft
;;	    helm
;;	    helm-fuzzy-find
	    ;;	    helm-projectile
	    ivy
	    ivy-rich
	    swiper
	    counsel
	    undo-tree
	    spaceline
	    spaceline-all-the-icons
	    org-sticky-header
	    plantuml-mode
	    org-ql
	    ruby-end
	    lsp-mode
	    tuareg
	    doom-themes
	    shadowenv
	    org-sidebar
	    visual-fill-column
	    toml-mode
	    rust-mode
	    cargo
	    company
	    flycheck
	    flycheck-rust
	    lsp-ui
	    ))
         ;; Fetch dependencies from all packages.
         (reqs (mapcar 'dependencies packages))
         ;; Append these to the original list, and remove any duplicates.
         (packages (delete-dups (apply 'append packages reqs))))
    (dolist (package packages)
      (upgrade-or-install-package package)))

  ;; This package is only relevant for Mac OS X.
  (when (memq window-system '(mac ns))
    (upgrade-or-install-package 'exec-path-from-shell))

  (package-initialize))

(eval-when-compile
  (require 'use-package))

;; Mac OS X

;;    I run this configuration mostly on Mac OS X, so we need a couple of
;;    settings to make things work smoothly. In the package section
;;    =exec-path-from-shell= is included (only if you're running OS X), this is
;;    to include environment-variables from the shell. It makes useing Emacs
;;    along with external processes a lot simpler. I also prefer using the
;;    =Command=-key as the =Meta=-key.

(when (memq window-system '(mac ns))
  (setq mac-option-modifier 'meta
        x-select-enable-clipboard t)
  (run-with-idle-timer 5 nil 'exec-path-from-shell-initialize)
  (setq ns-use-srgb-colorspace t))

;; Some mac-bindings interfere with Emacs bindings.
(when (boundp 'mac-pass-command-to-system)
  (setq mac-pass-command-to-system nil))

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

;; Set =utf-8= as preferred coding system.
(set-language-environment "UTF-8")

;; Call =auto-complete= default configuration, which enables =auto-complete=
;;    globally.
;(eval-after-load 'auto-complete-config '(ac-config-default))

;; Modes

;;    There are some modes that are enabled by default that I don't find
;;    particularly useful. We create a list of these modes, and disable all of
;;    these.

(dolist (mode
         '(tool-bar-mode                ; No toolbars, more room for text.
           menu-bar-mode                ; No file/etc bar
           scroll-bar-mode))            ; No scroll bars either.
  (funcall mode 0))


;; Let's apply the same technique for enabling modes that are disabled by
;;    default.

(dolist (mode
         '(;abbrev-mode               ; E.g. sopl -> System.out.println.
           column-number-mode         ; Show column number in mode line.
           delete-selection-mode      ; Replace selected text.
           recentf-mode               ; Recently opened files.
           show-paren-mode            ; Highlight matching parentheses.
	         ;;desktop-save-mode
	         electric-pair-mode
	         multiple-cursors-mode
	         ;;helm-mode
           global-undo-tree-mode))    ; Undo as a tree.
  (funcall mode 1))

;; Set variables for desktop-save to function smoothly
;;(setq desktop-path '("~/.emacs.d/"))
;;(setq desktop-dirname user-emacs-directory)
;;(setq desktop-base-file-name "emacs-desktop")
; remove desktop after it's been read
;; (add-hook 'desktop-after-read-hook
;;     '(lambda ()
;;        ;; desktop-remove clears desktop-dirname
;;        (setq desktop-dirname-tmp desktop-dirname)
;;        (desktop-remove)
;;        (setq desktop-dirname desktop-dirname-tmp)))

;; This makes =.md=-files open in =markdown-mode=.
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Visual
;;    Change the color-theme to =monokai= (downloaded using =package=).
                                        ;(load-theme 'challenger-deep t)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'doom-Iosvkem t)
;;(load-theme 'vscode-default-dark t)
;;(load-theme 'doom-palenight t)
;;(load-theme 'pastelmac t)
;;(load-theme 'avk-daylight t)

(custom-theme-set-faces
 'user
 '(default ((t (:background nil :height 140))))
; '(mode-line ((t (:family "M+ 2m" :box (:line-width 1 :color "#100f22")))))
 '(mode-line ((t (:family "M+ 2m" :box (:line-width 1 :color "#000000")))))
 '(fixed-pitch ((t (:weight light :family "Input"))))
 '(variable-pitch ((t (:weight light :family "SF Pro Text")))))

(setq-default line-spacing 0.3)
(set-face-attribute 'default nil :height 140)
(set-frame-font "Input-14" nil t)

(set-cursor-color "#AAA")

(setq-default cursor-type 'bar)
(setq-default blink-cursor-mode -1)
(setq-default truncate-lines t)
(setq-default left-margin-width 0 right-margin-width 2)
(setq-default global-visual-line-mode nil)
(global-display-line-numbers-mode)

(set-window-buffer nil (current-buffer))

;; (use-package spaceline-all-the-icons
;;   :init (require 'spaceline)
;;   :config (spaceline-all-the-icons-theme))

;; remove trailing whitespace
;;(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; always autocomplete
(use-package auto-complete
  :config
  (global-auto-complete-mode t))

;; hook up projectile-rails
(add-hook 'projectile-mode-hook 'projectile-rails-on)

;; [[https://github.com/milkypostman/powerline][Powerline]] is an extension to customize the mode line. This is modified
;;    version =powerline-nano-theme=.

(require 'powerline)
(powerline-nano-theme)
(add-hook 'desktop-after-read-hook 'powerline-reset)

;; macOS
(when (memq window-system '(mac ns))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)) ;; {light, dark}
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

;; Ido

;;    Interactive do (or =ido-mode=) changes the way you switch buffers and
;;    open files/directories. Instead of writing complete file paths and buffer
;;    names you can write a part of it and select one from a list of
;;    possibilities. Using =ido-vertical-mode= changes the way possibilities
;;    are displayed, and =flx-ido-mode= enables fuzzy matching.

;; (dolist (mode
;;          '(ido-mode                   ; Interactivly do.
;;            ido-everywhere             ; Use Ido for all buffer/file reading.
;;            ;ido-vertical-mode          ; Makes ido-mode display vertically.
;;            flx-ido-mode))             ; Toggle flx ido mode.
;;   (funcall mode 1))


;; We can set the order of file selections in =ido=. I prioritize source
;;    files along with =org=- and =tex=-files.

;; (setq ido-file-extensions-order
;;       '(".el" ".scm" ".lisp" ".java" ".c" ".h" ".org" ".tex"))

;; Sometimes when using =ido-switch-buffer= the =*Messages*= buffer get in
;;    the way, so we set it to be ignored (it can be accessed using =C-h e=, so
;;    there is really no need for it in the buffer list).

;; (add-to-list 'ido-ignore-buffers "*Messages*")

;; To make =M-x= behave more like =ido-mode= we can use the =smex=
;;    package. It needs to be initialized, and we can replace the binding to
;;    the standard =execute-extended-command= with =smex=.

(smex-initialize)
(global-set-key (kbd "M-x") 'smex)


;; Interactive functions
;;    <<sec:defuns>>

;;    To search recent files useing =ido-mode= we add this snippet from
;;    [[http://www.emacswiki.org/emacs/CalendarWeekNumbers][EmacsWiki]].

;; (defun recentf-ido-find-file ()
;;   "Find a recent file using Ido."
;;   (interactive)
;;   (let ((f (ido-completing-read "Choose recent file: " recentf-list nil t)))
;;     (when f
;;       (find-file f))))

(defun whack-whitespace (arg)
  "Delete all white space from point to the next word.  With prefix ARG
    delete across newlines as well.  The only danger in this is that you
    don't have to actually be at the end of a word to make it work.  It
    skips over to the next whitespace and then whacks it all to the next
    word."
  (interactive "P")
  (let ((regexp (if arg "[ \t\n]+" "[ \t]+")))
    (re-search-forward regexp nil t)
    (replace-match "" nil nil)))

;; Projectile configuration

;;(projectile-global-mode)

;;(setq projectile-completion-system 'helm)
;;(helm-projectile-on)

;; (use-package helm
;;   :init
;;   (setq helm-display-header-line nil)
;;   (set-face-attribute 'helm-source-header nil :height 0.1)
;;   (helm-autoresize-mode 1)
;;   (setq helm-autoresize-max-height 30)
;;   (setq helm-split-window-in-side-p t))

;;(use-package helm-fuzzy-find)

;; (setq shell-file-name "/bin/sh")

;(use-package flycheck
;  :hook (prog-mode . flycheck-mode))

(use-package company
  :hook (prog-mode . company-mode)
  :config (setq company-tooltip-align-annotations t)
          (setq company-minimum-prefix-length 1))

(use-package lsp-mode
  :commands lsp
  :config (setq lsp-rust-server 'rust-analyzer))

(use-package lsp-ui)

(use-package toml-mode)

(use-package rust-mode
  :hook (rust-mode . lsp))

;; Add keybindings for interacting with Cargo
(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; Increase GC threshold
(setq gc-cons-threshold 20000000)

;; Shadowenv
(use-package shadowenv
  :init
  (setq shadowenv-binary-location "/usr/local/bin/shadowenv")
  :hook (after-init . shadowenv-global-mode))

;; Language Features
(use-package ruby-end)

(use-package hydra
  :init
  (defhydra hydra-zetteldeft (:columns 3
                              :color yellow
                              :exit t)
    "zetteldeft"
    ("n" zetteldeft-new-file "New file")
    ("N" zetteldeft-new-file-and-link "New file with link")
    ("b" zetteldeft-new-file-and-backlink "New file with backlink")
    ("f" zetteldeft-find-file "Find file")
    ("t" zetteldeft-avy-tag-search "Tag search")
    ("l" zetteldeft-follow-link "Follow link")
    ("r" zetteldeft-file-rename "Rename file")
    ("h" zetteldeft-go-home "Go to index")
    ("g" deft-refresh "Refresh")
    ("i" zetteldeft-find-file-id-insert "Insert link id")
    ("I" zetteldeft-find-file-full-title-insert "Insert full link title")
    ("s" deft "Search")
    ("q" nil "quit")
    ("B" zetteldeft-backlink-add "Add backlink"))
  :bind ("<f2>" . hydra-zetteldeft/body))

;; Ivy
(use-package ivy
  :init
  (require 'swiper)
  (require 'counsel)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-wrap t)
  (setcdr (assoc 'counsel-M-x ivy-initial-inputs-alist) "")
  (setq ivy-re-builders-alist
	'((t . ivy--regex-plus)))
  :bind (("C-s" . swiper-isearch)
	 ("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)
	 ("C-x C-r" . counsel-recentf)
	 ("M-y" . counsel-yank-pop)
	 ("C-x b" . ivy-switch-buffer)
	 ("C-c v" . ivy-push-view)
	 ("C-c V" . ivy-pop-view)
	 ("C-c h" . counsel-org-goto)
	 ("C-c y" . counsel-org-link)
	 ("C-x t" . counsel-org-tag))
  :config
  (ivy-mode 1)
  (setq projectile-completion-system 'ivy))

(use-package ivy-rich
  :demand t
  :after ivy
  :init
  (setq ivy-rich-path-style 'abbrev)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  :config
  (ivy-rich-mode 1))

;; Magit
(use-package magit
  :bind
  ("C-x g" . magit-status))
(setq with-editor-emacsclient-executable "/Users/jahfer/.nix-profile/bin/emacsclient")

;; Org Brain
(use-package org-brain :ensure t
  :after org
  :config
  (bind-key "C-c b" 'org-brain-prefix-map org-mode-map)
  (setq org-id-track-globally t)
  (setq org-id-locations-file "~/.emacs.d/.org-id-locations")
  (add-hook 'before-save-hook #'org-brain-ensure-ids-in-buffer)
  (setq org-brain-visualize-default-choices 'all)
  (setq org-brain-title-max-length 12)
  (setq org-brain-include-file-entries nil
        org-brain-file-entries-use-title nil))

;; ;; Allows you to edit entries directly from org-brain-visualize
;; (use-package polymode
;;   :hook
;;   ((org-brain-visualize-mode . org-brain-polymode)))
(default-font-height)

;; Org mode
(use-package org
  :init
  (require 'org-ql)
  (require 'org-tempo) ;; <s<tab> for source block
  (require 'browse-url)
  (org-id-update-id-locations)
  (run-at-time "24:01" nil 'org-id-update-id-locations)
  (add-hook 'org-agenda-finalize-hook 'org-id-update-id-locations 'append)
  (defun jh/el ()
    (interactive)
    (print (cadr (org-element-at-point))))
  (defun jh/find-resources ()
    (interactive)
    (let ((headline (org-element-property :title (org-element-at-point)))
	  (links (save-restriction
		   (org-narrow-to-subtree)
		   (org-element-map (org-element-parse-buffer) 'link
		     (lambda (item)
		       (message (org-element-property :type item))
		       (when (not (equal "id" (org-element-property :type item)))
			 (concat "[["
				 (org-element-property :raw-link item)
				 "]["
				 (or (car (org-element-contents item))
				     (org-element-property :raw-link item))
				 "]]\n"
				 )))))))
      (with-current-buffer (get-buffer-create "*jh-org-resources*")
	(read-only-mode 1)
	(org-mode)
	(let ((inhibit-read-only t))
	  (delete-region (point-min) (point-max))
	  (insert (concat "#+NAME: " headline "\n" "#+BEGIN_LINKS" "\n"))
	  (mapc
	   (lambda (raw-link)
	     (when raw-link
	       (insert raw-link)))
	   links)
	  (insert "#+END_LINKS\n\n"))
	(pop-to-buffer "*jh-org-resources*"))))
  (defun org-gh-open (str)
    (let* ((split (org-gh-split-string str))
           (repo (first split))
           (issue (second split)))
      (if issue
	  (browse-url (concat "https://github.com/" repo "/issues/" issue))
	(browse-url (concat "https://github.com/" repo)))))
  (defun org-gh-split-string (str)
    (let* ((strlist (split-string str "#"))
	   (repo (first strlist))
           (issue (second strlist)))
      (list repo issue)))
  (defun make-orgcapture-frame ()
    "Create a new frame and run org-capture."
    (interactive)
    (make-frame '((name . "*org-capture-note*") (width . 80) (height . 16)
                  (top . 400) (left . 300)
                  (font . "-apple-Monaco-medium-normal-normal-*-13-*-*-*-m-0-iso10646-1")
                  ))
    (select-frame-by-name "*org-capture-note*")
    (org-capture nil "n")
    (delete-other-windows))
  (defun close-orgcapture-frame ()
    (interactive)
    (ignore-errors
      (select-frame-by-name "*org-capture-note*")
      (delete-frame)))1
  (defun insert-gh-link-at-point (link)
    (interactive "Github Link: ")
    (save-match-data
      (let ((matches (make-hash-table)))
	(string-match "https?://github\.com/\\([A-Za-z]+/[A-Za-z]+\\)/[A-Za-z]+/\\([0-9]+\\)" link)
	(puthash 'repo (match-string 1 link) matches)
	(puthash 'id (match-string 2 link) matches)
	(let ((shorthand (concat (gethash 'repo matches) "#" (gethash 'id matches))))
	  (insert (concat "[[gh:" shorthand "][" shorthand  "]]"))))))
  (defun org-linked-refs ()
    (interactive)
    (let* ((id (org-entry-get (point) "ID"))
           (query (cond (id `(link :target ,(concat "id:" id)))
			(t (error "Entry has no ID property")))))
      (org-sidebar-ql (org-ql-search-directories-files)
	query
	:super-groups '((:auto-property "CATEGORY"));;'((:auto-property "agenda-group"))
	:title (concat " " (org-get-heading t t t t) " references"))))
  (defun org-unlinked-refs ()
    (interactive)
    (let* ((components (org-heading-components))
	   (h (nth 4 components))
	   (matches (org-ql-select (org-agenda-files) h))
	   (cands (make-hash-table :test 'equal)))
      (mapcar
       (lambda (x)
	 (puthash (org-element-property :raw-value x) x cands))
       matches)
      (ivy-read "Unlinked references:"
	        cands
		:predicate (lambda (x)
			     (not (eq x )))
		:action (lambda (x)
			  (let* ((element (gethash x cands))
				 (char-loc (org-element-property :begin element)))
			    (with-ivy-window
			      (goto-char char-loc))))
		:require-match t)
      ))

  (let* ((variable-tuple
          (cond ((x-list-fonts "Input")  '(:font "Input"))
  		          ((x-family-fonts "Monospace")    '(:font "Monospace"))
  		          (nil (warn "Cannot find a Monospace Font."))))
  	     (headline-text
  	      (cond ((x-list-fonts "SF Pro Display") '(:family "SF Pro Display"))
  		          ((x-family-fonts "Sans Serif") '(:family "Sans Serif"))
  		          (nil (warn "Cannot find a Sans Serif Font."))))
  	     (base-font-color     (face-foreground 'default nil 'default))
  	     (headline           `(:inherit default :weight normal :foreground ,base-font-color :height 140))
  	     (body               `(:inherit default :weight normal :foreground ,base-font-color :height 140))
  	     (link               `(:inherit default :weight normal :underline t :style line :height 140 :foreground "DeepSkyBlue1"))
	     )
;    (buffer-face-set `((t ,@body ,@variable-tuple)))
    (custom-theme-set-faces
     'user
     `(org-verbatim ((t (,@body :font "Input" :foreground "red"))))
     `(org-block   ((t (:slant italic :height 1.0))))
     `(org-link    ((t (,@link :inherit default))))
     `(org-todo    ((t (,@variable-tuple :box nil :height 1.0 :background nil))))
     `(org-date    ((t (,@link ,@variable-tuple :foreground "yellow"))))
     `(org-default ((t (,@body ,@variable-tuple))))
     `(org-level-8 ((t (,@body ,@variable-tuple))))
     `(org-level-7 ((t (,@body ,@variable-tuple))))
     `(org-level-6 ((t (,@body ,@variable-tuple))))
     `(org-level-5 ((t (,@body ,@variable-tuple))))
     `(org-level-4 ((t (,@headline ,@variable-tuple))))
     `(org-level-3 ((t (,@headline ,@variable-tuple))))
     `(org-level-2 ((t (,@headline ,@variable-tuple))))
     `(org-level-1 ((t (,@headline ,@variable-tuple))))
     `(org-document-title ((t (,@headline ,@variable-tuple :underline nil))))))
  :hook
  ((org-mode . visual-line-mode)
   (org-mode . electric-pair-mode)
   (org-mode . buffer-face-mode)
   (org-mode . visual-fill-column-mode)
   (org-capture-after-finalize . close-orgcapture-frame)
   (org-capture-prepare-finalize . org-id-get-create))
  :config
  (setq org-plantuml-jar-path (expand-file-name "~/bin/plantuml.jar"))
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)
                                                           (dot . t)))

  (setq org-directory "/Users/jahfer/iCloud/Org/")
  (add-to-list 'auto-mode-alist '("\\.org_archive\\'" . org-mode))
  (font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (setq-default
   visual-fill-column-center-text t
   visual-fill-column-fringes-outside-margins t
   visual-fill-column-width 100
   org-log-done t
   org-support-shift-select t
   org-use-speed-commands t
   org-agenda-compact-blocks nil
   org-display-custom-times t
   org-time-stamp-custom-formats '("<%Y/%m/%d %a>" . "<%Y/%m/%d %a %_l:%M%#p>")
   org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
   org-id-locations-file "~/.emacs.d/.org-id-locations"
   org-id-track-globally t
   org-adapt-indentation t
   org-image-actual-width 700
   org-hide-emphasis-markers t
   org-startup-with-inline-images nil
   org-outline-path-complete-in-steps nil
   org-refile-allow-creating-parent-nodes 'confirm
   org-refile-use-outline-path 'file
   org-priority-faces '((?A . (:foreground "red"))
			(?B . (:foreground "#c195dc"))
			(?C . (:foreground "grey")))
   org-todo-keyword-faces '(("CANCELED" . (:foreground "red" :weight bold))
			    ("WAITING" . (:foreground "orange" :weight bold)))
   org-agenda-block-separator nil
   org-global-properties '((Effort_ALL . "5min 10min 30min 1h 2h 1d 1w"))
   org-columns-default-format "#+COLUMNS: %4PRIORITY(Rank) %60ITEM(Task) %TODO(State) %4Effort(Est.){:}"
   org-agenda-hide-tags-regexp (regexp-opt '("work" "home" "note" "unfiled"))
   org-agenda-files (list
		     (concat org-directory "tasks.org")
		     (concat org-directory "tasks.org_archive")
		     (concat org-directory "inbox.org")
		     (concat org-directory "notes.org"))
   org-default-notes-file (concat org-directory "inbox.org")
   org-refile-targets '((("/Users/jahfer/iCloud/Org/tasks.org") . (:maxlevel . 3))
			(("/Users/jahfer/iCloud/Org/notes.org") . (:maxlevel . 4)))
   org-capture-templates '(("n" "Note" entry (file+olp+datetree "/Users/jahfer/iCloud/Org/notes.org" "Daily Notes")
                            "* %?\n")
                           ("t" "Todo" entry (file+headline "/Users/jahfer/iCloud/Org/inbox.org" "Inbox Tasks")
                            "* TODO %?\n"))
   org-agenda-custom-commands  '(("q" "Important tasks"
				  ((todo "STARTED"
					 ((org-agenda-overriding-header " Where you left off")))
				   (tags "work/DUMMY"
					 ((org-agenda-overriding-header " ")))
				   (tags-todo "-TODO=\"STARTED\"+PRIORITY=\"A\""
					      ((org-agenda-overriding-header " Maybe something new"))))
				  ((org-agenda-prefix-format '((agenda  . " %i %-15c%?-12t% s") ;; file name + org-agenda-entry-type
							       (timeline  . "  % s")
							       (todo  . " %i %-19c% s%6 e")
							       (tags . " %i %-19c% s%6 e")
							       (search . " %i %-12:c")))))
				 ("w" "Work list"
				  ;; Block filter
                                  ((tags "unfiled/TODO"
					 ((org-agenda-overriding-header " Unfiled")))
				   (tags "work/DUMMY"
					 ((org-agenda-overriding-header " ")))
				   (todo "STARTED|REVIEWED"
					 ((org-agenda-overriding-header " In Progress")))
				   (tags "work/DUMMY"
					 ((org-agenda-overriding-header " ")))
				   (tags-todo "-unfiled-PRIORITY=\"C\"+TODO=\"TODO\""
					      ((org-agenda-overriding-header " Next")
					       (org-agenda-sorting-strategy '((tags priority-down effort-up category-keep)))))
				   (tags "work/DUMMY"
					 ((org-agenda-overriding-header " ")))
				   (agenda "" ((org-agenda-skip-deadline-if-done t)
					       (org-agenda-skip-scheduled-if-done t)))
				   (tags "work/DUMMY"
					 ((org-agenda-overriding-header " ")))
				   (todo "WAITING"
					 ((org-agenda-overriding-header " Stalled")))
				   (tags "work/DUMMY"
					 ((org-agenda-overriding-header " ")))
				   (tags "PRIORITY=\"C\"+TODO=\"TODO\""
                                         ((org-agenda-overriding-header " Someday")
					  (org-todo-keyword-faces '(("TODO" . (:foreground "#999")))))))
				  ;; Settings
				  ((org-agenda-prefix-format '((agenda  . " %i %-15c%?-12t% s") ;; file name + org-agenda-entry-type
							       (timeline  . "  % s")
							       (todo  . " %i %-19c% s%6 e")
							       (tags . " %i %-19c% s%6 e")
							       (search . " %i %-12:c"))))
				  ("/Users/jahfer/iCloud/Org/public/agenda.html"))
				 ("h" "Home list"
				  ((tags "home/TODO"
					 ((org-agenda-overriding-header "Todo:")))))))
  (org-add-link-type "gh" 'org-gh-open))


;; ERC

(setq erc-hide-list '("JOIN" "PART" "QUIT" "CHANGE"))

;;(require 'ox-publish)
(setq org-publish-project-alist
      '(
	("org-notes"
	 :base-directory "~/Dropbox/org/"
	 :base-extension "org"
	 :publishing-directory "~/src/org-public/"
	 :recursive t
	 :publishing-function org-html-publish-to-html
	 :headline-levels 2
	 :auto-preamble t)

	("org-static"
	 :base-directory "~/Dropbox/org/"
	 :base-extension "css\\|js"
	 :publishing-directory "~/src/org-public/"
	 :recursive t
	 :publishing-function org-publish-attachment)

	("org" :components ("org-notes" "org-static"))))

;; Reminders
; http://doc.norang.ca/org-mode.html (17.1)
; Erase all reminders and rebuilt reminders for today from the agenda
(defun jh/org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))
; Rebuild the reminders everytime the agenda is displayed
(add-hook 'org-agenda-finalize-hook 'jh/org-agenda-to-appt 'append)
; Appointments are set up when Emacs starts
(jh/org-agenda-to-appt)
; Activate appointments so we get notifications
(appt-activate t)
; If we leave Emacs running overnight - reset the appointments one minute after midnight
(run-at-time "24:01" nil 'jh/org-agenda-to-appt)

(defun jh/current-file ()
  (interactive)
  (message (file-truename deft-directory))
  (message (buffer-file-name)))

;; Zetteldeft
(use-package deft
  :init
  (defun jh/deft-open-other ()
    (interactive)
    (deft-open-file-other-window t))
  (defun jh/deft-open-preview ()
    (interactive)
    (deft-open-file-other-window))
  :config
  (setq deft-directory "/Users/jahfer/Library/Mobile Documents/com~apple~CloudDocs/Org/notes/"
        deft-recursive t
        deft-extensions '("org")
	deft-default-extension "org"))

(use-package zetteldeft
  :after deft
  :config
  (zetteldeft-set-classic-keybindings)
  (setq deft-use-filename-as-title t)
  (setq deft-org-mode-title-prefix nil)
  (setq zetteldeft-title-suffix "\n#+STARTUP: showall indent\n#+TAGS: ")
  (setq zetteldeft-backlink-prefix "#+BACKLINK: ")
  (setq zetteldeft-home-id "2020-07-29-1344"))

;; Key bindings

(defun reverse-transpose-sexps (arg)
  (interactive "*p")
  (transpose-sexps (- arg))
  ;; when transpose-sexps can no longer transpose, it throws an error and code
  ;; below this line won't be executed. So, we don't have to worry about side
  ;; effects of backward-sexp and forward-sexp.
  (backward-sexp (1+ arg))
  (forward-sexp 1))

;; Allow moving forms down
(global-set-key (kbd "C-M-y") 'reverse-transpose-sexps)

;; Bind some native Emacs functions.
(global-set-key (kbd "C-x k")    'kill-this-buffer)

;; Cursor management
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Bindings for [[http://magit.github.io][Magit]].
(global-set-key (kbd "C-c m") 'magit-status)

;; Bindings for =move-text=.
(global-set-key (kbd "<s-up>")    'move-text-up)
(global-set-key (kbd "<s-down>")  'move-text-down)

;; org mode
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c o")
                (lambda () (interactive) (find-file (concat org-directory "tasks.org"))))
(global-set-key (kbd "C-c i")
                (lambda () (interactive) (find-file (concat org-directory "inbox.org"))))

;; helm
;; (global-set-key (kbd "C-c h") 'helm-command-prefix)
;; (global-unset-key (kbd "C-x c"))
;; (global-set-key (kbd "M-x") 'helm-M-x)
;; (global-set-key (kbd "C-x b") 'helm-mini)
;; (global-set-key (kbd "C-x C-f") 'helm-find-files)
;; (global-set-key (kbd "C-x C-r") 'helm-recentf)
;; (global-set-key (kbd "C-c g") 'helm-google-suggest)
;; (global-set-key (kbd "C-x C-r")  'helm-recentf)

(global-set-key (kbd "C-=") #'text-scale-increase)
(global-set-key (kbd "C--") #'text-scale-decrease)

(global-set-key (kbd "C-c C-n") 'make-orgcapture-frame)

;; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
;; (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
;; (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
(defun jh/switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(use-package key-chord
  :config
  (key-chord-mode 1)
  (key-chord-define-global "BB" #'jh/switch-to-previous-buffer))

;; projectile

(require 'projectile)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)

(defun jah-copy-file-path (&optional @dir-path-only-p)
  "Copy the current buffer's file path or dired path to `kill-ring'.
Result is full path.
If `universal-argument' is called first, copy only the dir path.

If in dired, copy the file/dir cursor is on, or marked files.

If a buffer is not file and not dired, copy value of `default-directory' (which is usually the “current” dir when that buffer was created)

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

;; (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
;;       helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
;;       helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
;;       helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
;;       helm-ff-file-name-history-use-recentf t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(key-chord company lsp-ui flycheck-rust flycheck cargo toml-mode elpher projectile-ripgrep ripgrep hydra magit exec-path-from-shell graphviz-dot-mode zetteldeft deft plantuml-mode polymode visual-fill-column uncrustify-mode ivy-rich seq paredit multiple-cursors move-text monokai-theme ido-vertical-mode flx-ido use-package auto-complete org-sidebar ruby-end counsel swiper ivy yasnippet helm-fuzzy-find lsp-mode rust-mode doom-themes org cl-lib undo-tree tuareg tree-mode sublime-themes spaceline-all-the-icons smex shadowenv projectile-rails org-sticky-header htmlize helm-projectile helm-mode-manager color-theme-solarized)))
