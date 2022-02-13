;; https://github.com/okeri/cfg
(require 'package)
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/"))
(setq packages
      '(cff lsp-ui flycheck yasnippet yaml-mode ivy-rich fish-mode company
	    counsel cmake-mode meson-mode cargo pinentry popup google-translate))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(dolist (package packages)
  (unless (package-installed-p package)
    (package-install package)))

(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'term-file-aliases '("foot" . "xterm"))

(require 'yasnippet nil t)

;; vars
(defalias 'yes-or-no-p 'y-or-n-p)

;; settings
(setq fill-column 80
      xterm-query-timeout nil
      indent-tabs-mode nil
      comment-style 'indent
      inhibit-startup-message t
      make-backup-files nil
      auto-save-list-file-name nil
      auto-save-default nil
      save-abbrevs nil
      column-number-mode t
      format-on-save t
      custom-file "/dev/null"
      company-backends '(company-capf company-files company-nxml company-cmake)
      company-async-timeout 3
      epa-pinentry-mode 'loopback
      project-find-functions '(project-try-ccj project-try-cargo project-try-pvc project-try-makefile)
      interprogram-cut-function 'wl-clipboard
      lsp-enable-links nil
      lsp-eldoc-enable-hover nil
      lsp-enable-folding nil
      lsp-semantic-tokens-enable t
      lsp-semantic-tokens-apply-modifiers nil
      lsp-modeline-diagnostics-enable nil
      lsp-modeline-workspace-status-enable nil
      lsp-imenu-container-name-separator "::"
      lsp-ui-sideline-enable nil
      lsp-ui-doc-enable nil
      lsp-ui-peek-enable nil
      lsp-prefer-flymake nil
      lsp-log-io nil
      lsp-enable-file-watchers nil
      lsp-idle-delay 0.500
      clangd-args '("--header-insertion=never" "--completion-style=detailed" "--pch-storage=memory" "--clang-tidy=1" "--query-driver=/usr/bin/g++")
      read-process-output-max (* 1024 1024)
      flycheck-mode-line '(:eval (my-flycheck-status))
      gc-cons-threshold 100000000
      c-syntactic-indentation nil
      compilation-scroll-output t
      use-dialog-box nil
      vc-annotate-background "black"
      vc-annotate-background-mode nil
      display-line-numbers-width-start 4
      w32-get-true-file-atttributes nil
      gud-key-prefix "\C-x\C-g"
      recentf-max-saved-items 256
      ya-cppref-path-to-doc-root "/usr/share/cpp/reference/"
      ivy-height 16
      ivy-fixed-height-minibuffer t
      ivy-use-virtual-buffers t
      ivy-virtual-abbreviate 'full
      enable-recursive-minibuffers t
      ivy-format-function 'ivy-format-function-line
      cff-source-regexps '(("\\.c$" . (lambda (base) (concat base ".c")))
			   ("\\.cc$" . (lambda (base) (concat base ".cc")))
			   ("\\.cxx$" . (lambda (base) (concat base ".cxx")))
			   ("\\.cpp$" . (lambda (base) (concat base ".cpp")))
			   ("\\.cu$" . (lambda (base) (concat base ".cu"))))

      ivy-rich-display-transformers-list
      '(ivy-switch-buffer
        (:columns
         ((ivy-rich-candidate (:width 0.15))
          (ivy-rich-switch-buffer-size (:width 7))
          (ivy-rich-switch-buffer-major-mode (:width 20 :face warning))
          (get-buffer-project (:width 15 :face success))
	  (get-buffer-relproj-path (:width 115)))
         :predicate
         (lambda (cand) (get-buffer cand)))
        counsel-M-x
        (:columns
         ((counsel-M-x-transformer (:width 40))
          (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
        counsel-describe-function
        (:columns
         ((counsel-describe-function-transformer (:width 40))
          (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
        counsel-describe-variable
        (:columns
         ((counsel-describe-variable-transformer (:width 40))
          (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))
        counsel-recentf
        (:columns
         ((ivy-rich-candidate (:width 0.8))
          (ivy-rich-file-last-modified-time (:face font-lock-comment-face)))))
      google-translate-default-target-language "ru"
      google-translate-default-source-language "de"
      auto-mode-alist (append auto-mode-alist '(("\\.rs\\'" . rust-mode)
						("\\.cu\\'" . c++-mode)
						("\\.cl\\'" . c-mode)
						("\\.sl\\'" . c-mode)
						("\\.qml\\'" . qml-mode))))

;; init
(normal-erase-is-backspace-mode 0)
(show-paren-mode 1)
(menu-bar-mode 0)
(xterm-mouse-mode)
(recentf-mode 1)
(ivy-mode 1)
(ivy-rich-mode 1)
(counsel-mode 1)
(put 'downcase-region 'disabled nil)
(pinentry-start)

;; bindings
(global-set-key [f7] 'my-compile)
(global-set-key [f8] 'next-error)
(global-set-key [\C-f8] 'previous-error)
(global-set-key [f9] 'flycheck-next-error)
(global-set-key [\C-f9] 'flycheck-previous-error)
(global-set-key [f10] 'toggle-format-on-save)
(global-set-key [f12] 'kill-emacs)
(global-set-key [\C-/] 'undo)
(global-set-key [\C-_] 'undo)
(global-set-key [?\C-c left] 'uncomment-region)
(global-set-key [?\C-c right] 'comment-region)
(global-set-key [?\C-c ?d] 'vc-diff)
(global-set-key [?\C-c ?w] 'vc-annotate)
(global-set-key [?\C-c ?l] 'vc-print-log)
(global-set-key [?\C-c ?k] 'vc-region-history)
(global-set-key [?\C-c ?\C-c] 'counsel-imenu)
(global-set-key [?\C-c ?\t] 'untabify)
(global-set-key [?\C-x ?x] 'previous-multiframe-window)
(global-set-key [?\C-x ?\C-x] 'next-multiframe-window)
(global-set-key [?\C-c ?\C-g] 'google-translate-at-point)
(global-set-key [?\C-c ?g] 'google-translate-at-point-reverse)
(global-set-key [\C-left] 'previous-multiframe-window)
(global-set-key [\C-right] 'next-multiframe-window)
(global-set-key [?\C-x ?e] 'counsel-git-grep)
(global-set-key [?\C-x ?\C-e] 'counsel-ag)
(global-set-key [?\C-r] 'swiper-thing-at-point)
(global-set-key [?\C-s] 'swiper)
(global-set-key [?\C-x ?f] 'ivy-switch-buffer)
(global-set-key [?\C-x ?g] 'ivy-switch-buffer-other-window)
(global-set-key [(meta /)] 'company-manual-begin)
(global-set-key [?\C-j] 'eval-print-last-sexp)
(global-set-key [mouse-4] 'scroll-down)
(global-set-key [mouse-5] 'scroll-up)


;; extensions
(defun wl-clipboard(text)
  (setq wl-copy-process
	  (make-process
	   :name "wl-copy"
	   :buffer nil
	   :command '("wl-copy" "-f" "-n")
	   :connection-type 'pipe))
    (process-send-string wl-copy-process text)
    (process-send-eof wl-copy-process))

(defun message-box(st &optional crap)
  (dframe-message st))

;; project support
(defun set-project-name (path)
  ;; TODO: look for same names in opening buffers
  (setq-local project-name (file-name-nondirectory path)))

(defun project-try-template (dir id)
  (let* ((f (locate-dominating-file dir id))
	 (proj (when f (file-name-directory f))))
    (when proj
      (setq-local project (file-truename (directory-file-name proj)))
      (set-project-name project)
      (cons proj (concat proj (file-name-directory id))))))

(defun project-try-pvc(dir)
  (let ((proj (project-try-vc dir)))
    (setq-local project
		(if proj
		    (file-truename (directory-file-name (cdr proj)))
		  ""))
    (set-project-name project)
    proj))

(defun project-try-ccj(dir)
  (let ((proj (or (project-try-template dir "compile_commands.json")
		  (project-try-template dir "build/compile_commands.json"))))
    (when proj
      (setq-local ccjpath (cdr proj))
      (when (file-exists-p (concat ccjpath "build.ninja"))
	(setq-local buildpath (cons "ninja" ccjpath)))
      (cons 'vc (car proj)))))

(defun project-try-makefile(dir)
  (let ((proj (or (project-try-template dir "Makefile")
		  (project-try-template dir "build/Makefile"))))
    (when proj
      (setq-local buildpath (cons "make" (cdr proj)))
      (cons 'vc (car proj)))))

(defun project-try-cargo(dir)
  (let ((proj (project-try-template dir "Cargo.toml")))
    (when proj
      (cons 'vc (car proj)))))

(defun find-compilation-database()
    (if (boundp 'ccjpath) ccjpath
      (let ((root (car (project-roots (project-current t)))))
	(if (boundp 'ccjpath)
	    (file-truename ccjpath)
	  root))))

(defun get-buffer-project (candidate)
  (let* ((buffer (get-buffer candidate)))
	(if (local-variable-p 'project-name buffer)
	    (buffer-local-value 'project-name buffer) "")))


(defun get-buffer-relproj-path (candidate)
  (let* ((buffer (get-buffer candidate))
	 (filename (buffer-local-value 'buffer-file-name buffer))
	 (proj
	    (when (local-variable-p 'project buffer)
	      (buffer-local-value 'project buffer)))
	 (relpath
	  (if filename
	      (if (local-variable-p 'project buffer)
		  (let ((proj (buffer-local-value 'project buffer)))
		    (if (not (string-empty-p proj))
			(substring-no-properties filename (+ 1 (length proj)))
		      filename))
		filename)
	    "")))
    relpath))

(defun my-compile()
  "Suggest to compile of project directory"
  (interactive)
  (when (or (not (boundp 'compile-history))
	    (= (length compile-history) 0))
    (setq compile-history '("make -k ")))
  (when (and (boundp 'project) project)
    (let ((curr
	   (if (and (boundp 'buildpath)
		    (file-directory-p (cdr buildpath)))
	       (concat (car buildpath) " -C " (cdr buildpath))
	     (concat "make -k -C " project))))
    (unless (catch 'found
	      (dolist (v compile-history)
		(when (string= curr v)
		  (throw 'found t)))
	      nil)
      (push curr compile-history))))
  (when (> (length compile-history) 0)
    (setq compile-command (car compile-history)))
  (execute-extended-command nil "compile"))

;; lsp, code, formatting
(defun setup-lsp()
  (setq lsp-clients-clangd-args clangd-args)
  (add-to-list 'lsp-clients-clangd-args  (concat "--compile-commands-dir=" (find-compilation-database)))
  (lsp)
  (yas-minor-mode-on)
  (local-set-key [?\C-x ?d] 'cff-find-other-file)
  (local-set-key [?\C-x ?\C-r] 'lsp-rename)
  (local-set-key [?\C-x ?\C-d] 'lsp-find-definition)
  (local-set-key [?\C-x ?\C-a] 'xref-pop-marker-stack)
  (local-set-key [?\C-c ?\C-c] 'counsel-imenu))

(defun format-region(start end)
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point) (point))))
  (lsp-format-region start end)
  (back-to-indentation))

(defun my-lsp-dispay() 
  "Alternative (minimalistic) UI to lsp-ui-doc :)"
  (when (bound-and-true-p lsp-mode)
    (let ((diags (lsp--cur-line-diagnotics)))
      (when (and (= (length diags) 0) (lsp--capability "hoverProvider"))
	(lsp--send-request-async
	 (lsp--make-request "textDocument/hover"
			    (lsp--text-document-position-params))
	 (lambda (info)
	   (when info
	     (message (replace-regexp-in-string "%" "" (lsp-ui-doc--extract
						(gethash "contents" info)))))))))))
(defun hl-nonzero(sym hl)
  (if (eq sym 0) "0"
    (propertize (int-to-string sym) 'face hl)))

(defun my-flycheck-status (&optional status)
  (let ((text (pcase (or status flycheck-last-status-change)
                (`not-checked "")
                (`no-checker "-")
                (`running "*")
                (`errored "!")
                (`finished
                 (let-alist (flycheck-count-errors flycheck-current-errors)
                   (if (or .error .warning)
		       (format "%s|%s" (hl-nonzero (or .error 0) 'error)
			       (hl-nonzero (or .warning 0) 'warning))
                     "ok")))
                (`interrupted ".")
                (`suspicious "?"))))
    (if (string-empty-p text) ""
      (concat "  check: " text))))

(defun toggle-format-on-save()
  (interactive)
  (if format-on-save
      (prog1
	  (setq format-on-save nil)
	(message "format on save disabled"))
    (prog1
	(setq format-on-save t)
      (message "format on save enabled"))))

;; hacks
(with-eval-after-load 'counsel
  (let ((done (where-is-internal #'ivy-done     ivy-minibuffer-map t))
	(alt  (where-is-internal #'ivy-alt-done ivy-minibuffer-map t)))
    (define-key counsel-find-file-map done #'ivy-alt-done)
    (define-key counsel-find-file-map alt  #'ivy-done)))

(with-eval-after-load 'ivy
  (define-key ivy-switch-buffer-map [?\C-d] #'ivy-switch-buffer-kill))

(with-eval-after-load 'yasnippet
  (yas-load-directory (car yas-snippet-dirs) t))

(with-eval-after-load 'google-translate-tk
  (defun google-translate--search-tkk ()
    "Search TKK."
    (list 430675 2721866130)))

(defadvice vc-mode-line (after strip-backend () activate)
  (when (stringp vc-mode)
      (setq vc-mode (concat " " (replace-regexp-in-string 
                   (format "^ %s[:-]" (vc-backend buffer-file-name))
                   " " vc-mode)))))

(defun my-lsp-headerline--arrow-icon () "" "")
(advice-add 'lsp-headerline--arrow-icon :override #'my-lsp-headerline--arrow-icon)

;; hooks and etc...
(add-hook 'lsp-eldoc-hook 'my-lsp-dispay)
(add-hook 'prog-mode-hook
	  (lambda()
	    (display-line-numbers-mode)
	    (company-mode)))


(add-hook 'c-mode-common-hook
	  (lambda()
	    (find-compilation-database)
	    (local-set-key (kbd "TAB") 'format-region)
	    (abbrev-mode 0)
	    (setup-lsp)))

(add-hook 'java-mode-hook
	  (lambda ()
	    (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
	    (modify-syntax-entry ?@ "< b" java-mode-syntax-table)
	    (setup-lsp)))

(add-hook 'rust-mode-hook
	  (lambda()
	    (local-set-key [\C-f7] 'compile)
	    (local-set-key [f7] 'cargo-process-build)
	    (setup-lsp)))

(add-hook 'emacs-lisp-mode-hook
	  (lambda()
	    (local-set-key [?\C-x ?\C-d] 'find-function-at-point)))

(add-hook 'python-mode-hook 'setup-lsp)
(add-hook 'meson-mode-hook 'find-compilation-database)

(add-hook 'before-save-hook
	  (lambda()
	    (when format-on-save
	      (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
		(lsp-format-buffer))
	      (when (derived-mode-p 'python-mode)
		(delete-trailing-whitespace)))))

(add-hook 'window-configuration-change-hook
	  (lambda ()
	    (setq mode-line-format
		  '("%e" mode-line-modified mode-line-buffer-identification
		    "  " mode-name (vc-mode vc-mode)
		    flycheck-mode-line "  " mode-line-position
		    mode-line-end-spaces))))

(add-hook 'conf-mode-hook 'display-line-numbers-mode)

;; theme
(deftheme okeri)
(custom-theme-set-faces 'okeri
 '(error ((t (:foreground "#cd0000"))))
 '(success ((t (:bold t :foreground "#228b22"))))
 '(shadow ((t (:foreground "#767676"))))
 '(highlight ((t (:background "#303030"))))
 '(region ((t (:inherit highlight))))
 '(company-tooltip ((t (:background "grey" :foreground "black"))))
 '(company-tooltip-selection ((t (:background "#005f5f" :foreground "black"))))
 '(diff-added ((t (:foreground "#00cd00"))))
 '(diff-context ((t (:foreground "white"))))
 '(diff-file-header ((t (:bold t :foreground "#585858"))))
 '(diff-header ((t (:foreground "#6c6c6c"))))
 '(diff-removed ((t (:foreground "#cd0000"))))
 '(diff-refine-added ((t (:background "#002000"))))
 '(diff-refine-changed ((t (:background "#202020"))))
 '(diff-refine-removed ((t (:background "#200000"))))
 '(font-lock-comment-face ((t (:inherit shadow))))
 '(font-lock-constant-face ((t (:foreground "#af87ff"))))
 '(font-lock-doc-face ((t (:bold t :foreground "#d75f00"))))
 '(font-lock-function-name-face ((t (:foreground "#ffffff")))) ;
 '(font-lock-keyword-face ((t (:foreground "#00cdcd"))))
 '(font-lock-type-face ((t (:foreground "#20afff"))))
 '(font-lock-string-face ((t (:foreground "#63a330"))))
 '(font-lock-preprocessor-face ((t (:foreground "#ff8070"))))
 '(font-lock-variable-name-face ((t  (:inherit default))))
 '(font-lock-builtin-face ((t (:foreground "#5f5f87"))))
 '(lsp-ui-doc-background ((t (:background "#00005f"))))
 '(lsp-face-semhl-interface ((t  (:inherit font-lock-variable-name-face))))
 '(lsp-face-semhl-definition ((t (:inherit font-lock-function-name-face))))
 '(lsp-face-semhl-macro ((t  (:inherit default))))
 '(lsp-face-semhl-namespace ((t  (:inherit font-lock-constant-face))))
 '(lsp-headerline-breadcrumb-path-face ((t (:foreground "#20afff"))))
 '(minibuffer-prompt ((t (:foreground "#00afff"))))
 '(org-table ((t (:foreground "#00cdcd"))))
 '(line-number ((t (:foreground "#626262"))))
 '(mode-line ((t (:background "#1a1a1a" :foreground "#767676" :box (:line-width -1 :style released-button)))))
 '(mode-line-inactive ((t (:background "#121212" :foreground "#444444" :box (:line-width -1 :color "#121212" :style nil))))))

(enable-theme 'okeri)
