;; https://github.com/okeri/cfg
(require 'package)
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/"))
(setq packages
      '(cff eglot yaml-mode yasnippet ivy-rich fish-mode company rust-mode
	    counsel cmake-mode meson-mode cargo pinentry popup google-translate))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(dolist (package packages)
  (unless (package-installed-p package)
    (package-install package)))

(require 'yasnippet)
(require 'eglot)

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
      interprogram-cut-function 'wl-clipboard
      read-process-output-max (* 1024 1024)
      check-mode-line '(:eval (check-status))
      gc-cons-threshold 100000000
      c-syntactic-indentation nil
      compilation-scroll-output t
      use-dialog-box nil
      vc-annotate-background "black"
      vc-annotate-background-mode nil
      display-line-numbers-width-start 4
      w32-get-true-file-atttributes nil
      gud-key-prefix "\C-x\C-g"
      eldoc-echo-area-display-truncation-message nil
      recentf-max-saved-items 512
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
(put 'check-mode-line 'risky-local-variable t)
(pinentry-start)
(add-to-list 'term-file-aliases '("foot" . "xterm"))
(add-to-list 'eglot-server-programs '((c++-mode c-mode) . ("clangd" "--header-insertion=never" "--completion-style=detailed" "--pch-storage=memory" "--query-driver=/usr/bin/g++")))

;; bindings
(global-set-key [f6] 'flymake-show-buffer-diagnostics)
(global-set-key [f7] 'compile)
(global-set-key [f8] 'next-error)
(global-set-key [\C-f8] 'previous-error)
(global-set-key [f9] 'flymake-goto-next-error)
(global-set-key [\C-f9] 'flymake-goto-previous-error)
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
(global-set-key [?\C-c ?c] 'eglot-code-actions)
(global-set-key [?\C-c ?\C-r] 'eglot-rename)
(global-set-key [?\C-c ?\t] 'untabify)
(global-set-key [?\C-c ?\C-g] 'google-translate-at-point)
(global-set-key [?\C-c ?g] 'google-translate-at-point-reverse)
(global-set-key [?\C-c ?e] 'counsel-ag)
(global-set-key [\C-left] 'previous-multiframe-window)
(global-set-key [\C-right] 'next-multiframe-window)
(global-set-key [?\C-x ?e] 'thing-counsel-git-grep)
(global-set-key [?\C-x ?\C-e] 'thing-counsel-ag)
(global-set-key [?\C-x ?\C-r] 'xref-find-references)
(global-set-key [?\C-x ?\C-d] 'xref-find-definitions)
(global-set-key [?\C-x ?\C-a] 'xref-pop-marker-stack)
(global-set-key [?\C-x ?x] 'previous-multiframe-window)
(global-set-key [?\C-x ?\C-x] 'next-multiframe-window)
(global-set-key [?\C-r] 'swiper-thing-at-point)
(global-set-key [?\C-s] 'swiper)
(global-set-key [?\C-x ?f] 'ivy-switch-buffer)
(global-set-key [?\C-x ?g] 'ivy-switch-buffer-other-window)
(global-set-key [(meta /)] 'company-manual-begin)
(global-set-key [?\C-j] 'eval-print-last-sexp)
(global-set-key [mouse-4] 'scroll-down)
(global-set-key [mouse-5] 'scroll-up)


;; extensions
(defalias 'yes-or-no-p 'y-or-n-p)

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

(defun action-at-point(action)
  (interactive)
  (let ((thing (ivy-thing-at-point)))
    (when (use-region-p)
      (deactivate-mark))
    (funcall action (regexp-quote thing))))

(defun thing-counsel-ag()
  (interactive)
  (action-at-point 'counsel-ag))

(defun thing-counsel-git-grep()
  (interactive)
  (action-at-point 'counsel-git-grep))

(with-eval-after-load 'counsel
  (let ((done (where-is-internal #'ivy-done     ivy-minibuffer-map t))
	(alt  (where-is-internal #'ivy-alt-done ivy-minibuffer-map t)))
    (define-key counsel-find-file-map done #'ivy-alt-done)
    (define-key counsel-find-file-map alt  #'ivy-done)))

(with-eval-after-load 'ivy
  (define-key ivy-switch-buffer-map [?\C-d] #'ivy-switch-buffer-kill))

;; project paths & compilation support
(defun update-buildinfo(project)
  (when (or (not (boundp 'compile-history))
 	     (= (length compile-history) 0))
     (setq compile-history '("make -k ")))
  (when (not (boundp 'project-buildinfo))
    (setq project-buildinfo (make-hash-table :size 16 :test 'equal)))
  (when (not (gethash project project-buildinfo))
    (let ((buildcmd (cond
		  ((file-exists-p (concat project "build/build.ninja"))
		   (concat "ninja -C " (concat project "build")))
		  ((file-exists-p (concat project "build/Makefile"))
		   (concat "make -C " (concat project "build")))
		  ((file-exists-p (concat project "Makefile"))
		   (concat "make -C " project)))))
      (puthash project buildcmd project-buildinfo)
      (when buildcmd
	(push buildcmd compile-history))
      (setq compile-command (car compile-history)))))


(advice-add
 'project-try-vc
 :around
 (lambda (func &rest args)
   (when buffer-file-name
     (let ((data (apply func args)))
       (when data
	 (let* ((project (cdr data))
		(project-path (file-truename project)))
	   (setq-local project-name
		       (file-name-nondirectory (directory-file-name project)))
	   (setq-local project-file
		       (substring-no-properties buffer-file-name
						(length project-path)))
	   (update-buildinfo project-path)))
       data))))

(defun safe-local-string(buffer symbol)
  (or (and (buffer-local-boundp symbol buffer) (buffer-local-value symbol buffer))
      ""))

(defun get-buffer-project (candidate)
  (safe-local-string (get-buffer candidate) 'project-name))

(defun get-buffer-relproj-path (candidate)
  (safe-local-string (get-buffer candidate) 'project-file))


;; mode-line
(defun hl-nonzero(sym hl)
  (if (eq sym 0) "0"
    (propertize (int-to-string sym) 'face hl)))

(defun flymake-diags-count(type)
    (let ((count 0))
      (dolist (d (flymake-diagnostics))
	(when (= (flymake--severity type)
		 (flymake--severity (flymake-diagnostic-type d)))
          (cl-incf count)))
      count))

(defun check-status()
  (if (bound-and-true-p eglot--managed-mode)
      (let ((errors (flymake-diags-count :error))
	    (warnings (flymake-diags-count :warning)))
	(concat "  check: "
		(if (or (< 0 warnings) (< 0 errors))
		    (format "%s|%s"
			    (hl-nonzero (or errors 0) 'error)
			    (hl-nonzero (or warnings 0) 'warning))
		  "ok")))
    ""))

(advice-add 'vc-mode-line :after (lambda (&rest args)
  (when (stringp vc-mode)
      (setq vc-mode (concat " " (replace-regexp-in-string
                   (format "^ %s[:-]" (vc-backend buffer-file-name))
                   " " vc-mode))))))

(add-hook 'window-configuration-change-hook
	  (lambda()
	    (setq mode-line-format
		  '("%e" mode-line-modified mode-line-buffer-identification
		    "  " mode-name (vc-mode vc-mode)
		    check-mode-line "  " mode-line-position
		    mode-line-end-spaces))
	    (set-display-table-slot standard-display-table 5 ?│)))

;; code, formatting
(defun format-region(start end)
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point) (point))))
  (eglot-format start end)
  (back-to-indentation))


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
(advice-add 'eglot--format-markup :filter-args
	    (lambda (markup)
	      (let ((elem (car markup)))
		(plist-put elem :value (replace-regexp-in-string
			  (regexp-quote "  \n") "\n"
			  (plist-get elem :value) nil 'literal)))
	       markup))

(with-eval-after-load 'google-translate-tk
  (defun google-translate--search-tkk ()
    "Search TKK."
    (list 430675 2721866130)))

(add-hook 'eglot-managed-mode-hook
          (lambda ()
            (setq eldoc-documentation-functions
                  (cons #'flymake-eldoc-function
                        (remove #'flymake-eldoc-function eldoc-documentation-functions)))
	    ))

;; programming
(add-hook 'prog-mode-hook
	  (lambda()
	    (display-line-numbers-mode)
	    (company-mode)
	    (yas-minor-mode-on)))

(add-hook 'c-mode-common-hook
	  (lambda()
	    (local-set-key [?\C-x ?d] 'cff-find-other-file)
	    (local-set-key (kbd "TAB") 'format-region)
	    (local-set-key [?\C-c ?\C-c] 'counsel-imenu)
	    (abbrev-mode 0)
	    (eglot-ensure)))

(add-hook 'java-mode-hook
	  (lambda()
	    (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
	    (modify-syntax-entry ?@ "< b" java-mode-syntax-table)
	    (eglot-ensure)))

(add-hook 'rust-mode-hook
	  (lambda()
	    (local-set-key [f7] 'cargo-process-build)
	    (eglot-ensure)))

(add-hook 'before-save-hook
	  (lambda()
	    (when format-on-save
	      (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'rust-mode)
		(eglot-format-buffer))
	      (when (derived-mode-p 'prog-mode)
		(delete-trailing-whitespace)))))

(add-hook 'python-mode-hook
	  (lambda()
	    (local-set-key [?\C-c ?\C-r] 'eglot-rename)
	    (eglot-ensure)))

(add-hook 'conf-mode-hook 'display-line-numbers-mode)

;; theme
(deftheme okeri)
(custom-theme-set-faces 'okeri
 '(error ((t (:foreground "red"))))
 '(success ((t (:bold t :foreground "#228b22"))))
 '(shadow ((t (:foreground "#767676"))))
 '(highlight ((t (:background "#303030"))))
 '(region ((t (:inherit highlight))))
 '(company-tooltip ((t (:background "grey" :foreground "black"))))
 '(company-tooltip-selection ((t (:background "#005f5f" :foreground "black"))))
 '(diff-added ((t (:foreground "green"))))
 '(diff-context ((t (:foreground "white"))))
 '(diff-file-header ((t (:bold t :foreground "#585858"))))
 '(diff-header ((t (:foreground "#6c6c6c"))))
 '(diff-removed ((t (:foreground "red"))))
 '(diff-refine-added ((t (:background "#002000"))))
 '(diff-refine-changed ((t (:background "#202020"))))
 '(diff-refine-removed ((t (:background "#200000"))))
 '(font-lock-comment-face ((t (:inherit shadow))))
 '(font-lock-constant-face ((t (:foreground "#af87ff"))))
 '(font-lock-doc-face ((t (:bold t :foreground "#d75f00"))))
 '(font-lock-function-name-face ((t (:foreground "brightwhite")))) ;
 '(font-lock-keyword-face ((t (:foreground "cyan"))))
 '(font-lock-type-face ((t (:foreground "#20afff"))))
 '(font-lock-string-face ((t (:foreground "#3cb371"))))
 '(font-lock-preprocessor-face ((t (:foreground "#ff8070"))))
 '(font-lock-variable-name-face ((t  (:inherit default))))
 '(font-lock-builtin-face ((t (:foreground "#5f5f87"))))
 '(eglot-diagnostic-tag-unnecessary-face ((t  (:inherit warning))))
 '(minibuffer-prompt ((t (:foreground "#00afff"))))
 '(org-table ((t (:inherit "font-lock-keyword-face"))))
 '(line-number ((t (:foreground "#626262"))))
 '(mode-line ((t (:background "#1a1a1a" :foreground "#767676" :box (:line-width -1 :style released-button)))))
 '(mode-line-inactive ((t (:background "#121212" :foreground "#444444" :box (:line-width -1 :color "#121212" :style nil))))))
(enable-theme 'okeri)
