(require 'company-racer)
(require 'rust-mode)
(require 'cargo-process)
(require 'racer)


(setq racer-rust-src-path "/usr/src/rust/src")

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(add-hook 'rust-mode-hook
	  (lambda()
	    (local-set-key [f7] 'cargo-process-build)
	    (local-set-key [?\C-x ?\C-r] 'racer-describe)
	    (local-set-key [?\C-x ?\C-d] 'racer-find-definition)
	    (local-set-key [?\C-x ?d] 'racer--find-file)
	    (racer-mode)))
