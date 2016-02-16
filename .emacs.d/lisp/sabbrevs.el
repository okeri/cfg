;; define C abbrevs
(define-abbrev global-abbrev-table "hdrg" ""
  '(lambda() (let ((name buffer-file-name))
	       (when name
		 (setq name (upcase (replace-regexp-in-string
				     "[^a-zA-Z]+" "_" (file-name-nondirectory
						       name)))))
	       (save-excursion 	(beginning-of-buffer)
				(insert "#ifndef " name "\n#define "
					name "\n\n")
				(end-of-buffer)
				(insert "\n\n#endif\n")))))

(define-abbrev global-abbrev-table "incl" "#include <>"
  '(lambda() (backward-char)(error "")))
(define-abbrev global-abbrev-table "inc" "#include \"\""
  '(lambda() (backward-char)(error "")))
(define-abbrev global-abbrev-table  "ifb" "if () {\n\n}"
  '(lambda() (c-indent-line)(backward-char 2)(c-indent-line)
     (search-backward ")")(error "")))
(define-abbrev global-abbrev-table "whileb" "while () {\n\n}"
  '(lambda() (c-indent-line)(backward-char 2)(c-indent-line)
     (search-backward ")") (error "")))
(define-abbrev global-abbrev-table "forb" "for (;;) {\n\n}"
  '(lambda() (c-indent-line) (backward-char 2) (c-indent-line)
       (search-backward ";;")(error "")))
(define-abbrev global-abbrev-table "structb" "struct {\n\n};"
  '(lambda() (c-indent-line) (backward-char 3)
     (c-indent-line)(error "")))
(define-abbrev global-abbrev-table "mainb"
  "int main(int argc, char *argv[]) {\n\nreturn 0;\n}"
  '(lambda()
     (previous-line 1)(c-indent-line)
     (previous-line 1)(c-indent-line)
     (error "")))
(define-abbrev global-abbrev-table "ifeb" "if () {\n\n} else {\n\n}"
  '(lambda()(c-indent-region (- (point) 16) (point) t)
     (search-backward ")")(error "")))
(define-abbrev global-abbrev-table "switchb"
  "switch () {\ncase  :\n\nbreak;\ncase  :\n\nbreak;\ndefault:\n\nbreak;\n}"
  '(lambda()
     (c-indent-region (- (point) 52) (point) t)
     (search-backward ")")(error "")))
