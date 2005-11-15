;; LPC mode
;;
;; Emacs Lisp Archive Entry
;; Package: lpc-mode
;; Filename: lpc-mode.el
;; Version: 0.12
;; Keywords: languages, LPC
;; Author: Vivek Dasmohapatra <vivek@etla.org>
;; Maintainer: Vivek Dasmohapatra <vivek@etla.org>
;; Created: 2002-08-31
;; Description: syntax highlighting/indentation for LPC
;; URL: http://rtfm.etla.org/emacs/lpc-mode/
;; Compatibility: Emacs21
;; Last-Updated: Tue 2002-09-17 23:59:51 +0100

;; This file is NOT part of GNU Emacs

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;; Copyright (C) 2002 Vivek Dasmohapatra <vivek@etla.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; OK Nick: first stab at LPC mode:
;; 0.01: 'foo and 'C' should be handled correctly.
;; 0.02 ... 0.06: intermediates.
;; 0.07: ultra-hairy ({#'[][,&] syntax now scanned for. Bleurgh.
;; 0.08: ({ ... }) syntax added as brace-list
;; 0.09: rip up and rewrite as a proper cc-mode based mode
;; 0.10: miscellaneous bugfixes.
;; 0.11: should compile cleanly now _and_ work as well (I hope)
;; 0.12: bug in `lpc-font-lock-map' keyword/function definition highlighting

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HOW TO INSTALL:
;;
;; ;; either put lpc-mode.el in your load path and use:
;; (autoload 'lpc-mode  "lpc-mode" t)
;;
;; ;; or have:
;; (autoload 'lpc-mode  "/path/to/lpc-mode.el" t)
;;
;; ;; then:
;; (setq auto-mode-alist
;;       (append '(("\\.lpc$" . lpc-mode)) auto-mode-alist)) )
;;
;; Nick: You'll have to do similar things to handler.el to get that to
;; work, let me know if you need this done.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; elisp-dep-block >>
(require 'custom    );(defface)
(require 'cc-mode   );(c-electric-brace)
(require 'regexp-opt);(regexp-opt-depth regexp-opt)
(require 'font-lock )
  ;;(font-lock-add-keywords font-lock-fontify-region font-lock-mode)
;; elisp-dep-block <<

(defconst lpc-mode-version "0.12")

(eval-and-compile
  (defmacro lpc-defsyntax (name doc klist)
    "Declare a cc-mode syntax variable of lpc-N-keywords and a regex
lpc-N-regex to go along with it, based on the keyword list K."
    (let* ((n  name )
	   (d  doc  )
	   (k  klist)
	   (ln (format "lpc-%s-keywords" n))
	   (ld (format "%s (list)"       d))
	   (ls (intern                  ln))
	   (rn (format "lpc-%s-regex"    n))
	   (rd (format "%s (regex)"      d))
	   (rs (intern                  rn))
	   (kwds                        nil))
      (setq kwds (if (stringp (car k)) k (eval k)))
      ;;(message "%s" (format "%S" kwds))
      `(progn (defconst ,ls ',kwds              ,ld)
	      (defconst ,rs (regexp-opt ',kwds) ,rd)) ))

  (lpc-defsyntax type
		 "LPC primitive type keywords."
		 ("int" "mapping" "mixed" "object" "status" "string" "void"))

  (lpc-defsyntax specifier
		 "LPC declaration specifier keywords."
		 ("nomask" "private" "public" "static" "varargs"))

  (lpc-defsyntax other-decl
		 "LPC keywords starting other decl-level constructs."
		 ("inherit"))

  (lpc-defsyntax block-stmt-1
		 "LPC keywords followed directly by a block."
		 ("do" "else"))

  (lpc-defsyntax block-stmt-2
		 "LPC keywords followed by a paren sexp and then by a block."
		 ("for" "if" "switch" "while" "foreach"))
  
  (lpc-defsyntax simple-stmt
		 "LPC statement keywords followed by an expression or nothing."
		 ("break" "continue" "return"))

  (lpc-defsyntax label
		 "LPC keywords introducing labels in blocks."
		 ("case" "default"))

  (lpc-defsyntax all
		 "LPC keywords."
		 (append lpc-type-keywords
			 lpc-specifier-keywords
			 lpc-other-decl-keywords
			 lpc-block-stmt-1-keywords
			 lpc-block-stmt-2-keywords
			 lpc-simple-stmt-keywords
			 lpc-label-keywords       ))

  (lpc-defsyntax default-highlight
		 "LPC keywords (for default highlighting)"
		 (append lpc-specifier-keywords
			 lpc-block-stmt-1-keywords
			 lpc-block-stmt-2-keywords
			 lpc-simple-stmt-keywords ))
  
  (lpc-defsyntax conditional
		 "LPC conditional keywords"
		 (append lpc-block-stmt-1-keywords lpc-block-stmt-2-keywords))
  )

(defconst lpc-comment-start-regex c-C++-comment-start-regexp)
(defconst lpc-special-brace-lists '((?{ . ?})) )
(defconst lpc-magic-quote-comma   '(9))
(defconst lpc-magic-symbol-name   '(3))

(defvar lpc-mode-syntax-table nil)
(if lpc-mode-syntax-table
    nil
  (setq lpc-mode-syntax-table    (make-syntax-table))
  (c-populate-syntax-table     lpc-mode-syntax-table)
  (modify-syntax-entry ?\' "'" lpc-mode-syntax-table) )

(defun lpc-modify-syntax-at (beg end syntax)
  "Apply a syntax-property value syntax from beg to end."
  (if (<= (point-max) end) nil; noop
    (progn
      ;;(message "(%d x %d) => %S" beg end syntax)
      (put-text-property beg      end 'syntax-table   syntax)
      (put-text-property (1- end) end 'rear-nonsticky t     ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code by Seth Golub <seth AT cs DOT wustl DOT edu>, 1996-02-01,
;; no licence.
;;
;; modified slightly to bring this up to date, didn't work quite right
;; out of the box:
(defun lpc-maybe-electric-brace (arg)
  "Insert character and maybe correct line's indentation."
  (interactive "P")
  (if (= last-command-char ?{)
      (if (= (preceding-char) ?\()
          (self-insert-command (prefix-numeric-value arg))
        (c-electric-brace arg))
    ;; (= last-command-char ?})
    (let (start-point state containing-sexp)
      (save-excursion (beginning-of-defun)
                      (setq start-point (point)))
      (save-excursion (setq state (parse-partial-sexp (point) start-point 0)))
      (setq containing-sexp (car (cdr state)))
      (if (and containing-sexp (save-excursion
                                 (goto-char (1- containing-sexp))
                                 (looking-at "(")))
          (progn
            (self-insert-command (prefix-numeric-value arg))
            (lpc-scan-magic-quote))
        (c-electric-brace arg)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst lpc-magic-quote-regex "({\\s-*#'\\([^\\s-\n,}]+\\|,\\)\\s-*[,}]")

(defun lpc-magic-comma-p (pt)
  (let ((bol nil) (eol nil) (pos nil) (ret nil))
    (save-excursion
      (goto-char pt)
      (end-of-line)
      (setq eol (point))
      (beginning-of-line)
      (setq bol (point))
      (while (and (not ret)
                  (setq pos (re-search-forward lpc-magic-quote-regex eol t)))
        ;;(message "magic pattern at %d/%d" (1- pos) pt)
        (if (/= (1- pos) pt) nil
          (setq ret (list (- (match-beginning 1) 1)
                          (match-beginning       1)
                          (match-end             1)
                          bol)) ) )) ret))

(defun lpc-scan-magic-quotes ()
  (save-excursion
    (let ((qpos nil) (wbeg nil) (wend nil))
      (while (re-search-forward lpc-magic-quote-regex nil t)
        (setq qpos (+ (match-beginning 0) 3)
              wbeg (match-beginning       1)
              wend (match-end             1))
        (lpc-modify-syntax-at qpos (1+ qpos) lpc-magic-quote-comma)
        (lpc-modify-syntax-at wbeg wend      lpc-magic-symbol-name)
        )
      )
    )
  )

(defun lpc-scan-magic-quote ()
  (save-excursion
    (let ((coord nil) (qpos nil) (wbeg nil) (wend nil) (bol nil))
      (if (setq coord (lpc-magic-comma-p (1- (point))))
          (progn
            (setq qpos  (car         coord)
                  wbeg  (cadr        coord)
                  wend  (car  (cddr coord))
                  bol   (cadr (cddr coord)))
            ;;(message "magic pattern at (%d %d %d)" qpos wbeg wend)
            (lpc-modify-syntax-at qpos (1+ qpos) lpc-magic-quote-comma)
            (lpc-modify-syntax-at wbeg wend      lpc-magic-symbol-name)
            (font-lock-fontify-region bol wend) )
        )
      )
    )
  )

(defun lpc-maybe-quote-ref (arg)
  "Kludge to work around multiple syntactic meanings of `,' `[' et al in LPC."
  (interactive "P")
  (self-insert-command (prefix-numeric-value arg))
  (lpc-scan-magic-quote) )

(defvar lpc-mode-map nil "Keymap for LPC mode buffers.")
(if lpc-mode-map
    nil
  (setq lpc-mode-map (c-make-inherited-keymap))
  (define-key lpc-mode-map "\C-c:"    'c-scope-operator)
  (define-key lpc-mode-map "{"        'lpc-maybe-electric-brace)
  (define-key lpc-mode-map "}"        'lpc-maybe-electric-brace)
  (define-key lpc-mode-map ","        'lpc-maybe-quote-ref)
  (define-key lpc-mode-map "\C-c\C-e" 'c-macro-expand)
  )

(defvar lpc-mode-hook nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; font-lock support:

(defvar  lpc-reference-face 'lpc-reference-face)
(defface lpc-reference-face
  '((((class color) (background  dark)) (:foreground "bisque"   ))
    (((class color) (background light)) (:foreground "dark blue")))
  "LPC mode face for quoted symbols")

(defconst lpc-type-depth (regexp-opt-depth lpc-type-regex))

(defconst lpc-font-lock-map
  (append
   c-font-lock-keywords-1
   (list
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; what follows is mostly ripped from font-lock.el, mostly...
    `(eval . (cons (concat "\\<\\(" ,lpc-type-regex "\\)\\>")
		   'font-lock-type-face))
    
    (concat "\\<\\(" lpc-default-highlight-regex "\\)\\>")

    '("\\<\\(case\\)\\>" (1 font-lock-keyword-face)
      ("\\(-[0-9]+\\|\\sw+\\)"
       ;; Return limit of search.
       (save-excursion (skip-chars-forward "^:\n") (point))
       nil
       (1 font-lock-constant-face nil t)))

    '(":" ("^[ \t]*\\(\\sw+\\)[ \t]*:[ \t]*$"
	   (beginning-of-line) (end-of-line)
	   (1 font-lock-constant-face)))

    `(eval . (list
	      (concat "\\<\\(" ,lpc-type-regex "\\)\\>"
		      "\\([ \t*&]+\\sw+\\>\\)*")
	      ;; Fontify each declaration item.
	      (list
	       'font-lock-match-c-style-declaration-item-and-skip-to-next
	       ;; Start with point after all type specifiers.
	       (list 'goto-char
		     (list 'or
			   (list 'match-beginning
				 (+ ,lpc-type-depth 2))
			   '(match-end 1)))
	       ;; Finish with point after first type specifier.
	       '(goto-char (match-end 1))
	       ;; Fontify as a variable or function name.
	       '(1 (if (match-beginning 2)
		       font-lock-function-name-face
		     font-lock-variable-name-face)))))

    ;; Fontify anything at beginning of line as a declaration or definition.
    '("^\\(\\sw+\\)\\>\\([ \t*]+\\sw+\\>\\)*"
      (1 font-lock-type-face)
      (font-lock-match-c-style-declaration-item-and-skip-to-next
       (goto-char (or (match-beginning 2) (match-end 1))) nil
       (1 (if (match-beginning 2)
	      font-lock-function-name-face
	    font-lock-variable-name-face))))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; and now native LPC highlighting:
    '("\\('.'\\|'\\\\.'\\)"             1 font-lock-string-face  keep)
    ;; lambda thingies
    '("{\\s-*\\(#\\)"                   1 font-lock-builtin-face keep)
    '("'\\([^}, \t;]+\\)"               1 lpc-reference-face     keep)
    '("'\\(,\\)[,} \t\n]"               1 lpc-reference-face     keep)
    ;; 
    '("\\(\\binherit\\)\\s-+\\s\".+\";" 1 font-lock-builtin-face    t)
    )
   )
  )

(defun lpc-set-font-lock-defaults ()
  "Set up LPC mode font-lock stuff."
  (let ((font-lock-defaults '(lpc-font-lock-map 
			      nil
			      nil
			      ((?_  . "w") (?\' . "'"))
			      beginning-of-defun
			      (font-lock-mark-block-function . mark-defun))))
    (font-lock-set-defaults)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bring it all together:

(defun lpc-mode ()
  (interactive)
  (c-initialize-cc-mode)
  (kill-all-local-variables)
  (set-syntax-table lpc-mode-syntax-table)
  (make-variable-buffer-local 'parse-sexp-lookup-properties)
  (set 'parse-sexp-lookup-properties t)
  (setq major-mode             'lpc-mode
 	mode-name              "LPC")
  (use-local-map lpc-mode-map)
  (c-common-init)
  (setq c-keywords             (c-identifier-re lpc-all-regex)
	c-special-brace-lists  lpc-special-brace-lists
	comment-start          "// "
 	comment-end            ""
 	c-conditional-key      lpc-conditional-regex
	c-comment-start-regexp lpc-comment-start-regex
	c-extra-toplevel-key   lpc-other-decl-regex
  	;; c-class-key         nil ;; cannot set this to nil or ""
	c-method-key           nil
 	c-baseclass-key        nil
	c-recognize-knr-p      nil
	c-lambda-key           nil
	c-inexpr-block-key     nil
	c-inexpr-class-key     nil)
  (lpc-set-font-lock-defaults)
  (lpc-scan-magic-quotes)
  (if (not noninteractive)
      (turn-on-font-lock)
    (let ((font-lock-mode t) (noninteractive nil))
      (turn-on-font-lock)))
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'lpc-mode-hook)
  (c-update-modeline)
  )

(provide 'lpc-mode)
;; lpc-mode.el ends here
