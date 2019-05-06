;;; expresso-mode.el --- Major mode for editing Expresso files -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2019 Spiros Boosalis

;; Version: 0.0.0
;; Package-Requires: ((emacs "25") seq pcase)
;; Author:  Spiros Boosalis <samboosalis@gmail.com>
;; Homepage: https://github.com/sboosali/expresso-mode
;; Keywords: language faces
;; Created: 01 May 2019

;; This file is not part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Major mode for editing Expresso files (« .x » extension).
;;
;; Commands:
;;
;; • `expresso-mode'
;;
;; Variables:
;;
;; • `expresso-builtin-keywords-list'
;; • `expresso-builtin-operators-list'
;;
;; 

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; Builtins:

(eval-when-compile 
  (require 'cl-lib))

;;----------------------------------------------;;

(progn
  (require 'pcase)
  (require 'seq))

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

(defgroup expresso

  nil

  "Customize the behavior of `expresso-mode'."

  :link (url-link :tag "GitHub" "https://github.com/sboosali/expresso-mode")

  :group 'language)

;;==============================================;;



































e.g. the hyphen character (`-`) in Expresso Mode (`expresso-mode`) is:

- a punctuation character (`-`), e.g. `(2 - 3)` or `(xs --. y)`.
- the characters of a (single-line) *start-of-comment* sequence (`--`), e.g. `-- ...`.
- the second character of a (multi-line) *start-of-comment* sequence (`{-`), e.g. `{- ...`.
- the first character of a (multi-line) *end-of-comment* sequence (`-}`), e.g. `... -}`.





(defconst expresso-mode-bracket-alist

  '(

 ( "{" . "}" )
 ( "{|" . "|}" )
 ( "[" . "]" )
 ( "<" . ">" )
 ( "<|" . "|>" )
 ( "(" . ")" )

 ( "{-" . "-}" )

  )
;; ( "" . "" )

  "

See URL `https://github.com/willtim/Expresso/blob/master/src/Expresso/Parser.hs'.")


(defcustom expresso-mode-keywords

  '(
 "import"
  "forall" "type"
  "let" "in" "case" "of" "override"
  "if" "then" "else" 

 "Int"
 "Double"
 "Bool"
 "Char"
 "Text"

 "Eq" "Ord" "Num"
  "_"
  "fix" 
  "True" "False"

   fun "error"   ErrorPrim
  , fun "show"    Show
  , fun "not"     Not
  , fun "uncons"  ListUncons
  , fun "fix"     FixPrim
  , fun "double"  Double
  , fun "floor"   Floor
  , fun "ceiling" Ceiling
  , fun "abs"     Abs
  , fun "mod"     Mod
  , fun "absurd"  Absurd
  , fun "pack"    Pack
  , fun "unpack"  Unpack

  )

  "

See URL `https://github.com/willtim/Expresso/blob/master/src/Expresso/Parser.hs'.")



(defcustom expresso-mode-builtin-operators

  '( 

  -- Unary, Prefix:

  "-"

  -- Unary, Postfix:

  "{..}"

  -- Binary:

   ":" "\\" "=>" "->"
    "," ";"
    "=" ":=" "|" "." ":"
   ">>" "<<"  
  "==" "/="
   ">" "<" ">=" "<="
   "+" "-" "*" "/" 
   "&&" "||" 
  "++" 
  "<>" 

  )

  "

See URL `https://github.com/willtim/Expresso/blob/master/src/Expresso/Parser.hs'.")






    , P.identStart     = letter
    , P.identLetter    = alphaNum <|> oneOf "_

P.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"


(defconst expresso-mode-mode-syntax-table


  (let ((TABLE (make-syntax-table)))


    ;; « " » delimits string literals,
    ;; « ' » delimits character literals:

    (modify-syntax-entry ?\" "\"" TABLE)
    (modify-syntax-entry ?\' "\"" TABLE)

    ;; « - » is punctuation, but 
    ;; « -- » is a comment starter:

    (modify-syntax-entry ?- ". 12" TABLE)


    ;; « \n » is a comment ender:

    (modify-syntax-entry ?\n ">" TABLE)


    ;; Identifiers can have « _ » and « ' »,
    ;; but « ' » already has a Syntax Class (see above):

    (modify-syntax-entry ?\_ "_" TABLE)
 ;; (modify-syntax-entry ?\' "_" TABLE)


    ;; « . » is an operator:

    (modify-syntax-entry ?. "" TABLE)

    ;; ‘()’, ‘[]’, ‘{}’ — are Bracket Character-Pairs:

    (modify-syntax-entry ?\( "()" TABLE)
    (modify-syntax-entry ?\) ")(" TABLE)

    (modify-syntax-entry ?\{ "(}" TABLE)
    (modify-syntax-entry ?\} "){" TABLE)

    (modify-syntax-entry ?\[ "(]" TABLE)
    (modify-syntax-entry ?\] ")[" TABLE)

    ;; « » is a :

    (modify-syntax-entry ? "" TABLE)


    ;; « » is a :

    (modify-syntax-entry ? "" TABLE)


    ;; « » is a :

    (modify-syntax-entry ? "" TABLE)


    ;; « » is a :

    (modify-syntax-entry ? "" TABLE)


    ;; « » is a :

    (modify-syntax-entry ? "" TABLE)


    ;; « » is a :

    (modify-syntax-entry ? "" TABLE)


    ;; « » is a :

    (modify-syntax-entry ? "" TABLE)


    ;; « » is a :

    (modify-syntax-entry ? "" TABLE)


    ;; « » is a :

    (modify-syntax-entry ? "" TABLE)

    TABLE))

;; Notes:
;;
;; Word constituents: ‘w’
;; Symbol constituents: ‘_’
;; Punctuation characters: ‘.’
;; Open parenthesis characters: ‘(’
;; Close parenthesis characters: ‘)’
;; String quotes: ‘"’
;; Comment starters: ‘<’
;; Comment enders: ‘>’
;;
;;
;;
;;
;;
;;

;; e.g. Expresso expressions:
;; 
;; λ> let sqmag = r -> r.x*r.x + r.y*r.y
;; 
;; λ> {| x = "foo" |} >> {| x := "bar" |} -- Type checks
;; 
;; λ> 
;; 
;; λ> 
;; 
;; λ> 
;; 
;; λ> 
;; 
;; λ> 
;; 
;; λ> 
;; 
;; λ> 
;; 
;; λ> 
;; 
;; λ> 
;; 
;; λ> 
;; 
;; λ> 
;; 
;; λ> 
;; 
;; λ> 
;; 
;; λ> 
;; 
;; λ> 
;; 
;; λ> 
;; 
;; λ> 
;; 
;; λ> 
;; 
;; λ> 
;; 
;; λ> 
;; 
;; λ> 
;; 
;; λ> 
;; 
;; λ> 
;; 
;; λ> 
;; 
;; λ> 
;; 
;; λ> 
;; 
;; λ> 
;; 
;; λ> 
;; 
;; λ> 
;; 
;; λ> 
;; 
;; λ> 
;; 
;; λ> 





(define-derived-mode expresso-mode-mode prog-mode "Expresso Mode"

  :syntax-table expresso-mode-mode-syntax-table


  (font-lock-fontify-buffer))


"Expresso is a (lightweight) records-based expression language.



“Expresso is a minimal statically-typed functional programming language, designed with embedding and/or extensibility in mind. Possible use cases for such a minimal language include configuration (à la Nix), data exchange (à la JSON) or even a starting point for a custom external DSL.”

Links:

• URL `https://github.com/willtim/Expresso#readme'"






























;;----------------------------------------------;;
;; Constants -----------------------------------;;
;;----------------------------------------------;;

(defconst expresso-mode-help-buffer-name

  "*Help Expresso Mode Tutorial*"

  "`buffer-name' for `expresso-mode-help'.")

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

(defcustom expresso-keywords

  '(
    ""
   )

  "`expresso' keywords.

a `listp' of `stringp's."

  :type '(repeated (string :tag "Keyword"))

  :safe #'listp
  :group 'expresso)

;;----------------------------------------------;;

(defcustom expresso-builtin-functions

  '(
    ""
   )

  "Names of functions built-into `expresso'.

a `listp' of `stringp's."

  :type '(repeated (string :tag "Function (Builtin)"))

  :safe #'listp
  :group 'expresso)

;;----------------------------------------------;;

(defcustom expresso-builtin-types

  '(
    ""
   )

  "Names of types built-into `expresso'.

a `listp' of `stringp's."

  :type '(repeated (string :tag "Type (Builtin)"))

  :safe #'listp
  :group 'expresso)

;;----------------------------------------------;;

(defcustom expresso-file-regexps

  '(
    "\\.xxx-file-extension-xxx\\'"
   )

  "Match a `expresso-mode' file.

a `listp' of `regexp's.

Filename Patterns, in particular, can be File Extensions.
See `auto-mode-alist' for the syntax."

  :type '(repeated (regexp :tag "File Pattern"))

  :safe #'listp
  :group 'expresso)







;;----------------------------------------------;;

(defvar expresso-keyword-faces-alist

  '(
    ("^#.*"      . font-lock-comment-face)       ;; comments at start of line
    ("<dgn.*?>"  . font-lock-builtin-face)       ;; 
    ("^<.*?>"    . font-lock-function-name-face) ;; LHS nonterminals
    ("<.*?>"     . font-lock-variable-name-face) ;; other nonterminals
    ("{.*?}"     . font-lock-variable-name-face) ;;
    ("="         . font-lock-constant-face)      ;; "goes-to" symbol
    (";"         . font-lock-constant-face)      ;; statement delimiter
    ("\|"        . font-lock-keyword-face)       ;; "OR" symbol
    ("\+"        . font-lock-keyword-face)       ;; 
    ("\["        . font-lock-keyword-face)       ;; 
    ("\]"        . font-lock-keyword-face)       ;; 
    )

  "Alist of regexps and faces, for syntax-highlighting `expresso-mode'.")











;;----------------------------------------------;;
;; Keymaps -------------------------------------;;
;;----------------------------------------------;;

(defvar expresso-mode-map

  (let ((KEYMAP (make-sparse-keymap)))

    (define-key KEYMAP (kbd "C-c C-h") #'expresso-mode-help)

    KEYMAP)

  "Keymap for `expresso-mode'.")

;;----------------------------------------------;;
;; Hooks ---------------------------------------;;
;;----------------------------------------------;;

(defcustom expresso-mode-hook

  '()

  "List of functions to run after `expresso-mode' is enabled.

Use to enable minor modes coming with `expresso-mode' or run an
arbitrary function.

Note that  `expresso-indentation-mode' and `expresso-indent-mode' should not be
run at the same time."

  :options '(superword-mode
             subword-mode

             flyspell-prog-mode
             highlight-uses-mode

             ;; interactive-expresso-mode
             ;; expresso-decl-scan-mode
             ;; expresso-indentation-mode
            )

  :type 'hook
  :group 'expresso)

;;----------------------------------------------;;
;; Menu ----------------------------------------;;
;;----------------------------------------------;;

(easy-menu-define expresso-mode-menu expresso-mode-map

  "Menu for Expresso Mode."

  `("Expresso"

    ["Customize"          (customize-group 'expresso)]
    "---"
    ["Indent line"        indent-according-to-mode]
    ["(Un)Comment region" comment-region mark-active]
    "---"
    ,(if (default-boundp 'eldoc-documentation-function)
         ["Doc mode" eldoc-mode
          :style toggle :selected (bound-and-true-p eldoc-mode)]
       ["Doc mode" expresso-doc-mode
        :style toggle :selected (and (boundp 'expresso-doc-mode) expresso-doc-mode)])
    "---"
    ))

;;----------------------------------------------;;
;; Faces ---------------------------------------;;
;;----------------------------------------------;;

(defgroup expresso-faces nil

  "Fonts and colors for Expresso Mode.

Customize the appearence of `expresso-mode'."

  :prefix 'expresso
  :group  'expresso)

;;==============================================;;

(defface expresso-default-face

  '((t :inherit default))

  "Default Expresso face."

  :group 'expresso-faces)

;;----------------------------------------------;;

(defface expresso-keyword-face

  '((t :inherit font-lock-keyword-face))

  "Face for Expresso keywords."

  :group 'expresso-faces)

;;----------------------------------------------;;

(defface expresso-builtin-face

  '((t :inherit font-lock-builtin-face))

  "Face for Expresso builtins."

  :group 'expresso-faces)

;;----------------------------------------------;;

(defface expresso-type-face

  '((t :inherit font-lock-type-face))

  "Face for Expresso types."

  :group 'expresso-faces)

;;----------------------------------------------;;

(defface expresso-function-face

  '((t :inherit font-lock-function-name-face))

  "Face for Expresso functions."

  :group 'expresso-faces)

;;----------------------------------------------;;

(defface expresso-variable-face

  '((t :inherit font-lock-variable-name-face))

  "Face for Expresso variables."

  :group 'expresso-faces)

;;----------------------------------------------;;

(defface expresso-constant-face

  '((t :inherit font-lock-constant-face))

  "Face for Expresso constants."

  :group 'expresso-faces)

;;----------------------------------------------;;

(defface expresso-string-face

  '((t :inherit font-lock-string-face))

  "Face for Expresso strings."

  :group 'expresso-faces)

;;----------------------------------------------;;

(defface expresso-operator-face

  '((t :inherit font-lock-keyword-face))

  "Face for Expresso operators."

  :group 'expresso-faces)

;;----------------------------------------------;;

(defface expresso-comment-face

  '((t :inherit font-lock-comment-face))

  "Face for Expresso comments."

  :group 'expresso-faces)

;;----------------------------------------------;;

(defface expresso-comment-face

  '((t :inherit font-lock-comment-delimiter-face))

  "Face for Expresso comment delimieters (i.e. « “--” »)."

  :group 'expresso-faces)

;; expresso-definition-face       → font-lock-function-name-face
;; expresso-operator-face         → font-lock-variable-name-face

;;----------------------------------------------;;
;; Syntax --------------------------------------;;
;;----------------------------------------------;;

;;;###autoload
(defvar expresso-mode-syntax-table

  (let ((TABLE (make-syntax-table))
        )

    ;; « - » is punctuation (as an operator),
    ;; but « -- » is a comment-starter:

    (modify-syntax-entry ?- ". 123" TABLE)

    ;; « \n » is a comment-ender:

    (modify-syntax-entry ?\n ">" TABLE)

    ;; Whitespace (i.e. spaces, tabs, newlines) is conventional:

    (modify-syntax-entry ?\  " " TABLE)
    (modify-syntax-entry ?\t " " TABLE)
    ;; (see above for the Syntax Class of « \n »):

    ;; Brackets (i.e. parens, curly braces, square braces):

    (modify-syntax-entry ?\( "()"    TABLE)
    (modify-syntax-entry ?\) ")("    TABLE)
    (modify-syntax-entry ?\[ "(]"    TABLE)
    (modify-syntax-entry ?\] ")["    TABLE)
    (modify-syntax-entry ?\{ "(}1nb" TABLE) ; « "n" » means: Multi-Line Coments can be nested.
    (modify-syntax-entry ?\} "){4nb" TABLE)

    ;; Operator identifiers are like Haskell

    (modify-syntax-entry ?~  "." TABLE)
    (modify-syntax-entry ?!  "." TABLE)
    (modify-syntax-entry ?@  "." TABLE)
    (modify-syntax-entry ?\# "." TABLE)
    (modify-syntax-entry ?$  "." TABLE)
    (modify-syntax-entry ?%  "." TABLE)
    (modify-syntax-entry ?^  "." TABLE)
    (modify-syntax-entry ?&  "." TABLE)
    (modify-syntax-entry ?*  "." TABLE)
    ;; (see above for the Syntax Class of « - »)
    (modify-syntax-entry ?=  "." TABLE)
    (modify-syntax-entry ?+  "." TABLE)
    (modify-syntax-entry ?.  "." TABLE)
    (modify-syntax-entry ?<  "." TABLE)
    (modify-syntax-entry ?>  "." TABLE)
    (modify-syntax-entry ?/  "." TABLE)
    (modify-syntax-entry ?:  "." TABLE)
    (modify-syntax-entry ?\? "." TABLE)
    (modify-syntax-entry ?\\ "." TABLE)
    (modify-syntax-entry ?|  "." TABLE)

    ;; « " » is a string delimiter:

    (modify-syntax-entry ?\" "\"" TABLE)

    ;; Identifiers can have apostrophes and underscores (like Haskell)
    ;; (« _ » is the “Symbol” Syntax Class):

    (modify-syntax-entry ?\' "_" TABLE)
    (modify-syntax-entry ?\_ "_" TABLE)

    ;; Identifiers can have (uppercase or lowercase) letters
    ;: and digits (like Haskell).
    ;; (« w » is the “Word” Syntax Class):

    (modify-syntax-entry ?0 "w" TABLE)  ; digits...
    (modify-syntax-entry ?1 "w" TABLE)
    (modify-syntax-entry ?2 "w" TABLE)
    (modify-syntax-entry ?3 "w" TABLE)
    (modify-syntax-entry ?4 "w" TABLE)
    (modify-syntax-entry ?5 "w" TABLE)
    (modify-syntax-entry ?6 "w" TABLE)
    (modify-syntax-entry ?7 "w" TABLE)
    (modify-syntax-entry ?8 "w" TABLE)
    (modify-syntax-entry ?9 "w" TABLE)
    (modify-syntax-entry ?a "w" TABLE)  ; letters...
    (modify-syntax-entry ?A "w" TABLE)
    (modify-syntax-entry ?b "w" TABLE)
    (modify-syntax-entry ?B "w" TABLE)
    (modify-syntax-entry ?c "w" TABLE)
    (modify-syntax-entry ?C "w" TABLE)
    (modify-syntax-entry ?d "w" TABLE)
    (modify-syntax-entry ?D "w" TABLE)
    (modify-syntax-entry ?e "w" TABLE)
    (modify-syntax-entry ?E "w" TABLE)
    (modify-syntax-entry ?f "w" TABLE)
    (modify-syntax-entry ?F "w" TABLE)
    (modify-syntax-entry ?g "w" TABLE)
    (modify-syntax-entry ?G "w" TABLE)
    (modify-syntax-entry ?h "w" TABLE)
    (modify-syntax-entry ?H "w" TABLE)
    (modify-syntax-entry ?i "w" TABLE)
    (modify-syntax-entry ?I "w" TABLE)
    (modify-syntax-entry ?j "w" TABLE)
    (modify-syntax-entry ?J "w" TABLE)
    (modify-syntax-entry ?k "w" TABLE)
    (modify-syntax-entry ?K "w" TABLE)
    (modify-syntax-entry ?l "w" TABLE)
    (modify-syntax-entry ?L "w" TABLE)
    (modify-syntax-entry ?m "w" TABLE)
    (modify-syntax-entry ?M "w" TABLE)
    (modify-syntax-entry ?n "w" TABLE)
    (modify-syntax-entry ?N "w" TABLE)
    (modify-syntax-entry ?o "w" TABLE)
    (modify-syntax-entry ?O "w" TABLE)
    (modify-syntax-entry ?p "w" TABLE)
    (modify-syntax-entry ?P "w" TABLE)
    (modify-syntax-entry ?q "w" TABLE)
    (modify-syntax-entry ?Q "w" TABLE)
    (modify-syntax-entry ?r "w" TABLE)
    (modify-syntax-entry ?R "w" TABLE)
    (modify-syntax-entry ?s "w" TABLE)
    (modify-syntax-entry ?S "w" TABLE)
    (modify-syntax-entry ?t "w" TABLE)
    (modify-syntax-entry ?T "w" TABLE)
    (modify-syntax-entry ?u "w" TABLE)
    (modify-syntax-entry ?U "w" TABLE)
    (modify-syntax-entry ?v "w" TABLE)
    (modify-syntax-entry ?V "w" TABLE)
    (modify-syntax-entry ?w "w" TABLE)
    (modify-syntax-entry ?W "w" TABLE)
    (modify-syntax-entry ?x "w" TABLE)
    (modify-syntax-entry ?X "w" TABLE)
    (modify-syntax-entry ?y "w" TABLE)
    (modify-syntax-entry ?Y "w" TABLE)
    (modify-syntax-entry ?z "w" TABLE)
    (modify-syntax-entry ?Z "w" TABLE)

    TABLE)

  "Expresso Mode's `syntax-table-p'.")

;;----------------------------------------------;;
;; Mode ----------------------------------------;;
;;----------------------------------------------;;

(define-derived-mode expresso-mode prog-mode "Expresso"

  "Major mode for editing Expresso files.

========================================
= Configuration ========================
========================================

Examaple `use-package' declaration:

    (use-package expresso-mode
    
      :diminish (expresso-mode \" ☕\")
    
      :custom
    
      (expresso- t \"Enable \")
    
      :custom-face
    
      (expresso-keyword-face ((t (:weight bold :slant italic :underline t))) \"Keywords are bolded and italicized and underlined (but uncolored).\")
    
      :config
    
      ())

========================================
= Tutorial =============================
========================================



========================================
= Minor Modes ==========================
========================================

Indentation is provided by `expresso-indent-mode' (TODO!).

REPLs are provided by `interactive-expresso-mode' (TODO!).

Register other minor modes via `expresso-mode-hook'.

========================================
= Keymaps ==============================
========================================

(all bindings in this docstring are relative to `expresso-mode-map'.)\\<expresso-mode-map>

========================================
= Versions =============================
========================================

Call `expresso-version' to get the version of the currently-loaded Expresso Mode.

Call `expresso-program-version' to get the version of the currently-registered command `expresso'
(i.e. on the environment-variable `$PATH' or `%PATH%').

========================================"

  :group 'expresso

  (progn

    (setq font-lock-defaults (list nil nil))

    (set-syntax-table expresso-mode-syntax-table)

    #'expresso-mode))

  ;; paragraph-{start,separate} should treat comments as paragraphs as well.
  (setq-local paragraph-start (concat " *{-\\| *-- |\\|" page-delimiter))
  (setq-local paragraph-separate (concat " *$\\| *\\({-\\|-}\\) *$\\|" page-delimiter))
  (setq-local fill-paragraph-function 'expresso-fill-paragraph)
  ;; (setq-local adaptive-fill-function 'expresso-adaptive-fill)
  (setq-local comment-start "-- ")
  (setq-local comment-padding 0)
  (setq-local comment-start-skip "[-{]-[ \t]*")
  (setq-local comment-end "")
  (setq-local comment-end-skip "[ \t]*\\(-}\\|\\s>\\)")
  (setq-local forward-sexp-function #'expresso-forward-sexp)
  (setq-local parse-sexp-ignore-comments nil)
  (setq-local syntax-propertize-function #'expresso-syntax-propertize)

  ;; Set things up for eldoc-mode.
  (setq-local eldoc-documentation-function 'expresso-doc-current-info)
  ;; Set things up for imenu.
  (setq-local imenu-create-index-function 'expresso-ds-create-imenu-index)
  ;; Set things up for font-lock.
  (setq-local font-lock-defaults
              '((expresso-font-lock-keywords)
                nil nil nil nil
                (font-lock-syntactic-face-function
                 . expresso-syntactic-face-function)
                ;; Get help from font-lock-syntactic-keywords.
                (parse-sexp-lookup-properties . t)
                (font-lock-extra-managed-props . (composition expresso-type))))
  ;; Preprocessor definitions can have backslash continuations
  (setq-local font-lock-multiline t)
  ;; Expresso's layout rules mean that TABs have to be handled with extra care.
  ;; The safer option is to avoid TABs.  The second best is to make sure
  ;; TABs stops are 8 chars apart, as mandated by the Expresso Report.  --Stef
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 8)
  (setq-local comment-auto-fill-only-comments t)
  ;; Expresso is not generally suitable for electric indentation, since
  ;; there is no unambiguously correct indent level for any given line.
  (when (boundp 'electric-indent-inhibit)
    (setq electric-indent-inhibit t))

  ;; dynamic abbrev support: recognize Expresso identifiers
  ;; Expresso is case-sensitive language
  (setq-local dabbrev-case-fold-search nil)
  (setq-local dabbrev-case-distinction nil)
  (setq-local dabbrev-case-replace nil)
  (setq-local dabbrev-abbrev-char-regexp "\\sw\\|[.]")
  (setq expresso-literate nil)
  (add-hook 'before-save-hook 'expresso-mode-before-save-handler nil t)
  (add-hook 'after-save-hook 'expresso-mode-after-save-handler nil t)
  ;; provide non-interactive completion function
  (add-hook 'completion-at-point-functions
            'expresso-completions-completion-at-point
            nil
            t)
  (expresso-indentation-mode)

;;----------------------------------------------;;
;; Commands ------------------------------------;;
;;----------------------------------------------;;

(defun expresso-version ()

  "Returns the (currently-loaded) version of `expresso-mode'.

Output:

• a `listp' of `numberp's."

  (interactive)

  (let ((ECHO-VERSION? (called-interactively-p 'any))
        )

    (pkg-info-package-version 'expresso-mode ECHO-VERSION?)))

;;----------------------------------------------;;

(defun expresso-program-version ()

  "Returns the (currently-loaded) version of `expresso-mode'.

Output:

• a `stringp'."

  (interactive)

  (let ((ECHO-VERSION?   (called-interactively-p 'any))
        (PROGRAM-VERSION (expresso-program-execute "--numeric-version"))
        )

   (when ECHO-VERSION?
     (message "« expresso » program version: %s" ROGRAM-VERSION))

   PROGRAM-VERSION))

;;----------------------------------------------;;

(defun expresso-mode-help ()

  "Open a (Help Buffer) tutorial for `expresso-mode'."

  (interactive)

  (describe-function #'expresso-mode))

;;----------------------------------------------;;

;;;###autoload
(defun expresso-forward-sexp (&optional arg)

  "Expresso specific version of `forward-sexp'.

Move forward across one balanced expression (sexp).  With ARG, do
it that many times.  Negative arg -N means move backward across N
balanced expressions.  This command assumes point is not in a
string or comment.

If unable to move over a sexp, signal `scan-error' with three
arguments: a message, the start of the obstacle (a parenthesis or
list marker of some kind), and end of the obstacle."

  (interactive "^p")

  (or arg (setq arg 1))
  (if (< arg 0)
      (while (< arg 0)
        (skip-syntax-backward "->")
        ;; Navigate backwards using plain `backward-sexp', assume that it
        ;; skipped over at least one Expresso expression, and jump forward until
        ;; last possible point before the starting position. If applicable,
        ;; `scan-error' is signalled by `backward-sexp'.
        (let ((end (point))
              (forward-sexp-function nil))
          (backward-sexp)
          (let ((cur (point)))
            (while (< (point) end)
              (setf cur (point))
              (expresso-forward-sexp)
              (skip-syntax-forward "->"))
            (goto-char cur)))
        (setf arg (1+ arg)))
    (save-match-data
      (while (> arg 0)
        (when (expresso-lexeme-looking-at-token)
          (cond ((member (match-string 0) (list "(" "[" "{"))
                 (goto-char (or (scan-sexps (point) 1) (buffer-end 1))))
                ((member (match-string 0) (list ")" "]" "}"))
                 (signal 'scan-error (list "Containing expression ends prematurely."
                                           (match-beginning 0)
                                           (match-end 0))))
                (t (goto-char (match-end 0)))))
        (setf arg (1- arg))))))

;;----------------------------------------------;;
;; Functions -----------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;

(defun expresso-program-execute (&rest arguments)

  "Invoke `expresso-program' with ARGUMENTS.

Output:

• a `stringp'.

Examples:

• M-: (expresso-program-execute \"--version\")
    ; equivalent to « $ expresso --version »
    ⇒ 

"

  (let (
        )

   (shell-command expresso-program arguments)))

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; 
;;
;;

;; Font-Lock faces:
;;
;; `font-lock-builtin-face'
;; `font-lock-comment-delimiter-face'
;; `font-lock-comment-face'
;; `font-lock-constant-face'
;; `font-lock-doc-face'
;; `font-lock-function-name-face'
;; `font-lock-keyword-face'
;; `font-lock-negation-char-face'
;; `font-lock-preprocessor-face'
;; `font-lock-regexp-grouping-backslash'
;; `font-lock-regexp-grouping-construct'
;; `font-lock-string-face'
;; `font-lock-title-face'
;; `font-lock-type-face'
;; `font-lock-variable-name-face'
;; `font-lock-warning-face'

;; Background (for future contributors and/or maintainers):
::
;; • URL `http://www.wilfred.me.uk/blog/2015/03/19/adding-a-new-language-to-emacs/'
;; • URL `http://www.wilfred.me.uk/blog/2014/09/27/the-definitive-guide-to-syntax-highlighting/'

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'expresso)

;;; expresso-mode.el ends here