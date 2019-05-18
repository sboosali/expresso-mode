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
  (require 'rx)
  (require 'pcase)
  (require 'cl-lib))

;;----------------------------------------------;;

(progn
  (require 'seq))

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

(defgroup expresso

  nil

  "Customize the behavior of `expresso-mode'."

  :link (url-link :tag "GitHub" "https://github.com/sboosali/expresso-mode#readme")

  :group 'language)

;;==============================================;;

(defcustom expresso-filename-extensions

  '( "x" "expresso" )

  "File extensions for Expresso files."

  :type '(repeat (string :tag "File Extension"))

  :safe #'listp
  :group 'expresso)

;;----------------------------------------------;;



































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






;;     , P.identStart     = letter
;;     , P.identLetter    = alphaNum <|> oneOf "_
;; P.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"








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
;; Hooks ---------------------------------------;;
;;----------------------------------------------;;

(defcustom expresso-mode-hook

  '()

  "Commands to run after `expresso-mode' is enabled.

Type: a `listp' of `functionp's.

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
;; Faces ---------------------------------------;;
;;----------------------------------------------;;

(defgroup expresso-faces nil

  "Fonts and colors for Expresso Mode.

Customize the appearence of `expresso-mode'."

  :prefix 'expresso
  :group  'expresso)

;;==============================================;;

(defface expresso-default-face

  '((t :inherit default)
    )

  "Default Expresso face."

  :group 'expresso-faces)

;;----------------------------------------------;;

(defface expresso-keyword-face

  '((t :inherit font-lock-keyword-face)
    )

  "Face for Expresso keywords."

  :group 'expresso-faces)

;;----------------------------------------------;;

(defface expresso-builtin-face

  '((t :inherit font-lock-builtin-face)
    )

  "Face for Expresso builtins."

  :group 'expresso-faces)

;;----------------------------------------------;;

(defface expresso-type-face

  '((t :inherit font-lock-type-face)
    )

  "Face for Expresso types."

  :group 'expresso-faces)

;;----------------------------------------------;;

(defface expresso-builtin-type-face

  '((t :inherit expresso-type-face)
    )

  "Face for Expresso builtin types."

  :group 'expresso-faces)

;;----------------------------------------------;;

(defface expresso-function-face

  '((t :inherit font-lock-function-name-face)
    )

  "Face for Expresso functions."

  :group 'expresso-faces)

;;----------------------------------------------;;

(defface expresso-variable-face

  '((t :inherit font-lock-variable-name-face)
    )

  "Face for Expresso variables."

  :group 'expresso-faces)

;;----------------------------------------------;;

(defface expresso-constant-face

  '((t :inherit font-lock-constant-face)
    )

  "Face for Expresso constants."

  :group 'expresso-faces)

;;----------------------------------------------;;

(defface expresso-string-face

  '((t :inherit font-lock-string-face)
    )

  "Face for Expresso strings."

  :group 'expresso-faces)

;;----------------------------------------------;;

(defface expresso-operator-face

  '((t :inherit font-lock-keyword-face)
    )

  "Face for Expresso operators."

  :group 'expresso-faces)

;;----------------------------------------------;;

(defface expresso-comment-face

  '((t :inherit font-lock-comment-face)
    )

  "Face for Expresso comments."

  :group 'expresso-faces)

;;----------------------------------------------;;

(defface expresso-comment-face

  '((t :inherit font-lock-comment-delimiter-face)
    )

  "Face for Expresso comment delimieters (i.e. “--”)."

  :group 'expresso-faces)

;; expresso-definition-face       → font-lock-function-name-face
;; expresso-operator-face         → font-lock-variable-name-face

;;----------------------------------------------;;
;; Regexps -------------------------------------;;
;;----------------------------------------------;;

(defun expresso-builtin-regex ()

  "Return a `regexp' matching any Expresso builtin.

Customize:

• Variable `expresso-builtins'"

  (expresso--regexp-opt expresso-builtins))

;;----------------------------------------------;;

(defun expresso-type-regex ()

  "Return a `regexp' matching any Expresso type.

Customize:

• Variable `expresso-types'"

  (expresso--regexp-opt expresso-types))

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
    (modify-syntax-entry ?=  "." TABLE) ; the equal sign is the definition operator.
    (modify-syntax-entry ?+  "." TABLE)
    (modify-syntax-entry ?,  "." TABLE) ; the comma is the delimiter within any bracket.
    (modify-syntax-entry ?.  "." TABLE) ; the period is the record selection operator.
    (modify-syntax-entry ?<  "." TABLE)
    (modify-syntax-entry ?>  "." TABLE)
    (modify-syntax-entry ?/  "." TABLE)
    (modify-syntax-entry ?:  "." TABLE)
    (modify-syntax-entry ?\? "." TABLE)
    (modify-syntax-entry ?\\ "." TABLE) ; the backslash is the Lacks-Constraint type operator.
    (modify-syntax-entry ?|  "." TABLE) ; the vertical bar is the record extension operator.

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

  "Expresso Mode's `syntax-table-p'.

For example, the hyphen character (i.e. « - ») in Expresso Mode plays several roles:

• a punctuation character (« - ») — e.g. `(2 - 3)` or `(xs --. y)`.
• the characters of a (single-line) *start-of-comment* sequence (« -- ») — e.g. « -- ... ».
• the second character of a (multi-line) *start-of-comment* sequence (« {- ») — e.g. « {- ... ».
• the first character of a (multi-line) *end-of-comment* sequence (« -} ») — e.g. « ... -} ».

These roles (punctuation and single-line comment and multi-line comment) are represented by this Syntax Entry:

    (modify-syntax-entry ?- \". 123\" `expresso-mode-syntax-table')")

;;----------------------------------------------;;
;; Comments:

(defcustom expresso-comment-start "-- "

  "`comment-start' for `expresso-mode'."

  :type '(regexp)
  :safe #'stringp
  :group 'expresso)

;;----------------------------;;

(defcustom expresso-comment-start-skip

  (rx (or "--" "{-" (syntax comment-start)) (0+ blank))

  "`comment-start-skip' for `expresso-mode'."

  :type '(regexp)
  :safe #'stringp
  :group 'expresso)

;;----------------------------;;

(defcustom expresso-comment-padding 0

  "`comment-padding' for `expresso-mode'."

  :type '(choice (string  :tag "Padding (string)          ")
                 (integer :tag "Padding (number of spaces)"))
  :safe t
  :group 'expresso)

;;----------------------------;;

(defcustom expresso-comment-end

  ""

  "`comment-end' for `expresso-mode'."

  :type '(regexp)
  :safe #'stringp
  :group 'expresso)

;;----------------------------;;

(defcustom expresso-comment-end-skip

  "\\(-}\\|\\s>\\)"

  (rx (0+ blank) (or "-}" (syntax comment-end)))

  "`comment-end-skip' for `expresso-mode'."

  :type '(regexp)
  :safe #'stringp
  :group 'expresso)

;;----------------------------------------------;;
;; Paragraphs:

(defcustom expresso-paragraph-start

  (concat " *{-\\| *-- |\\|" page-delimiter)

  "`paragraph-start' for `expresso-mode'."

  :type '(regexp)
  :safe #'stringp
  :group 'expresso)

;;----------------------------;;

(defcustom expresso-paragraph-separate

  (concat " *$\\| *\\({-\\|-}\\) *$\\|" page-delimiter)

  "`paragraph-separate' for `expresso-mode'."

  :type '(regexp)
  :safe #'stringp
  :group 'expresso)

;;----------------------------------------------;;
;; Indentation ---------------------------------;;
;;----------------------------------------------;;

(defcustom expresso-basic-offset 2

  "`basic-offset' (of indentation) for `expresso-mode'."

  :type '(integer :tag "Offset")
  :safe #'integerp
  :group 'expresso)

;;----------------------------------------------;;
;; ElDoc ---------------------------------------;;
;;----------------------------------------------;;

(defvar expresso-doc-table

  (let* ((TEST #'equal)
         (TABLE (make-hash-table :test TEST))
         )

    (puthash "map" "map : forall a b. (a -> b) -> [a] -> [b]" TABLE)

    TABLE)

  "Associate types and functions with signatures or with documentation.

Type is a `hash-table-p':

• whose keys are `stringp's.
• whose values are `stringp's.")

;;----------------------------------------------;;

(defun expresso-doc-current-info ()

  "`eldoc-documentation-function' for `expresso-mode'.

Output:

• a `stringp' or nil.
  a one-line message: documentation or a signature.

Expresso Eldoc displays the types (& kinds) 
of standard library functions (& types) and of builtins.

Expresso's Standard Library is defined in these files:

• « lib/Prelude.x »
• « lib/List.x »
• « lib/Text.x »

Links:

• URL `https://github.com/willtim/Expresso/tree/0.1.2.0/lib'"

  ())                                   ;TODO

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
;; Completion ----------------------------------;;
;;----------------------------------------------;;

(cl-defun expresso-read-file (&key directory (recursive t) prompt)

  "Read an Expresso file (with completion).

Inputs:

• DIRECTORY — a `stringp'.
  A directory filepath.
  Defaults to `default-directory'.

• RECURSIVE — a `booleanp'.
  Whether to search DIRECTORY's subdirectories (i.e. recursively),
  or just its files (i.e. non-recursively).
  Defaults to `t' (i.e. recursively).

• PROMPT — a `stringp'.
  A prompt for the user (without a trailing colon & space).

Users:

• the « import » statement (in an Expresso file).
• the « :load » subcommand (in the Expresso REPL)."

  (interactive)

  ;;(cl-check-type directory (or string nil))
  ;;(cl-check-type recursive boolean)

  (let* ((PROMPT (format "%s: " (or prompt
                                    "Expresso File")))
         (DIRECTORY  (or directory default-directory))
         (FILES-INCLUSIVE (expresso-list-files :directory DIRECTORY :recursive recursive))
         (FILE-CURRENT (buffer-file-name))
         (FILES-EXCLUSIVE               ; exclude the current file, if present (i.e. no recursive imports).
          (if FILE-CURRENT
              (seq-remove (lambda (s) (equal s FILE-CURRENT))
                          FILES-INCLUSIVE)
            FILES-INCLUSIVE))
         )

    (let* ((FILE-ABSOLUTE (completing-read PROMPT FILES-EXCLUSIVE))
           (FILE-RELATIVE (if FILE-CURRENT
                              (file-relative-name FILE-ABSOLUTE DIRECTORY)
                            FILE-ABSOLUTE))
           )

      FILE-RELATIVE)))

;;----------------------------------------------;;

(cl-defun expresso-list-files (&key directory (recursive t))

  "List all relevant Expresso files.

Inputs:

• DIRECTORY — a `stringp'.
  A directory filepath.
  Defaults to `default-directory'.

• RECURSIVE — a `booleanp'.
  Whether to search DIRECTORY's subdirectories (i.e. recursively),
  or just its files (i.e. non-recursively).
  Defaults to `t'.

Output:

• a `listp' of `stringp's."

  ;;(cl-check-type directory (or string nil))
  ;;(cl-check-type recursive boolean)

  (let* ((DIRECTORY  (or directory default-directory))
         (REGEX      (rx "." (or "x" "expresso") eos))
         (RECURSIVE? (or recursive))
         )

    (if RECURSIVE?
        (directory-files-recursively DIRECTORY REGEX)
      (directory-files DIRECTORY nil REGEX))))

;;----------------------------------------------;;
;; Keymaps -------------------------------------;;
;;----------------------------------------------;;

(defvar expresso-mode-map

  (let ((KEYMAP (make-sparse-keymap)))

    (define-key KEYMAP (kbd "C-c C-h") #'expresso-mode-help)

    KEYMAP)

  "Keymap for `expresso-mode'.

its “Prefix Command” is bound to « \\[expresso-mode-keymap] ».

its current bindings are:

\\{expresso-mode-keymap}")

;;----------------------------------------------;;

(define-prefix-command 'expresso-mode-map nil "🍵 Expresso")

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

    ;; Font Lock:

    (set-syntax-table expresso-mode-syntax-table)

    (setq font-lock-defaults (list nil nil))

    (setq-local syntax-propertize-function #'expresso-syntax-propertize)

    ;; Comments:

    (setq-local comment-start      expresso-comment-start)
    (setq-local comment-padding    expresso-comment-padding)
    (setq-local comment-start-skip expresso-comment-start-skip)
    (setq-local comment-end        expresso-comment-end)
    (setq-local comment-end-skip   expresso-comment-end-skip)

    ;; Paragraphs:

    (setq-local paragraph-start    expresso-paragraph-start)
    (setq-local paragraph-separate expresso-paragraph-separate)

    (setq-local fill-paragraph-function #'expresso-fill-paragraph)
    (setq-local adaptive-fill-function  #'expresso-adaptive-fill)

    ;; Movement:

    (setq-local forward-sexp-function #'expresso-forward-sexp)
    (setq-local parse-sexp-ignore-comments nil)

    ;; Indentation:

    (setq-local indent-tabs-mode                nil)
    (setq-local comment-auto-fill-only-comments t)

    (when (boundp 'electric-indent-inhibit)
      (setq electric-indent-inhibit t))

    ;; ElDoc:

    (add-function :before-until (local 'eldoc-documentation-function)
                   #'expresso-doc-current-info)

    ;; IMenu:

    (setq-local imenu-create-index-function #'expresso-ds-create-imenu-index)

    #'expresso-mode))

;;----------------------------------------------;;
;; REPL ----------------------------------------;;
;;----------------------------------------------;;

(defgroup expresso nil

  "Expresso Expression Language."

  :prefix "expresso-"
  :group 'languages)

;;----------------------------------------------;;

(defcustom expresso-program "expresso"

  "Path to the program used by `inferior-expresso'."

  :type #'string
  :group 'expresso)

;;----------------------------;;

(defcustom expresso-arguments '()

  "Commandline arguments to pass to `expresso-program'."

  :type #'string
  :group 'expresso)

;;----------------------------;;

(defcustom expresso-prompt-regexp

  "λ>"

  "Regexp for matching `inferior-expresso' prompt."

  :type #'string
  :group 'expresso)

;;----------------------------;;

(defcustom expresso-repl-commands

  '("{"
    "cd"
    "env"
    "h"
    "help"
    "load"
    "peek"
    "quit"
    "reset"
    "type"
    )

  "Expresso REPL commands (e.g. « :help »).

a `listp' of `stringp's."

  :type #'listp
  :group 'expresso)

;;----------------------------;;

(defcustom inferior-expresso-buffer-name

  "*expresso*"

  "Buffer name for `inferior-expresso'."

  :type #'string
  :group 'expresso)

;;----------------------------------------------;;

;;;###autoload
(defun inferior-expresso (&optional name)

    "Run an inferior instance of program `expresso' within Emacs.

• NAME — a `stringp' or `bufferp'.
  defaults to `inferior-expresso-buffer-name'."

    (interactive "P")

    (let* ((PROGRAM expresso-program)
           (BUFFER-NAME (or name inferior-expresso-buffer-name))
           (BUFFER      (get-buffer-create BUFFER-NAME))
         )

      (when (not (comint-check-proc inferior-expresso-buffer-name))
            (apply #'make-comint-in-buffer "expresso" inferior-expresso-buffer-name PROGRAM expresso-arguments))
      (pop-to-buffer-same-window inferior-expresso-buffer-name)
      (inferior-expresso-mode)))

;;----------------------------------------------;;

(defun inferior-expresso-initialize ()

    "Initialize `inferior-expresso-mode'."

    (setq comint-use-prompt-regexp t))

;;----------------------------------------------;;

(define-derived-mode inferior-expresso-mode comint-mode "expresso"

  "Major mode for the Expresso REPL.



Keymap:

\\<inferior-expresso-mode-map>"

  nil
  "expresso"

  (progn

  (setq comint-prompt-regexp expresso-prompt-regexp)
  (setq comint-prompt-read-only t)

  (setq-local font-lock-defaults '(expresso-font-lock-keywords t))
  (setq-local paragraph-start expresso-prompt-regexp)
  (setq-local indent-line-function #'expresso-indent-line)

  ())

;;----------------------------------------------;;
;; Functions -----------------------------------;;
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
;;; Utilities ----------------------------------;;
;;----------------------------------------------;;

(defun expresso--regexp-opt (strings)

  "Return a regular expression matching anything in STRINGS.

Inputs:

• STRINGS — a `listp' of `stringp's.

Output:

• a `regexp'.
  Matches a syntactic symbol (see Info Node `(emacs) ') which is in STRINGS.

Examples:

• M-: (expresso--regexp-opt '(\"abc\" \"123\"))
      \"\\_<\\(123\\|abc\\)\\_>\"

Notes:

• Boundaries are respected.
  i.e. the output doesn't match substrings
  within a word or symbol, only the entire string.

Related:

• Calls `regexp-opt'"

  (let* ((STRINGS (identity strings))
         )
    (regexp-opt STRINGS 'symbols)))

;; ^ e.g.:
;;
;; • M-: (expresso--regexp-opt '("def" "123"))
;;     → "\\_<\\(123\\|def\\)\\_>"
;;
;; • M-: (if (string-match-p (expresso--regexp-opt '("def" "123")) "def") t nil)
;;     → t
;; • M-: (if (string-match-p (expresso--regexp-opt '("def" "123")) "abcdef") t nil)
;;     → nil
;; • M-: (if (string-match-p (expresso--regexp-opt '("def" "123")) "defghi") t nil)
;;     → nil
;;
;; 

;;----------------------------------------------;;
;; Effects -------------------------------------;;
;;----------------------------------------------;;

(progn

  (add-hook 'auto-mode-alist (cons (rx ".x" eos) #'expresso-mode))

  (add-hook 'inferior-expresso-mode-hook #'inferior-expresso-initialize)

  ())

;;----------------------------------------------;;
;; Examples ------------------------------------;;
;;----------------------------------------------;;

;; e.g. the Expresso REPL:
;; 
;;     $ expresso
;;     
;;     Expresso 0.1.2.0 REPL
;;     Type :help or :h for a list of commands
;;     Loading Prelude from /home/sboo/haskell/Expresso/lib/Prelude.x
;;     λ> :h
;;     REPL commands available from the prompt:
;;     <expression>                evaluate an expression
;;     :peek <expression>          evaluate, but not deeply
;;     :load <filename>            import record expression as a module
;;     :{\n ..lines.. \n:}\n       multiline command
;;     :cd <path>                  change current working directory
;;     :type <term>                show the type of <term>
;;     :reset                      reset REPL, unloading all definitions
;;     :env                        dump bound symbols in the environment
;;     :quit                       exit REPL
;;     :help                       display this list of commands
;;

;; « :env »
;;
;;   λ> :env
;;   
;;   all           : forall a        . (a -> Bool) -> [a] -> Bool
;;   and           :                    [Bool] -> Bool
;;   any           : forall a        . (a -> Bool) -> [a] -> Bool
;;   catMaybes     : forall a        . [Maybe a] -> [a]
;;   concat        : forall a        . [[a]] -> [a]
;;   const         : forall a b      . b -> a -> b
;;   either        : forall a a1 b   . (a1 -> b) -> (a -> b) -> Either a1 a -> b
;;   elem          : forall a        . (Eq a) => a -> [a] -> Bool
;;   filter        : forall a        . (a -> Bool) -> [a] -> [a] -> [a]
;;   foldl         : forall a b      . (b -> a -> b) -> b -> [a] -> b
;;   foldr         : forall a b      . (a -> b -> b) -> b -> [a] -> b
;;   fromMaybe     : forall b        . b -> Maybe b -> b
;;   id            : forall b        . b -> b
;;   isJust        : forall a        . Maybe a -> Bool
;;   isNothing     : forall a        . Maybe a -> Bool
;;   just          : forall a        . a -> Maybe a
;;   left          : forall a b      . a -> Either a b
;;   length        : forall a        . [a] -> Int
;;   listToMaybe   : forall a        . [a] -> Maybe a
;;   map           : forall a a1     . (a -> a1) -> [a] -> [a1]
;;   mapMaybe      : forall a a1     . (a -> a1) -> Maybe a -> Maybe a1
;;   maybe         : forall a b      . b -> (a -> b) -> Maybe a -> b
;;   maybeToList   : forall a        . Maybe a -> [a]
;;   mkOverridable : forall b r      . (r\override_) => ({r} -> {r}) -> {override_ : ({r} -> b) -> {r} -> b | r}
;;   notElem       : forall a        . (Eq a) => a -> [a] -> Bool
;;   nothing       : forall a        . Maybe a
;;   null          : forall a        . [a] -> Bool
;;   or            :                    [Bool] -> Bool
;;   override      : forall a b r r1 . (r\override_, r1\override_) => {override_ : a -> {r} -> {r} | r1} -> a -> {override_ : ({r} -> b) -> {r} -> b | r}
;;   right         : forall a b      . b -> Either a b
;;

;; « :type »
;;
;;   λ> :type all
;;   forall a. (a -> Bool) -> [a] -> Bool
;;   
;;   λ> :type Bool
;;   forall a r. (r\Bool) => a -> <Bool : a | r>
;;

;;----------------------------------------------;;

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

;; Syntax-Table Notes:
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

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'expresso)

;;; expresso-mode.el ends here