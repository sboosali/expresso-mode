;;; expresso-mode.el --- Major mode for editing Expresso files -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2019 Spiros Boosalis

;; Version: 0.0.0
;; Package-Requires: ((emacs "25") seq pcase)
;; Author:  Spiros Boosalis <samboosalis@gmail.com>
;; Homepage: https://github.com/sboosali/expresso-mode#readme
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

;; Major mode for editing Expresso files and running Expresso REPLs.
;;
;; Features include:
;;
;; • Edit Expresso expression (« .x » files).
;; • Run Expresso commands (the « expresso ») file.
;;
;; Commands include:
;;
;; • `expresso-mode'
;; • `run-expresso'
;; • `expresso-eval-dwim'
;;
;; Variables include:
;;
;; • `expresso-builtin-keywords-list'
;; • `expresso-builtin-operators-list'
;;
;; Configuration:
;;
;;     (use-package expresso-mode
;;       :commands    (expresso-mode run-expresso expresso-eval-dwim)
;;       :bind        (:map expresso-mode-map ("C-c C-e" . expresso-eval-dwim))
;;       :auto        ("\\.x\\'" . expresso-mode)
;;       :interpreter ("expresso" . expresso-mode)
;;       :preface     (setq expresso-setup-p nil)
;;       :config      ())
;; 
;; Installation:
;;
;; • install from source — « $ wget https://raw.githubusercontent.com/sboosali/expresso-mode/master/expresso-mode.el ».
;; • install from MELPA (via `package-install') — « M-x package-install RET expresso-mode RET ».
;;
;; If installation isn't successful, you can still interact with an Expresso REPL via `comint-run':
;;
;;     M-: (comint-run "expresso")
;;
;; 
;;
;;

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; Builtins:

(eval-when-compile
  (require 'rx)
  (require 'pcase))

;;----------------------------------------------;;

(progn
  (require 'smie)
  (require 'cl-lib)
  (require 'seq))

;;----------------------------------------------;;
;; Macros --------------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Constants -----------------------------------;;
;;----------------------------------------------;;

(defconst expresso-program-name "expresso"

  "Program used by `inferior-expresso'.

a `stringp', an executable filepath.

Can be:

• a program name — which is found via `executable-find'.
• an absolute filepath — which has “already been found”.")

;;----------------------------------------------;;

(defconst expresso-filepath-regexp

  (rx ".x" eos)

  "Match an Expresso file.

a `regexpp's.

(Filename Patterns, in particular, can be File Extensions.
See `auto-mode-alist' for the syntax.)")

;;----------------------------------------------;;

(defconst expresso-mode-help-buffer-name

  "*Help Expresso Mode Tutorial*"

  "`buffer-name' for `expresso-mode-help'.")

;;----------------------------------------------;;

;;; Major Mode...

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

(defgroup expresso nil

  "Expresso Expression Language

Customize the behavior/appearence of `expresso-mode'."

  :link (url-link :tag "GitHub" "https://github.com/sboosali/expresso-mode#readme")

  :group 'language)

;;==============================================;;

(defvar expresso-setup-p t

  "Setup Expresso when Requiring (or Autoloading) « expresso-mode.el ».

If `expresso-setup-p' is:

• t   — Expresso Mode will be setup automatically.
• nil — Expresso Mode must be setup manually, via `expresso-setup'.

Example:

    ;; Don't setup Expresso automatically:
    (setq expresso-setup-p nil)
    (require 'expresso-mode)

Related:

• `expresso-setup'.")

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
;; Variables -----------------------------------;;
;;----------------------------------------------;;

(defvar expresso-types-table

  (let* ((TEST #'equal)
         (TABLE (make-hash-table :test TEST))
         )

    ;; « Prelude.x »:

    (puthash "all"            "all : forall a. (a -> Bool) -> [a] -> Bool"                                                                                                       TABLE)
    (puthash "and"            "and : [Bool] -> Bool"                                                                                                                             TABLE)
    (puthash "any"            "any : forall a. (a -> Bool) -> [a] -> Bool"                                                                                                       TABLE)
    (puthash "catMaybes"      "catMaybes : forall a. [Maybe a] -> [a]"                                                                                                           TABLE)
    (puthash "concat"         "concat : forall a. [[a]] -> [a]"                                                                                                                  TABLE)
    (puthash "const"          "const : forall a b. b -> a -> b"                                                                                                                  TABLE)
    (puthash "either"         "either : forall a b c. (a -> c) -> (b -> c) -> Either a b -> c"                                                                                   TABLE)
    (puthash "elem"           "elem : forall a. (Eq a) => a -> [a] -> Bool"                                                                                                      TABLE)
    (puthash "filter"         "filter : forall a. (a -> Bool) -> [a] -> [a] -> [a]"                                                                                              TABLE)
    (puthash "foldl"          "foldl : forall a b. (b -> a -> b) -> b -> [a] -> b"                                                                                               TABLE)
    (puthash "foldr"          "foldr : forall a b. (a -> b -> b) -> b -> [a] -> b"                                                                                               TABLE)
    (puthash "fromMaybe"      "fromMaybe : forall b. b -> Maybe b -> b"                                                                                                          TABLE)
    (puthash "id"             "id : forall b. b -> b"                                                                                                                            TABLE)
    (puthash "isJust"         "isJust : forall a. Maybe a -> Bool"                                                                                                               TABLE)
    (puthash "isNothing"      "isNothing : forall a. Maybe a -> Bool"                                                                                                            TABLE)
    (puthash "just"           "just : forall a. a -> Maybe a"                                                                                                                    TABLE)
    (puthash "left"           "left : forall a b. a -> Either a b"                                                                                                               TABLE)
    (puthash "length"         "length : forall a. [a] -> Int"                                                                                                                    TABLE)
    (puthash "listToMaybe"    "listToMaybe : forall a. [a] -> Maybe a"                                                                                                           TABLE)
    (puthash "map"            "map : forall a b. (a -> b) -> [a] -> [b]"                                                                                                         TABLE)
    (puthash "mapMaybe"       "mapMaybe : forall a b. (a -> b) -> Maybe a -> Maybe b"                                                                                            TABLE)
    (puthash "maybe"          "maybe : forall a b. b -> (a -> b) -> Maybe a -> b"                                                                                                TABLE)
    (puthash "maybeToList"    "maybeToList : forall a. Maybe a -> [a]"                                                                                                           TABLE)
    (puthash "mkOverridable"  "mkOverridable : forall b r. (r\\override_) => ({r} -> {r}) -> {override_ : ({r} -> b) -> {r} -> b | r}"                                           TABLE)
    (puthash "notElem"        "notElem : forall a. (Eq a) => a -> [a] -> Bool"                                                                                                   TABLE)
    (puthash "nothing"        "nothing : forall a. Maybe a"                                                                                                                      TABLE)
    (puthash "null"           "null : forall a. [a] -> Bool"                                                                                                                     TABLE)
    (puthash "or"             "or : [Bool] -> Bool"                                                                                                                              TABLE)
    (puthash "override"       "override : forall a b r r1. (r\\override_, r1\\override_) => {override_ : a -> {r} -> {r} | r1} -> a -> {override_ : ({r} -> b) -> {r} -> b | r}" TABLE)
    (puthash "right"          "right : forall a b. b -> Either a b"                                                                                                              TABLE)

    ;; « List.x »:

    ;; « Text.x »:

    TABLE)

  "Associate types and functions with signatures.

a `hash-table-p':

• whose keys are `stringp's.
• whose values are `stringp's.

Expresso's ”Standard Library” is defined in these files:

• « lib/Prelude.x »
• « lib/List.x »
• « lib/Text.x »

Customization:

  ❶ Add new bindings via `puthash'. For example:

         (puthash \"increment\" \"Int -> Int\" expresso-types-table)

  ❷ Remove bindings via `remhash'. For example:

         (remhash \"fix\" expresso-types-table)

Links:

• URL `https://github.com/willtim/Expresso/blob/0.1.2.0/lib/Prelude.x'")

;; e.g. (expresso-doc-current-info "map")

;;----------------------------------------------;;
;; Custom Variables ----------------------------;;
;;----------------------------------------------;;

(defcustom expresso-value-definition-keyword-list

  '("let"
    )

  "Expresso keywords which start a Value Definition (i.e. a function or constant).

a `listp' of `stringp's.

Links:

• URL `https://github.com/willtim/Expresso/blob/master/src/Expresso/Parser.hs'."

  :type '(repeated (string :tag "Keyword"))

  :safe #'listp
  :group 'expresso)

;;----------------------------------------------;;

(defcustom expresso-type-definition-keyword-list

  '("type"
    )

  "Expresso keywords which start a Type Definition.

a `listp' of `stringp's.

Links:

• URL `https://github.com/willtim/Expresso/blob/master/src/Expresso/Parser.hs'."

  :type '(repeated (string :tag "Keyword"))

  :safe #'listp
  :group 'expresso)

;;----------------------------------------------;;

(defcustom expresso-keywords

  `("import"
    "forall"
    "in"
    "case"
    "of"
    "override"
    "if"
    "then"
    "else"
    ,@expresso-value-definition-keyword-list
    ,@expresso-type-definition-keyword-list
    )

  "Expresso keywords.

a `listp' of `stringp's.

Links:

• URL `https://github.com/willtim/Expresso/blob/master/src/Expresso/Parser.hs'."

  :type '(repeated (string :tag "Keyword"))

  :safe #'listp
  :group 'expresso)

;;----------------------------------------------;;

(defcustom expresso-builtin-functions

  '(
    "abs"
    "absurd"
    "ceiling"
    "double"
    "error"
    "fix"  
    "floor"
    "mod"
    "not"
    "pack"
    "show"
    "uncons"
    "unpack"
   )

  "Names of functions built-into Expresso.

a `listp' of `stringp's."

  :type '(repeated (string :tag "Function (Builtin)"))

  :safe #'listp
  :group 'expresso)

;;----------------------------------------------;;

(defcustom expresso-builtin-types

  '("Bool"
    "Char"
    "Double"
    "Int"
    "Text"
   )

  "Names of types built-into Expresso.

a `listp' of `stringp's."

  :type '(repeated (string :tag "Type (Builtin)"))

  :safe #'listp
  :group 'expresso)

;;----------------------------------------------;;

(defcustom expresso-builtin-classes

  '("Num"
    "Eq"
    "Ord"
   )

  "Names of classes built-into Expresso.

a `listp' of `stringp's."

  :type '(repeated (string :tag "Class (Builtin)"))

  :safe #'listp
  :group 'expresso)

;;----------------------------------------------;;

(defcustom expresso-builtin-constructors

  '("True"
    "False"
   )

  "Names of constructors built-into Expresso.

a `listp' of `stringp's."

  :type '(repeated (string :tag "Constructor"))

  :safe #'listp
  :group 'expresso)

;;----------------------------------------------;;

(defcustom expresso-builtin-function-operators

  '(
    ;; Unary/Prefix Functions:

    "-"

    ;; Binary Functions:

    ">>"
    "<<"
    
    "=="
    "/="
    
    ">"
    "<"
    ">="
    "<="
    
    "+"
    "-"
    "*"
    "/" 

    "&&"
    "||" 

    "++" 
    "<>" 

    )

  "Names of operators built-into Expresso.

a `listp' of `stringp's.

See URL `https://github.com/willtim/Expresso/blob/master/src/Expresso/Parser.hs'.")

  :type '(repeated (string :tag "Operator"))

  :safe #'listp
  :group 'expresso)

;;----------------------------------------------;;

(defcustom expresso-builtin-lexeme-operators

  '(
    ;; Unary/Prefix Lexemes:

    ;; Unary/Postfix Lexemes:

    ";"
    "{..}"

    ;; Binary/Infix Lexemes:

    ","
    "->"
    "."
    ":"
    ":"
    ":="
    "="
    "=>"
    "\\"
    "|"
    )

  "Names of lexemes in Expresso's syntax.

a `listp' of `stringp's.

See URL `https://github.com/willtim/Expresso/blob/master/src/Expresso/Parser.hs'.")

;;----------------------------------------------;;

(defcustom expresso-builtin-constants

  '("_"
   )

  "Names of constants (and special variables) built-into Expresso.

a `listp' of `stringp's."

  :type '(repeated (string :tag "Constant"))

  :safe #'listp
  :group 'expresso)

;;----------------------------------------------;;

(defconst expresso-bracket-alist

  '(( "{"  . "}"  )                     ; Records
    ( "{|" . "|}" )                     ; “Difference Records”
    ( "["  . "]"  )                     ; Lists
    ( "<"  . ">"  )                     ; Variants
    ( "<|" . "|>" )                     ; “Variant Embedding”
    ( "(:" . ")"  )                     ; “Operator Sections”

    ( "("  . ")"  )                     ; Grouping
    ( "{-" . "-}" )                     ; Comments
    )

  "Matching bracket-pairs in Expresso's Syntax.

See URL `https://github.com/willtim/Expresso/blob/master/src/Expresso/Parser.hs'.")


;;     , P.identStart     = letter
;;     , P.identLetter    = alphaNum <|> oneOf "_
;; P.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"

;;----------------------------------------------;;
;; Functions: Accessors ------------------------;;
;;----------------------------------------------;;

(defun expresso-get-definition-keywords ()

  "Accessor for `expresso-definition-keyword-list'.

Output:

• a `listp' of `stringp's."

  (let* ()

    expresso-definition-keyword-list))

;;----------------------------------------------;;

(defun expresso-get-builtin-operators ()

  "Accessor for `expresso-builtin-*-operators' (Expresso's builtin operators).

Output:

• a `listp' of `stringp's.
  Merges `expresso-builtin-lexeme-operators' with `expresso-builtin-function-operators'."

  (let* ()

    (concat expresso-builtin-lexeme-operators
            expresso-builtin-function-operators)))

;;----------------------------------------------;;
;; SMIE ----------------------------------------;;
;;----------------------------------------------;;

(defconst expresso-smie-grammar

  (smie-prec2->grammar
   (smie-merge-prec2s

    ;; :

    (smie-bnf->prec2

     '((id)
       (insts (inst) (insts ";" insts))
       (inst (exp) (inst "iuwu-mod" exp)
             ;; Somewhat incorrect (both can be used multiple times),
             ;; but avoids lots of conflicts:
             (exp "and" exp) (exp "or" exp))
       (exp  (exp1) (exp "," exp) (exp "=" exp)
             (id " @ " exp))
       (exp1 (exp2) (exp2 "?" exp1 ":" exp1))
       (exp2 (exp3) (exp3 "." exp3))
       (exp3 ("def" insts "end")
             ("begin" insts-rescue-insts "end")
             ("do" insts "end")
             ("class" insts "end") ("module" insts "end")
             ("for" for-body "end")
             ("[" expseq "]")
             ("{" hashvals "}")
             ("{" insts "}")
             ("while" insts "end")
             ("until" insts "end")
             ("unless" insts "end")
             ("if" if-body "end")
             ("case"  cases "end"))
       (formal-params ("opening-|" exp "closing-|"))
       (for-body (for-head ";" insts))
       (for-head (id "in" exp))
       (cases (exp "then" insts)
              (cases "when" cases) (insts "else" insts))
       (expseq (exp) );;(expseq "," expseq)
       (hashvals (exp1 "=>" exp1) (hashvals "," hashvals))
       (insts-rescue-insts (insts)
                           (insts-rescue-insts "rescue" insts-rescue-insts)
                           (insts-rescue-insts "ensure" insts-rescue-insts))
       (itheni (insts) (exp "then" insts))
       (ielsei (itheni) (itheni "else" insts))
       (if-body (ielsei) (if-body "elsif" if-body)))

     '((nonassoc "in") (assoc ";") (right " @ ")
       (assoc ",") (right "="))

     '((assoc "when"))

     '((assoc "elsif"))

     '((assoc "rescue" "ensure"))

     '((assoc ",")))

    ;; :

    (smie-precs->prec2

     '((right "=")
       (right "+=" "-=" "*=" "/=" "%=" "**=" "&=" "|=" "^="
              "<<=" ">>=" "&&=" "||=")
       (nonassoc ".." "...")
       (left "&&" "||")
       (nonassoc "<=>")
       (nonassoc "==" "===" "!=")
       (nonassoc "=~" "!~")
       (nonassoc ">" ">=" "<" "<=")
       (left "^" "&" "|")
       (left "<<" ">>")
       (left "+" "-")
       (left "*" "/" "%")
       (left "**")
       (assoc ".")))))

  "Simplified BNF grammar (URL `http://www.cse.buffalo.edu/~regan/cse305/RubyBNF.pdf')")

(defconst ruby-smie-grammar

  (smie-prec2->grammar
   (smie-merge-prec2s
    (smie-bnf->prec2
     '((id)
       (insts (inst) (insts ";" insts))
       (inst (exp) (inst "iuwu-mod" exp)
             ;; Somewhat incorrect (both can be used multiple times),
             ;; but avoids lots of conflicts:
             (exp "and" exp) (exp "or" exp))
       (exp  (exp1) (exp "," exp) (exp "=" exp)
             (id " @ " exp))
       (exp1 (exp2) (exp2 "?" exp1 ":" exp1))
       (exp2 (exp3) (exp3 "." exp3))
       (exp3 ("def" insts "end")
             ("begin" insts-rescue-insts "end")
             ("do" insts "end")
             ("class" insts "end") ("module" insts "end")
             ("for" for-body "end")
             ("[" expseq "]")
             ("{" hashvals "}")
             ("{" insts "}")
             ("while" insts "end")
             ("until" insts "end")
             ("unless" insts "end")
             ("if" if-body "end")
             ("case"  cases "end"))
       (formal-params ("opening-|" exp "closing-|"))
       (for-body (for-head ";" insts))
       (for-head (id "in" exp))
       (cases (exp "then" insts)
              (cases "when" cases) (insts "else" insts))
       (expseq (exp) );;(expseq "," expseq)
       (hashvals (exp1 "=>" exp1) (hashvals "," hashvals))
       (insts-rescue-insts (insts)
                           (insts-rescue-insts "rescue" insts-rescue-insts)
                           (insts-rescue-insts "ensure" insts-rescue-insts))
       (itheni (insts) (exp "then" insts))
       (ielsei (itheni) (itheni "else" insts))
       (if-body (ielsei) (if-body "elsif" if-body)))
     '((nonassoc "in") (assoc ";") (right " @ ")
       (assoc ",") (right "="))
     '((assoc "when"))
     '((assoc "elsif"))
     '((assoc "rescue" "ensure"))
     '((assoc ",")))

    (smie-precs->prec2
     '((right "=")
       (right "+=" "-=" "*=" "/=" "%=" "**=" "&=" "|=" "^="
              "<<=" ">>=" "&&=" "||=")
       (nonassoc ".." "...")
       (left "&&" "||")
       (nonassoc "<=>")
       (nonassoc "==" "===" "!=")
       (nonassoc "=~" "!~")
       (nonassoc ">" ">=" "<" "<=")
       (left "^" "&" "|")
       (left "<<" ">>")
       (left "+" "-")
       (left "*" "/" "%")
       (left "**")
       (assoc ".")))))

  "SMIE Grammar.

Simplified BNF grammar (URL `http://www.cse.buffalo.edu/~regan/cse305/RubyBNF.pdf')")
```

e.g. `ruby-smie-rules`:

``` elisp
;;----------------------------------------------;;

(defun ruby-smie-rules (kind token)

  "SMIE Rules

Output:

• an ‘integerp’ — the current correct indentation level."

  (pcase (cons kind token)

    (`(:elem . basic) ruby-indent-level)
    ;; "foo" "bar" is the concatenation of the two strings, so the second
    ;; should be aligned with the first.

    (`(:elem . args) (if (looking-at "\\s\"") 0))
    ;; (`(:after . ",") (smie-rule-separator kind))

    (`(:before . ";")
     (cond
      ((smie-rule-parent-p "def" "begin" "do" "class" "module" "for"
                           "while" "until" "unless"
                           "if" "then" "elsif" "else" "when"
                           "rescue" "ensure" "{")
       (smie-rule-parent ruby-indent-level))
      ;; For (invalid) code between switch and case.
      ;; (if (smie-parent-p "switch") 4)
      ))

    (`(:before . ,(or `"(" `"[" `"{"))
     (cond
      ((and (equal token "{")
            (not (smie-rule-prev-p "(" "{" "[" "," "=>" "=" "return" ";"))
            (save-excursion
              (forward-comment -1)
              (not (eq (preceding-char) ?:))))
       ;; Curly block opener.
       (ruby-smie--indent-to-stmt))
      ((smie-rule-hanging-p)
       ;; Treat purely syntactic block-constructs as being part of their parent,
       ;; when the opening token is hanging and the parent is not an
       ;; open-paren.
       (cond
        ((eq (car (smie-indent--parent)) t) nil)
        ;; When after `.', let's always de-indent,
        ;; because when `.' is inside the line, the
        ;; additional indentation from it looks out of place.
        ((smie-rule-parent-p ".")
         ;; Traverse up the call chain until the parent is not `.',
         ;; or `.' at indentation, or at eol.
         (while (and (not (ruby-smie--bosp))
                     (equal (nth 2 (smie-backward-sexp ".")) ".")
                     (not (ruby-smie--bosp)))
           (forward-char -1))
         (smie-indent-virtual))
        (t (smie-rule-parent))))))

    (`(:after . ,(or `"(" "[" "{"))
     ;; FIXME: Shouldn't this be the default behavior of
     ;; `smie-indent-after-keyword'?
     (save-excursion
       (forward-char 1)
       (skip-chars-forward " \t")
       ;; `smie-rule-hanging-p' is not good enough here,
       ;; because we want to reject hanging tokens at bol, too.
       (unless (or (eolp) (forward-comment 1))
         (cons 'column (current-column)))))

    (`(:before . " @ ")
     (save-excursion
       (skip-chars-forward " \t")
       (cons 'column (current-column))))

    (`(:before . "do") (ruby-smie--indent-to-stmt))

    (`(:before . ".")
     (if (smie-rule-sibling-p)
         (and ruby-align-chained-calls 0)
       (smie-backward-sexp ".")
       (cons 'column (+ (current-column)
                        ruby-indent-level))))

    (`(:before . ,(or `"else" `"then" `"elsif" `"rescue" `"ensure"))
     (smie-rule-parent))

    (`(:before . "when")
     ;; Align to the previous `when', but look up the virtual
     ;; indentation of `case'.
     (if (smie-rule-sibling-p) 0 (smie-rule-parent)))

    (`(:after . ,(or "=" "+" "-" "*" "/" "&&" "||" "%" "**" "^" "&"
                     "<=>" ">" "<" ">=" "<=" "==" "===" "!=" "<<" ">>"
                     "+=" "-=" "*=" "/=" "%=" "**=" "&=" "|=" "^=" "|"
                     "<<=" ">>=" "&&=" "||=" "and" "or"))
     (and (smie-rule-parent-p ";" nil)
          (smie-indent--hanging-p)
          ruby-indent-level))

    (`(:after . ,(or "?" ":")) ruby-indent-level)

    (`(:before . ,(guard (memq (intern-soft token) ruby-alignable-keywords)))
     (when (not (ruby--at-indentation-p))
       (if (ruby-smie--indent-to-stmt-p token)
           (ruby-smie--indent-to-stmt)
         (cons 'column (current-column)))))

    (`(:before . "iuwu-mod")
     (smie-rule-parent ruby-indent-level))

    (_ 0))

(defun expresso-smie-setup ()

  "`smie-setup' with `expresso-smie-grammar' and `expresso-smie-rules'."

  (smie-setup expresso-smie-grammar
              #'expresso-smie-rules
              :forward-token  #'expresso-smie--forward-token
              :backward-token #'expresso-smie--backward-token))

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

(defface expresso-variable-face

  '((t :inherit font-lock-variable-name-face)
    )

  "Face for Expresso variables."

  :group 'expresso-faces)

;;----------------------------------------------;;

(defface expresso-function-face

  '((t :inherit font-lock-function-name-face)
    )

  "Face for Expresso functions."

  :group 'expresso-faces)

;;----------------------------------------------;;

(defface expresso-operator-face

  '((t :inherit font-lock-keyword-face)
    )

  "Face for Expresso operators."

  :group 'expresso-faces)

;;----------------------------------------------;;

(defface expresso-constructor-face

  '((t :inherit font-lock-constructor-name-face)
    )

  "Face for Expresso constructors."

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
;; Functions: Regexps --------------------------;;
;;----------------------------------------------;;

(defun expresso-keyword-regexp ()

  "Return a `regexp' matching any Expresso keyword.

Customize:

• Variable `expresso-keywords'"

  (expresso--regexp-opt expresso-keywords))

;;----------------------------------------------;;

(defun expresso-builtin-regexp ()

  "Return a `regexp' matching any Expresso builtin.

Customize:

• Variable `expresso-builtins'"

  (expresso--regexp-opt expresso-builtins))

;;----------------------------------------------;;

(defun expresso-type-regexp ()

  "Return a `regexp' matching any Expresso type.

Customize:

• Variable `expresso-types'"

  (expresso--regexp-opt expresso-types))

;;----------------------------------------------;;
;; Font Lock -----------------------------------;;
;;----------------------------------------------;;

(defconst expresso--font-lock-keywords-keyword

  (cons (expresso-keyword-regexp) 'expresso-keyword-face)

  "Highlighting keywords.")

;;----------------------------------------------;;

(defconst expresso--font-lock-keywords-shebang

  (list (rx buffer-start "#!" (0+ not-newline) eol)
       '(0 font-lock-comment-face))

  "Highlighting the “shebang line” (e.g. « #!/bin/env expresso »).")

;;----------------------------------------------;;

(defvar expresso-font-lock-keywords

  (list expresso--font-lock-keywords-keyword
        expresso--font-lock-keywords-
     ;; expresso--font-lock-keywords-
        expresso--font-lock-keywords-shebang)

  "`font-lock-keywords' for `expresso-mode'.

a `listp' associating `regexpp's with `facep's.

(For “Search-based Fontification”,
a.k.a. “Keyword-based Syntax-Highlighting”).")

;;----------------------------------------------;;

(defvar expresso-font-lock-defaults

  (let ((expresso-font-lock-keywords-only             t)   ; Search-Based Fontification
        (expresso-font-lock-keywords-case-fold-search nil) ; Case-Insensitive
        )
    (list 'expresso-font-lock-keywords expresso-font-lock-keywords-only expresso-font-lock-keywords-case-fold-search))

  "`font-lock-defaults' for `expresso-mode'.")

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

;;==============================================;;

(defun expresso-indent-line ()

  "`indent-line-function' for `expresso-mode'.

Inputs:

• .

Output:

• either:

    • nil — indentation was performed successfully.
    • ‘noindent’ — indentation isn't possible (e.g. within a string literal).

Effects:

• Point  — may move `point'.
• Buffer — may modify the current buffer by ❶ adding whitespace or ❷ removing whitespace.

Links:

• URL `'

Related:

• `'"

  (let* (
         )

    (save-excursion

      

      ())))

;; Users expect Emacs to indent code correctly regardless of its current state. You’ll need to examine the syntax around point to calculate the current nesting level.
;;
;; - (1) This is usually a matter of searching the buffer backwards from point, counting instances of { (or equivalent scope delimiter). You then adjust the current line to be indented (* my-mode-tab-width count). Provided you’re careful with { in strings and comments, this works.
;;
;; - (2) Alternatively, Emacs provides the Simple Minded Indentation Engine (SMIE). You write a BNF grammar and you get basic indentation and movement commands for free.

;;----------------------------------------------;;
;; ElDoc ---------------------------------------;;
;;----------------------------------------------;;

(defun expresso-doc-current-info (&optional symbol)

  "`eldoc-documentation-function' for `expresso-mode'.

Input:

• SYMBOL — a `stringp' or nil.
  Expresso symbol whose signature and/or documentation will be output.
  Defaults to the “symbol-at-`point'”.

Output:

• a `stringp' or nil.
  a one-line message. 
  ❶ a signature and/or ❷ documentation.

Expresso Eldoc displays the types (& kinds) 
of standard library functions (& types) and of builtins.

Related:

• `expresso-types-table'

Links:

• URL `https://github.com/willtim/Expresso/tree/0.1.2.0/lib'"

  (interactive)

  (when-let* ((CURRENT-SYMBOL (or symbol (thing-at-point 'symbol)))
              (CURRENT-TYPE   (gethash CURRENT-SYMBOL expresso-types-table))
              (CURRENT-DOC    CURRENT-TYPE)
              )

    (when (called-interactively-p 'any)
      (message "%s" CURRENT-DOC))

    CURRENT-DOC))

;;----------------------------------------------;;
;; Completion ----------------------------------;;
;;----------------------------------------------;;

(cl-defun expresso-completion-at-point ()

  "`completion-at-point' for `expresso-mode'.

Behavior:

• « import \" » completes with filenames of the proper extension."

  ())

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

its “Prefix Command” (function `expresso-mode-map')
is bound to « \\[expresso-mode-keymap] ».

its current bindings are:

\\{expresso-mode-keymap}")

;;----------------------------------------------;;

(define-prefix-command 'expresso-mode-map nil "🍵 Expresso")

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
;; Mode ----------------------------------------;;
;;----------------------------------------------;;

(define-derived-mode expresso-mode prog-mode "Expresso"

  "Major mode for editing Expresso files.

Expresso is a (lightweight) records-based expression language. 
From the language's homepage at URL `https://github.com/willtim/Expresso#readme':

“Expresso is a minimal statically-typed functional programming
 language, designed with embedding and/or extensibility in mind.
 Possible use cases for such a minimal language include configuration
 (à la Nix), data exchange (à la JSON) or even a starting point for a
 custom external DSL.”

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

========================================
= Links ================================
========================================

• URL `https://github.com/willtim/Expresso#readme'

========================================"

  :group 'expresso

  :syntax-table expresso-mode-syntax-table

  (progn

    ;; Font Lock:

    (setq-local font-lock-defaults expresso-font-lock-defaults)
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

    (setq-local indent-line-function #'expresso-indent-line)

    (setq-local basic-offset expresso-basic-offset)
    (setq-local indent-tabs-mode nil)
    (setq-local comment-auto-fill-only-comments t)

    ;; (when (boundp 'electric-indent-inhibit)
    ;;   (setq electric-indent-inhibit t))

    ;; ElDoc:

    (add-function :before-until (local 'eldoc-documentation-function) #'expresso-doc-current-info)

    ;; IMenu:

    (setq-local imenu-create-index-function #'expresso-ds-create-imenu-index)

    ;; Effects:

    (font-lock-fontify-buffer)

    #'expresso-mode))

;;; REPL...

;;----------------------------------------------;;
;; Variables: REPL -----------------------------;;
;;----------------------------------------------;;

(defgroup inferior-expresso nil

  "Expresso REPL.

Customize the behavior/appearence of `inferior-expresso-mode'."

  :link (url-link :tag "GitHub" "https://github.com/sboosali/expresso-mode#readme")

  :prefix 'expresso
  :group 'shell
  :group 'expresso)

;;==============================================;;

(defcustom expresso-program expresso-program-name

  "Program used by `inferior-expresso'.

a `stringp', an executable filepath.

Can be:

• a program name — which is found via `executable-find'.
• an absolute filepath — which has “already been found”.

Defaults to `expresso-program-name'."

  :type '(string :tag "Program or Filepath")

  :safe #'string
  :group 'expresso)

;;----------------------------;;

(defcustom expresso-switches '("-i")

  "Commandline options & arguments to pass to `expresso-program'.

a `listp' of `stringp's."

  :type '(repeated (string :tag "Option or Argument"))

  :safe #'string
  :group 'expresso)

;;----------------------------;;

(defcustom expresso-command nil

  "Commandline invocation of `expresso-program'.

If `expresso-command' is non-nil, 
it shadows `expresso-program' plus `expresso-switches'
(see `expresso-get-command').

Type can be:

• a `stringp'.
• a `listp' of `stringp's."

  :type '(choice (string :tag "Commandline")
                 (repeated (string :tag "Command (words)")))

  :safe #'string
  :group 'expresso)

;;----------------------------;;

(defcustom expresso-prompt-regexp

  (rx bol "λ>" space)

  "`comint-prompt-regexp' for `inferior-expresso-mode'.

Regexp for matching the Expresso REPL's prompt."

  :type '(regexp :tag "Prompt")

  :safe #'string
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

a `listp' of `stringp's.

Notes:

• Don't include a leading colon.
  e.g. the « :help » command is represented by the « \"help\" » string."

  :type '(repeated (string :tag "REPL Command"))

  :safe #'listp
  :group 'expresso)

;;----------------------------;;

(defcustom inferior-expresso-buffer-name

  "*expresso*"

  "Buffer name for `inferior-expresso'."

  :type '(string :tag "Buffer")

  :safe #'string
  :group 'expresso)

;;----------------------------------------------;;
;; Commands: REPL ------------------------------;;
;;----------------------------------------------;;

;;;###autoload
(defun inferior-expresso (&optional buffer)

    "Launch an Expresso REPL.

Run an inferior instance of program `expresso' within Emacs.

• BUFFER — a `stringp' or `bufferp'.
  Defaults to `inferior-expresso-buffer-name'."

    (interactive "P")

    (let* ((PROGRAM     expresso-program)
           (BUFFER-NAME (or buffer inferior-expresso-buffer-name))
           (BUFFER      (get-buffer-create BUFFER-NAME))
           )

      (when (not (comint-check-proc inferior-expresso-buffer-name))
        (apply #'make-comint-in-buffer "expresso" inferior-expresso-buffer-name PROGRAM expresso-switches))

      (pop-to-buffer-same-window inferior-expresso-buffer-name)

      (inferior-expresso-mode)))

;;----------------------------------------------;;
;; Functions: REPL -----------------------------;;
;;----------------------------------------------;;

;;;###autoload
(defun inferior-expresso-setup ()

  "Setup Inferior Expresso.

Related:

• Gated by `expresso-setup-p'.
• Inverted by `expresso-unload-function'."

  (progn

    (add-hook 'inferior-expresso-mode-hook #'inferior-expresso-initialize)

    ()))

;;----------------------------------------------;;

(defun inferior-expresso-initialize ()

    "Initialize `inferior-expresso-mode'."

    (setq comint-use-prompt-regexp t))

;;----------------------------------------------;;
;; Mode: REPL ----------------------------------;;
;;----------------------------------------------;;

;;;###autoload
(define-derived-mode inferior-expresso-mode comint-mode "Inferior Expresso"

  "Major mode for Expresso REPL (an inferior process).

Keymap:

\\<inferior-expresso-mode-map>"

  nil

  "expresso"

  (progn

    ;; Prompt:

    (setq-local comint-prompt-regexp expresso-prompt-regexp)
    (setq-local comint-prompt-read-only t)
    (setq-local paragraph-start expresso-prompt-regexp)

    ;; Syntax-Highlighting:

    (setq-local font-lock-defaults expresso-font-lock-defaults)

    ;; Indentation:

    (setq-local indent-line-function #'expresso-indent-line)
    (setq-local indent-tabs-mode nil)
    (setq-local basic-offset expresso-basic-offset)

    ;; Modeline:

    (setq-local mode-line-process '(":%s"))

    ()))


































;;----------------------------------------------;;
;; Functions -----------------------------------;;
;;----------------------------------------------;;

;;;###autoload
(defun expresso-setup ()

  "Setup Expresso when Requiring (or Autoloading) « expresso-mode.el ».

”Setup“ includes:

• Registering `expresso-mode' with `auto-mode-alist'.
• Registering `expresso-mode' with `interpreter-mode-alist'.

Related:

• Gated by `expresso-setup-p'.
• Inverted by `expresso-unload-function'."

  (progn

    (add-to-list 'auto-mode-alist        (cons expresso-filepath-regexp #'expresso-mode))
    (add-to-list 'interpreter-mode-alist (cons "expresso" #'expresso-mode))

    ()))

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

(defun expresso-comint-send-string (string)

  "`comint-send-string' for `inferior-expresso-mode'.

Inputs:

• STRING — a `stringp'."

  ())

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

(defun expresso-repl-dwim ()

  "DWIM: start Expresso REPL, or switch to it."

  (let* (
         )

    TODO))

;;----------------------------------------------;;

(defun expresso-send-region-to-shell (beg end)

  "Send region (between BEG and END) to the Expresso REPL.

Inputs:

• BEG — an `integerp'.
• END — an `integerp'."

  (interactive "r")

  (let* ((STRING (buffer-substring-no-properties beg end))
       )

  (expresso-comint-send-string STRING)))

;;----------------------------------------------;;

(defun expresso-send-buffer-to-shell (&optional buffer)

  "Send BUFFER to the Expresso REPL.

Inputs:

• BUFFER — a `bufferp'."

  (interactive)

  (let* ((BUFFER (current-buffer))
       )

  (with-current-buffer BUFFER

    (expresso-send-region-to-shell (point-min) (point-max)))))

;;----------------------------------------------;;

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
;; Unloading -----------------------------------;;
;;----------------------------------------------;;

(defun expresso-unload-function ()

  "`unload-feature' for `expresso'.

Inverts `expresso-setup' and `inferior-expresso-setup'
(which get executed by « (load \"expresso.el\") »).

Effects:

• Unregisters `expresso-mode' from `auto-mode-alist'.
• Unregisters `expresso-mode' from `interpreter-mode-alist'."

  (progn

    (setq auto-mode-alist
          (cl-remove #'expresso-mode auto-mode-alist        :test #'equal :key #'cdr))

    (setq interpreter-mode-alist
          (cl-remove #'expresso-mode interpreter-mode-alist :test #'equal :key #'cdr))

    ()))

;;----------------------------------------------;;
;; Effects -------------------------------------;;
;;----------------------------------------------;;

(when (bound-and-true-p 'expresso-setup-p)

  (expresso-setup)
  (inferior-expresso-setup))

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
;; λ> {x = 1, y = True}
;; 
;; λ> let sqmag = r -> r.x*r.x + r.y*r.y
;; 
;; λ> let sqmag = {x, y} -> x*x + y*y
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
;; λ> (: forall a. Eq a => { x : <Foo : Int, Bar : a> }) { x = Bar "abc" }
;; {x = Bar "abc"}
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

;; e.g. Qualified Import:
;; 
;; λ> let List = import "List.x"

;; e.g. Unqualified Import:
;; 
;; λ> let {..} = import "List.x"

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