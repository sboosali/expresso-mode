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

  "Customize `expresso-mode'."

;;:link (url-link "")

;;:group '
  )

;;==============================================;;





























;; expresso-mode-

;; URL `http://www.wilfred.me.uk/blog/2015/03/19/adding-a-new-language-to-emacs/'
;; URL `http://www.wilfred.me.uk/blog/2014/09/27/the-definitive-guide-to-syntax-highlighting/'





e.g. the hyphen character (`-`) in Haskell Mode (`haskell-mode`) is:

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

;; e.g.:
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
































(defvar expresso-keywords

  '("imported" "exported")

  "Keywords for `expresso-mode'.")

;;----------------------------------------------;;

(defvar expresso-file-extensions

  '("\\.sapi\\'" "\\.sapi.py\\'")

  "File Extensions for `expresso-mode'.
Override `python-mode'  for this compound-file-extension.")

;;----------------------------------------------;;

(defvar expresso-keyword-faces-alist

  '(
    ("^#.*"      . 'font-lock-comment-face)       ;; comments at start of line
    ("<dgn.*?>"  . 'font-lock-builtin-face)       ;; 
    ("^<.*?>"    . 'font-lock-function-name-face) ;; LHS nonterminals
    ("<.*?>"     . 'font-lock-variable-name-face) ;; other nonterminals
    ("{.*?}"     . 'font-lock-variable-name-face) ;;
    ("="         . 'font-lock-constant-face)      ;; "goes-to" symbol
    (";"         . 'font-lock-constant-face)      ;; statement delimiter
    ("\|"        . 'font-lock-keyword-face)       ;; "OR" symbol
    ("\+"        . 'font-lock-keyword-face)       ;; 
    ("\["        . 'font-lock-keyword-face)       ;; 
    ("\]"        . 'font-lock-keyword-face)       ;; 
    )

  "Alist of regexps and faces, for syntax-highlighting `expresso-mode'.")

;;----------------------------------------------;;
;; Faces ---------------------------------------;;
;;----------------------------------------------;;

(defgroup expresso-faces nil

  "Fonts and colors for Expresso."

  :prefix 'expresso
  :group  'expresso)

;;==============================================;;

expresso-keyword-face          → font-lock-keyword-face
expresso-constructor-face      → font-lock-type-face
expresso-definition-face       → font-lock-function-name-face
expresso-operator-face         → font-lock-variable-name-face
expresso-default-face          → nil
expresso-comment-face          → font-lock-doc-face

font-lock-builtin-face
font-lock-comment-delimiter-face
font-lock-comment-face
font-lock-constant-face
font-lock-doc-face
font-lock-function-name-face
font-lock-keyword-face
font-lock-negation-char-face
font-lock-preprocessor-face
font-lock-regexp-grouping-backslash
font-lock-regexp-grouping-construct
font-lock-string-face
font-lock-title-face
font-lock-type-face
font-lock-variable-name-face
font-lock-warning-face

;;----------------------------------------------;;
;; Syntax --------------------------------------;;
;;----------------------------------------------;;

;;;###autoload
(defvar expresso-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\  " " table)
    (modify-syntax-entry ?\t " " table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\' "_" table)
    (modify-syntax-entry ?_  "_" table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[  "(]" table)
    (modify-syntax-entry ?\]  ")[" table)

    (modify-syntax-entry ?\{  "(}1nb" table)
    (modify-syntax-entry ?\}  "){4nb" table)
    (modify-syntax-entry ?-  ". 123" table)
    (modify-syntax-entry ?\n ">" table)

    (modify-syntax-entry ?\` "$`" table)

    (mapc (lambda (x)
            (modify-syntax-entry x "." table))
          "!#$%&*+./:<=>?@^|~,;\\")

    ;; Haskell symbol characters are treated as punctuation because
    ;; they are not able to form identifiers with word constituent 'w'
    ;; class characters.
    (dolist (charcodes haskell--char-syntax-symbols)
      (modify-syntax-entry charcodes "." table))
    ;; ... and for identifier characters
    (dolist (charcodes haskell--char-syntax-identifiers)
      (modify-syntax-entry charcodes "w" table))

    table)
  "Expresso Mode's `syntax-table-p'.")

;;----------------------------------------------;;
;; Commands ------------------------------------;;
;;----------------------------------------------;;

(define-derived-mode expresso-mode prog-mode "Expresso"

  "Major mode for editing Expresso files.

."

  (progn

    (setq font-lock-defaults (list nil nil))

    (set-syntax-table expresso-mode-syntax-table)

    #'expresso-mode))

;;==============================================;;

(defun expresso-mode-version ()

  "Returns the (currently-loaded) version of `expresso-mode'.

Output:

• a `listp' of `numberp's."

  (interactive)

  (let ((ECHO-VERSION? (called-interactively-p 'any))
        )

  (pkg-info-package-version 'expresso-mode ECHO-VERSION?)))

;;----------------------------------------------;;

(defun expresso-mode-help ()

  "Open a (Help Buffer) tutorial for `expresso-mode'."

  (interactive)

  )

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

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'expresso)

;;; expresso-mode.el ends here