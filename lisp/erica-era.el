;;; GNU Emacs Configuration File -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Nattakit Hosapsin <nattakit@hosapsin.com>
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(eval-when-compile
  (require 'rx))

(defconst erabasic--font-lock-defaults
  (let ((instructions
         '(;; base
           "sif" "if" "elseif" "else" "endif" "call" "continue" "break"
           "drawline" "jump" "goto" "force_begin" "wait" "restart"
           ;; printing
           "data" "dataform" "datalist" "endlist" "enddata"
           "customdrawline" "drawlineform" "reuselastline"
           "clearline" "print_img" "print_rect" "print_space"
           ;; display
           "setcolor" "resetcolor" "setbgcolor" "resetbgcolor"
           "setcolorbyname" "setbgcolorbyname" "getcolor" "getdefcolor"
           "getbgcolor" "getdefbgcolor" "getfocuscolor" "fontbold"
           "fontitalic" "fontregular" "fontstyle" "getstyle" "chkfont"
           "setfont" "getfont" "forcekana" "alignment" "currentalign"
           "redraw" "currentredraw" "printcperline" "lineisempty"
           "barstr" "skipdisp" "noskip" "endnoskip" "isskip" "mouseskip"
           ;; formatting
           "toupper" "tolower" "tohalf" "tofull" "tostr" "isnumeric"
           "strlen" "strlens" "strlenform" "strlenu" "strlensu" "strlenformu"
           "substring" "substringu" "charatu" "strfind" "strfindu" "strcount"
           "split" "replace" "escape" "unicode" "encodetouni" "power" "abs"
           "sign" "sqrt" "getbit" "max" "min" "limit" "inrange" "setbit"
           "clearbit" "invertbit"
           ;; characters
           "addchara" "delchara" "swapchara" "sortchara" "getchara" "adddefchara"
           "addvoidchara" "delallchara" "pickupchara" "existcsv" "findchara"
           "findlastchara" "copychara" "addcopychara"
           ;; variables
           "varsize" "resetdata" "resetglobal" "reset_stain" "swap" "csvname"
           "csvcallname" "csvnickname" "csvmastername" "csvbase" "csvcstr" "csvabl"
           "csvtalent" "csvmark" "csvexp" "csvrelation" "csvjule" "csvequip"
           "csvcflag" "getnum" "getpalamlv" "getexplv" "findelement"
           "findlastelement" "varset" "cvarset" "arrayshift" "arrayremove"
           "arraysort" "arraycopy" "arraymsort" "cupcheck"
           ;; save data
           "savedata" "loaddata" "deldata" "chkdata" "savenos" "saveglobal"
           "loadglobal" "outputlog" "savechara" "loadchara" "chkcharadata"
           "find_charadata" "savetext" "loadtext"
           ;; time
           "gettime" "getmilisecond" "getsecond"
           ;;input
           "input" "inputs" "tinput" "tinputs" "twait" "oneinput" "oneinputs"
           "toneinput" "toneinputs" "waitanykey" "inputmousekey"
           ;; controlflow
           "for" "next" "while" "wend" "repeat" "rend" "do" "loop"
           "selectcase" "case" "caseelse" "endselect"
           ;; randomization
           "randomize" "dumprand" "initrand"
           ;; system
           "begin" "calltrain" "dotrain" "throw"
           ;; calling
           "tryjump" "trycall" "trygoto" "jumpform" "callform" "gotoform"
           "tryjumpform" "trycallform" "trygotoform" "callf" "callformf"
           "callevent" "trycjump" "tryccall" "trycgoto" "trycjumpform"
           "tryccallform" "trycgotoform" "catch" "endcatch" "trycalllist"
           "tryjumplist" "trygotolist" "func" "endfunc"
           ;; return
           "return" "returnform" "returnf"
           ;; debugging
           "debugprint" "debugprintl" "debugprintform" "debugprintforml"
           "debugclear" "assert"
           ;; tooltips
           "tooltip_setcolor" "tooltop_setdelay" "tooltip_setduration"
           ;; html printing
           "html_print" "html_tagsplit"
           ;; await
           "await" "getkey" "getkeytriggered" "mousex" "mousey" "isactive"
           ;; graphics
           "gcreate" "gcreatefromfile" "gdispose" "gclear" "gfillrectangle"
           "gdrawg" "gdrawgwithmask" "gdrawsprite" "gsetcolor" "gsetcolor"
           "gsetbrush" "gsetfont" "gsetpen" "gcreated" "gwidth" "gheight"
           "ggetcolor" "gsave" "gload" "spritecreate" "spriteanimaecreate"
           "spriteanimeaddframe" "spritedispose" "spritegetcolor" "spritecreated"
           "spritewidth" "spriteheight" "spriteposx" "spriteposy" "spritesetpos"
           "spritemove" "cbgsetg" "cbgsetsprite" "cbgclear" "cbgclearbutton"
           "cbgremoverange" "cbgremovebmap" "cbgsetbmapg" "cbgsetbuttonsprite"
           "setanimetimer"))
        (boi '(bol (*? space) (? ";!;")))
        (eoi '((+? space)))
        (varmod '(or "const" "dynamic" "ref" "charadata" "savedata" "global"))
        (identifier '(+ (or ?_ digit alpha))))
    `((;; comment
       (,(rx-to-string '(: "[SKIPSTART]" (* anychar) "[SKIPEND]")) 0 font-lock-comment-face)
       (,(rx-to-string '(: ";!;")) 0 font-lock-comment-face)
       (,(rx-to-string '(: ?\; (? (: (not ?\!) (not ?\;))) (* any))) 0 font-lock-comment-face)
       ;; preprocessors
       (,(rx-to-string `(: ,@boi (group "#dim" (? "s") (? (+? space) ,varmod)) ,@eoi)) 1 font-lock-preprocessor-face)
       (,(rx-to-string `(: ,@boi "#" (+ alpha) ,@eoi)) 0 font-lock-preprocessor-face)
       ;; instructions
       (,(rx-to-string `(: ,@boi (group (or ,@instructions)) ,@eoi)) 1 font-lock-keyword-face)
       (,(rx-to-string `(: ,@boi (group "print" (? (or ?v ?s "form" "forms")) (? (or ?k ?d)) (? (or ?l ?w))) ,@eoi)) 1 font-lock-keyword-face)
       (,(rx-to-string `(: ,@boi (group "printsiggle" (? (or ?v ?s "form" "forms")) (? (or ?k ?d))) ,@eoi)) 1 font-lock-keyword-face)
       (,(rx-to-string `(: ,@boi (group "print" (? "form") (or "c" "lc") (? (or ?k ?d))) ,@eoi)) 1 font-lock-keyword-face)
       (,(rx-to-string `(: ,@boi (group "printdata" (? (or ?k ?d)) (? (or ?l ?w))) ,@eoi)) 1 font-lock-keyword-face)
       (,(rx-to-string `(: ,@boi (group "printbutton" (? (or "c" "lc"))) ,@eoi)) 1 font-lock-keyword-face)
       (,(rx-to-string `(: ,@boi (group "printplain" (? "form")) ,@eoi)) 1 font-lock-keyword-face)
       ;; template strings
       (,(rx-to-string `(: ?@ ?\" (+? any) ?\")) 0 font-lock-string-face)
       ;; {...} templates
       (,(rx-to-string `(: ?{ (+? any) ?})) 0 font-lock-variable-name-face)
       ;; %...% templates
       (,(rx-to-string `(: ?% (+? (or alpha punct)) (+? any) ?%)) 0 font-lock-variable-name-face)
       ;; string
       (,(rx-to-string `(: ?\" (+? any) ?\")) 0 font-lock-string-face)
       ;; user-defined instruction signature
       (,(rx-to-string `(: ,@boi ?@ ,identifier)) 0 font-lock-function-name-face)
       ;; labels
       (,(rx-to-string `(: ,@boi ?$ ,identifier)) 0 font-lock-builtin-face))
      t ;; keyword-only
      t ;; case-fold
      )))

;;;###autoload
(define-derived-mode erabasic-mode prog-mode "erabasic"
  "Major mode for era basic files."
  ;; :abbrev-table erabasic-mode-abbrev-table
  (setq font-lock-defaults erabasic--font-lock-defaults)
  (setq-local comment-start ";")
  ;; (setq-local indent-line-function #'erabasic-indent-line)
  (setq-local indent-tabs-mode t))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.erb\\'" . erabasic-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.erh\\'" . erabasic-mode))

(provide 'erica-era)
