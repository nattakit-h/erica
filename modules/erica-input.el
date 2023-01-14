;;; GNU Emacs Configuration File -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Nattakit Hosapsin <nattakit@hosapsin.com>
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


;;; Language environment

(set-language-environment "UTF-8")

;; input method

(quail-define-package
 "erica" ; NAME
 "UTF-8" ; LANGUAGE
 "Λ"   ; TITLE
 t       ; GUIDANCE
 "Unicode characters input method for scheme programming." ; DOCSTRING
 nil     ; TRANSLATION-KEY
 t       ; FORGET-LAST-SELECTION
 nil     ; DETERMINISTIC
 nil     ; KBD-TRANSLATE
 nil     ; SHOW-LAYOUT
 nil     ; CREATE-DECODE-MAP
 nil     ; MAXIMUM-SHORTEST
 nil     ; OVERLAY-PLIST
 nil     ; UPDATE-TRANSLATION-FUNCION
 nil     ; CONVERSION-KEYS
 t)      ; SIMPLE

(quail-define-rules
 ("\\ga" ?α)    ; GREEK SMALL LETTER ALPHA
 ("\\gb" ?β)    ; GREEK SMALL LETTER BETA
 ("\\gg" ?γ)    ; GREEK SMALL LETTER GAMMA
 ("\\gd" ?δ)    ; GREEK SMALL LETTER DELTA
 ("\\ge" ?ε)    ; GREEK SMALL LETTER EPSILON
 ("\\gz" ?ζ)    ; GREEK SMALL LETTER ZETA
 ("\\gy" ?η)    ; GREEK SMALL LETTER ETA
 ("\\gh" ?θ)    ; GREEK SMALL LETTER THETA
 ("\\gi" ?ι)    ; GREEK SMALL LETTER IOTA
 ("\\gk" ?κ)    ; GREEK SMALL LETTER KAPPA
 ("\\gl" ?λ)    ; GREEK SMALL LETTER LAMDA
 ("\\gm" ?μ)    ; GREEK SMALL LETTER MU
 ("\\gn" ?ν)    ; GREEK SMALL LETTER NU
 ("\\gc" ?ξ)    ; GREEK SMALL LETTER XI
 ("\\go" ?ο)    ; GREEK SMALL LETTER OMNICRON
 ("\\gp" ?π)    ; GREEK SMALL LETTER PI
 ("\\gr" ?ρ)    ; GREEK SMALL LETTER RHO
 ("\\gs" ?σ)    ; GREEK SMALL LETTER SIGMA
 ("\\gt" ?τ)    ; GREEK SMALL LETTER TAU
 ("\\gu" ?υ)    ; GREEK SMALL LETTER UPSILON
 ("\\gf" ?φ)    ; GREEK SMALL LETTER PHI
 ("\\gx" ?χ)    ; GREEK SMALL LETTER CHI
 ("\\gq" ?ψ)    ; GREEK SMALL LETTER PSI
 ("\\gw" ?ω)    ; GREEK SMALL LETTER OMEGA

 ("\\gA" ?Α)    ; GREEK CAPITAL LETTER ALPHA
 ("\\gB" ?Β)    ; GREEK CAPITAL LETTER BETA
 ("\\gG" ?Γ)    ; GREEK CAPITAL LETTER GAMMA
 ("\\gD" ?Δ)    ; GREEK CAPITAL LETTER DELTA
 ("\\gE" ?Ε)    ; GREEK CAPITAL LETTER EPSILON
 ("\\gZ" ?Ζ)    ; GREEK CAPITAL LETTER ZETA
 ("\\gY" ?Η)    ; GREEK CAPITAL LETTER ETA
 ("\\gH" ?Θ)    ; GREEK CAPITAL LETTER THETA
 ("\\gI" ?Ι)    ; GREEK CAPITAL LETTER IOTA
 ("\\gK" ?Κ)    ; GREEK CAPITAL LETTER KAPPA
 ("\\gL" ?Λ)    ; GREEK CAPITAL LETTER LAMDA
 ("\\gM" ?Μ)    ; GREEK CAPITAL LETTER MU
 ("\\gN" ?Ν)    ; GREEK CAPITAL LETTER NU
 ("\\gC" ?Ξ)    ; GREEK CAPITAL LETTER XI
 ("\\gO" ?Ο)    ; GREEK CAPITAL LETTER OMNICRON
 ("\\gP" ?Π)    ; GREEK CAPITAL LETTER PI
 ("\\gR" ?Ρ)    ; GREEK CAPITAL LETTER RHO
 ("\\gS" ?Σ)    ; GREEK CAPITAL LETTER SIGMA
 ("\\gT" ?Τ)    ; GREEK CAPITAL LETTER TAU
 ("\\gU" ?Υ)    ; GREEK CAPITAL LETTER UPSILON
 ("\\gF" ?Φ)    ; GREEK CAPITAL LETTER PHI
 ("\\gX" ?Χ)    ; GREEK CAPITAL LETTER CHI
 ("\\gQ" ?Ψ)    ; GREEK CAPITAL LETTER PSI
 ("\\gW" ?Ω)    ; GREEK CAPITAL LETTER OMEGA

 ("\\^0" ?⁰)    ; SUPERSCRIPT DIGIT ZERO
 ("\\^1" ?¹)    ; SUPERSCRIPT DIGIT ONE
 ("\\^2" ?²)    ; SUPERSCRIPT DIGIT TWO
 ("\\^3" ?³)    ; SUPERSCRIPT DIGIT THREE
 ("\\^4" ?⁴)    ; SUPERSCRIPT DIGIT FOUR
 ("\\^5" ?⁵)    ; SUPERSCRIPT DIGIT FIVE
 ("\\^6" ?⁶)    ; SUPERSCRIPT DIGIT SIX
 ("\\^7" ?⁷)    ; SUPERSCRIPT DIGIT SEVEN
 ("\\^8" ?⁸)    ; SUPERSCRIPT DIGIT EIGHT
 ("\\^9" ?⁹)    ; SUPERSCRIPT DIGIT NINE
 ("\\^+" ?⁺)    ; SUPERSCRIPT PLUS SIGN
 ("\\^-" ?⁻)    ; SUPERSCRIPT MINUS
 ("\\^=" ?⁼)    ; SUPERSCRIPT EQUALS SIGN
 ("\\^(" ?⁽)    ; SUPERSCRIPT LEFT PARENTHESIS
  ("\\^)" ?⁾)    ; SUPERSCRIPT RIGHT PARENTHESIS
 ("\\^i" ?ⁱ)    ; SUPERSCRIPT LATIN SMALL LETTER I
 ("\\^n" ?ⁿ)    ; SUPERSCRIPT LATIN SMALL LETTER N

 ("\\_0" ?₀)    ; SUBSCRIPT ZERO
 ("\\_1" ?₁)    ; SUBSCRIPT ONE
 ("\\_2" ?₂)    ; SUBSCRIPT TWO
 ("\\_3" ?₃)    ; SUBSCRIPT THREE
 ("\\_4" ?₄)    ; SUBSCRIPT FOUR
 ("\\_5" ?₅)    ; SUBSCRIPT FIVE
 ("\\_6" ?₆)    ; SUBSCRIPT SIX
 ("\\_7" ?₇)    ; SUBSCRIPT SEVEN
 ("\\_8" ?₈)    ; SUBSCRIPT EIGHT
 ("\\_9" ?₉)    ; SUBSCRIPT NINE
 ("\\_+" ?₊)    ; SUBSCRIPT PLUS SIGN
 ("\\_-" ?₋)    ; SUBSCRIPT MINUS
 ("\\_=" ?₌)    ; SUBSCRIPT EQUALS SIGN
 ("\\_(" ?₍)    ; SUBSCRIPT LEFT PARENTHESIS
  ("\\_)" ?₎)    ; SUBSCRIPT RIGHT PARENTHESIS

 ("\\ma" ?∀)    ; FOR ALL
 ("\\mu" ?∪)    ; UNION
 ("\\mi" ?∩)    ; INTERSECTION

 ("\\\\" ?\\))  ; REVERSE SOLIDUS

(activate-input-method "erica")

(provide 'erica-input)

;;; End of File
