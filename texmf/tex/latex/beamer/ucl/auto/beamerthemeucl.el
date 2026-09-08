;; -*- lexical-binding: t; -*-

(TeX-add-style-hook
 "beamerthemeucl"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("calc" "") ("helvet" "scaled") ("eulervm" "")))
   (TeX-run-style-hooks
    "calc"
    "helvet"
    "eulervm")
   (LaTeX-add-lengths
    "bannerheight"
    "stripeheight"
    "bannerimagetrim"))
 :latex)

