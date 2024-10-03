(TeX-add-style-hook
 "beamerthemeucl"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("helvet" "scaled")))
   (TeX-run-style-hooks
    "calc"
    "helvet")
   (LaTeX-add-lengths
    "bannerheight"
    "stripeheight"
    "bannerimagetrim"))
 :latex)

