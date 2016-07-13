(TeX-add-style-hook
 "MD_en_Haskell"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("book" "a4paper" "12pt" "twoside")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("babel" "spanish") ("fontenc" "T1") ("helvet" "scaled=.90") ("tocloft" "titles") ("hyperref" "pdftex" "pdfauthor={Maria Dolores Valverde}" "pdftitle={MD en Haskell}" "pdfstartview=FitH" "bookmarks=false" "colorlinks=true" "urlcolor=blue" "unicode=true")))
   (TeX-run-style-hooks
    "latex2e"
    "definiciones"
    "licenciaCC"
    "Introduccion"
    "Introduccion_Teoria_Grafos"
    "Apendices"
    "book"
    "bk12"
    "inputenc"
    "babel"
    "fontenc"
    "graphicx"
    "fancyvrb"
    "makeidx"
    "amsmath"
    "amsthm"
    "latexsym"
    "mathpazo"
    "helvet"
    "cmtt"
    "a4wide"
    "tkz-berge"
    "tocloft"
    "minitoc"
    "hyperref"
    "caption"
    "fancyhdr")
   (TeX-add-symbols
    "mtctitle")
   (LaTeX-add-bibliographies))
 :latex)

