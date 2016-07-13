(TeX-add-style-hook
 "EjemplosGrafosLista"
 (lambda ()
   (LaTeX-add-index-entries
    "\\texttt{grafoCiclo}"
    "\\texttt{grafoAmistad}"
    "\\texttt{completo}"
    "\\texttt{bipartitoCompleto}"
    "\\texttt{grafoEstrella}"
    "\\texttt{grafoRueda}"
    "\\texttt{grafoCirculante}"
    "\\texttt{grafoPetersenGen}"
    "\\texttt{grafoThomson}"
    "\\texttt{grafoPetersen}"
    "\\texttt{grafoMoebiusCantor}"))
 :latex)

