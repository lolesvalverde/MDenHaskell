(TeX-add-style-hook
 "definiciones"
 (lambda ()
   (TeX-add-symbols
    '("entradaCodigo" 1)
    '("programa" 1)
    '("ignora" 1)
    '("entrada" 1)
    '("comentario" 1)
    '("alert" 1)
    "incrementaEjercicio"
    "liff"
    "lif"
    "valor")
   (LaTeX-add-environments
    "teorema"
    "lema"
    "corolario"
    "proposicion"
    "definicion"
    "nota"
    "ejemplo"
    "demostracion"
    "enumerate"
    "itemize"))
 :latex)

