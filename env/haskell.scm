; guix environment --manifest=haskell.scm
( specifications->manifest
 '( "make"
    "texlive"
    "emacs-graphviz-dot-mode"
    "dot2tex"
    "texlive-pstool"
    "graphviz"
    "ghc"
  )
)
