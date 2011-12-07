import Text.Pandoc

transform (CodeBlock _ code)
  = RawBlock "mediawiki" $ "<haskell>\n" ++ code ++ "\n</haskell>"

transform (Table [] [AlignLeft,AlignLeft,AlignLeft] [0.0,0.0,0.0] [] _)
  = Null

transform x = x

main = interact $ writeNative defaultWriterOptions
                . bottomUp transform
                . readNative
