import Text.Pandoc

transform (CodeBlock _ code)
  = RawBlock "mediawiki" $ "<haskell>\n" ++ code ++ "\n</haskell>"

transform x = x

main = interact $ writeNative defaultWriterOptions
                . bottomUp transform
                . readNative
