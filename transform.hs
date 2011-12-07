import Text.Pandoc

transformInline (Code _ code)
  = RawInline "mediawiki" $ "<code>" ++ code ++ "</code>"

transformInline x = x

transformBlock (CodeBlock _ code)
  = RawBlock "mediawiki" $ "<haskell>\n" ++ code ++ "\n</haskell>"

transformBlock (Table [] [AlignLeft,AlignLeft,AlignLeft] [0.0,0.0,0.0] [] _)
  = Null

transformBlock (BulletList ([Plain [Link _ ('#':_,"")]]:_))
  = Null

transformBlock  x = x

main = interact $ writeNative defaultWriterOptions
                . bottomUp transformInline
                . bottomUp transformBlock
                . readNative
