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

transformBlock HorizontalRule
  = Null

transformBlock  x = x

removeNull (Pandoc meta blocks)
  = Pandoc meta $ filter (/= Null) blocks

main = interact $ writeNative defaultWriterOptions
                . removeNull
                . bottomUp transformInline
                . bottomUp transformBlock
                . readNative
