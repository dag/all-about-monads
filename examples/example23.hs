{- Author:     Jeff Newbern
   Maintainer: Jeff Newbern <jnewbern@nomaware.com>
   Time-stamp: <Mon Aug 18 15:53:12 2003>
   License:    GPL
-}

{- DESCRIPTION

Example 23 - Using the ReaderT monad transformer

Usage: Compile the code to produce a simple but flexible template
       substitution system.
       
       The first argument is an initial template to evaluate.
       It would typically reference named variables and template files.
       
       Any arguments following the template are assumed to be
       variable definitions of the form "var=value".  These establish
       variable bindings for the initial template.
       
Try: ./ex23 '$<template1.txt>'
     ./ex23 '${language}' 'language=Haskell'
     ./ex23 '$"template3.txt"'
     ./ex23 '$<template3.txt>'
     ./ex23 '===$<no such file>==='
     ./ex23 '$<template2.txt>'
     ./ex23 '$<template2.txt>' 'var=dog'
     ./ex23 '$<template2.txt|var=dog>'
     ./ex23 '$<template4.txt|variable=cat>'
     ./ex23 '$<template5.txt>' 'which=3'
     ./ex23 '$<template5.txt|which=3>'
     ./ex23 '$<template6.txt|which=5>'
     ./ex23 '$<template6.txt|which=5,var=dog,variable=cat>'
-}

{- We use the Parsec monadic parser combinator library to parse
   template files -}
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token

import IO hiding (try)  -- "try" is also defined in the Parsec libraries
import qualified IO (try)
import Monad
import System
import List (intersperse)
import Control.Monad.Reader

-- This the abstract syntax representation of a template
--              Text       Variable     Quote        Include                   Compound
data Template = T String | V Template | Q Template | I Template [Definition] | C [Template]
data Definition = D Template Template

-- Templates are members of the Show class
instance Show Template where
  show (T s)    = s
  show (V t)    = "${" ++ (show t) ++ "}"
  show (Q t)    = "$\"" ++ (show t) ++ "\""
  show (I t ds) = let name        = (show t)
                      definitions = concat (intersperse ", " (map show ds))
                  in case definitions of
		       []        -> "$<" ++ name ++ ">"
		       otherwise -> "$<" ++ name ++ "|" ++ definitions ++ ">"
  show (C ts)   = concatMap show ts

instance Show Definition where
  show (D t d) = (show t) ++ "=" ++ (show d)

{- Here we define a parser for templates. -}

-- parse a (possibly compound) template.
-- the [Char] argument is a list of characters not allowed in the template.
template :: [Char] -> Parser Template
template except = do ts <- many1 (simpleTemplate except)
                     case ts of
	               [t]       -> return t
		       otherwise -> return (C ts)

-- parse a simple template: text, a variable pattern, a quote pattern, or a include pattern
-- the [Char] argument is a list of characters not allowed in the template.
simpleTemplate :: [Char] -> Parser Template
simpleTemplate except =  (text except)
                     <|> (try variable)
		     <|> (try quote)
		     <|> include

-- parse a dollar-sign that doesn't begin a variable, quote, or include pattern 
dollar :: Parser Char
dollar = try (do c <- char '$'
                 notFollowedBy (oneOf "{<\"")
                 return c)
	 <?> ""

-- parse a character that isn't part of a pattern and
-- isn't in the list of excluded characters.
textChar :: [Char] -> Parser Char
textChar except = noneOf ("$" ++ except) <|> dollar

-- parse a string of allowed characters
-- the [Char] argument is a list of characters not allowed in the text.
text :: [Char] -> Parser Template
text except = do str <- many1 (textChar except)
                 return (T str)
              <?> "text"

-- parse a variable pattern
variable :: Parser Template
variable = do t <- between (string "${") (char '}') (template "}")
              return (V t)
	   <?> "variable pattern"

-- parse a quoted-inclusion pattern
quote :: Parser Template
quote = do t <- between (string "$\"") (char '\"') (template "\"")
           return (Q t)
	   <?> "quoted include pattern"

-- parse a resolved-inclusion pattern
include :: Parser Template
include = between (string "$<") (char '>') includeBody
          <?> "include pattern"

-- parse the body of an inclusion pattern
includeBody :: Parser Template
includeBody = do t  <- (template "|>")
                 ds <- option [] definitions
		 return (I t ds)

-- parse a list of definitions
definitions :: Parser [Definition]
definitions = do char '|'
                 ds <- definition `sepBy1` (char ',')
		 return ds
		 
-- parse a single definition
definition :: Parser Definition
definition = do t1 <- (template "=,>")
                char '='
		t2 <- (template ",>")
		return (D t1 t2)
	     <?> "variable definition"
	     
-- Our environment consists of an association list of named variable  values
-- an association list of named variable values. 
type Environment = [(String,String)]

-- lookup a variable from the environment
lookupVar :: String -> Environment -> Maybe String
lookupVar = lookup

-- add a list of resolved definitions to the environment
addDefs :: [(String,String)] -> Environment -> Environment
addDefs = (++)

-- this is the type of our monad
type TemplateReader a = ReaderT Environment IO a

-- resolve a Definition and produce a (name,value) pair
resolveDef :: Definition -> TemplateReader (String,String)
resolveDef (D t d) = do name <- resolve t
                        value <- resolve d
                        return (name,value)

-- resolve a template into a string
resolve :: Template -> TemplateReader String
resolve (T s)    = return s
resolve (V t)    = do varName  <- resolve t
                      varValue <- asks (lookupVar varName)
                      case varValue of
                        Just s  -> return s
                        Nothing -> return ""
resolve (Q t)    = do tmplName <- resolve t
                      body     <- liftIO $ IO.try (readFile tmplName)
                      case body of
		        Left err -> do liftIO $ hPutStrLn stderr (show err)
				       return ""
                        Right s  -> return s
resolve (I t ds) = do tmplName <- resolve t
                      body     <- liftIO $ IO.try (parseFromFile (template []) tmplName)
                      case body of
		        Left err          -> do liftIO $ hPutStrLn stderr (show err)
			                        return ""
                        Right (Left err') -> do liftIO $ hPutStrLn stderr (show err')
			                        return ""
			Right (Right t')  -> do defs <- mapM resolveDef ds
                                                local (addDefs defs) (resolve t')
resolve (C ts)   = (liftM concat) (mapM resolve ts)

-- Read the command line arguments, parse the template file, the user template, and any
-- variable definitions.  Then construct the environment and print the resolved user template.
main :: IO ()
main = do args <- getArgs
	  let pattern = args!!0
	      defs    = map (break (=='=')) (drop 1 args)  -- split into ("var","=value")
	      env     = map (\ (x,y) -> (x,tail y)) defs   -- get rid of '='
	  case parse (template []) "template" pattern of
	    Left err -> hPutStrLn stderr (show err)
	    Right t  -> (runReaderT (resolve t) env) >>= putStr

-- END OF FILE
