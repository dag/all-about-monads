{- Author:     Jeff Newbern
   Maintainer: Jeff Newbern <jnewbern@nomaware.com>
   Time-stamp: <Mon Aug 18 15:15:38 2003>
   License:    GPL
-}

{- DESCRIPTION

Example 16 - Using the Reader monad

Usage: Compile the code to produce a simple but flexible template
       substitution system.
       
       The first argument is a file (look at template.txt) which
       contains templates within [name]...[END] pairs.  The templates
       are text, but the special sequences ${var}, $"name", and $<name>
       cause substitutions.  The ${var} form is replaced by the value
       of the variable.  The $"name" form is replaced by the named
       template, but the template is "quoted", so that variable
       patterns, etc. within it are not treated specially.  The
       $<name> form inserts the named template and performs all
       substitutions specified in the template.  Variable values
       can be introduced or overriden within an included template
       by using the form $<name|var1=value1,var2=value2,...,varN=valueN>.

       The second argument is an initial template to evaluate.
       It would typically reference named templates in the templates
       file specified in the first argument.
       
       Any arguments following the template are assumed to be
       variable definitions of the form "var=value".  These establish
       variable bindings for the initial template.
       
Try: ./ex16 template.txt '$<#1>'
     ./ex16 template.txt '${language}' 'language=Haskell'
     ./ex16 template.txt '$"#3"'
     ./ex16 template.txt '$<#3>'
     ./ex16 template.txt '===$<no such file>==='
     ./ex16 template.txt '$<#2>'
     ./ex16 template.txt '$<#2>' 'var=dog'
     ./ex16 template.txt '$<#2|var=dog>'
     ./ex16 template.txt '$<#4|variable=cat>'
     ./ex16 template.txt '$<#5>' 'which=3'
     ./ex16 template.txt '$<#5|which=3>'
     ./ex16 template.txt '$<#6|which=5>'
     ./ex16 template.txt '$<#6|which=5,var=dog,variable=cat>'
-}

{- We use the Parsec monadic parser combinator library to parse
   template files -}
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token

import IO hiding (try)  -- "try" is also defined in the Parsec libraries
import Monad
import System
import Maybe
import List (intersperse)
import Control.Monad.Reader

-- This the abstract syntax representation of a template
--              Text       Variable     Quote        Include                   Compound
data Template = T String | V Template | Q Template | I Template [Definition] | C [Template]
data Definition = D Template Template
data NamedTemplate = NT String Template

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

instance Show NamedTemplate where
  show (NT n t) = "[" ++ n ++ "]" ++ (show t) ++ "[END]\n"

{- Here we define a parser for templates. -}

-- parse a file containing named templates
templateFile :: Parser [NamedTemplate]
templateFile = do nts <- many namedTemplate
                  eof
		  return nts

-- parse a single named template
namedTemplate :: Parser NamedTemplate
namedTemplate = do n <- name
                   t <- (template []) <?> "template"
		   end
		   spaces
		   return (NT n t)

-- parse a named template label
name :: Parser String
name = between (char '[') (char ']') (many1 (noneOf "]")) <?> "label"

-- parse a named template [END] keyword
end :: Parser String
end = string "[END]" <?> "[END]"

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

-- parse a left bracket that isn't part of an [END] keyword
leftBracket :: Parser Char
leftBracket = try (do s <- (try end) <|> (string "[")
                      case s of
		        "[END]" -> pzero
		        "["     -> return '[')
	      <?> ""

-- parse a character that isn't part of a pattern or END keyword and
-- isn't in the list of excluded characters.
textChar :: [Char] -> Parser Char
textChar except = noneOf ("$[" ++ except) <|> dollar <|> leftBracket

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
	     
-- Our environment consists of an association list of named templates and
-- an association list of named variable values. 
data Environment = Env {templates::[(String,Template)],
                        variables::[(String,String)]}

-- lookup a variable from the environment
lookupVar :: String -> Environment -> Maybe String
lookupVar name env = lookup name (variables env)

-- lookup a template from the environment
lookupTemplate :: String -> Environment -> Maybe Template
lookupTemplate name env = lookup name (templates env)

-- add a list of resolved definitions to the environment
addDefs :: [(String,String)] -> Environment -> Environment
addDefs defs env = env {variables = defs ++ (variables env)}
                      
-- resolve a Definition and produce a (name,value) pair
resolveDef :: Definition -> Reader Environment (String,String)
resolveDef (D t d) = do name <- resolve t
                        value <- resolve d
                        return (name,value)

-- resolve a template into a string
resolve :: Template -> Reader Environment (String)
resolve (T s)    = return s
resolve (V t)    = do varName  <- resolve t
                      varValue <- asks (lookupVar varName)
		      return $ maybe "" id varValue
resolve (Q t)    = do tmplName <- resolve t
                      body     <- asks (lookupTemplate tmplName)
                      return $ maybe "" show body 
resolve (I t ds) = do tmplName <- resolve t
                      body     <- asks (lookupTemplate tmplName)
                      case body of
                        Just t' -> do defs <- mapM resolveDef ds
                                      local (addDefs defs) (resolve t')
                        Nothing -> return ""
resolve (C ts)   = (liftM concat) (mapM resolve ts)

-- turn a named template into a (name,template) pair
stripName :: NamedTemplate -> (String, Template)
stripName (NT n t) = (n,t)

-- Read the command line arguments, parse the template file, the user template, and any
-- variable definitions.  Then construct the environment and print the resolved user template.
main :: IO ()
main = do args     <- getArgs
          let tmplFile = args!!0
	      pattern  = args!!1
	      defs     = drop 2 args
	  nts      <- parseFromFile templateFile tmplFile
	  case nts of
	    (Left err) -> print err
	    (Right _)  -> return ()
	  let tmpl = parse (template []) "pattern" pattern
	  case tmpl of
	    (Left err) -> print err
	    (Right _)  -> return ()
	  let ds     = map (break (=='=')) defs
	      ds'    = map (\ (x,y) -> (x,tail y)) ds
	      ntl    = either (const []) id nts
	      env    = Env (map stripName ntl) ds'
	      t      = either (const (T "")) id tmpl
	      result = runReader (resolve t) env
	  putStr result

-- END OF FILE
