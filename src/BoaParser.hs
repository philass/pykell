-- Skeleton file for Boa Parser.
--

module BoaParser (ParseError, parseString, parse, parseValue, notParse) where

import BoaAST
import Text.ParserCombinators.Parsec
-- add any other other imports you need

--type ParseError = String -- you may replace this

true :: GenParser Char st Value
true = do
         spaces
         string "True"
         spaces
         return TrueVal

false :: GenParser Char st Value
false = do
         spaces
         string "False"
         spaces
         return FalseVal

                

none :: GenParser Char st Value
none = do
         spaces
         string "None"
         spaces
         return NoneVal

int :: GenParser Char st Value
int = do
          spaces
          n <- many1 digit
          spaces
          return $ IntVal $ read n

ident :: GenParser Char st Exp
ident = do
          fl <- letter <|> char '_'
          lv <- many (alphaNum <|> char '_')
          return  $ Var (fl:lv)

-- TODO handle spaces/white character/ escaped characters
stringParse :: GenParser Char st Value
stringParse = do
                oneOf "'"
                str <- many alphaNum
                oneOf "'"
                return $ StringVal str

parseValue = true <|> false <|> none <|> int <|> stringParse

parseExprValue :: GenParser Char st Exp
parseExprValue = do Const <$> parseValue


notParse :: GenParser Char st Exp
notParse = do
              string "not "
              spaces
              Not <$> expression

--operParse = do
--              spaces
--              exp1 <- expression
--              spaces
--              op <- operation
--              spaces
--              exp2 <- expression
--              return (Oper op exp1 exp2)
--

parenParse = do
              spaces
              char '('
              exp <- expression
              char '('
              return exp

times :: Exp -> GenParser Char st Exp
times e = do
              spaces
              char '*'
              spaces
              exp <- expression
              return (Oper Times e exp)

plus e = do
              spaces
              char '+'
              spaces
              exp <- expression
              return (Oper Plus e exp)

minus e = do
              spaces
              char '-'
              spaces
              exp <- expression
              return (Oper Minus e exp)
divide e = do
              spaces
              string "//"
              spaces
              exp <- expression
              return (Oper Div e exp)


less e = do
              spaces
              char '-'
              spaces
              exp <- expression
              return (Oper Minus e exp)
eq e = do
              spaces
              string "=="
              spaces
              exp <- expression
              return (Oper Eq e exp)

modu e = do
        spaces
        char '%'
        spaces
        exp <- expression
        return (Oper Mod e exp)

greater e = do
            spaces
            char '>'
            spaces
            exp <- expression
            return (Oper Greater e exp)

inOp e = do
          spaces
          string "in"
          spaces
          exp <- expression
          return (Oper In e exp)

operation e = times e <|> plus e <|> minus e <|> divide e <|> less e <|> eq e <|> modu e<|> greater e <|> inOp e

--TODO
--


--
--TODO
--
-- Exp
-- -----------------
--ident ( exprz )
-- [ Exprz ]
-- '[' Exprz ']'
-- '[' Expr forClause Clauzes ']'
-------------------------
-- Oper
------------------------
-- ForClause
-- ifClause
-- Clausez
-- Exprz
-- Exprs

stmtAssign :: GenParser Char st Stmt
stmtAssign = do
                Var id <- ident
                spaces
                char '='
                spaces
                v <- expression
                return $ SDef id v

exprToStmt = do
                v <- expression
                return $ SExp v



-- Add OperParse back in
--
idexprz = do
            Var id <- ident
            spaces
            char '('
            spaces
            exps <- exprz
            spaces
            char ')'
            return (Call id exps)

exprzOpt = do
            spaces
            char ','
            spaces
            expression

eps = do
        spaces
        return []

exprz = do
          spaces
          exp <- expression
          rem <- many1 exprzOpt <|> eps
          return (exp:rem)

list1 = do 
          spaces
          char '['
          spaces
          exp <- exprz
          spaces 
          char ']'
          return (List exp)

list2 = do
        spaces
        char '['
        exp <- expression
        spaces
        fc <- forclauseparse
        spaces
        c <- clausez <|> eps
        return (Compr exp (fc:c))

forclauseparse = do
                   string "for"
                   spaces
                   Var id <- ident
                   spaces
                   string "in"
                   spaces
                   exp <- expression
                   return (CCFor id exp)

ifclauseparse = do
                  spaces
                  string "if"
                  spaces
                  exp <- expression
                  return (CCIf exp)


clausez = many1 (ifclauseparse <|> forclauseparse)

ex = try parseExprValue <|> try idexprz <|> try list1 <|> try parenParse <|> try notParse <|> try ident <|> try list2

eopt :: Exp -> GenParser Char st Exp
eopt e = do 
           spaces
           return e

expression = do
               spaces
               e <- ex
               res <- operation e <|> eopt e
               return res

stmt :: GenParser Char st Stmt
stmt =  try stmtAssign <|>  try exprToStmt

programOpt = do
               spaces
               char ';'
               spaces
               stmt

parseProgramWithOpt :: GenParser Char st [Stmt]
parseProgramWithOpt = do
              spaces
              s <- stmt
              smts <- many1 programOpt <|> eps
              return (s:smts)

parseProgramSingleStmt = do
                      s <- stmt
                      spaces
                      return [s]

parseProgram = parseProgramWithOpt 
parseString :: String -> Either ParseError Program
parseString = parse parseProgram "Failed to Parse!"
