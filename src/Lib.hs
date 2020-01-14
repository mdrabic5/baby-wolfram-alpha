module Lib
    ( createExpression, deriveBy, substitute, evaluate, beautify, Tree
    ) where


import Data.Char
import Data.List



-- 1 - Data types and some basic functions


-- data type which represents elements as symbols
data Symbol = SymOp Operator
            | SymFun Function
            | SymIdent String
            | SymNum Double
            | SymLBracket
            | SymRBracket
            | SymEnd
    deriving (Show, Eq)

-- data type which represents operators 
data Operator = Add | Sub | Mul | Div | Pow
    deriving (Show, Eq)

-- data type which represents functions
data Function = Sin | Cos | Exp | Log
    deriving (Show, Eq)

functions = ["sin", "cos", "exp", "log"]
operators = ["+", "-", "*", "/", "^"]

-- function which converts given Operator type to string
opToStr :: Operator -> String
opToStr Add = " + "
opToStr Sub = " - "
opToStr Mul = " * "
opToStr Div = " / "
opToStr Pow = " ^ "

-- function which converts given Function type to string
funToStr :: Function -> String
funToStr Sin = "sin "
funToStr Cos = "cos "
funToStr Exp = "exp "
funToStr Log = "log "

-- function which converts given string to Operator type
operator :: String -> Operator
operator c | c == "+" = Add
           | c == "-" = Sub
           | c == "*" = Mul
           | c == "/" = Div
           | c == "^" = Pow

-- function which converts given string to Function type
function :: String -> Function
function c | c == "sin" = Sin
           | c == "cos" = Cos
           | c == "exp" = Exp
           | c == "log" = Log

-- tree-like data structure which consist of nodes that represent grouped symbols
    -- SumNode has only [+ | -] as Operator
    -- ProdNode has only [* | /] as Operator
    -- PowNode has only [^] as Operator and the right sub-tree has to be only a number
    -- FuncNode has only [Sin | Cos | Exp | Log] as Function
    -- UnaryNode has only [+ | -] as Operator
    -- NumNode has number as its child
    -- VarNode has string as its child
data Tree = SumNode Operator Tree Tree
          | ProdNode Operator Tree Tree
          | PowNode Operator Tree Tree
          | FuncNode Function Tree
          | UnaryNode Operator Tree
          | NumNode Double
          | VarNode String
    deriving (Eq)

-- defining an instance of the Show class for Tree data type
instance Show Tree where
    show t = removeRedundantBrackets $ show' t

-- helper function for function show, constructs an expression from tree
show' :: Tree -> String
show' (SumNode o l r)  = "( " ++ show' l ++ opToStr o ++ show' r ++ " )"
show' (ProdNode o l r) = "( " ++ show' l ++ opToStr o ++ show' r ++ " )"
show' (PowNode o l r)  = "( " ++ show' l ++ opToStr o ++ show' r ++ " )"
show' (FuncNode f t)   = funToStr f ++ show' t
show' (UnaryNode o t)  | o == Add  = show' t
                       | otherwise = "( - " ++ show' t ++ " )"
show' (NumNode n)      = show n
show' (VarNode x)      = x



-- 2 - Tokenizer


-- helper function for determening if string is Integer
isInteger s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False
 
-- helper function for determening if string is Double
isDouble s = case reads s :: [(Double, String)] of
  [(_, "")] -> True
  _         -> False

-- function that splits given string into elements and creates a list of symbols
tokenize :: String -> [Symbol]
tokenize xs = tok' $ words (removeRedundantBrackets xs) 
    where tok' []     = []
          tok' (c:cs) | elem c operators          = SymOp (operator c) : tok' cs
                      | elem c functions          = SymFun (function c) : tok' cs
                      | c == "("	              = SymLBracket : tok' cs
                      | c == ")"                  = SymRBracket : tok' cs
                      | isInteger c || isDouble c = SymNum (read c::Double) : tok' cs
                      | isAlpha $ head c          = SymIdent  c : tok' cs
                      | otherwise                 = error $ "Input expression is invalid! Cannot tokenize " ++ c



-- 3 - Parser


-- helper function that returns following symbol in a list
lookAhead :: [Symbol] -> Symbol
lookAhead []     = SymEnd
lookAhead (c:cs) = c

-- helper function that returns the tail of the list
accept :: [Symbol] -> [Symbol]
accept []     = error "Nothing to accept"
accept (t:ts) = ts

  -- 3.1 - The grammar for creating a tree

-- Expression production
    -- only for [+ -] operators, they have the highest priority in tree
    -- E <- T [+ -] E
    -- E <- T
expression :: [Symbol] -> (Tree, [Symbol])
expression sym = 
   let (termTree, sym') = term sym
   in
      case lookAhead sym' of
         (SymOp op) | elem op [Add, Sub] -> 
            let (exTree, sym'') = expression (accept sym') 
            in (SumNode op termTree exTree, sym'')
         _ -> (termTree, sym')

-- Term production
    -- only for [* /] operators, they have the highest priority in tree after [+ -] operators
    -- T <- T2 [* /] T
    -- T <- T2
term :: [Symbol] -> (Tree, [Symbol])
term sym = 
   let (term2Tree, sym') = term2 sym
   in 
      case lookAhead sym' of
         (SymOp op) | elem op [Mul, Div] ->
            let (termTree, sym'') = term (accept sym') 
            in (ProdNode op term2Tree termTree, sym'')
         _ -> (term2Tree, sym')

-- Term2 production
    -- only for [^] operator, it has the highest priority in tree after [* /] operators
    -- T2 <- F [^] Number
    -- T2 <- F [^] [-] Number
    -- T2 <- F
term2 :: [Symbol] -> (Tree, [Symbol])
term2 sym = 
   let (facTree, sym') = factor sym
   in
      case lookAhead sym' of
         (SymOp op) | elem op [Pow] ->
            let (fac2Tree, sym'') = factor (accept sym') 
            in 
              case fac2Tree of
                NumNode x -> (PowNode op facTree fac2Tree, sym'')
                UnaryNode o (NumNode x) -> (PowNode op facTree fac2Tree, sym'')
                _ -> error "Second argument in function pow has to be number only"
         _ -> (facTree, sym')

-- Factor production
    -- F <- Number
    -- F <- Identifier
    -- F <- [+ -] F
    -- F <- "(" E ")"
    -- F <- [Sin Cos Exp Log] F
factor :: [Symbol] -> (Tree, [Symbol])
factor sym = 
   case lookAhead sym of
      (SymNum x)     -> (NumNode x, accept sym)
      (SymIdent str) -> (VarNode str, accept sym)
      (SymOp op) | elem op [Add, Sub] -> 
            let (facTree, sym') = factor (accept sym) 
            in (UnaryNode op facTree, sym')
      (SymFun fun) -> 
            let (facTree, sym') = factor (accept sym) 
            in (FuncNode fun facTree, sym')
      SymLBracket  -> 
         let (expTree, sym') = expression (accept sym)
         in
            if lookAhead sym' /= SymRBracket 
            then error "Missing right parenthesis"
            else (expTree, accept sym')
      _ -> error $ "Parse error on symbol: " ++ show sym

-- parse funtion that applies the rules of grammar to symbols in order to create the expression tree
parse :: [Symbol] -> Tree
parse sym | null sym' = tree
          | otherwise = error $ "Input expression is invalid! There are some leftover symbols: " ++ show sym'
    where (tree, sym') = expression sym

-- function that transforms given string into tree
createExpression :: String -> Tree
createExpression = parse . tokenize



-- 4 - Evaluator


-- function that takes an expression tree and calculates the result of an expression
evaluate :: Tree -> Double
evaluate (SumNode op l r) | op == Add = l' + r'
                          | op == Sub = l' - r'
    where l' = evaluate l
          r' = evaluate r
evaluate (ProdNode op l r) | op == Mul = l' * r'
                           | op == Div && r' == 0 = error "Divide by zero error"
						   | otherwise            = l' / r'
    where l' = evaluate l
          r' = evaluate r
evaluate (PowNode op l r) = l' ** r'
    where l' = evaluate l
          r' = evaluate r
evaluate (FuncNode fu t) | fu == Sin = sin x
                         | fu == Cos = cos x
                         | fu == Exp = exp x
                         | fu == Log = log x
    where x = evaluate t
evaluate (UnaryNode op t) | op == Add = x
                          | op == Sub = -x
    where x = evaluate t
evaluate (NumNode x) = x
evaluate _ = error "Tree still contains variables, substitute them before evaluation"



-- 5 - Substitute


-- function that replaces given string with number in tree
substitute :: String -> Tree -> Double -> Tree
substitute s t n | elem s $ words $ show t = substitute' s t n
                 | otherwise               = error ("Cannot substitute, because variable " ++ s ++ " doesn't exist in expression")

-- helper function for substitute that goes through tree and replaces given string with given number in all tree
substitute' :: String -> Tree -> Double -> Tree
substitute' s (SumNode o l r)  n = SumNode o (substitute' s l n) (substitute' s r n)
substitute' s (ProdNode o l r) n = ProdNode o (substitute' s l n) (substitute' s r n)
substitute' s (PowNode o l r)  n = PowNode o (substitute' s l n) (substitute' s r n)
substitute' s (FuncNode f t)   n = FuncNode f (substitute' s t n)
substitute' s (UnaryNode o t)  n = UnaryNode o (substitute' s t n)
substitute' s t@(VarNode x)    n | x == s && n<0  = UnaryNode Sub (NumNode $ abs n)
                                 | x == s && n>=0 = NumNode n
                                 | otherwise      = t
substitute' s t@(NumNode x)    n = t



-- 6 - Derive


-- helper function for product rule formula
productRule :: String -> String -> String -> String -> String
productRule f f' g g' = "( " ++ f' ++ " * " ++ g ++ " + " ++ f ++ " * " ++ g' ++ " )"

-- helper function for quotient rule formula
quotientRule :: String -> String -> String -> String -> String
quotientRule f f' g g' = "( ( " ++ f' ++ " * " ++ g ++ " - " ++ f ++ " * " ++ g' ++ " ) / " ++ g ++ " ^ 2.0 )"

-- helper function for power rule formula
powerRule :: String -> String -> String -> String -> String
powerRule f f' g g' = "( " ++ g ++ " * " ++ f ++ " ^ " ++ "( " ++ show (read g - 1.0) ++ " ) * " ++ f' ++ " )"

-- function for partial derivative, it derives given expression by given variable 
deriveBy :: Tree -> String -> Tree
deriveBy tr x = createExpression . snd $ derive tr x
    where derive (NumNode a) x = (show a, "0")
          derive (VarNode s) x | x == s    = (s, "1.0")
                               | otherwise = (s, "0")
          derive (UnaryNode o t) x = 
            let (f, f') = derive t x
            in 
               case o of
                 Add -> (f, f')
                 Sub -> if (f==x)
                        then ("( - " ++ f ++ " )", "( - " ++ f' ++ " )") 
                        else ("( - " ++ f ++ " )", f')
          derive (SumNode o l r) x = 
            let (f, f') = derive l x
                (g, g') = derive r x
            in ("( " ++ f ++ opToStr o ++ g ++ " )", "( " ++ f' ++ opToStr o ++ g' ++ " )")
          derive (ProdNode o l r) x = 
            let (f, f') = derive l x
                (g, g') = derive r x
            in 
               case o of
                 Mul -> ("( " ++ f ++ opToStr o ++ g ++ " )", productRule f f' g g')
                 Div -> ("( " ++ f ++ opToStr o ++ g ++ " )", quotientRule f f' g g')
          derive (PowNode o l r) x = 
            let (f, f') = derive l x
                (g, g') = derive r x
            in ("( " ++ f ++ opToStr o ++ g ++ " )", powerRule f f' g g')
          derive (FuncNode fu t) x = 
            let (f, f') = derive t x
            in 
               case fu of 
                 Sin -> ("( " ++ funToStr fu ++ f ++ " )", "( cos " ++ f ++ " * " ++ f' ++ " )")
                 Cos -> ("( " ++ funToStr fu ++ f ++ " )", "( - sin " ++ f ++ " * " ++ f' ++ " )")
                 Exp -> ("( " ++ funToStr fu ++ f ++ " )", "( exp " ++ f ++ " * " ++ f' ++ " )")
                 Log -> ("( " ++ funToStr fu ++ f ++ " )", "( 1.0 / " ++ f ++ " * " ++ f' ++ " )")



-- 7 - Simplification for redundant brackets in expressions


-- helper function that scans the unparenthesized operators between the parentheses 
-- and defines variable x which is the lowest priority operator  
calculateX :: [String] -> String
calculateX s = cal 0 s "0"
    where cal _ []     r = r
          cal 0 (x:xs) r | x == "("                    = cal 1 xs r
                         | elem x ["-", "+", "*", "/"] = if elem r ["-", "+"] then cal 0 xs r
                                                         else if elem r ["*", "/"] then cal 0 xs x
                                                         else cal 0 xs x
                         | otherwise                   = cal 0 xs r
          cal n (x:xs) r | x == ")"  = cal (n-1) xs r
                         | x == "("  = cal (n+1) xs r
                         | otherwise = cal n xs r

-- algorithm that checks if given brackets are redundant or not
--
-- basic algorithm:
-- Let lc be operator immediately left of the left parenthesis, or "0"
-- Let rc be operator immediately right of the right parenthesis, or "0"
-- If lc is "0" and rc is "0":
--   Redundant
-- Else:
--   Scan the unparenthesized operators between the parentheses
--   Let x be the lowest priority operator
--   If x has lower priority than lc or rc:
--     Not redundant
--   Else:
--     Redundant
--
-- some changes are added in algorithm afterwards for operators [- / ^] and for functions
isRedundant :: [String] -> (Int, Int) -> Bool
isRedundant array (p1, p2) | elem lc functions || elem lc ["^", "/"] || rc == "^" = False
                           | lc == "0" && rc == "0"                               = True
                           | otherwise = if (elem x ["+", "-"] && (elem lc ["*", "/"] || elem rc ["*", "/"])) then False
                                         else (if elem x ["+", "-"] && lc == "-" then False else True)
    where lc    = if (p1 == 0) then "0" else array !! (p1-1)
          rc    = if (p2 == (length array - 1)) then "0" else array !! (p2+1)
          x     = calculateX range 
          range = take (p2 - p1 - 1) $ drop (p1+1) array

-- helper function that removes redundant brackets from expression
removeElements :: [(String, Int)] -> [Int] -> String
removeElements array ns = rem' array ns "" 
    where rem' []           _  rez = rez
          rem' ((xs,n):xss) ns rez | elem n ns = rem' xss ns rez
                                   | otherwise = rem' xss ns (rez ++ xs ++ " ")

-- function that searches for redundant brackets and than calls function removeElements which removes those brackets
removeRedundantBrackets :: String -> String
removeRedundantBrackets ys = rem array [] []
    where rem []     st1 st2 = removeElements array st2
          rem (x:xs) st1 st2 | fst x == "(" = rem xs ((snd x) : st1) st2
                             | fst x == ")" = if isRedundant elements (head st1, snd x) 
                                              then rem xs (tail st1) (head st1 : snd x : st2)
                                              else rem xs (tail st1) st2
                             | otherwise    = rem xs st1 st2 
          elements           = words ys
          array              = zip elements [0..]



-- 8 - Beautify

-- beautify function
-- removes unnecessary zeros inside operations
-- simplifies expressions: E * 1 = E
-- 						   E / 1 = E
-- 						   0 ^ E = 0
--                         E ^ 0 = 1
--                         E ^ 1 = E
--                         - 0   = 0
--                         + 0   = 0
beautify :: Tree -> Tree
beautify (SumNode o l r)  | l' == (NumNode 0.0) && r' == (NumNode 0.0) = NumNode 0.0
                          | l' == (NumNode 0.0)                        = r'
                          | r' == (NumNode 0.0)                        = l' 
                          | otherwise                                  = SumNode o l' r'
    where l' = beautify l
          r' = beautify r
beautify (ProdNode o l r) | o == Div && r' == (NumNode 0.0)            = error $ "Divide by zero error"
                          | l' == (NumNode 0.0) || r' == (NumNode 0.0) = NumNode 0.0
                          | l' == (NumNode 1.0)                        = r'
                          | r' == (NumNode 1.0)                        = l'
                          | otherwise                                  = ProdNode o l' r'
    where l' = beautify l
          r' = beautify r
beautify (PowNode o l r)  | l' == (NumNode 0.0) = NumNode 0.0
                          | r' == (NumNode 0.0) = NumNode 1.0
                          | r' == (NumNode 1.0) = l'
                          | otherwise           = PowNode o l' r'
    where l' = beautify l
          r' = beautify r
beautify (FuncNode f t)   = FuncNode f (beautify t)
beautify (UnaryNode o t)  | t' == (NumNode 0.0) = NumNode 0.0
                          | otherwise           = UnaryNode o t'
    where t' = beautify t
beautify t@(VarNode x)    = t
beautify t@(NumNode x)    = t
