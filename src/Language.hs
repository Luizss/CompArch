module Language where

import Language.C
import Language.C.Parser
import Language.C.Data.Position
import qualified Data.ByteString as BS
import Data.Either
import Language.C.System.Preprocess
import Language.C.System.GCC
import Language.C.Pretty
import Language.C.Syntax.AST
import Language.C.Data.Ident
import Language.C.Data.Ident
import Data.Maybe

type AST = CTranslUnit

readCFile :: String -> IO (Either ParseError AST)
readCFile fileName = do
  contents <- BS.readFile fileName
  -- parsing with preprocessing
  parseCFile (newGCC "gcc") Nothing [] fileName

--go :: IO ()
go = do
  ans <- readCFile "Examples/example1.c"
  case ans of
    Left err -> putStrLn $ show err
    Right ast -> do
      ans <- processAST ast
      print ans
      return ()

--processAST :: AST -> IO ()
processAST ast = do
  let doc = prettyUsingInclude ast
  putStrLn $ show doc
  putStrLn $ show (fmap (\_ -> ()) ast)
  --let rAST = ast2rast ast
  --return rAST

----------------------------------------------------------

type RAST = [FUNC]

type FUNCNAME = String

data FUNC = FUNC FUNCNAME [FDEF]

-- TODO To be defined: something like that
data FDEF = FUNCCALL FUNCNAME
          | MEMUSAGE Int
          | IF FDEF FDEF
          | IFELSE FDEF FDEF FDEF
          | WHILE
          | FOR
          | DOWHILE
          deriving (Show, Eq)

----------------------------------------------------------

-- -- Stands for Relevant AST
-- data RAST = RAST [REDECL] deriving (Show, Eq)

-- -- Relevant External Declaration
-- data REDECL = RDECL RDECL
--             | RSA RSA
--             | RFUNC RFUNC
--             | DontCare
--             deriving (Show, Eq)


-- data RDECL = RDECL_ () deriving (Show, Eq)

-- data RSA = RSA_ () deriving (Show, Eq)

-- data RFUNC = RFUNC_ () deriving (Show, Eq)

-----------------------------------------------------------

ast2rast :: AST -> RAST
ast2rast (CTranslUnit externalDecls _) = edecls2RAST externalDecls

edecls2RAST :: [CExternalDeclaration a] -> [FUNC]
edecls2RAST = map fromJust . filter isJust . map edecl2FUNC

edecl2FUNC :: CExternalDeclaration a -> Maybe FUNC
edecl2FUNC edecl = case edecl of
  CFDefExt funcDef -> Just $ functionDef2FUNC funcDef
  _ -> Nothing

functionDef2FUNC :: CFunctionDef a -> FUNC
functionDef2FUNC (CFunDef _declSpec declarator _args statement _nodeInfo)
  = FUNC (getName declarator) (statement2FDEF statement)
  where
    getName :: CDeclarator a -> String
    getName (CDeclr (Just (Ident name num _)) _ _ _ _) = name

-- Statement = Control Flow
statement2FDEF :: CStatement a -> FDEF
statement2FDEF statement = case statement of
  -- A statement of the form @case expr : stmt@
  -- CCase (CExpression a) (CStatement a) a -> undefined
  CCase expr stmt _ -> undefined -- controlflow

  -- A case range of the form @case lower ... upper : stmt@
  -- CCases (CExpression a) (CExpression a) (CStatement a) a
  CCases expr1 expr2 stmt _ -> undefined -- controlflow

  -- The default case @default : stmt@
  -- CDefault (CStatement a) a
  CDefault (CStatement a) a -> undefined -- controlflow

  -- A simple statement, that is in C: evaluating an expression with
  -- side-effects and discarding the result.
  -- CExpr (Maybe (CExpression a)) a
  CExpr mexpr _ -> undefined -- recursive (can be a func call) -- Memo usage is in here

  -- compound statement @CCompound localLabels blockItems at@
  CCompound [Ident] [CCompoundBlockItem a] a
  -- conditional statement @CIf ifExpr thenStmt maybeElseStmt at@

  CIf (CExpression a) (CStatement a) (Maybe (CStatement a)) a
  -- switch statement @CSwitch selectorExpr switchStmt@, where
  -- @switchStmt@ usually includes /case/, /break/ and /default/
  -- statements
  CSwitch (CExpression a) (CStatement a) a
  -- while or do-while statement @CWhile guard stmt isDoWhile at@
  CWhile (CExpression a) (CStatement a) Bool a
  -- for statement @CFor init expr-2 expr-3 stmt@, where @init@ is
  -- either a declaration or initializing expression
  CFor (Either (Maybe (CExpression a)) (CDeclaration a))
    (Maybe (CExpression a))
    (Maybe (CExpression a))
    (CStatement a)
    a
  -- continue statement
  CCont a
  -- break statement
  CBreak a
  -- return statement @CReturn returnExpr@
  CReturn (Maybe (CExpression a)) a

-- Filter The C AST

---------------------------------
-- TODO
-- C -> IL -> Graph

-- IL: MEMORY ACCESS | CONTROLFLOW (LOOPS | CONDITION)
