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
import Data.List
import Control.Monad.State.Lazy
import Debug.Trace
import System.Directory
import Control.Monad (forM)
import System.Exit
import System.FilePath.Posix

type AST = CTranslUnit

readCFile :: String -> IO (Either ParseError AST)
readCFile fileName = do
  contents <- BS.readFile fileName
  -- parsing with preprocessing
  parseCFilePre fileName
  parseCFile (newGCC "gcc") Nothing [] fileName

preprocessFile :: String -> String -> IO InputStream
preprocessFile includeDir filePath = do
  ans <- runPreprocessor (newGCC "gcc") cppArgs
  case ans of
    Left exitCode -> exitWith exitCode
    Right stream -> return stream
  where cppArgs = CppArgs
          [IncludeDir includeDir, IncludeFile filePath]
          []
          Nothing --"tmp"
          filePath
          Nothing

langMain :: IO ()
langMain = do
  -- Every program is inside /Programs
  let programsDir = "Programs"
  allDirs <- listDirectory programsDir
  -- Every program is a directory
  let graphsDir = "Graphs"
  createDirectoryIfMissing False graphsDir
  allGraphs <- forM allDirs $ \dir -> do
    let includeDir = programsDir ++ "/" ++ dir
    allFiles <- listDirectory includeDir
    case find (== "main.c") allFiles of
      Nothing -> error $ "No main.c in " ++ includeDir ++ "."
      Just mainFile -> do
        let mainFilePath = includeDir ++ "/" ++ mainFile
        stream <- preprocessFile includeDir mainFilePath
        let program = parseC stream (position 0 mainFile 0 0 Nothing) --parseCFilePre readCFile (programsDir ++ "/" ++ file)
        case program of
          Left err -> error $ show err
          Right ast -> do
            print (prettyUsingInclude ast)
            -- filter the relevant part of the AST
            let ans = ast2rast ast
            print $ length ans
            --print $ map func_name ans
            --print $ find ((=="main"). func_name) ans
            --print ans
            -- Compute the memory usage graph
            let graph = rast2graph ans
            -- write graph to file
            writeFile (graphsDir ++ "/" ++ dir ++ "_graph") (prettyShowGraph graph)
            return graph
  putStrLn "MaxLen: "
  print $  maximum . map length . concat . (map (map (\(V v x cs) -> cs))) .(map (\(Graph f r) -> f : r)) $ allGraphs
  return ()

go = langMain

----------------------------------------------------------

type RAST = [FUNC]

data FUNC = FUNC {
  func_name :: String
  , func_stmt :: FSTMT
  } deriving (Show, Eq)

data FSTMT = EXPR FEXPR FSTMT
           | FCALL String FSTMT
           | LOOP FSTMT FSTMT
           | IF FSTMT (Maybe FSTMT) FSTMT
           | RETURN
           deriving (Show, Eq)

type FEXPR = [MEMorFUNC]

data MEMorFUNC = MEMUSE MemUsage
               | FCALL_ String
               deriving (Show, Eq)

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
  = FUNC (getName declarator) (statement2FSTMT statement)
  where
    getName :: CDeclarator a -> String
    getName (CDeclr (Just (Ident name num _)) _ _ _ _) = name

-- Statement = Control Flow
statement2FSTMT :: CStatement a -> FSTMT
statement2FSTMT stmt = case stmt of

  CCompound _ comps _ -> compounds2stmt comps

  CExpr (Just expr) _ -> EXPR (expr2FEXPR expr) RETURN

  CExpr Nothing _ -> RETURN

  CIf expr stmt mstmt _ -> EXPR (expr2FEXPR expr) (
    IF (statement2FSTMT stmt)
       (statement2FSTMT <$> mstmt)
       RETURN
    )

  CWhile expr stmt _ _
    -> LOOP (EXPR (expr2FEXPR expr) (statement2FSTMT stmt)) RETURN

  CFor _ _ (Just expr) stmt _
    -> LOOP (EXPR (expr2FEXPR expr) (statement2FSTMT stmt)) RETURN

  CReturn Nothing _ -> RETURN
  CReturn (Just expr) _ -> EXPR (expr2FEXPR expr) RETURN

  _ -> RETURN

compounds2stmt :: [CCompoundBlockItem a] -> FSTMT
compounds2stmt = mergeFSTMTs . map fromJust . filter isJust . map compound2stmt
  where
    compound2stmt :: CCompoundBlockItem a -> Maybe FSTMT
    compound2stmt comp = case comp of
      CBlockStmt stmt -> Just (statement2FSTMT stmt)
      _ -> Nothing

mergeFSTMTs :: [FSTMT] -> FSTMT
mergeFSTMTs = walk
  where
    walk [] = RETURN
    walk (now : rest) = case now of
      EXPR n cont -> EXPR n (walk (cont : rest))
      FCALL f cont -> FCALL f (walk (cont : rest))
      LOOP s cont -> LOOP s (walk (cont : rest))
      IF s ms cont -> IF s ms (walk (cont : rest))
      RETURN -> walk rest

--unary
--cindex
--funccall = call (var name)

expr2FEXPR :: CExpression a -> FEXPR
expr2FEXPR expr = case expr of
  CComma es _ -> sgo es
  CAssign _ e1 e2 _ -> go e1 ++ go e2
  CCond e1 me e2 _ ->  go e1 ++ mgo me ++ go e2
  CBinary _ e1 e2 _ -> go e1 ++ go e2
  CCast _ e _ -> go e
  CUnary CAdrOp e _ -> [MEMUSE 1] ++ go e
  CUnary CIndOp e _ -> [MEMUSE 1] ++ go e
  CUnary _ e _ -> go e
  CSizeofExpr e _ -> go e
  CSizeofType _ _ -> [MEMUSE 0]
  CAlignofExpr e _ -> go e
  CAlignofType _ _ -> [MEMUSE 0]
  CComplexReal e _ -> go e
  CComplexImag e _ -> go e
  CIndex e1 e2 _ -> [MEMUSE 1] ++ go e1 ++ go e2
  CCall (CVar (Ident name _ _) _) as _ -> [FCALL_ name] ++ sgo as
  CCall _ as _ -> sgo as
  CMember e _ _ _ -> go e
  CVar _ _ -> [MEMUSE 0]
  CConst _ -> [MEMUSE 0]
  CCompoundLit _ _ _ -> [MEMUSE 0]
  CGenericSelection e _ _ -> go e
  CStatExpr _ _ -> [MEMUSE 0]
  CLabAddrExpr _ _ -> [MEMUSE 0]
  CBuiltinExpr _ -> [MEMUSE 0]
  where
    go = expr2FEXPR

    mgo :: Maybe (CExpression a) -> FEXPR
    mgo me = case me of
      Nothing -> [MEMUSE 0]
      Just e -> expr2FEXPR e

    sgo :: [CExpression a] -> FEXPR
    sgo = concat . map go

-- Filter The C AST

data Vertex v a = V v a [v]
                deriving (Show, Eq)

data Graph v a = Graph (Vertex v a) [Vertex v a]
               deriving (Show, Eq)

type MemUsage = Double

type VertexId = Int

type Graph' = Graph VertexId MemUsage

rast2graph :: RAST -> Graph'
rast2graph allDefs = case find ((== "main") . func_name) allDefs of
  Just (FUNC _ mainStmt) -> makeGraph allDefs mainStmt
  Nothing -> error "No main"

newVertexId :: State VertexId VertexId
newVertexId = do
  ans <- get
  put $ ans + 1
  return ans

makeGraph :: RAST -> FSTMT -> Graph'
makeGraph allDefs stmt = evalState (makeGraph' allDefs stmt graph) 1
  where graph = Graph (V 0 0 []) []

extractMemUsage :: FEXPR -> MemUsage
extractMemUsage = sum . map (\(MEMUSE m) -> m) . filter isMEMUSE
  where
    isMEMUSE x = case x of
      MEMUSE _ -> True
      _ -> False

extractFCALLs :: FEXPR -> [String]
extractFCALLs = map (\(FCALL_ s) -> s) . filter isFCALL
  where
    isFCALL x = case x of
      FCALL_ _ -> True
      _ -> False

makeGraph' :: RAST -> FSTMT -> Graph' -> State VertexId Graph'
makeGraph' allDefs stmt graph = case stmt of
  EXPR fexpr cont -> do

    traceShowM $ "Hmmm1... " ++ show graph

    let memUsage = extractMemUsage fexpr
        Graph (V v x conns) rest = graph

    -- ugly
    let fcalls = extractFCALLs fexpr
        g [] = cont
        g (s:ss) = FCALL s (g ss)

    go (g fcalls) $ Graph (V v (x + memUsage) conns) rest

  FCALL funcName cont -> do

    traceShowM $ "Hmmm2... " ++ show graph

    case find ((==funcName) . func_name) allDefs of

      Nothing -> do
        --traceShowM $ "Hmmm21... " ++ show graph
        ans <- go cont graph --error $ "No function named " ++ funcName ++ "."
        --traceShowM $ "Hmmm22... " ++ show ans
        return ans

      Just (FUNC _ funcBody) -> do

        let Graph (V f x conns) rest = graph

        vr <- newVertexId
        --traceShowM $ "aaaaaaaaaaaaaa"
        Graph (V f' x' conns') rest' <- go funcBody $ Graph (V vr 0 []) [] --graph

        go cont $ Graph (V f' x' conns') (V f x (vr:conns) : (rest ++ rest')) --graph'

  LOOP loop cont -> do

    traceShowM $ "Hmmm3... " ++ show graph

    let Graph (V f x conns) rest = graph

    vl <- newVertexId

    Graph (V f' x' conns') rest' <- go loop $ Graph (V vl 0 []) []

    vr <- newVertexId

    traceShowM $ "Hmmm31... " ++ show (f', x', f, conns', V f' x' (f : conns'))

    go cont
      $ Graph
      (V vr 0 [f])
      (V f' x' (vr : conns')
       : V f x (vl : conns)
       : nub (rest ++ rest'))

    --go cont $ Graph (V f' x' (f : conns')) (V f x (vl:conns) : nub (rest ++ rest'))

  IF br1 (Just br2) cont -> do

    traceShowM $ "Hmmm4... " ++ show graph

    let Graph (V f x conns) rest = graph

    v1 <- newVertexId
    v2 <- newVertexId

    Graph (V f1 x1 conns1) rest1 <- go br1 $ Graph (V v1 0 []) []
    Graph (V f2 x2 conns2) rest2 <- go br2 $ Graph (V v2 0 []) []

    vr <- newVertexId

    go cont
      $ Graph (V vr 0 [])
      (V f1 x1 (vr:conns1)
       : V f2 x2 (vr:conns2)
       : V f x (v1:v2:conns)
       : nub (rest ++ rest1 ++ rest2))

  IF br1 Nothing cont -> do

    traceShowM $ "Hmmm5... " ++ show graph

    let Graph (V f x conns) rest = graph

    v1 <- newVertexId

    Graph (V f1 x1 conns1) rest1 <- go br1 $ Graph (V v1 0 []) []

    traceShowM $ "Hey " ++ show (rest, rest1, V f1 x1 conns1, V f x conns)
    vr <- newVertexId

    go cont $ Graph (V vr 0 []) (V f1 x1 (vr:conns1) : V f x (v1:vr:conns) : nub (rest ++ rest1))

  RETURN -> do
    --traceShowM $ "Hmmm6... " ++ show graph
    return graph

  where go = makeGraph' allDefs

prettyShowGraph :: Graph' -> String
prettyShowGraph (Graph focus rest)
  = "{ " ++ intercalate ", " (map prettyShowVertex all) ++ " }"
  where

    all = focus : rest

    prettyShowVertex :: Vertex VertexId MemUsage -> String
    prettyShowVertex (V vid mUse conns)
      = "\"" ++ show vid ++ "\": [" ++ show mUse ++ "," ++ show conns ++ "]"
