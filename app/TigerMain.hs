module Main (main) where
import           Control.Monad
import           Control.Monad.State   hiding (evalState)
import           Data.Either
import           Data.Maybe
import           System.Console.GetOpt
import qualified System.Environment    as Env
import           System.Exit
import           System.IO

import           TigerAbs
import           TigerEscap
import           TigerParser
import           TigerPretty
import           TigerSeman
import           TigerTemp
import           TigerUnique
import           TigerFrame
import           TigerCanon
import           TigerColoring
import qualified TigerTree             as Tree
import qualified TigerSymbol           as Symb
import qualified TigerMunch2           as Munch
import qualified TigerAsm              as Asm

import           Text.Parsec           (runParser)

import Debug.Trace

data Options = Options {
        optArbol     :: Bool
        ,optDebEscap :: Bool
        ,optIR       :: Bool
        ,optAsm      :: Bool
        ,optFile     :: Bool
    }
    deriving Show

defaultOptions :: Options
defaultOptions = Options {optArbol = False, optDebEscap = False, optIR = False, optAsm = False, optFile = False}

options :: [OptDescr (Options -> Options)]
options = [ Option ['a'] ["arbol"] (NoArg (\opts -> opts {optArbol = True})) "Muestra el AST luego de haber realizado el cálculo de escapes"
            , Option ['e'] ["escapada"] (NoArg (\opts -> opts {optDebEscap = True})) "Stepper escapadas"
            , Option ['i'] ["ir"] (NoArg (\opts -> opts {optIR = True})) "Muestra la representacion intermedia"
            , Option ['s'] ["asm"] (NoArg (\opts -> opts {optAsm = True})) "Imprime el assembly en pantalla" 
            , Option ['f'] ["file"] (NoArg (\opts -> opts {optFile = True})) "Imprime el assembly a un archivo .s" 
          ]

compilerOptions :: [String] -> IO (Options, [String])
compilerOptions argv = case getOpt Permute options argv of
                        (o,n,[]) -> return (foldl (flip id) defaultOptions o, n)
                        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where
        header = "Se usa: tiger fileName [OPTIONS] "

showExp :: Exp -> IO ()
showExp e = do
    putStrLn ""
    putStrLn "## Mostramos el AST (PP Gracias a Emilio Lopez Junior) ##"
    --putStrLn $ renderExp e
    putStrLn "#########################################################"
    putStrLn "######### Mostramos el AST (plano para debugeo) #########"
    putStrLn $ show e
    putStrLn "#########################################################"
    putStrLn ""

showIR :: ([Frag], [([Tree.Stm], Frame)]) -> IO ()
showIR (chars,procs) = do
    putStrLn ""
    putStrLn "## Mostramos la IR (Gracias a Maradona Senior) ##"
    putStrLn "Strings:"
    putStrLn $ show chars
    putStrLn "Procs:"
    let stms = concat $ fst $ unzip procs
    foldMap (putStrLn . show) stms
    putStrLn "#################################################"
    putStrLn ""


calculoEscapadas :: Exp -> Options -> IO Exp
calculoEscapadas rawAST opts =
  if (optDebEscap opts)
  then
    fail "NO DEBBUGING!"
  else
    either (\err ->
               putStrLn "Error de Escap:" >>
               fail (show err)
           ) return (calcularEEsc rawAST)

-- | templabRel does all generic unique computations (Seman -> Canon)
templabRel :: Exp -> StGen (Either Symb.Symbol ([Frag], [([Tree.Stm], Frame)]))
templabRel ast = do
  -- recover Frags from Semantic Analysis
  frags <- runFrags ast -- runFrags ast :: StGen (Either Symbol [Frag])
  either  (return . Left) -- return error
          (\frags -> do
              let (chars, procs) = sepFrag frags -- sepFrag frags :: ([Frag],[(Stm,Frame)])
              canon <- mapM (\(st,fr) -> (flip evalStateT firstTank (canonM st)) >>= \st' -> return (st',fr)) procs -- canonize stm fragments
              return $ Right (chars, canon)
          )
          frags

-- | makeAssembly does all assembly related computations (Munch -> Live -> Color)
makeAssembly :: ([Frag], [([Tree.Stm], Frame)]) -> StGen (Either Symb.Symbol [String])
makeAssembly (chars,procs) = do
  let (stms, frms) = unzip procs
  -- Munch
  inss <- mapM Munch.codeGen stms
  -- procEntryExit2
  let inss' = zipWith procEntryExit2 frms inss
  -- Color
  unq <- get
  let (frms'',inss'') = unzip $ zipWith (\frm ins -> runColoring frm ins unq) frms inss'
  -- procEntryExit3
  --return $ [strings ++ body]
  --return (Right [])
  let body = map (Asm.format show) (concat inss'')
  let body2 = map Symb.unpack body
  return $ Right $ body2

-- Toma opciones, nombre del archivo, source code
-- Devuelve el archivo parseado o el error
parserStep :: Options -> String -> String -> IO Exp
parserStep opts nm sc = either
  (\perr -> error $ "Parser error" ++ show perr)
  return
  $ runParser expression () nm sc

writeToFile :: String -> Either Symb.Symbol [String] -> IO ()
writeToFile name asm =
  let f = foldr (\a as -> a ++ "\n" ++ as ) "\n"
      filename = name ++ ".s"
  in either print (writeFile filename . f) asm


main :: IO ()
main = do
    -- getArgs returns a list of command line arguments (not including program name)
    -- s is a tiger program file, opts are the tiger compiler options
    s:opts <- Env.getArgs
    (opts', _) <- compilerOptions opts
    sourceCode <- readFile s
    -- parse source code of tiger program, returning an Exp
    rawAst <- parserStep opts' s sourceCode
    -- calculate escapes
    ast <- calculoEscapadas rawAst opts'
    -- option to show ast tree
    when (optArbol opts') (showExp ast)
    -- Semantic analysis & Canonization
        -- canons,stgenCounter :: (Either Symb.Symbol ([Frag], [([Tree.Stm], Frame)]), Integer)
    let (canons, stgenCounter) = evalState (templabRel ast) 0
    when (optIR opts') (either print showIR canons)
    let assembly = canons >>= return . makeAssembly
        namep = take ((length s) - 4) s -- name prefix
    when (optFile opts') $ either print ((writeToFile namep) . fst . (flip evalState stgenCounter)) assembly
    putStrLn "Tiger Compiler finished"
