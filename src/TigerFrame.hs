module TigerFrame where

import           TigerTemp
import           TigerTree

import           TigerAbs                       ( Escapa(..) )

import           TigerSymbol

import           Prelude                 hiding ( exp )
import           Data.Map                as Map

--

-- | Registros muy usados.
fp, sp, rv, hi, lo :: Temp
-- | Frame pointer
fp = pack "$fp"
-- | Stack pointer
sp = pack "$sp"
-- | Return value -- FIXME
rv = pack "$rv"
-- | HI registro-ro
hi = pack "HI"
-- | LO registro-ro
lo = pack "LO"
-- | ZERO
zero = pack "$zero"
-- | Return Adress
ra = pack "$ra"


a0, a1, a2, a3 :: Temp
a0 = pack "$a0"
a1 = pack "$a1"
a2 = pack "$a2"
a3 = pack "$a3"

s0, s1, s2, s3, s4, s5, s6, s7, s8 :: Temp
s0 = pack "$s0"
s1 = pack "$s1"
s2 = pack "$s2"
s3 = pack "$s3"
s4 = pack "$s4"
s5 = pack "$s5"
s6 = pack "$s6"
s7 = pack "$s7"
s8 = pack "$s8"

t0, t1, t2, t3, t4, t5, t6, t7, t8 :: Temp
t0 = pack "$t0"
t1 = pack "$t1"
t2 = pack "$t2"
t3 = pack "$t3"
t4 = pack "$t4"
t5 = pack "$t5"
t6 = pack "$t6"
t7 = pack "$t7"
t8 = pack "$t8"
t9 = pack "$t9" 

-- | Mapping from special temps to their names TODO
tempMap = empty :: Map.Map Temp String


-- CONSULTAR: Agregamos hi y lo a algun lado?
specialregs, argregs, calleesaves, callersaves :: [Temp]
-- | Special Regs
specialregs = [fp, sp, rv, zero, ra]
-- | Argument Regs
argregs     = [a0, a1, a2, a3]
-- | Callee Saves
calleesaves = [s0, s1, s2, s3, s4, s5, s6, s7, s8]
-- | Caller Saves
callersaves = [t0, t1, t2, t3, t4, t5, t6, t7, t8, t9]


-- | Word size in bytes
wSz :: Int
wSz = 4
-- | Base two logarithm of word size in bytes
log2WSz :: Int
log2WSz = 2

-- Estos offsets se utilizan para el calculo de acceso de variables que escapan
-- (principalmente)
-- | Offset
fpPrev :: Int
fpPrev = 0
-- | Donde se encuentra el FP del nivel anterior (no necesariamente el llamante?)
-- CONSULTAR que valor toma fpPrevLev
fpPrevLev :: Int
fpPrevLev = 0

-- | Esto es un offset previo a al lugar donde se encuentra el lugar de las variables
-- o de los argumentos.
argsGap, localsGap :: Int
argsGap = wSz
localsGap = 4

-- | Dan inicio a los contadores de argumentos, variables y registros usados.
-- Ver |defaultFrame|
argsInicial, regInicial, localsInicial :: Int
argsInicial = 0
regInicial = 1
localsInicial = 0

-- | Listas de regustros que define la llamada y registros especiales
calldefs, specialregs :: [Temp]
calldefs = [rv]
specialregs = [rv, fp, sp]

-- | Tipo de dato que define el acceso a variables.
data Access =
  -- | En memoria, acompañada de una dirección
  InFrame Int
  -- | En un registro
  | InReg Temp
    deriving Show

-- | Definición de fragmento usado en en la traducción.
-- Son los bloques que van al assembler de formal individual.
data Frag =
  -- | Es un procedimiento (recordar que ahora todo es un procedimiento)
  -- ya que el resultado viene como un efecto lateral en el |rv|
  Proc Stm Frame
  -- | Es una cadena de caracteres literal, en general esto va en el segmento de datos del assembler.
  | AString Label [Symbol]

-- | Función que nos permite separar los procedimientos y las cadenas de caracteres.
sepFrag :: [Frag] -> ([Frag], [(Stm, Frame)])
sepFrag xs = (reverse ass, reverse stmss)
 where
  (ass, stmss) = foldl
    (\(lbls, stms) x -> case x of
      Proc st fr -> (lbls, (st, fr) : stms)
      AString{}  -> (x : lbls, stms)
    )
    ([], [])
    xs

instance Show Frag where
    show (Proc s f) = "Frame:" ++ show f ++ '\n': show s
    show (AString l ts) = show l ++ ":\n" ++ (foldr (\t ts -> ("\n\t" ++ unpack t) ++ ts) "" ts)

-- | |Frame| es lo que representa el marco de activación dinámico, es la
-- información que vamos a utilizar eventualmente para construir el marco de
-- activación real al momento de efectuar las llamadas a funciones. Que consiste en:
data Frame = Frame {
        -- | Nombre que lleva en el assembler.
        name        :: Symbol,
        -- | Argumentos, si escapan o no.
        formals     :: [Escapa],
        -- | Variables Locales , si escapan o no.
        locals      :: [Escapa],
        -- | Contadores de cantidad de argumentos, variables y registros.
        actualArg   :: Int,
        actualLocal :: Int,
        actualReg   :: Int
    }
    deriving Show
-- Nota: claramente pueden no llevar contadores y calcularlos en base a la longitud de
-- las listas |formals| y |locals|.

defaultFrame :: Frame
defaultFrame = Frame
  { name        = empty
  , formals     = []
  , locals      = []
  , actualArg   = argsInicial
  , actualLocal = localsInicial
  , actualReg   = regInicial
  }

--------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------

-- TODOS A stack por i386
prepFormals :: Frame -> [Access]
prepFormals fs = reverse $ snd
  (foldl (\(n, rs) _ -> (n + argsGap, InFrame n : rs))
         (argsInicial, [])
         (formals fs)
  )

newFrame :: Symbol -> [Escapa] -> Frame
newFrame nm fs = defaultFrame { name = nm, formals = fs }

-- | Función auxiliar que hace una llamada externa.
externalCall :: String -> [Exp] -> Exp
externalCall s = Call (Name $ pack s)

-- | A medida que vamos procesando los argumentos vamos pidiendo 'memoria' para ellos.
-- Dependiendo de la arquitectura algunos pueden ir por memoria o por stack. Salvo obviamente
-- que escapen, en ese caso tienen que ir a memoria.
allocArg :: (Monad w, TLGenerator w) => Frame -> Escapa -> w (Frame, Access)
allocArg fr Escapa =
  let actual = actualArg fr
      acc    = InFrame $ actual * wSz + argsGap
  in  return (fr { actualArg = actual + 1 }, acc)
allocArg fr NoEscapa = do
  s <- newTemp
  return (fr, InReg s)

allocLocal :: (Monad w, TLGenerator w) => Frame -> Escapa -> w (Frame, Access)
allocLocal fr Escapa =
  let actual = actualLocal fr
      acc    = InFrame $ actual * wSz + localsGap
  in  return (fr { actualLocal = actual + 1 }, acc)
allocLocal fr NoEscapa = do
  s <- newTemp
  return (fr, InReg s)

-- Función auxiliar par el calculo de acceso a una variable, siguiendo el Static Link.
-- Revisar bien antes de usarla, pero ajustando correctamente la variable |fpPrevLev|
-- debería estar relativamente cerca de la solución
auxexp :: Int -> Exp
auxexp 0 = Temp fp
auxexp n = Mem (Binop Plus (auxexp (n - 1)) (Const fpPrevLev))

exp
  :: 
  -- Acceso de la variable.
     Access
    -- Diferencia entre el nivel que se usa y donde se definió.
  -> Int
  -> Exp
exp (InFrame k) e = Mem (Binop Plus (auxexp e) (Const k))
  -- Si esta en un registro, directamente damos ese acceso. Por definición el
  -- nivel tendría que ser el mismo, sino hay un error en el calculo de escapes.
exp (InReg l) c | c == 0    = error "Megaerror en el calculo de escapes?"
                | otherwise = Temp l

procEntryExit2 :: Frame -> [TigerMunch.Instr] -> [TigerMunch.Instr]
procEntryExit2 fram bod = bod ++ [TigerMunch.OPER{ oassem = ""
                                                 , osrc   = [zero, ra, sp] ++ calleesaves
                                                 , odst   = []
                                                 , ojmp   = Just []}]
