module TigerFrame where

import           TigerTemp
import           TigerTree

import           TigerAbs                ( Escapa(..) )
import qualified TigerAsm

import           TigerSymbol

import           Prelude                 hiding ( exp )
--import           Data.Map                as Map


-- | Registros muy usados.
fp, sp, rv, hi, lo :: Temp
-- | Frame pointer
fp = pack "$fp"
-- | Stack pointer
sp = pack "$sp"
-- | Return value --TODO: ojo las runtime
rv = pack "$v0"
-- | HI registro-ro  --TODO: borrar
hi = pack "HI"
-- | LO registro-ro
lo = pack "LO"
-- | ZERO
zero = pack "$zero"
-- | Return Adress
ra = pack "$ra"

-- | primeros 4 parametros pasados a una subrutina
a0, a1, a2, a3 :: Temp
a0 = pack "$a0"
a1 = pack "$a1"
a2 = pack "$a2"
a3 = pack "$a3"

-- | callee-saves (tienen que reestablecerse al finalizar la subrutina)
s0, s1, s2, s3, s4, s5, s6, s7:: Temp
s0 = pack "$s0"
s1 = pack "$s1"
s2 = pack "$s2"
s3 = pack "$s3"
s4 = pack "$s4"
s5 = pack "$s5"
s6 = pack "$s6"
s7 = pack "$s7"
-- s8 = pack "$s8" {- NO! $s8 == $fp -}

-- | caller-saves (guardados por el llamante, libres para la subrutina)
t0, t1, t2, t3, t4, t5, t6, t7, t8, t9 :: Temp
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
--tempMap = empty :: Map.Map Temp String

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
-- RESPUESTA: es donde esta el primer arg, o sea, el static-link!
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
argsInicial = 4 -- MIPS reserva 4 lugares para los 4 argregs
regInicial = 1
localsInicial = 0

-- | Listas de registros que define la llamada y registros especiales
specialregs, argregs, calleesaves, callersaves, calldefs :: [Temp]
-- | Special Regs
specialregs = [fp, sp, rv, zero, ra, lo]
-- | Argument Regs
argregs     = [a0, a1, a2, a3]
-- | Callee Saves
calleesaves = [s0, s1, s2, s3, s4, s5, s6, s7]
-- | Caller Saves
callersaves = [t0, t1, t2, t3, t4, t5, t6, t7, t8, t9]
-- | Tipo de dato que define el acceso a variables.
calldefs = rv : ra : callersaves

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

-- | formals extrae una lista de accesos denotando la ubicación donde los parámetros
-- serán mantenidos en tiempo de ejecución, es decir, como se verán dentro del callee
-- (Appel, pág 135)
-- MIPS requiere primeros 4 a registro y el resto a stack, todos los argumentos son
-- buscados por el callee en stack en: $sp + frameSize + n
prepFormals :: Frame -> [Access]
prepFormals fr = 
  let (a,b) = splitAt 4 (formals fr)
  in (regFormals a) ++ (stackFormals b)
    where regFormals xs = map InReg $ take (Prelude.length xs) argregs
          stackFormals xs = reverse $ snd
                              (foldl (\(n,rs) _ -> (n + argsGap, InFrame n:rs))
                                     (argsInicial * argsGap, [])
                                     xs
                              )

{-
-- TODOS A stack por i386
prepFormals :: Frame -> [Access]
prepFormals fs = reverse $ snd
  (foldl (\(n, rs) _ -> (n + argsGap, InFrame n : rs))
         (argsInicial, [])
         (formals fs)
  )

prepFormals :: Frame -> [Access]
prepFormals fr = reverse $ snd
  (foldl (\(n,rs) esc -> 
            case esc of Escapa -> (n + argsGap, InFrame n:rs)
                        NoEscapa -> (n, InReg ():rs)
         )
    (argsInicial, [])
    (formals fs)
  )
-}

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
      acc    = InFrame $ actual * wSz + argsGap -- formals en sp + n
  in  return (fr { formals = formals fr ++ [Escapa], actualArg = actual + 1 }, acc)
allocArg fr NoEscapa = do
  s <- newTemp
  return (fr {formals = formals fr ++ [NoEscapa]}, InReg s)

allocLocal :: (Monad w, TLGenerator w) => Frame -> Escapa -> w (Frame, Access)
allocLocal fr Escapa =
  let actual = actualLocal fr
      acc    = InFrame $ -(actual * wSz + localsGap) -- locals en sp - n
  in  return (fr { locals = locals fr ++ [Escapa], actualLocal = actual + 1 }, acc)
allocLocal fr NoEscapa = do
  s <- newTemp
  return (fr {locals = locals fr ++ [NoEscapa]}, InReg s)

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
exp (InReg l) _ = Temp l


-- procEntryExit2 marks special registers as source to keep them live and prevent
-- the register allocator to try and use them for some other purpose
procEntryExit2 :: Frame -> [TigerAsm.Instr] -> [TigerAsm.Instr]
procEntryExit2 fram bod = bod ++ [TigerAsm.IOPER{ TigerAsm.oassem = TigerAsm.NOOP
                                                , TigerAsm.osrc   = specialregs ++ calleesaves
                                                , TigerAsm.odst   = []
                                                , TigerAsm.ojmp   = Just []}]


-- procEntryExit3 does several things, using info about the frame size:
--  1. assembly function prologue
--  2. stack pointer entry adjustment
--  3. stack pointer exit adjustment
--  4. assembly function epilogue
procEntryExit3 :: Frame -> [TigerAsm.Instr] -> [TigerAsm.Instr]
procEntryExit3 fr inss = let
    prologue = [] --TODO
    epilogue = [] --TODO
  in prologue ++ inss ++ epilogue