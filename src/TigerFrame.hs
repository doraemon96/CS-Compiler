module TigerFrame where

import           TigerTemp
import           TigerTree

import           TigerAbs                ( Escapa(..) )
import qualified TigerAsm

import           TigerSymbol

import           Prelude                 hiding ( exp )

import Debug.Trace


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
argsInicial = 1 -- MIPS reserva 4 lugares para los 4 argregs
regInicial = 1
localsInicial = 0

mipsRegArgs = 4 -- MIPS pone los primeros 4 en registros y el resto en frame

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
-- | Registros
registers = specialregs ++ argregs ++ calleesaves ++ callersaves

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
        actualReg   :: Int,
        -- | Contador del maximo nro de argumentos de fn que llama el frame actual
        maxArgs     :: Int
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
  , maxArgs     = 0
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
allocArg fr NoEscapa =
  let actual = actualArg fr
  in if (actual < mipsRegArgs)
     then do
        let s = argregs !! actual
        return (fr {formals = formals fr ++ [NoEscapa], actualArg = actual + 1}, InReg s)
     else do
        let acc = InFrame $ actual * wSz + argsGap
        return (fr { formals = formals fr ++ [Escapa], actualArg = actual + 1 }, acc)

allocLocal :: (Monad w, TLGenerator w) => Frame -> Escapa -> w (Frame, Access)
allocLocal fr Escapa =
  let actual = actualLocal fr
      acc    = InFrame $ -(actual * wSz + localsGap) -- locals en sp - n
  in  return (fr { locals = locals fr ++ [Escapa], actualLocal = actual + 1 }, acc)
allocLocal fr NoEscapa = do
  s <- newTemp
  return (fr {locals = locals fr ++ [NoEscapa]}, InReg s)

-- | Función auxiliar para luego reservar espacio para argumentos de llamada
callArgs :: Frame -> Int -> Frame
callArgs f i = f{maxArgs = max (maxArgs f) i}

-- Función auxiliar par el calculo de acceso a una variable, siguiendo el Static Link.
-- Revisar bien antes de usarla, pero ajustando correctamente la variable |fpPrevLev|
-- debería estar relativamente cerca de la solución
auxexp :: Int -> Exp
auxexp 0 = Temp fp
auxexp n | n > 0 = Mem (Binop Plus (auxexp (n - 1)) (Const fpPrevLev))
         | n < 0 = error "auxexp negative"

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


string :: Frag -> [TigerAsm.Instr]
string (AString lab syms) = [
  TigerAsm.IDIREC{
    TigerAsm.dir = TigerAsm.ALIGN 2
    },
  TigerAsm.ILABEL{
    TigerAsm.lassem = TigerAsm.LABEL lab,
    TigerAsm.llab = lab
    },
  TigerAsm.IDIREC{
    TigerAsm.dir = TigerAsm.WORD ((TigerSymbol.length $ appends syms) - 2) -- resto comillas
    },
  TigerAsm.IDIREC{
    TigerAsm.dir = TigerAsm.ASCIIZ (unpack $ appends syms)
    }
  ]
string _ = error "Not a string in string #s00"


saverestore_callee :: (Monad w, TLGenerator w) => [Temp] -> w ([Stm],[Stm])
saverestore_callee regs = do
  temps <- mapM (\_ -> newTemp) regs
  let ziptemps = zip temps [0..]
      sav = map (\(t, i) -> Move (Temp t) (Temp $ regs !! i)) ziptemps
      res = map (\(t, i) -> Move (Temp $ regs !! i) (Temp t)) ziptemps
  return (sav, res)

-- procEntryExit1 does the view-shift (places incoming arguments where they belong)
-- and implements the save and restore of callee-save registers.
--  + save "escaping" arguments (including SL) into the frame
--  + move nonescaping arguments into fresh temporaries
--  + store instructions to save any callee-save registers (including RA) used withing the fn
--  ++ fn ++
--  + load instructions to restore the callee-save registers
procEntryExit1 :: (Monad w, TLGenerator w) => Frame -> Stm -> w Stm
procEntryExit1 frame body = do
  (save,restore) <- saverestore_callee (calleesaves ++ argregs)
  --(save,restore) <- return ([],[])
  return $ sequence $ save ++ [body] ++ restore
    where sequence [st]     = st
          sequence (st:sts) = Seq st (sequence sts)


-- procEntryExit2 marks special registers as source to keep them live and prevent
-- the register allocator to try and use them for some other purpose
procEntryExit2 :: Frame -> [TigerAsm.Instr] -> [TigerAsm.Instr]
procEntryExit2 fram bod = bod ++ [TigerAsm.IOPER{ TigerAsm.oassem = TigerAsm.NOOP
                                                , TigerAsm.osrc   = specialregs -- ++ calleesaves
                                                , TigerAsm.odst   = []
                                                , TigerAsm.ojmp   = Just []}]


-- procEntryExit3 does several things, using info about the frame size:
--  1. assembly function prologue
--  2. stack pointer entry adjustment
--  3. stack pointer exit adjustment
--  4. assembly function epilogue
-- references
--  - https://www.cs.swarthmore.edu/~newhall/cs75/s09/spim_mips_intro.html
--  -https://courses.cs.washington.edu/courses/cse410/09sp/examples/MIPSCallingConventionsSummary.pdf
procEntryExit3 :: Frame -> [TigerAsm.Instr] -> [TigerAsm.Instr]
procEntryExit3 fr inss = let
    fname = unpack $ name fr
    --
    local_space = actualLocal fr
    param_space = maxArgs fr
    saves_space = 0 --NOTE: here we're not using calleesaves because of procEntryExit2... (?)
    stack_space = stack_space' + pad
    stack_space' = local_space + 2 + saves_space + param_space
    --
    pad = if (Prelude.even stack_space') then 0 else 1 --make sure its multiple of 2 (or 8??)
    ra_offset = (local_space + pad + 1) * wSz
    fp_offset = ra_offset + (1 * wSz)
    sp_offset = fp_offset + (saves_space + param_space) * wSz
    --
    flabel = [
      TigerAsm.ILABEL{
        TigerAsm.lassem = TigerAsm.LABEL (name fr),
        TigerAsm.llab = name fr
      }
      ]
    --
    directives = [
      TigerAsm.IDIREC{TigerAsm.dir = TigerAsm.TEXT}, --text section
      TigerAsm.IDIREC{TigerAsm.dir = TigerAsm.ALIGN 2}, --align to 2^2 = 4 bytes (32 bit data)
      TigerAsm.IDIREC{TigerAsm.dir = TigerAsm.GLOBL fname} --visible externally
      ]
    --
    stack_entry = [
      TigerAsm.IOPER{ --store ra in stack
        TigerAsm.oassem = TigerAsm.SW ra (-ra_offset) sp,
        TigerAsm.odst = [],
        TigerAsm.osrc = [],
        TigerAsm.ojmp = Nothing
      },
      TigerAsm.IOPER{ --store fp in stack
        TigerAsm.oassem = TigerAsm.SW fp (-fp_offset) sp,
        TigerAsm.odst = [],
        TigerAsm.osrc = [],
        TigerAsm.ojmp = Nothing
      },
      TigerAsm.IMOVE{ --push frame pointer
        TigerAsm.massem = TigerAsm.MOVE fp sp,
        TigerAsm.mdst = fp,
        TigerAsm.msrc = sp
      },
      TigerAsm.IOPER{ --push stack pointer
        TigerAsm.oassem = TigerAsm.ADDI sp sp (-sp_offset),
        TigerAsm.odst = [sp],
        TigerAsm.osrc = [sp],
        TigerAsm.ojmp = Nothing
      }
      ]
    --
    stack_exit = [
      TigerAsm.IMOVE{ --pop stack pointer
        TigerAsm.massem = TigerAsm.MOVE sp fp,
        TigerAsm.mdst = sp,
        TigerAsm.msrc = fp
      },
      TigerAsm.IOPER{ --pop frame pointer
        TigerAsm.oassem = TigerAsm.LW fp (-fp_offset) sp,
        TigerAsm.odst = [],
        TigerAsm.osrc = [],
        TigerAsm.ojmp = Nothing
      },
      TigerAsm.IOPER{ --fetch ra in stack
        TigerAsm.oassem = TigerAsm.LW ra (-ra_offset) sp,
        TigerAsm.odst = [],
        TigerAsm.osrc = [],
        TigerAsm.ojmp = Nothing
      }
      ]
    --
    return = [
      TigerAsm.IOPER{
        TigerAsm.oassem = TigerAsm.J ra,
        TigerAsm.odst = [],
        TigerAsm.osrc = [ra],
        TigerAsm.ojmp = Just []
      },
      TigerAsm.IOPER{
        TigerAsm.oassem = TigerAsm.NOOP,
        TigerAsm.odst = [],
        TigerAsm.osrc = [],
        TigerAsm.ojmp = Nothing
      }
      ]
    --
    prologue = directives ++ flabel ++ stack_entry --label after directives?
    epilogue = stack_exit ++ return
  in traceShow ("SS",param_space) $ prologue ++ inss ++ epilogue