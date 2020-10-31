{-# LANGUAGE GADTs #-}
module TigerAsm where

import           Data.Text
import qualified Data.Map as Map

import           TigerSymbol
import           TigerTemp (Temp, Label)

--TODO: Change assem to corresponding types (lassem is directive?)
data Instr = IOPER { oassem :: MIPSAsm
                  , odst   :: [Temp] -- kill, use
                  , osrc   :: [Temp] -- gen, def
                  , ojmp   :: Maybe [Label] }
           | IMOVE { massem :: MIPSAsm
                  , mdst   :: Temp
                  , msrc   :: Temp }
           | ILABEL { lassem :: MIPSAsm
                   , llab   :: Label }
           | IDIREC { dir :: MIPSDir}
    deriving (Show, Eq, Ord)

-- | MIPSAsm represents MIPS assembly instructions in a way that Haskell can
-- manipulate it (so that replacing labels and temps is easier).
-- This data structure may use labels, temps & ints (and nothing else).
data MIPSAsm = 
        -- aritmetic
          ADD Temp Temp Temp | ADDI Temp Temp Int 
        | ADDIU Temp Temp Int | ADDU Temp Temp Temp
        | SUB Temp Temp Temp | SUBU Temp Temp Temp
        | MULT Temp Temp | MULTU Temp Temp
        | DIV Temp Temp | DIVU Temp Temp
        -- conditional
        | BEQ Temp Temp Label | BNE Temp Temp Label
        | BGTZ Temp Label | BGEZ Temp Label 
        | BLTZ Temp Label | BLEZ Temp Label
        | SLT Temp Temp Temp | SLTU Temp Temp Temp
        -- jump
        | J Label | JAL Label | JR Temp
        -- shifts TODO
        | SLL Temp Temp Int | SRL Temp Temp Int
        | SLLV Temp Temp Temp | SRLV Temp Temp Temp
        -- bitwise TODO
        | AND Temp Temp Temp | OR Temp Temp Temp 
        -- memory
        | LW Temp Int Temp | SW Temp Int Temp
        | LI Temp Int | LA Temp Label
        | MFHI Temp | MFLO Temp
        | MOVE Temp Temp
        -- other
        | LABEL Label | NOOP
  deriving (Show, Eq, Ord)

-- | MIPSDir represents MIPS assembly directives.
data MIPSDir =
          ALIGN Int 
        | ASCII String | ASCIIZ String
        | DATA | DATAA Int
        | TEXT | TEXTA Int
        | WORD Int
        | GLOBL String
  deriving (Show, Eq, Ord)

-- | MipsCom represents MIPS assembly comments. (ToDo)
data MIPSCom = COMMENT String
  deriving Show

{-
instance Show MIPSAsm where
    show (ADD t1 t2 t3) = ws ["add", cm [show t1, show t2, show t3]]
    show (ADDI t1 t2 i) = ws ["addi", cm [show t1, show t2, show i]]
    show (ADDIU t1 t2 i) = ws ["addiu", cm [show t1, show t2, show i]]
    show (ADDU t1 t2 t3) = ws ["addu", cm [show t1, show t2, show t3]]
    show (SUB t1 t2 t3) = ws ["sub", cm [show t1, show t2, show t3]]
    show (SUBU t1 t2 t3) = ws ["subu", cm [show t1, show t2, show t3]]
    show (MULT t1 t2) = ws ["mult", cm [show t1, show t2]]
    show (MULTU t1 t2) = ws ["multu", cm [show t1, show t2]]
    show (DIV t1 t2) = ws ["div", cm [show t1, show t2]]
    show (DIVU t1 t2) = ws ["divu", cm [show t1, show t2]]
    show (BEQ t1 t2 l) = ws ["beq", cm [show t1, show t2, show l]]
    show (BNE t1 t2 l) = ws ["bne", cm [show t1, show t2, show l]]
    show (BGTZ t1 l) = ws ["bgtz", cm [show t1, show l]]
    show (BGEZ t1 l) = ws ["bgez", cm [show t1, show l]]
    show (BLTZ t1 l) = ws ["bltz", cm [show t1, show l]]
    show (BLEZ t1 l) = ws ["blez", cm [show t1, show l]]
    show (SLT t1 t2 t3) = ws ["slt", cm [show t1, show t2, show t3]]
    show (SLTU t1 t2 t3) = ws ["sltu", cm [show t1, show t2, show t3]]
    show (J l) = ws ["j", show l]
    show (JAL l) = ws ["jal", show l]
    show (JR l) = ws ["jr", show l]
    show (SLL t1 t2 i) = ws ["sll", cm [show t1, show t2, show i]]
    show (SRL t1 t2 i) = ws ["srl", cm [show t1, show t2, show i]]
    show (SLLV t1 t2 t3) = ws ["sllv", cm [show t1, show t2, show t3]]
    show (SRLV t1 t2 t3) = ws ["srlv", cm [show t1, show t2, show t3]]
    show (AND t1 t2 t3) = ws ["and", cm [show t1, show t2, show t3]]
    show (OR t1 t2 t3) = ws ["or", cm [show t1, show t2, show t3]]
    show (LW t1 i t2) = ws ["lw", cm [show t1, os (show i) (show t2)]]
    show (SW t1 i t2) = ws ["sw", cm [show t1, os (show i) (show t2)]]
    show (LI t1 i) = ws ["li", cm [show t1, show i]]
    show (LA t1 l1) = ws ["la", cm [show t1, show l1]]
    show (MFHI t) = ws ["mfhi", show t]
    show (MFLO t) = ws ["mflo", show t]
    show (MOVE t1 t2) = ws ["move", cm [show t1, show t2]]
    show (LABEL l) = show l ++ ":"
    show (NOOP) = "noop"
-}

format :: (Temp -> Text) -> Instr -> Text
format f ins@(IOPER{}) = format' f (oassem ins)
format f ins@(IMOVE{}) = format' f (massem ins)
format f ins@(ILABEL{}) = format' f (lassem ins)
format f ins@(IDIREC{}) = formatdir (dir ins)


format' :: (Temp -> Text) -> MIPSAsm -> Text
format' f (ADD t1 t2 t3) = ws [pack "add", cm (Prelude.map f [t1, t2, t3])]
format' f (ADDI t1 t2 i) = ws [pack "addi", cm ((Prelude.map f [t1,  t2]) ++ [pack $ show i])]
format' f (ADDIU t1 t2 i) = ws [pack "addiu", cm ((Prelude.map f [t1,  t2]) ++ [pack $ show i])]
format' f (ADDU t1 t2 t3) = ws [pack "addu", cm (Prelude.map f [t1,  t2,  t3])]
format' f (SUB t1 t2 t3) = ws [pack "sub", cm (Prelude.map f [t1,  t2,  t3])]
format' f (SUBU t1 t2 t3) = ws [pack "subu", cm (Prelude.map f [t1,  t2,  t3])]
format' f (MULT t1 t2) = ws [pack "mult", cm (Prelude.map f [t1,  t2])]
format' f (MULTU t1 t2) = ws [pack "multu", cm (Prelude.map f [t1,  t2])]
format' f (DIV t1 t2) = ws [pack "div", cm (Prelude.map f [t1,  t2])]
format' f (DIVU t1 t2) = ws [pack "divu", cm (Prelude.map f [t1,  t2])]
format' f (BEQ t1 t2 l) = ws [pack "beq", cm ((Prelude.map f [t1,  t2]) ++ [l])]
format' f (BNE t1 t2 l) = ws [pack "bne", cm ((Prelude.map f [t1,  t2]) ++ [l])]
format' f (BGTZ t1 l) = ws [pack "bgtz", cm ([f t1, l])]
format' f (BGEZ t1 l) = ws [pack "bgez", cm ([f t1, l])]
format' f (BLTZ t1 l) = ws [pack "bltz", cm ([f t1, l])]
format' f (BLEZ t1 l) = ws [pack "blez", cm ([f t1, l])]
format' f (SLT t1 t2 t3) = ws [pack "slt", cm (Prelude.map f [t1,  t2,  t3])]
format' f (SLTU t1 t2 t3) = ws [pack "sltu", cm (Prelude.map f [t1,  t2,  t3])]
format' f (J l) = ws [pack "j", l]
format' f (JAL l) = ws [pack "jal", l]
format' f (JR l) = ws [pack "jr", l]
format' f (SLL t1 t2 i) = ws [pack "sll", cm ((Prelude.map f [t1,  t2]) ++ [pack $ show i])]
format' f (SRL t1 t2 i) = ws [pack "srl", cm ((Prelude.map f [t1,  t2]) ++ [pack $ show i])]
format' f (SLLV t1 t2 t3) = ws [pack "sllv", cm (Prelude.map f [t1,  t2,  t3])]
format' f (SRLV t1 t2 t3) = ws [pack "srlv", cm (Prelude.map f [t1,  t2,  t3])]
format' f (AND t1 t2 t3) = ws [pack "and", cm (Prelude.map f [t1,  t2,  t3])]
format' f (OR t1 t2 t3) = ws [pack "or", cm (Prelude.map f [t1,  t2,  t3])]
format' f (LW t1 i t2) = ws [pack "lw", cm ([f t1, os i (f t2)])]
format' f (SW t1 i t2) = ws [pack "sw", cm ([f t1, os i (f t2)])]
format' f (LI t1 i) = ws [pack "li", cm ([f t1, pack $ show i])]
format' f (LA t1 l1) = ws [pack "la", cm (Prelude.map f [t1,  l1])]
format' f (MFHI t) = ws [pack "mfhi", f t]
format' f (MFLO t) = ws [pack "mflo", f t]
format' f (MOVE t1 t2) = ws [pack "move", cm (Prelude.map f [t1,  t2])]
format' f (LABEL l) =  appends [l, pack ":"]
format' f (NOOP) = pack "nop"


-- add whitespace between args
ws :: [Text] -> Text
ws (x:[])   = x
ws (x:y:ys) = appends [x , pack " " , (ws (y:ys))]
-- add commas between args
cm :: [Text] -> Text
cm (x:[])   = x
cm (x:y:ys) = appends [x, (pack ","), (cm (y:ys))]
-- add offset with parenthesis
os :: Int -> Text -> Text
os offset temp = appends [(pack $ show offset), (pack "("), temp, (pack ")")]

-- format directive
formatdir :: MIPSDir -> Text
formatdir (ALIGN i) = ws [pack ".align", pack $ show i]
formatdir (ASCII str) = ws [pack ".ascii", pack str]
formatdir (ASCIIZ str) = ws [pack ".asciiz", pack str]
formatdir (DATA) = ws [pack ".data"]
formatdir (DATAA i) = ws [pack ".data", pack $ show i]
formatdir (TEXT) = ws [pack ".text"]
formatdir (TEXTA i) = ws [pack ".texta", pack $ show i]
formatdir (WORD i) = ws [pack ".word", pack $ show i]
formatdir (GLOBL nam) = ws [pack ".globl", pack nam]

--instance Show MIPSDir where

--instance Show MIPSCom where
--  show (COMMENT s) = "# " ++ s

replaceInstr :: Map.Map Temp Temp -> Instr -> Instr
replaceInstr m (IOPER oassem odst osrc ojmp) = IOPER (replaceAssem m oassem) odst osrc ojmp
replaceInstr m (IMOVE massem mdst msrc) = IMOVE (replaceAssem m massem) mdst msrc
replaceInstr m other = other

replaceAssem :: Map.Map Temp Temp -> MIPSAsm -> MIPSAsm
replaceAssem = replaceAssem' getme1

replaceAssemMissing :: Map.Map Temp Temp -> MIPSAsm -> MIPSAsm
replaceAssemMissing = replaceAssem' getme2

replaceAssem' :: (Map.Map Temp Temp -> Temp -> Temp) -> Map.Map Temp Temp -> MIPSAsm -> MIPSAsm
replaceAssem' getme m (ADD t1 t2 t3) = ADD (m `getme` t1) (m `getme` t2) (m `getme` t3)
replaceAssem' getme m (ADDI t1 t2 i) = ADDI (m `getme` t1) (m `getme` t2) i
replaceAssem' getme m (ADDIU t1 t2 i) = ADDIU (m `getme` t1) (m `getme` t2) i
replaceAssem' getme m (ADDU t1 t2 t3) = ADDU (m `getme` t1) (m `getme` t2) (m `getme` t3)
replaceAssem' getme m (SUB t1 t2 t3) = SUB (m `getme` t1) (m `getme` t2) (m `getme` t3)
replaceAssem' getme m (SUBU t1 t2 t3) = SUBU (m `getme` t1) (m `getme` t2) (m `getme` t3)
replaceAssem' getme m (MULT t1 t2) = MULT (m `getme` t1) (m `getme` t2)
replaceAssem' getme m (MULTU t1 t2) = MULTU (m `getme` t1) (m `getme` t2)
replaceAssem' getme m (DIV t1 t2) = DIV (m `getme` t1) (m `getme` t2)
replaceAssem' getme m (DIVU t1 t2) = DIVU (m `getme` t1) (m `getme` t2)
replaceAssem' getme m (BEQ t1 t2 l) = BEQ (m `getme` t1) (m `getme` t2) l
replaceAssem' getme m (BNE t1 t2 l) = BNE (m `getme` t1) (m `getme` t2) l
replaceAssem' getme m (BGTZ t1 l) = BGTZ (m `getme` t1) l
replaceAssem' getme m (BGEZ t1 l) = BGEZ (m `getme` t1) l
replaceAssem' getme m (BLTZ t1 l) = BLTZ (m `getme` t1) l
replaceAssem' getme m (BLEZ t1 l) = BLEZ (m `getme` t1) l
replaceAssem' getme m (SLT t1 t2 t3) = SLT (m `getme` t1) (m `getme` t2) (m `getme` t3)
replaceAssem' getme m (SLTU t1 t2 t3) = SLTU (m `getme` t1) (m `getme` t2) (m `getme` t3)
replaceAssem' getme m (J l) = J l
replaceAssem' getme m (JAL l) = JAL l
replaceAssem' getme m (JR l) = JR l
replaceAssem' getme m (SLL t1 t2 i) = SLL (m `getme` t1) (m `getme` t2) i
replaceAssem' getme m (SRL t1 t2 i) = SRL (m `getme` t1) (m `getme` t2) i
replaceAssem' getme m (SLLV t1 t2 t3) = SLLV (m `getme` t1) (m `getme` t2) (m `getme` t3)
replaceAssem' getme m (SRLV t1 t2 t3) = SRLV (m `getme` t1) (m `getme` t2) (m `getme` t3)
replaceAssem' getme m (AND t1 t2 t3) = AND (m `getme` t1) (m `getme` t2) (m `getme` t3)
replaceAssem' getme m (OR t1 t2 t3) = OR (m `getme` t1) (m `getme` t2) (m `getme` t3)
replaceAssem' getme m (LW t1 i t2) = LW (m `getme` t1) i (m `getme` t2)
replaceAssem' getme m (SW t1 i t2) = SW (m `getme` t1) i (m `getme` t2)
replaceAssem' getme m (LI t1 i) = LI (m `getme` t1) i
replaceAssem' getme m (LA t1 l1) = LA (m `getme` t1) l1
replaceAssem' getme m (MFHI t) = MFHI (m `getme` t)
replaceAssem' getme m (MFLO t) = MFLO (m `getme` t)
replaceAssem' getme m (MOVE t1 t2) = MOVE (m `getme` t1) (m `getme` t2)
replaceAssem' getme m (LABEL l) = LABEL l
replaceAssem' getme m (NOOP) = NOOP

getme1 :: Map.Map Temp Temp -> Temp -> Temp
getme1 m t = maybe (error "No color for temp!") (id) (Map.lookup t m)

getme2 :: Map.Map Temp Temp -> Temp -> Temp
getme2 m t = maybe (t) (id) (Map.lookup t m)