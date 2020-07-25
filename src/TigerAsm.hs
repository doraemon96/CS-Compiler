{-# LANGUAGE GADTs #-}
module TigerAsm where

import TigerTemp (Temp, Label)

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
    deriving (Show) --Eq

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

-- | MIPSDir represents MIPS assembly directives.
data MIPSDir =
          ALIGN Int 
        | ASCII String | ASCIIZ String
        | DATA | DATAA Int
        | TEXT | TEXTA Int
        | WORD [Int]

-- | MipsCom represents MIPS assembly comments. (ToDo)
data MIPSCom = COMMENT String


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

format :: (Temp -> String) -> Instr -> String
format f ins@(IOPER{}) = format' f (oassem ins)
format f ins@(IMOVE{}) = format' f (massem ins)
format f ins@(ILABEL{}) = format' f (lassem ins)


format' :: (Temp -> String) -> MIPSAsm -> String
format' f (ADD t1 t2 t3) = ws ["add", cm (map f [t1, t2, t3])]
format' f (ADDI t1 t2 i) = ws ["addi", cm ((map f [t1,  t2]) ++ [show i])]
format' f (ADDIU t1 t2 i) = ws ["addiu", cm ((map f [t1,  t2]) ++ [show i])]
format' f (ADDU t1 t2 t3) = ws ["addu", cm (map f [t1,  t2,  t3])]
format' f (SUB t1 t2 t3) = ws ["sub", cm (map f [t1,  t2,  t3])]
format' f (SUBU t1 t2 t3) = ws ["subu", cm (map f [t1,  t2,  t3])]
format' f (MULT t1 t2) = ws ["mult", cm (map f [t1,  t2])]
format' f (MULTU t1 t2) = ws ["multu", cm (map f [t1,  t2])]
format' f (DIV t1 t2) = ws ["div", cm (map f [t1,  t2])]
format' f (DIVU t1 t2) = ws ["divu", cm (map f [t1,  t2])]
format' f (BEQ t1 t2 l) = ws ["beq", cm ((map f [t1,  t2]) ++ [show l])]
format' f (BNE t1 t2 l) = ws ["bne", cm ((map f [t1,  t2]) ++ [show l])]
format' f (BGTZ t1 l) = ws ["bgtz", cm ([f t1, show l])]
format' f (BGEZ t1 l) = ws ["bgez", cm ([f t1, show l])]
format' f (BLTZ t1 l) = ws ["bltz", cm ([f t1, show l])]
format' f (BLEZ t1 l) = ws ["blez", cm ([f t1, show l])]
format' f (SLT t1 t2 t3) = ws ["slt", cm (map f [t1,  t2,  t3])]
format' f (SLTU t1 t2 t3) = ws ["sltu", cm (map f [t1,  t2,  t3])]
format' f (J l) = ws ["j", show l]
format' f (JAL l) = ws ["jal", show l]
format' f (JR l) = ws ["jr", show l]
format' f (SLL t1 t2 i) = ws ["sll", cm ((map f [t1,  t2]) ++ [show i])]
format' f (SRL t1 t2 i) = ws ["srl", cm ((map f [t1,  t2]) ++ [show i])]
format' f (SLLV t1 t2 t3) = ws ["sllv", cm (map f [t1,  t2,  t3])]
format' f (SRLV t1 t2 t3) = ws ["srlv", cm (map f [t1,  t2,  t3])]
format' f (AND t1 t2 t3) = ws ["and", cm (map f [t1,  t2,  t3])]
format' f (OR t1 t2 t3) = ws ["or", cm (map f [t1,  t2,  t3])]
format' f (LW t1 i t2) = ws ["lw", cm ([f t1, os (show i) (f t2)])]
format' f (SW t1 i t2) = ws ["sw", cm ([f t1, os (show i) (f t2)])]
format' f (LI t1 i) = ws ["li", cm ([f t1, show i])]
format' f (LA t1 l1) = ws ["la", cm (map f [t1,  l1])]
format' f (MFHI t) = ws ["mfhi", f t]
format' f (MFLO t) = ws ["mflo", f t]
format' f (MOVE t1 t2) = ws ["move", cm (map f [t1,  t2])]
format' f (LABEL l) =  (show l) ++ ":"
format' f (NOOP) = "noop"


-- add whitespace between args
ws :: [String] -> String
ws (x:[])   = x
ws (x:y:ys) = x ++ " " ++ (ws (y:ys))
-- add commas between args
cm :: [String] -> String
cm (x:[])   = x
cm (x:y:ys) = x ++ "," ++ (cm (y:ys))
-- add offset with parenthesis
os :: String -> String -> String
os offset temp = offset ++ "(" ++ temp ++ ")"


--instance Show MIPSDir where

instance Show MIPSCom where
    show (COMMENT s) = "# " ++ s


replaceMIPS :: (Temp -> String) -> MIPSAsm -> MIPSAsm
replaceMIPS = undefined