module TigerMunch where

import qualified TigerTemp          as Temp
import qualified TigerFrame         as Frame
import TigerTree
import TigerUnique

import Control.Monad.Trans.State.Lazy
import qualified Data.Text          as T
import qualified Data.List.Utils    as LU

data Instr = OPER { oassem :: String
                  , odst   :: [Temp.Temp] -- kill, use
                  , osrc   :: [Temp.Temp] -- gen, def
                  , ojmp   :: Maybe [Temp.Label] } 
           | MOVE { massem :: String
                  , mdst   :: Temp.Temp
                  , msrc   :: Temp.Temp } 
           | LABEL { lassem :: String
                   , llab   :: Temp.Label } deriving (Show, Eq)

type Monadita = StateT [Instr] StGen

class (Monad w, Temp.TLGenerator w) => Emitter w where
    emit   :: Instr -> w ()
    result :: (Temp.Temp -> w ()) -> w Temp.Temp

instance Emitter Monadita where
    emit i = do st <- get
                put $ i:st
    result gen = do t <- Temp.newTemp
                    gen t
                    return t

format :: (Temp.Temp -> String) -> Instr -> String
format f ins = case ins of
                    OPER{}  -> let seps = LU.split "%" $ oassem ins
                                in replace f seps (osrc ins) (odst ins)
                    MOVE{}  -> let seps = LU.split "%" $ massem ins
                                in replace f seps [msrc ins] [mdst ins]
                    LABEL{} -> lassem ins

-- replace :: SeparatedString -> Source -> Destination -> Result
replace :: (Temp.Temp -> String) -> [String] -> [Temp.Temp] -> [Temp.Temp] -> String
replace _ []       _    _    = ""
replace f (s:strs) srcs dsts = (s ++ (replace' f strs srcs dsts))

replace' _ []       _    _    = ""
replace' f (s:strs) srcs dsts | head s == 's' = (f (srcs !! (read [head (tail s)] :: Int)) ++ (tail (tail s))) ++ replace' f strs srcs dsts
                              | head s == 'd' = (f (dsts !! (read [head (tail s)] :: Int)) ++ (tail (tail s))) ++ replace' f strs srcs dsts
                              | otherwise = undefined


-- CODEGEN
-- TODO: Para que usamos el Frame aca?
codeGen :: Frame.Frame -> Stm -> [Instr]
codeGen frame stm = reverse $ snd $ fst $ TigerUnique.evalState (runStateT (munchStm stm :: Monadita ()) []) 0

-- MUNCH STM
munchStm :: (Emitter w) => Stm -> w ()
munchStm (Move (Mem (Binop Plus e1 (Const i))) e2) = do me1 <- munchExp e1
                                                        me2 <- munchExp e2
                                                        emit $ OPER{ oassem = "sw %s1, " ++ show i ++ "(%s0)\n" -- STORE M[%s0 + i] <- %s1
                                                                   , osrc   = [me1, me2]
                                                                   , odst   = []
                                                                   , ojmp   = Nothing}
munchStm (Move (Mem (Binop Plus (Const i) e1)) e2) = do me1 <- munchExp e1
                                                        me2 <- munchExp e2
                                                        emit $ OPER{ oassem = "sw %s1, " ++ show i ++ "(%s0)\n" -- STORE M[i + %s0] <- %s1
                                                                   , osrc   = [me1, me2]
                                                                   , odst   = []
                                                                   , ojmp   = Nothing}
munchStm (Move (Mem (Binop Minus e1 (Const i))) e2) = do me1 <- munchExp e1
                                                         me2 <- munchExp e2
                                                         emit $ OPER{ oassem = "sw %s1, -" ++ show i ++ "(%s0)\n" -- STORE M[%s0 - i] <- %s1
                                                                    , osrc   = [me1, me2]
                                                                    , odst   = []
                                                                    , ojmp   = Nothing}
-- RESTA NO CONMUTA: munchStm (Move (Mem (Binop Minus (Const i) e1)) e2)
munchStm (Move (Mem e1) (Mem e2)) = do me1 <- munchExp e1
                                       me2 <- munchExp e2
                                       t <- Temp.newTemp
                                       emit $ OPER{ oassem = "lw %d0, (%s1)\nsw %d0, (%s0)\n" -- MOVE M[%s0] <- M[%s1]
                                                  , osrc   = [me1, me2]
                                                  , odst   = [t] --TODO: preguntar si esta bien que haya un dst aca
                                                  , ojmp   = Nothing}
munchStm (Move (Mem (Const i)) e2) = do me2 <- munchExp e2
                                        emit $ OPER{ oassem = "sw %s0, " ++ show i ++ "($zero)\n" -- STORE M[%r0 + i] <- %s0
                                                   , osrc   = [me2]
                                                   , odst   = []
                                                   , ojmp   = Nothing}
munchStm (Move (Mem e1) e2) = do me1 <- munchExp e1
                                 me2 <- munchExp e2
                                 emit $ OPER{ oassem = "sw %s1, (%s0)\n" -- STORE M[%s0] <- %s1
                                            , osrc   = [me1, me2]
                                            , odst   = [] 
                                            , ojmp   = Nothing}
munchStm (Move (Temp i1) (Temp i2)) = do emit $ MOVE{ massem = "move %d0, %s0\n" -- MOVE %d0 <- %s0
                                                    , msrc   = i2
                                                    , mdst   = i1} 
munchStm (Move (Temp i) e2) = do me2 <- munchExp e2
                                 emit $ OPER{ oassem = "add %d0, %s0, $zero\n" -- ADD %d0 <- %s0 + %r0
                                            , osrc   = [me2]
                                            , odst   = [i] 
                                            , ojmp   = Nothing}
-- TODO: ambos exp reducen a temps, se puede reducir? (es decir, le ponemos MOVE o OPER?)
munchStm (Move e1 e2) = do me1 <- munchExp e1
                           me2 <- munchExp e2
                           emit $ OPER{ oassem = "move %d0, %s0\n" -- MOVE %d0 <- %s0
                                      , osrc   = [me2]
                                      , odst   = [me1]
                                      , ojmp   = Nothing}
munchStm (ExpS e1) = do munchExp e1 --TODO: testear que modifica el state
                        return ()
munchStm (Jump (Name lab1) lab2) | lab1 == lab2 = do
                                        emit $ OPER{ oassem = "j " ++ (T.unpack lab1) ++ "\n"
                                                   , osrc   = []
                                                   , odst   = []
                                                   , ojmp   = Just [lab2]}
                                        emit $ OPER{ oassem = "nop\n"
                                                   , osrc   = []
                                                   , odst   = []
                                                   , ojmp   = Nothing}
munchStm (Jump e1 lab) = undefined --TODO: preguntar
-- Sabemos que lab2 (porque viene de Canon) es 
munchStm (CJump rop e1 e2 lab1 lab2) = do me1 <- munchExp e1
                                          me2 <- munchExp e2
                                          case rop of
                                                TigerTree.EQ -> emit $ OPER{ oassem = "beq %s0, %s1, " ++ (T.unpack lab1) ++ "\n" -- EQ %s0, %s1, lab1
                                                                           , osrc   = [me1, me2]
                                                                           , odst   = []
                                                                           , ojmp   = Just [lab1, lab2]}
                                                NE -> emit $ OPER{ oassem = "bne %s0, %s1, " ++ (T.unpack lab1) ++ "\n" -- NE %s0, %s1, lab1
                                                                 , osrc   = [me1, me2]
                                                                 , odst   = []
                                                                 , ojmp   = Just [lab1, lab2]}
                                                TigerTree.LT -> do t <- Temp.newTemp  
                                                                   emit $ OPER{ oassem = "slt %d0, %s0, %s1\nbne %d0, $zero, " ++ (T.unpack lab1) ++ "\n" -- LT %s0, %s1, lab1
                                                                              , osrc   = [me1, me2]
                                                                              , odst   = [t] --TODO: consultar, es intermedio, se pone en dst?
                                                                              , ojmp   = Just [lab1, lab2]}
                                                TigerTree.GT -> do t <- Temp.newTemp  
                                                                   emit $ OPER{ oassem = "slt %d0, %s1, %s0\nbne %d0, $zero, " ++ (T.unpack lab1) ++ "\n" -- GT %s0, %s1, lab1
                                                                              , osrc   = [me1, me2]
                                                                              , odst   = [t] --TODO: consultar, es intermedio, se pone en dst?
                                                                              , ojmp   = Just [lab1, lab2]}
                                                LE -> do t <- Temp.newTemp  
                                                         emit $ OPER{ oassem = "beq %s0, %s1, " ++ (T.unpack lab1) ++ "\nslt %d0, %s0, %s1\nbne %d0, $zero, " ++ (T.unpack lab1) ++ "\n" -- LE %s0, %s1, lab1
                                                                    , osrc   = [me1, me2]
                                                                    , odst   = [t] --TODO: consultar, es intermedio, se pone en dst?
                                                                    , ojmp   = Just [lab1, lab2]}
                                                GE -> do t <- Temp.newTemp  
                                                         emit $ OPER{ oassem = "beq %s0, %s1, " ++ (T.unpack lab1) ++ "\nslt %d0, %s1, %s0\nbne %d0, $zero, " ++ (T.unpack lab1) ++ "\n" -- GE %s0, %s1, lab1
                                                                    , osrc   = [me1, me2]
                                                                    , odst   = [t] --TODO: consultar, es intermedio, se pone en dst?
                                                                    , ojmp   = Just [lab1, lab2]}
                                                ULT -> do t <- Temp.newTemp  
                                                          emit $ OPER{ oassem = "sltu %d0, %s0, %s1\nbne %d0, $zero, " ++ (T.unpack lab1) ++ "\n" -- ULT %s0, %s1, lab1
                                                                     , osrc   = [me1, me2]
                                                                     , odst   = [t] --TODO: consultar, es intermedio, se pone en dst?
                                                                     , ojmp   = Just [lab1, lab2]}
                                                UGT -> do t <- Temp.newTemp  
                                                          emit $ OPER{ oassem = "sltu %d0, %s1, %s0\nbne %d0, $zero, " ++ (T.unpack lab1) ++ "\n" -- UGT %s0, %s1, lab1
                                                                     , osrc   = [me1, me2]
                                                                     , odst   = [t] --TODO: consultar, es intermedio, se pone en dst?
                                                                     , ojmp   = Just [lab1, lab2]}
                                                ULE -> do t <- Temp.newTemp  
                                                          emit $ OPER{ oassem = "beq %s0, %s1, " ++ (T.unpack lab1) ++ "\nsltu %d0, %s0, %s1\nbne %d0, $zero, " ++ (T.unpack lab1) ++ "\n" -- ULE %s0, %s1, lab1
                                                                     , osrc   = [me1, me2]
                                                                     , odst   = [t] --TODO: consultar, es intermedio, se pone en dst?
                                                                     , ojmp   = Just [lab1, lab2]}
                                                UGE -> do t <- Temp.newTemp  
                                                          emit $ OPER{ oassem = "beq %s0, %s1, " ++ (T.unpack lab1) ++ "\nsltu %d0, %s1, %s0\nbne %d0, $zero, " ++ (T.unpack lab1) ++ "\n" -- UGE %s0, %s1, lab1
                                                                     , osrc   = [me1, me2]
                                                                     , odst   = [t] --TODO: consultar, es intermedio, se pone en dst?
                                                                     , ojmp   = Just [lab1, lab2]}
munchStm (Seq s1 s2) = munchStm s1 >> munchStm s2
munchStm (Label lab) = emit $ LABEL{ lassem = (T.unpack lab) ++ ":\n" -- lab :
                                   , llab = lab}


-- MUNCH EXP
munchExp :: (Emitter w) => Exp -> w Temp.Temp
munchExp (Const i) = result (\t -> emit $ OPER{ oassem = "addi %d0, $zero, " ++ show i ++ "\n" -- ADDI %d0 <- 0 + i
                                              , osrc   = []
                                              , odst   = [t]
                                              , ojmp   = Nothing})
munchExp (Name l) = result (\t -> emit $ LABEL{ lassem = T.unpack l
                                              , llab   = l}) --TODO: consultar si debe tener definicion
munchExp (Temp t) = return t
munchExp (Binop Plus e1 (Const i)) = do me1 <- munchExp e1
                                        result (\t -> emit $ OPER{ oassem = "addi %d0, %s0, " ++ show i ++ "\n" -- ADDI %d0 <- %s0 + i
                                                                 , osrc   = [me1]
                                                                 , odst   = [t]
                                                                 , ojmp   = Nothing})
munchExp (Binop Plus (Const i) e1) = do me1 <- munchExp e1
                                        result (\t -> emit $ OPER{ oassem = "addi %d0, " ++ show i ++ ", %s0\n" -- ADDI %d0 <- i + %s0
                                                                 , osrc   = [me1]
                                                                 , odst   = [t]
                                                                 , ojmp   = Nothing})
munchExp (Binop Plus e1 e2) = do me1 <- munchExp e1
                                 me2 <- munchExp e2
                                 result (\t -> emit $ OPER{ oassem = "add %d0, %s0, %s1\n" -- ADD %d0 <- %s0 + %s1
                                                          , osrc   = [me1, me2]
                                                          , odst   = [t]
                                                          , ojmp   = Nothing})
munchExp (Binop Minus e1 e2) = do me1 <- munchExp e1
                                  me2 <- munchExp e2
                                  result (\t -> emit $ OPER{ oassem = "sub %d0, %s0, %s1\n" -- SUB %d0 <- %s0 - %s1
                                                           , osrc   = [me1, me2]
                                                           , odst   = [t]
                                                           , ojmp   = Nothing})
munchExp (Binop Mul e1 e2) = do me1 <- munchExp e1
                                me2 <- munchExp e2
                                result (\t -> emit $ OPER{ oassem = "mul %d0, %s0, %s1\n" -- MUL $d0 <- %s0 * %s1
                                                         , osrc   = [me1, me2]
                                                         , odst   = [t]
                                                         , ojmp   = Nothing})
munchExp (Binop Div e1 e2) = do me1 <- munchExp e1
                                me2 <- munchExp e2
                                result (\t -> emit $ OPER{ oassem = "div %s0, %s1\nmflo %d0\n" -- DIV %d0 <- %s0 / %s1
                                                         , osrc   = [me1, me2]
                                                         , odst   = [t]
                                                         , ojmp   = Nothing})
munchExp (Binop And e1 e2) = do me1 <- munchExp e1
                                me2 <- munchExp e2
                                result (\t -> emit $ OPER{ oassem = "and %d0, %s0, %s1\n" -- AND %d0 <- %s0 & %s1
                                                         , osrc   = [me1, me2]
                                                         , odst   = [t]
                                                         , ojmp   = Nothing})
munchExp (Binop Or e1 e2) = do me1 <- munchExp e1
                               me2 <- munchExp e2
                               result (\t -> emit $ OPER{ oassem = "or %d0, %s0, %s1\n" -- OR %d0 <- %s0 | %s1
                                                        , osrc   = [me1, me2]
                                                        , odst   = [t]
                                                        , ojmp   = Nothing})
munchExp (Binop LShift e1 (Const i)) = do me1 <- munchExp e1
                                          result (\t -> emit $ OPER{ oassem = "sll %d0, %s0, " ++ show i ++ "\n" -- SLL %d0 <- %s0 << i
                                                                   , osrc   = [me1]
                                                                   , odst   = [t]
                                                                   , ojmp   = Nothing})
munchExp (Binop LShift e1 e2) = do me1 <- munchExp e1
                                   me2 <- munchExp e2
                                   result (\t -> emit $ OPER{ oassem = "sllv %d0, %s0, %s1\n" -- SLLV %d0 <- %s0 << (%s1 % 32)
                                                            , osrc   = [me1, me2]
                                                            , odst   = [t]
                                                            , ojmp   = Nothing})
munchExp (Binop RShift e1 (Const i)) = do me1 <- munchExp e1
                                          result (\t -> emit $ OPER{ oassem = "srl %d0, %s0, " ++ show i ++ "\n" -- SRL %d0 <- %s0 >> i
                                                                   , osrc   = [me1]
                                                                   , odst   = [t]
                                                                   , ojmp   = Nothing})
munchExp (Binop RShift e1 e2) = do me1 <- munchExp e1
                                   me2 <- munchExp e2
                                   result (\t -> emit $ OPER{ oassem = "srlv %d0, %s0, %s1\n" -- SRLV %d0 <- %s0 >> (%s1 % 32)
                                                            , osrc   = [me1, me2]
                                                            , odst   = [t]
                                                            , ojmp   = Nothing})
munchExp (Mem (Binop Plus e1 (Const i))) = do me1 <- munchExp e1
                                              result (\t -> emit $ OPER{ oassem = "lw %d0, " ++ show i ++ "(%s0)\n" -- LOAD %d0 <- M[%s0 + i]
                                                                       , osrc   = [me1]
                                                                       , odst   = [t]
                                                                       , ojmp   = Nothing})
munchExp (Mem (Binop Plus (Const i) e1)) = do me1 <- munchExp e1
                                              result (\t -> emit $ OPER{ oassem = "lw %d0, " ++ show i ++ "(%s0)\n" -- LOAD %d0 <- M[i + %s0]
                                                                       , osrc   = [me1]
                                                                       , odst   = [t]
                                                                       , ojmp   = Nothing})
munchExp (Mem (Const i)) = result (\t -> emit $ OPER{ oassem = "lw %d0, " ++ show i ++ "($zero)\n" -- LOAD %d0 <- M[i + 0]
                                                    , osrc   = []
                                                    , odst   = [t]
                                                    , ojmp   = Nothing})
munchExp (Mem e1) = do me1 <- munchExp e1
                       result (\t -> emit $ OPER{ oassem = "lw %d0, 0(%s0)" -- LOAD %d0 <- M[%s0 + 0]
                                                , osrc   = [me1]
                                                , odst   = [t]
                                                , ojmp   = Nothing})
-- Primeros 4 params en $a0 a $a3 (agregar a TigerFrame?)
-- Parametros subsecuentes en el stack
-- Primeros 16 bytes del stack no usados
-- 1er param en 16($sp), 2do en 20($sp), nesimo en 14+4n($sp)
-- Return value en $v0
munchExp (Call (Name f) es) = do args <- munchArgs es
                                 result (\t -> emit $ OPER{ oassem = "jal " ++ (T.unpack f) ++ "\n"
                                                          , osrc   = args
                                                          , odst   = Frame.calldefs
                                                          , ojmp   = Just [] })
munchExp (Eseq s1 e1) = do munchStm s1
                           munchExp e1

-- MUNCH ARGS
munchArgs :: (Emitter w) => [Exp] -> w [Temp.Temp]
munchArgs = munchArgs' 0

munchArgs' :: (Emitter w) => Int -> [Exp] -> w [Temp.Temp]
munchArgs' _ []     = return []
munchArgs' n (e:es) = do t    <- Temp.newTemp
                         args <- munchArgs' (n+1) es
                         return (t:args)

