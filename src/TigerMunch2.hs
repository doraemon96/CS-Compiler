module TigerMunch2 where

import qualified TigerTemp          as Temp
import qualified TigerFrame         as Frame
import TigerTree
import TigerUnique
import TigerAsm

import Control.Monad.Trans.State.Lazy
import qualified Data.Text          as T
import qualified Data.List.Utils    as LU

import Debug.Trace


type Monadita = StateT [Instr] StGen

class (Monad w, Temp.TLGenerator w) => Emitter w where
    emit   :: Instr -> w ()
    result :: (Temp.Temp -> w ()) -> w Temp.Temp

instance Emitter Monadita where
    emit i = do st <- get
                put $ st ++ [i]
    result gen = do t <- Temp.newTemp
                    gen t
                    return t

--TODO
{-
format :: (Temp.Temp -> String) -> Instr -> String
format f ins = case ins of
                    IOPER{}  -> let seps = LU.split "%" $ oassem ins
                                in replace f seps (osrc ins) (odst ins)
                    IMOVE{}  -> let seps = LU.split "%" $ massem ins
                                in replace f seps [msrc ins] [mdst ins]
                    ILABEL{} -> lassem ins



-- TODO
-- replace :: SeparatedString -> Source -> Destination -> Result
replace :: (Temp.Temp -> String) -> [String] -> [Temp.Temp] -> [Temp.Temp] -> String
replace _ []       _    _    = ""
replace f (s:strs) srcs dsts = (s ++ (replace' f strs srcs dsts))

replace' _ []       _    _    = ""
replace' f (s:strs) srcs dsts | head s == 's' = (f (srcs !! (read [head (tail s)] :: Int)) ++ (tail (tail s))) ++ replace' f strs srcs dsts
                              | head s == 'd' = (f (dsts !! (read [head (tail s)] :: Int)) ++ (tail (tail s))) ++ replace' f strs srcs dsts
                              | otherwise = undefined
-}

-- CODEGEN
-- TODO: Para que usamos el Frame aca?
--codeGen :: Frame.Frame -> Stm -> [Instr]
--codeGen frame stm = reverse $ snd $ fst $ TigerUnique.evalState (runStateT (munchStm stm :: Monadita ()) []) 0

-- | codeGen corre Munch sobre ciertos stm
--   lo mantenemos envuelto en StGen para generar uniques
codeGen :: [Stm] -> StGen [Instr]
codeGen []       = return []
codeGen (s:ss) = do
    i  <- codeGen' s
    is <- codeGen ss
    return (i ++ is)

codeGen' :: Stm -> StGen [Instr]
codeGen' s = snd <$> (runStateT (munchStm s :: Monadita ()) [])


-- MUNCH STM
munchStm :: (Emitter w) => Stm -> w ()
munchStm (Move (Temp t) c@(Call _ _)) = do
    munchStm (ExpS c)
    emit $ IMOVE{ massem = MOVE t Frame.rv
                , mdst = t
                , msrc = Frame.rv}
munchStm (ExpS c@(Call e@(Name l) args)) = do
    args' <- munchArgs args
    emit $ IOPER{ oassem = JAL l
                , osrc   = args'
                , odst   = Frame.calldefs ++ Frame.argregs --TODO: argregs?
                , ojmp   = Nothing}
    emit $ IOPER {oassem = NOOP, osrc = [], odst = [], ojmp = Nothing}
    -- TODO: Restore?
munchStm (Move (Mem (Binop Plus e1 (Const i))) e2) = do
    me1 <- munchExp e1
    me2 <- munchExp e2
    emit $ IOPER{ oassem = SW me2 i me1 -- STORE M[%s0 + i] <- %s1
                , osrc   = [me1, me2]
                , odst   = []
                , ojmp   = Nothing}
munchStm (Move (Mem (Binop Plus (Const i) e1)) e2) = do
    me1 <- munchExp e1
    me2 <- munchExp e2
    emit $ IOPER{ oassem = SW me2 i me1 -- STORE M[i + %s0] <- %s1
                , osrc   = [me1, me2]
                , odst   = []
                , ojmp   = Nothing}
munchStm (Move (Mem (Binop Minus e1 (Const i))) e2) = do
    me1 <- munchExp e1
    me2 <- munchExp e2
    emit $ IOPER{ oassem = SW me2 (-i) me1 -- STORE M[%s0 - i] <- %s1
            , osrc   = [me1, me2]
            , odst   = []
            , ojmp   = Nothing}
-- RESTA NO CONMUTA: munchStm (Move (Mem (Binop Minus (Const i) e1)) e2)
-- munchStm (Move (Mem e1) (Mem e2)) = do
--     me1 <- munchExp e1
--     me2 <- munchExp e2
--     t <- Temp.newTemp
--     emit $ IOPER{ oassem = "lw %d0, (%s1)\nsw %d0, (%s0)\n" -- MOVE M[%s0] <- M[%s1]
--                 , osrc   = [me1, me2]
--                 , odst   = [t] --TODO: preguntar si esta bien que haya un dst aca
--                 , ojmp   = Nothing}
munchStm (Move (Mem (Const i)) e2) = do
    me2 <- munchExp e2
    emit $ IOPER{ oassem = SW me2 i Frame.zero -- STORE M[%r0 + i] <- %s0
                , osrc   = [me2]
                , odst   = []
                , ojmp   = Nothing}
munchStm (Move (Mem e1) e2) = do 
    me1 <- munchExp e1
    me2 <- munchExp e2
    emit $ IOPER{ oassem = SW me2 0 me1 -- STORE M[%s0] <- %s1
            , osrc   = [me1, me2]
            , odst   = [] 
            , ojmp   = Nothing}
munchStm (Move (Temp i1) (Temp i2)) = do 
    emit $ IMOVE{ massem = MOVE i1 i2 -- MOVE %d0 <- %s0
                , msrc   = i2
                , mdst   = i1} 
munchStm (Move (Temp i) e2) = do 
    me2 <- munchExp e2
    emit $ IOPER{ oassem = ADD i me2 Frame.zero -- ADD %d0 <- %s0 + %r0
            , osrc   = [me2]
            , odst   = [i] 
            , ojmp   = Nothing}
munchStm (Move e1 e2) = do 
    me1 <- munchExp e1
    me2 <- munchExp e2
    emit $ IMOVE{ massem = MOVE me1 me2 -- MOVE %d0 <- %s0
                , msrc   = me2
                , mdst   = me1}
munchStm (ExpS e1) = do 
    munchExp e1 --TODO: testear que modifique el state
    return ()
munchStm (Jump (Name lab1) lab2) | lab1 == lab2 = do -- TODO: what
    emit $ IOPER{ oassem = J lab1
                , osrc   = []
                , odst   = []
                , ojmp   = Just [lab2]}
    emit $ IOPER{ oassem = NOOP, osrc = [], odst = [], ojmp = Nothing}
munchStm (Jump e1 lab) = undefined --TODO: preguntar
-- Sabemos que lab2 (porque viene de Canon) es 
munchStm (CJump rop e1 e2 lab1 lab2) = do 
    me1 <- munchExp e1
    me2 <- munchExp e2
    case rop of
        TigerTree.EQ -> do  (emit $ IOPER{ oassem = BEQ me1 me2 lab1 -- EQ %s0, %s1, lab1
                                    , osrc   = [me1, me2]
                                    , odst   = []
                                    , ojmp   = Just [lab1, lab2]})
                            (emit $ IOPER {oassem = NOOP, osrc = [], odst = [], ojmp = Nothing})
        TigerTree.NE -> do  emit $ IOPER{ oassem = BNE me1 me2 lab1 -- NE %s0, %s1, lab1
                                        , osrc   = [me1, me2]
                                        , odst   = []
                                        , ojmp   = Just [lab1, lab2]}
                            emit $ IOPER {oassem = NOOP, osrc = [], odst = [], ojmp = Nothing}
        TigerTree.LT -> do  t <- Temp.newTemp
                            emit $ IOPER{ oassem = SLT t me1 me2 -- LT %s0, %s1, lab1
                                        , osrc   = [me1, me2]
                                        , odst   = [t]
                                        , ojmp   = Just [lab1, lab2]}
                            emit $ IOPER{ oassem = BNE t Frame.zero lab1
                                        , osrc = [t]
                                        , odst = []
                                        , ojmp = Just [lab1, lab2]}
                            emit $ IOPER {oassem = NOOP, osrc = [], odst = [], ojmp = Nothing}
        TigerTree.GT -> do  t <- Temp.newTemp  
                            emit $ IOPER{ oassem = SLT t me2 me1 -- GT %s0, %s1, lab1
                                        , osrc   = [me1, me2]
                                        , odst   = [t]
                                        , ojmp   = Nothing}
                            emit $ IOPER{ oassem = BNE t Frame.zero lab1
                                        , osrc = [t]
                                        , odst = []
                                        , ojmp = Just [lab1, lab2]}
                            emit $ IOPER {oassem = NOOP, osrc = [], odst = [], ojmp = Nothing}
        TigerTree.LE -> do  t <- Temp.newTemp  
                            emit $ IOPER{ oassem = BEQ me1 me2 lab1      -- e1 = e2? JMP True
                                    , osrc   = [me1, me2]
                                    , odst   = []
                                    , ojmp   = Just [lab1]}
                            emit $ IOPER {oassem = NOOP, osrc = [], odst = [], ojmp = Nothing}
                            emit $ IOPER{ oassem = SLT t me1 me2         -- e1 < e2? t = 1 : t = 0
                                    , osrc   = [me1, me2]
                                    , odst   = [t]
                                    , ojmp   = Nothing}
                            emit $ IOPER{ oassem = BNE t Frame.zero lab1 -- t == 0? JMP True
                                    , osrc   = [t]
                                    , odst   = []
                                    , ojmp   = Just [lab1, lab2]}
                            emit $ IOPER {oassem = NOOP, osrc = [], odst = [], ojmp = Nothing}
        TigerTree.GE -> do  t <- Temp.newTemp  
                            emit $ IOPER{ oassem = BEQ me1 me2 lab1      -- e1 = e2? JMP True
                                    , osrc   = [me1, me2]
                                    , odst   = []
                                    , ojmp   = Just [lab1]}
                            emit $ IOPER {oassem = NOOP, osrc = [], odst = [], ojmp = Nothing}
                            emit $ IOPER{ oassem = SLT t me2 me1         -- e2 < e1? t = 1 : t = 0
                                    , osrc   = [me2, me1]
                                    , odst   = [t]
                                    , ojmp   = Nothing}
                            emit $ IOPER{ oassem = BNE t Frame.zero lab1 -- t == 0? JMP True
                                    , osrc   = [t]
                                    , odst   = []
                                    , ojmp   = Just [lab1, lab2]}
                            emit $ IOPER {oassem = NOOP, osrc = [], odst = [], ojmp = Nothing}
        TigerTree.ULT -> do t <- Temp.newTemp 
                            emit $ IOPER{ oassem = SLTU t me1 me2        -- e1 < e2? t = 1 : t = 0
                                    , osrc   = [me1, me2]
                                    , odst   = [t]
                                    , ojmp   = Nothing}
                            emit $ IOPER{ oassem = BNE t Frame.zero lab1 -- t == 0? JMP True
                                    , osrc   = [t]
                                    , odst   = []
                                    , ojmp   = Just [lab1, lab2]}
                            emit $ IOPER {oassem = NOOP, osrc = [], odst = [], ojmp = Nothing}
        TigerTree.UGT -> do t <- Temp.newTemp 
                            emit $ IOPER{ oassem = SLTU t me2 me1        -- e2 < e1? t = 1 : t = 0
                                        , osrc   = [me1, me2]
                                        , odst   = [t]
                                        , ojmp   = Nothing}
                            emit $ IOPER{ oassem = BNE t Frame.zero lab1 -- t == 0? JMP True
                                        , osrc = [t]
                                        , odst = []
                                        , ojmp = Just [lab1, lab2]}
                            emit $ IOPER {oassem = NOOP, osrc = [], odst = [], ojmp = Nothing}
        TigerTree.ULE -> do t <- Temp.newTemp
                            emit $ IOPER{ oassem = BEQ me1 me2 lab1      -- e1 = e2? JMP True
                                    , osrc   = [me1, me2]
                                    , odst   = []
                                    , ojmp   = Just [lab1]}
                            emit $ IOPER {oassem = NOOP, osrc = [], odst = [], ojmp = Nothing}
                            emit $ IOPER{ oassem = SLTU t me1 me2        -- e1 < e2? t = 1 : t = 0
                                    , osrc   = [me1, me2]
                                    , odst   = [t]
                                    , ojmp   = Nothing}
                            emit $ IOPER{ oassem = BNE t Frame.zero lab1 -- t == 0? JMP True
                                    , osrc   = [t]
                                    , odst   = []
                                    , ojmp   = Just [lab1, lab2]}
                            emit $ IOPER {oassem = NOOP, osrc = [], odst = [], ojmp = Nothing}
        TigerTree.UGE -> do t <- Temp.newTemp
                            emit $ IOPER{ oassem = BEQ me1 me2 lab1      -- e1 = e2? JMP True
                                    , osrc   = [me1, me2]
                                    , odst   = []
                                    , ojmp   = Just [lab1]}
                            emit $ IOPER {oassem = NOOP, osrc = [], odst = [], ojmp = Nothing}
                            emit $ IOPER{ oassem = SLTU t me2 me1         -- e2 < e1? t = 1 : t = 0
                                    , osrc   = [me2, me1]
                                    , odst   = [t]
                                    , ojmp   = Nothing}
                            emit $ IOPER{ oassem = BNE t Frame.zero lab1 -- t == 0? JMP True
                                    , osrc   = [t]
                                    , odst   = []
                                    , ojmp   = Just [lab1, lab2]}
                            emit $ IOPER {oassem = NOOP, osrc = [], odst = [], ojmp = Nothing}
munchStm (Seq s1 s2) = munchStm s1 >> munchStm s2
munchStm (Label lab) = emit $ ILABEL{ lassem = LABEL lab -- lab :
                                   , llab = lab}


-- MUNCH EXP (TODO)
munchExp :: (Emitter w) => Exp -> w Temp.Temp
munchExp (Const 0) = return Frame.zero
munchExp (Const i) = 
    result (\t -> emit $ IOPER{ oassem = LI t i -- t <- i
                                , osrc = []
                                , odst = [t]
                                , ojmp = Nothing})
munchExp (Name l) = 
    result (\t -> emit $ IOPER{ oassem = LA t l -- t <- *l
                                , osrc = []
                                , odst = [t]
                                , ojmp = Nothing})
munchExp (Temp t) = return t
munchExp (Binop Plus e1 (Const i)) = do --TODO: Inmediates can only be < 2^15 -1, check! (and negative?)
    me1 <- munchExp e1
    result (\t -> emit $ IOPER{ oassem = ADDI t me1 i -- ADDI t <- e1 + i
                                , osrc   = [me1]
                                , odst   = [t]
                                , ojmp   = Nothing})
munchExp (Binop Plus (Const i) e1) = do 
    me1 <- munchExp e1
    result (\t -> emit $ IOPER{ oassem = ADDI t me1 i -- t <- i + e1
                                , osrc   = [me1]
                                , odst   = [t]
                                , ojmp   = Nothing})
munchExp (Binop Plus e1 e2) = do 
    me1 <- munchExp e1
    me2 <- munchExp e2
    result (\t -> emit $ IOPER{ oassem = ADD t me1 me2 -- t <- e1 + e2
                            , osrc   = [me1, me2]
                            , odst   = [t]
                            , ojmp   = Nothing})
munchExp (Binop Minus e1 e2) = do 
    me1 <- munchExp e1
    me2 <- munchExp e2
    result (\t -> emit $ IOPER{ oassem = SUB t me1 me2 -- t <- e1 - e2
                            , osrc   = [me1, me2]
                            , odst   = [t]
                            , ojmp   = Nothing})
munchExp (Binop Mul e1 e2) = do 
    me1 <- munchExp e1
    me2 <- munchExp e2
    result (\t -> do    emit $ IOPER{ oassem = MULT me1 me2  -- LO <- e1 * e2
                                    , osrc   = [me1, me2]
                                    , odst   = [Frame.lo]
                                    , ojmp   = Nothing}
                        emit $ IOPER{ oassem = MFLO t       -- t <- LO
                                    , osrc = [Frame.lo]
                                    , odst = [t]
                                    , ojmp = Nothing})
munchExp (Binop Div e1 e2) = do 
    me1 <- munchExp e1
    me2 <- munchExp e2
    result (\t -> do    emit $ IOPER{ oassem = DIV me1 me2 -- LO <- e1 / e2
                                    , osrc   = [me1, me2]
                                    , odst   = [Frame.lo]
                                    , ojmp   = Nothing}
                        emit $ IOPER{ oassem = MFLO t      -- t <- LO
                                    , osrc   = [Frame.lo]
                                    , odst   = [t]
                                    , ojmp   = Nothing}                    
            )
munchExp (Binop And e1 e2) = do 
    me1 <- munchExp e1
    me2 <- munchExp e2
    result (\t -> emit $ IOPER{ oassem = AND t me1 me2 -- t <- e1 & e2
                                , osrc   = [me1, me2]
                                , odst   = [t]
                                , ojmp   = Nothing})
munchExp (Binop Or e1 e2) = do 
    me1 <- munchExp e1
    me2 <- munchExp e2
    result (\t -> emit $ IOPER{ oassem = OR t me1 me2 -- t <- e1 | e2
                            , osrc   = [me1, me2]
                            , odst   = [t]
                            , ojmp   = Nothing})
munchExp (Binop LShift e1 (Const i)) = do 
    me1 <- munchExp e1
    result (\t -> emit $ IOPER{ oassem = SLL t me1 i -- SLL %d0 <- %s0 << i
                            , osrc   = [me1]
                            , odst   = [t]
                            , ojmp   = Nothing})
munchExp (Binop LShift e1 e2) = do 
    me1 <- munchExp e1
    me2 <- munchExp e2
    result (\t -> emit $ IOPER{ oassem = SLLV t me1 me2 -- SLLV %d0 <- %s0 << (%s1 % 32)
                            , osrc   = [me1, me2]
                            , odst   = [t]
                            , ojmp   = Nothing})
munchExp (Binop RShift e1 (Const i)) = do 
    me1 <- munchExp e1
    result (\t -> emit $ IOPER{ oassem = SRL t me1 i -- SRL %d0 <- %s0 >> i
                            , osrc   = [me1]
                            , odst   = [t]
                            , ojmp   = Nothing})
munchExp (Binop RShift e1 e2) = do 
    me1 <- munchExp e1
    me2 <- munchExp e2
    result (\t -> emit $ IOPER{ oassem = SRLV t me1 me2 -- SRLV %d0 <- %s0 >> (%s1 % 32)
                        , osrc   = [me1, me2]
                        , odst   = [t]
                        , ojmp   = Nothing})
munchExp (Mem (Binop Plus e1 (Const i))) = do 
    me1 <- munchExp e1
    result (\t -> emit $ IOPER{ oassem = LW t i me1 -- LOAD %d0 <- M[%s0 + i]
                            , osrc   = [me1]
                            , odst   = [t]
                            , ojmp   = Nothing})
munchExp (Mem (Binop Plus (Const i) e1)) = do 
    me1 <- munchExp e1
    result (\t -> emit $ IOPER{ oassem = LW t i me1 -- LOAD %d0 <- M[i + %s0]
                            , osrc   = [me1]
                            , odst   = [t]
                            , ojmp   = Nothing})
munchExp (Mem (Const i)) = result (\t -> emit $ IOPER{ oassem = LW t i Frame.zero -- LOAD %d0 <- M[i + 0]
                                                    , osrc   = []
                                                    , odst   = [t]
                                                    , ojmp   = Nothing})
munchExp (Mem e1) = do me1 <- munchExp e1
                       result (\t -> emit $ IOPER{ oassem = LW t 0 me1 -- LOAD %d0 <- M[%s0 + 0]
                                                , osrc   = [me1]
                                                , odst   = [t]
                                                , ojmp   = Nothing})
munchExp (Eseq s1 e1) = do munchStm s1
                           munchExp e1

-- MUNCH ARGS
-- Primeros 4 params en $a0 a $a3
-- y también los mandamos a stack (hasta 16($sp))
-- Parametros subsecuentes en el stack
-- nesimo param (0 index) en 4n($sp)
-- Return value en $v0
munchArgs :: (Emitter w) => [Exp] -> w [Temp.Temp]
munchArgs = munchArgs' 0

munchArgs' :: (Emitter w) => Int -> [Exp] -> w [Temp.Temp]
munchArgs' _ []     = return []
munchArgs' n (e:es) | n < (length Frame.argregs)  = do
                            t <- munchExp e
                            -- Move to arg register
                            let areg = Frame.argregs !! n
                            emit $ IMOVE { massem = MOVE areg t
                                        , mdst = areg 
                                        , msrc = t }
                            -- Store in stack (slower but easier to work with :sweatsmile:)
                            emit $ IOPER { oassem = SW t (n * Frame.wSz) Frame.sp
                                        , osrc = [t, Frame.sp]
                                        , odst = []
                                        , ojmp = Nothing}
                            args <- munchArgs' (n+1) es
                            return (areg:args)
                    | n >= (length Frame.argregs) = do 
                            t <- munchExp e
                            emit $ IOPER { oassem = SW t (n * Frame.wSz) Frame.sp
                                        , osrc = [t]
                                        , odst = [Frame.sp]
                                        , ojmp = Nothing}
                            args <- munchArgs' (n+1) es
                            return args

