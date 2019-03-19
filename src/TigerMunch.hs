module TigerMunch where

import qualified TigerTemp          as Temp
import TigerTree
import Data.Text                    as T

data Instr = OPER { oassem :: String
                  , odst   :: [Temp.Temp]
                  , osrc   :: [Temp.Temp]
                  , ojmp   :: Maybe [Temp.Label] } 
           | MOVE { massem :: String
                  , mdst   :: Temp.Temp
                  , msrc   :: Temp.Temp } 
           | LABEL { lassem :: String
                   , llab   :: Temp.Label }

class (Monad w, TLGenerator w) => Emitter w where
    emit :: Instr -> w

format :: (Temp.Temp -> String) -> Instr -> String
format = undefined

munchStm :: (Emitter w) => Stm -> w ()
munchStm (Seq s1 s2) = munchStm s1 >> munchStm s2
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
--munchStm (Move (Mem e1 e2)) = do me1 <- munchExp e1
--                                 me2 <- munchExp e2
--                                 emit $ OPER{ oassem = "sw %s1, " ++ show i ++ "(%s0)\n" -- MOVE M[%s0] <- M[%s1]
--                                            , osrc   = [me1, me2]
--                                            , odst   = []
--                                            , ojmp   = Nothing}

munchExp :: (Emitter w) => Exp -> w Temp.Temp
munchExp (Mem (Binop Plus e1 (Const i))) = do me1 <- munchExp e1
                                              t   <- newTemp
                                              emit $ OPER{ oassem = "lw %d0, " ++ show i ++ "(%s0)\n" -- LOAD %d0 <- M[%s0 + i]
                                                         , osrc   = [me1]
                                                         , odst   = [t]
                                                         , ojmp   = Nothing}
munchExp (Mem (Binop Plus (Cont i) e1)) = do me1 <- munchExp e1
                                             t   <- newTemp
                                             emit $ OPER{ oassem = "lw %d0, " ++ show i ++ "(%s0)\n" -- LOAD %d0 <- M[i + %s0]
                                                        , osrc   = [me1]
                                                        , odst   = [t]
                                                        , ojmp   = Nothing}
munchExp (Mem (Const i)) = do t <- newTemp
                              emit $ OPER{ oassem = "lw %d0, " ++ show i ++ "($zero)\n" -- LOAD %d0 <- M[i + 0]
                                         , osrc   = []
                                         , odst   = [t]
                                         , ojmp   = Nothing}
munchExp (Mem e1) = do me1 <- munchExp e1
                       t   <- newTemp
                       emit $ OPER{ oassem = "lw %d0, 0(%s0)" -- LOAD %d0 <- M[%s0 + 0]
                                  , osrc   = [me1]
                                  , odst   = [t]
                                  , ojmp   = Nothing}
munchExp (Binop Plus e1 (Const i)) = do me1 <- munchExp e1
                                        t   <- newTemp
                                        emit $ OPER{ oassem = "addi %d0, %s0, " ++ show i ++ "\n" -- ADDI %d0 <- %s0 + i
                                                   , osrc   = [me1]
                                                   , odst   = [t]
                                                   , ojmp   = Nothing}
munchExp (Binop Plus (Const i) e1) = do me1 <- munchExp e1
                                        t   <- newTemp
                                        emit $ OPER{ oassem = "addi %d0, " ++ show i ++ ", %s0\n" -- ADDI %d0 <- i + %s0
                                                   , osrc   = [me1]
                                                   , odst   = [t]
                                                   , ojmp   = Nothing}
munchExp (Const i) = do t <- newTemp
                        emit $ OPER{ oassem = "addi %d0, $zero, " ++ show i ++ "\n" -- ADDI %d0 <- 0 + i
                                   , osrc   = []
                                   , odst   = [t]
                                   , ojmp   = Nothing}
munchExp (Binop Plus e1 e2) = do me1 <- munchExp e1
                                me2  <- munchExp e2
                                t    <- newTemp
                                emit $ OPER{ oassem = "add %d0, %s0, %s1\n" -- ADD %d0 <- %s0 + %s1
                                           , osrc   = [me1, me2]
                                           , odst   = [t]
                                           , ojmp   = Nothing}
munchExp (Temp t) = return $ T.unpack t
