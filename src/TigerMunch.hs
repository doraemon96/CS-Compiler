module TigerMunch where

import qualified TigerTemp          as Temp
import TigerTree
import TigerUnique

import Control.Monad.Trans.State.Lazy
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
munchStm (Move (Temp i) e2) = do me2 <- munchExp e2
                                 emit $ OPER{ oassem = "add %d0, %s0, $zero\n" -- ADD %d0 <- %s0 + %r0
                                            , osrc   = [me2]
                                            , odst   = [i] 
                                            , ojmp   = Nothing}
munchStm (Label lab) = emit $ LABEL{ lassem = (T.unpack lab) ++ "\n" -- lab :
                                   , llab = lab}


munchExp :: (Emitter w) => Exp -> w Temp.Temp
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
munchExp (Const i) = result (\t -> emit $ OPER{ oassem = "addi %d0, $zero, " ++ show i ++ "\n" -- ADDI %d0 <- 0 + i
                                              , osrc   = []
                                              , odst   = [t]
                                              , ojmp   = Nothing})
munchExp (Binop Plus e1 e2) = do me1 <- munchExp e1
                                 me2  <- munchExp e2
                                 result (\t -> emit $ OPER{ oassem = "add %d0, %s0, %s1\n" -- ADD %d0 <- %s0 + %s1
                                                          , osrc   = [me1, me2]
                                                          , odst   = [t]
                                                          , ojmp   = Nothing})
munchExp (Temp t) = return t
