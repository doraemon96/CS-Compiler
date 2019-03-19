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

format :: (Temp.Temp -> String) -> Instr -> String
format = undefined

munchStm :: Stm -> [Instr]
munchStm (Seq s1 s2) = (munchStm s1) ++ (munchStm s2)
munchStm (Move (Mem (Binop Plus e1 (Const i))) e2) = [OPER{ oassem = "sw %s1 , " ++ itos i ++ "(%s0)\n"
                                                          , osrc = [munchExp e1, munchExp e2]
                                                          , odst = []
                                                          , ojmp = Nothing }]
munchStm () = undefined
munchStm () = undefined
munchStm () = undefined
munchStm (Label l) = [LABEL{lassem = (T.unpack l) ++ ":\n", llab = T.unpack l}]



munchExp :: Exp -> Temp.Temp
munchExp x = undefined