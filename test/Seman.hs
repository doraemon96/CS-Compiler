import           TigerAbs
import           TigerEscap
import           TigerParser (parse)
import           TigerQQ
import           TigerSymbol
import           TigerSeman
import           TigerTips
import           TigerUnique
import           TigerTrans

import           Tools

data Correct = Correct | Incorrect

cases :: [(String, Exp, Tipo, Correct)]
cases = [("let1", let1, let1T, Correct),("let2", let2, let2T, Correct),("let3", let3, let3T, Correct),
         ("let4", let4, let4T, Incorrect),("let5", let5, let5T, Correct),("let6", let6, let6T, Correct),
         ("unit", unit, unitT, Correct), ("nil", nil, nilT, Correct), ("int", int, intT, Correct),
         ("str", str, strT, Correct), ("op1", op1, op1T, Correct),("op2", op2, op2T, Correct),
         ("op3", op3, op3T, Correct), ("op4", op4, op4T, Correct),("op5", op5, op5T, Incorrect),
         ("op6", op6, op6T, Incorrect), ("seq1", seq1, seq1T, Correct), ("if1", if1, if1T, Correct),
         ("if2", if2, if2T, Correct), ("if3", if3, if3T, Incorrect), ("whl1", while1, while1T, Correct),
         ("whl2", while2, while2T, Incorrect), ("for1", for1, for1T, Correct), ("for2", for2, for2T, Incorrect),
         ("for3", for3, for3T, Incorrect), ("for4", for4, for4T, Incorrect), ("for5", for5, for5T, Incorrect),
         ("fun1", fun1, fun1T, Incorrect), ("fun2", fun2, fun2T, Incorrect), ("tip1", tip1, tip1T, Correct),
         ("tip2", tip2, tip2T, Correct), ("tip3", tip3, tip3T, Correct), ("tip4", tip4, tip4T, Incorrect),
         ("tip5", tip5, tip5T, Correct), ("tip6", tip6, tip6T, Correct), ("tip7", tip7, tip7T, Correct),
         ("cll1", cll1, cll1T, Correct)
         ]

main :: IO ()
main = do
  putStrLn "\n======= Test SEMAN in progress ======="
  mapM_ (\(n, e, t, c) -> do
          putStr $ n ++ ": \t"
          case c of
            Correct   -> either (badRes . unpack) (goodRes . show) (testExpTipo e t)
            Incorrect -> either (goodRes . unpack) (badRes . show) (testExpTipo e t)
        ) cases
  putStrLn "\n======= Good loc ======="
  testDir good_loc (testSTDGood tester)
  putStrLn "\n======= Type loc ======="
  testDir type_loc (testBad type_loc tester)
  putStrLn "\n======= BExp Generado ======="
  putStrLn "\n======= Test FIN ======="


testExpTipo :: Exp -> Tipo -> Either Symbol Tipo
testExpTipo e t = do
  (bexp, ty) <- testExp e
  if (?=) t ty
    then return t
    else Left (pack $ "Testing Seman: Incorrect type. Expected: " ++ show t ++ ". Actual: " ++ show ty)

testExp :: Exp -> Either Symbol (BExp, Tipo)
testExp e = fst $ evalState (runSeman e) 0

tester :: String -> Either Symbol (BExp, Tipo)
tester = either (fail $ "Testing Seman: Parser error")
                testExp
         . parse

-- Casos

unit  = [expr| () |]
unitT = TUnit

nil  = [expr| nil |]
nilT = TNil

int  = [expr| 2 |]
intT = TInt RW

str  = [expr| "Hola"|]
strT = TString

op1  = [expr|1 + 1|]
op1T = TInt RW

op2  = [expr|1 - 1|]
op2T = TInt RW

op3  = [expr|1 > 2|]
op3T = TBool

op4  = [expr|"h" < "f"|]
op4T = TBool

op5  = [expr|"h" + 1|]
op5T = TInt RW

op6  = [expr|1 - "h"|]
op6T = TInt RW

seq1  = [expr|let var x := 0 in  "hola"; x end|]
seq1T = TInt RW

if1  = [expr| if 1 = 2 then 0 else 1|]
if1T = TInt RW

if2  = [expr| if 1 = 2 then (0;())|]
if2T = TUnit

if3  = [expr| if 1 = 2 then "hola" else 3|]
if3T = TString

while1  = [expr| while 1 <> 2
                  do ()|]
while1T = TUnit

while2  = [expr| while "hola"
                  do ()|]
while2T = TUnit

for1  = [expr|for i := 0 to 5
          do ()|]
for1T = TUnit

for2  = [expr|for i := () to 5
          do ()|]
for2T = TUnit

for3  = [expr|for i := 0 to "hola"
          do ()|]
for3T = TUnit

for4  = [expr|for i := 0 to 5
          do 0|]
for4T = TUnit

for5  = [expr|for i := 0 to 5
          do i := 5|]
for5T = TUnit

let1 :: Exp
let1 = [expr|
                let
                  var a : int := 1
                in
                  a
                end|]
let1T = TInt RW

let2 :: Exp
let2 = [expr|
                let
                  var a : int := 1
                  function f(x:int):int = x
                in
                  f(a)
                end|]
let2T = TInt RW

let3 :: Exp
let3 = [expr|
                let
                  var a : string := "hola"
                  function f1(x:int):int = f2(x-1)
                  function f2(x:int):int = f1(x-1)
                  var a : int := 1
                in
                  f1(a)
                end|]
let3T = TInt RW

let4 :: Exp
let4 = [expr|
                let
                  var a : int := 1
                  function f(a:int):int = a
                  var a : string := "hola"
                in
                  f(a)
                end|]
let4T = TInt RW

let5 :: Exp
let5 = [expr|
                let
                  var a : int := 1
                  var b : int := a
                  var a : string := "hola"
                in
                  a
                end|]
let5T = TString

let6 :: Exp
let6 = [expr|
                let
                  var a : int := 10
                  function f():int = a
                  var a : string := "hola"
                in
                  f()
                end|]
let6T = TInt RW

fun1  = [expr|   let
                   function f():int = 0
                   function f():string = "hola"
                 in
                   f()
                 end|]
fun1T = TString

fun2  = [expr|   let
                   function f(x:int, x:int):int = 0
                 in
                   f()
                 end|]
fun2T = TString

tip1 = [expr|   let
                   type a = int
                   var a : a := 0
                 in
                   a
                 end|]
tip1T = TInt RW

tip2 = [expr|   let
                   type a = array of int
                   var a := a [10] of 0
                 in
                   a[0]
                 end|]
tip2T = TInt RW

tip3 = [expr|   let
                   type r = { a:int } 
                   var r := r {a = 0}
                 in
                   r.a
                 end|]
tip3T = TInt RW

tip4 = [expr|   let
                   type a = b
                   type b = a
                 in
                   0
                 end|]
tip4T = TInt RW

tip5 = [expr|   let
                   type l = {h:int, t:l}
                   var l1 := l {h = 0, t = nil}
                 in
                   l1.h
                 end|]
tip5T = TInt RW

tip6 = [expr|   let
                   type l = {h:int, t:l}
                   var l1 := l {h = 1, t = nil}
                   var l2 := l {h = 0, t = l1}
                 in
                   l2.t.h
                 end|]
tip6T = TInt RW

tip7 = [expr|   let
                   type l1 = {a: l2, b: l1}
                   type l2 = {c: l1}
                   var l1 : l1 := l1 {a = l2 {c = l1 {a = nil, b = nil}}, b = l1 {a = nil, b = nil}}
                 in
                   l1
                 end|]
tip7T = TNil

cll1  = [expr|   let
                   var a : int := 1
                   function f(x:int) := x := 2
                 in
                   (f(a);
                   a)
                 end|]
cll1T = TInt RW
