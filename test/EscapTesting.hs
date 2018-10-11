import           TigerAbs
import           TigerEscap
import           TigerParser (parse)
import           TigerQQ
import           TigerSymbol

import           Tools

-- Ejemplo de cómo testear...

-- Hay varios ejemplos acá.

-- Dos ejemplos concretos escritos en el archivo directamente :
-- + |ejemplo1| escrito usando el modulo TigerQQ.
-- + |ejemplo2| escrito directamente escribiendo el AST.

-- Dos ejemplos donde tenemos que leerlos y pasamos la url,
-- como mostrar los mensajes, el tester y el nombre del archivo.
-- + escapa.tig, __debe fallar__ y por ende se pone azul al fallar y rojo al no detectar el fallo.
-- + intro.tig, debe ejecutar normalmente y por ende se pasa rojo y azul dsp.

-- Testea toda la carpeta de Good Loc
-- Testea toda la carpeta de Type Loc
-- y termina.

main :: IO ()
main =
  putStrLn "\n======= Test ESCAPES in progress =======" >>
  either (const rednice)  (const bluefail) (calcularEEsc ejemplo1) >>
  either (const redfail) (const bluenice) (calcularEEsc ejemplo2) >>
  putStrLn "\n======= Test Ejemplo1 =======" >>
  print (calcularEEsc ejemplo1) >>
  putStrLn "\n==== [escapa.tig, intro.tig] ====" >>
  test "./test/test_code" (const bluefail) (const rednice) tester "escapa.tig" >>
  test "./test/test_code" (const redfail) (const bluenice) tester "intro.tig" >>
  putStrLn "\n==== Good loc ====" >>
  testDir good_loc (testSTDGood tester) >>
  putStrLn "\n==== Type Loc ====" >>
  testDir type_loc (testGood type_loc tester) >>
  putStrLn "\n======= Test ESCAPES FIN ======="

tester :: String -> Either Symbol Exp
tester = either (fail $ "Testing Escapes: Parser error")
                calcularEEsc
         . parse

ejemplo1 :: Exp -- La variable a escapa.
ejemplo1 = [expr|
                let
                  var a : int := 1
                  function f1(b : int):= a
                in
                  f1(a)
                end|]

ejemplo2 :: Exp -- La variable b no está definida.
ejemplo2 = LetExp
            [ VarDec (pack "a") NoEscapa Nothing (IntExp 1 (Simple 1 2)) (Simple 1 2)
            -- , VarDec "b" Nothing Nothing (IntExp 2 1) 2
            -- , VarDec "c" Nothing Nothing (IntExp 3 1) 3
            , FunctionDec
                    [ (pack "f1"
                      ,[(pack "a1", NoEscapa , NameTy $ pack "int")]
                      , Just $ pack "int",VarExp (SimpleVar $ pack "b") (Simple 5 5)
                      ,(Simple 5 6))
                    ]
            ]
            (IntExp 42 (Simple 8 1))
            (Simple 1 0)
