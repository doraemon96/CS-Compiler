module TigerSeman where

import           TigerAbs
import           TigerErrores               as E
import           TigerSres
import           TigerSymbol
import           TigerTips
import           TigerUnique
import           TigerTopSort

-- Segunda parte imports:
import           TigerTemp
import           TigerTrans                 as TTr
import           TigerFrame

-- Monads
import qualified Control.Conditional        as C
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans.Except

-- Data
import           Data.List                  as List
import           Data.Map                   as M
import           Data.Ord                   as Ord
import           Data.Maybe
import           Data.Text                  as T (append, pack)

-- Le doy nombre al Preludio.
import           Prelude                    as P

-- Debugging. 'trace :: String -> a -> a'
-- imprime en pantalla la string cuando se ejecuta.
import           Debug.Trace                

-- * Análisis Semántico, aka Inferidor de Tipos

-- ** Notas :

-- [1] No deberían fallar las búsquedas de variables. Recuerden que
-- el calculo de variables escapadas debería detectar las variables
-- no definidas.

-- [2] En la siguiente etapa vamos a ir generando el código intermedio
-- mezclado con esta etapa por lo que es muy posible que tengan que revisar
-- este modulo. Mi consejo es que sean /lo más ordenados posible/ teniendo en cuenta
-- que van a tener que reescribir bastante.

class (Demon w, Monad w) => Manticore w where
  -- | Inserta una Variable al entorno
    insertValV :: Symbol -> ValEntry -> w a -> w a
  -- | Inserta una Función al entorno
    insertFunV :: Symbol -> FunEntry -> w a -> w a
  -- | Inserta una Variable de sólo lectura al entorno
    insertVRO :: Symbol -> Access -> Int -> w a -> w a
  -- | Inserta una variable de tipo al entorno
    insertTipoT :: Symbol -> Tipo -> w a -> w a
  -- | Busca una función en el entorno
    getTipoFunV :: Symbol -> w FunEntry
  -- | Busca una variable en el entorno. Ver [1]
    getTipoValV :: Symbol -> w ValEntry
  -- | Busca un tipo en el entorno
    getTipoT :: Symbol -> w Tipo
  -- | Funciones de Debugging!
    showVEnv :: w a -> w a
    showVEnv' :: w ()
    showVEnv' = showVEnv (return ())
    showTEnv :: w a -> w a
    showTEnv' :: w ()
    showTEnv' = showTEnv (return ())
    --
    -- | Función monadica que determina si dos tipos son iguales.
    -- El catch está en que tenemos una especie de referencia entre los
    -- nombres de los tipos, ya que cuando estamos analizando la existencia de bucles
    -- en la definición permitimos cierto alias hasta que los linearizamos con el
    -- sort topológico.
    tiposIguales :: Tipo -> Tipo -> w Bool
    tiposIguales (RefRecord s) l@(TRecord _ u) = do
        st <- getTipoT s
        case st of
            TRecord _ u1 -> return (u1 == u)
            ls@RefRecord{} -> tiposIguales ls l
            _ -> E.internal $ pack "No son tipos iguales... 123+1"
    tiposIguales l@(TRecord _ u) (RefRecord s) = do
        st <- getTipoT s
        case st of
            TRecord _ u1 -> return (u1 == u)
            ls@RefRecord{} -> tiposIguales l ls
            _ -> E.internal $ pack "No son tipos iguales... 123+2"
    tiposIguales (RefRecord s) (RefRecord s') = do
        s1 <- getTipoT s
        s2 <- getTipoT s'
        tiposIguales s1 s2
    tiposIguales TNil  (RefRecord _) = return True
    tiposIguales (RefRecord _) TNil = return True
    tiposIguales (RefRecord _) _ = E.internal $ pack "No son tipos iguales... 123+3"
    tiposIguales  e (RefRecord s) = E.internal $ pack $ "No son tipos iguales... 123+4" ++ (show e ++ show s)
    tiposIguales a b = return ((?=) a b)
    --
    -- | Generador de uniques.
    --
    ugen :: w Unique

-- | Definimos algunos helpers

-- | `addpos` nos permite agregar información al error.
addpos :: (Demon w, Show b) => w a -> b -> w a
addpos t p = E.adder t (pack $ show p)

-- | Patrón de errores...
errorTiposMsg :: (Demon w, Show p)
              => p -> String -> Tipo -> Tipo -> w a
errorTiposMsg p msg t1 t2 = flip addpos p
    $ flip adder (pack msg)
    $ errorTipos t1 t2

depend :: Ty -> [Symbol]
depend (NameTy s)    = [s]
depend (ArrayTy s)   = [s]
depend (RecordTy ts) = concatMap (depend . snd) ts

-- | Función auxiliar que chequea cuales son los tipos
-- comparables.
-- Por ejemplo, ` if nil = nil then ...` es una expresión ilegal
-- ya que no se puede determinar el tipo de cada uno de los nils.
-- Referencia: [A.3.Expressions.Nil]
tiposComparables :: Tipo -> Tipo -> Oper -> Bool
tiposComparables TNil TNil EqOp  = False
tiposComparables TUnit _ EqOp    = False
tiposComparables _ _ EqOp        = True
tiposComparables TNil TNil NeqOp = False
tiposComparables TUnit _ NeqOp   = False
tiposComparables _ _ NeqOp       = True
tiposComparables _ _ _           = True

esInt :: Tipo -> Bool
esInt (TInt _) = True
esInt _        = False

-- | Función que chequea que los tipos de los campos sean los mismos
-- Ver 'transExp (RecordExp ...)'
-- Ver 'transExp (CallExp ...)'
cmpZip :: (Demon m, Monad m) => [(Symbol, Tipo)] -> [(Symbol, Tipo, Int)] -> m () --Bool
cmpZip [] [] = return ()
cmpZip [] _ = derror $ pack "Diferencia en la cantidad. 1"
cmpZip _ [] = derror $ pack "Diferencia en la cantidad. 2"
cmpZip ((sl,tl):xs) ((sr,tr,p):ys) =
        if ((?=) tl tr && sl == sr)
        then cmpZip xs ys
        else flip adder (pack ("Comparando " ++ show sl ++ " y " ++ show sr ++ ". ")) $ errorTipos tl tr

-- Función auxiliar que utilizaremos para separar una lista utilizando una
-- función que separe los elementos en pares.
splitWith :: (a -> Either b c) -> [a] -> ([b], [c])
addIzq :: ([a], [b]) -> a -> ([a],[b])
addDer :: ([a], [b]) -> b -> ([a],[b])
splitWith f = P.foldr (\x rs -> either (addIzq rs) (addDer rs) (f x)) ([] , [])
addIzq (as,bs) a = (a : as, bs)
addDer (as,bs) b = (as, b : bs)

buscarM :: Symbol -> [(Symbol, Tipo, Int)] -> Maybe (Tipo, Int)
buscarM s [] = Nothing
buscarM s ((s', t, i):xs) | s == s' = Just (t, i)
                        | otherwise = buscarM s xs

-- | __Completar__ 'transVar'.
-- El objetivo de esta función es obtener el tipo
-- de la variable a la que se está __accediendo__.
transVar :: (MemM w, Manticore w) => Var -> w (BExp, Tipo)
-- Leer Nota [1] para SimpleVar
-- ** transVar :: (Manticore w) => Var -> w ( () , Tipo)
transVar (SimpleVar s)      = do 
                                (ty, acc, lev) <- getTipoValV s
                                (,ty) <$> TTr.simpleVar acc lev
transVar (FieldVar v s)     = transVar v >>= \case
                                    (brec , TRecord lt _) -> maybe (derror (pack "Not a record field")) 
                                                                   (\(t, i) -> (,t) <$> TTr.fieldVar brec i) --chequear i
                                                                   (buscarM s lt)
                                    _                     -> derror $ pack "Not a record var"
transVar (SubscriptVar v e) = transVar v >>= \case
                                    (barr , TArray t _) -> transExp e >>= \case
                                                            (be , TInt _) -> (,t) <$> TTr.subscriptVar barr be
                                                            _             -> derror $ pack "Not a valid index"
                                    _                -> derror $ pack "Not an array var"

-- | __Completar__ 'TransTy'
-- El objetivo de esta función es dado un tipo
-- que proviene de la gramática, dar una representación
-- de tipo interna del compilador

-- | Nota para cuando se generarte código intermedio
-- que 'TransTy ' no necesita ni 'MemM ' ni devuelve 'BExp'
-- porque no se genera código intermedio en la definición de un tipo.
transTy :: (Manticore w) => Ty -> w Tipo
transTy (NameTy s)      = getTipoT s
transTy (RecordTy flds) = do fldsTys <- mapM (\(nm, cod) -> (nm,) <$> transTy cod) flds
                             let ordered = List.sortBy (Ord.comparing fst) fldsTys
                                 ziplist = List.zipWith (curry triplar) ordered [0..]
                             uniq <- ugen
                             return $ TRecord ziplist uniq
                          where triplar ((a,b),c) = (a,b,c)
transTy (ArrayTy s)     = do t'   <- getTipoT s
                             uniq <- ugen
                             return $ TArray t' uniq


fromTy :: (Manticore w) => Ty -> w Tipo
fromTy (NameTy s) = getTipoT s
fromTy _ = P.error "no debería haber una definición de tipos en los args..."

-- | transDecs es la encargada de tipar las definiciones y posteriormente
-- generar código intermedio solamente para las declaraciones de variables.
----------------------------------------
-- Aquí se encontraran con la parte más difícil de esta etapa,
-- que es la detección de bucles y correcta inserción de tipos
-- en el entorno.
-- + Para realizar correctamente la detección de cíclos se utiliza el algoritmo
--   de sort topologico. Pueden encontrar una simple implementación en el
--   archivo [TigerTopSort](src/TigerTopSort.hs).
-- + Para generar los representantes correspondientes de tipo |Tipo|, vamos a
--   necesitar generar valores potencialmente infinitos y para esto usaremos una
--   técnica conocida en la literatura de Haskell conocida como [Tying the
--   Knot](https://wiki.haskell.org/Tying_the_Knot)
----------------------------------------
--  **transDecs :: (Manticore w) => [Dec] -> w a -> w a
transDecs :: (MemM w, Manticore w) => [Dec] -> w (BExp,Tipo) -> w ([BExp],Tipo)
transDecs [] m = do (x,y) <- m
                    return ([x],y)
transDecs ((VarDec nm escap Nothing init p): xs) m = do (eb,et) <- transExp init
                                                        when (et == TNil) $ addpos (derror (pack "No se puede inferir tipo de record inicializado a nil.")) p
                                                        lev     <- TTr.getActualLevel
                                                        acc     <- TTr.allocLocal escap
                                                        bvar    <- TTr.varDec acc -- creo una variable
                                                        bass    <- TTr.assignExp bvar eb -- le asigno el valor inicial
                                                        (bexps,tipo) <- insertValV nm (et,acc,lev) (transDecs xs m)
                                                        return (bass:bexps, tipo)
transDecs ((VarDec nm escap (Just t) init p): xs) m = do (eb,et) <- transExp init
                                                         wt      <- addpos (getTipoT t) p 
                                                         bt      <- tiposIguales et wt
                                                         unless bt $ derror (pack "Tipos no compatibles #1")
                                                         lev     <- TTr.getActualLevel
                                                         acc     <- TTr.allocLocal escap
                                                         bvar    <- TTr.varDec acc -- creo una variable
                                                         bass    <- TTr.assignExp bvar eb -- le asigno el valor inicial
                                                         (bexps,tipo) <- insertValV nm (wt,acc,lev) (transDecs xs m)
                                                         return (bass:bexps, tipo)
transDecs ((FunctionDec fs) : xs)           m = let fs' = P.map (\ (nm , _, _ ,_ , _) -> nm) fs in
                                                if P.length fs' /= P.length (nub fs') then derror (pack "Declaracion duplicada") else
                                                P.foldr insertf (mapM_ inserte fs >> transDecs xs m) fs
                                                   where
                                                    -- Primer pasada: inserto la interfaz de funciones
                                                    insertf (nm,args,Nothing,_,p)  m' =
                                                        do largs <- mapM (\(_,_,at) -> fromTy at) args
                                                           lvl   <- TTr.topLevel
                                                           let flab = genlab nm p -- function label
                                                               escs = P.map snd3 args -- escape lists
                                                               nlvl = TTr.newLevel lvl flab escs -- new level
                                                           insertFunV nm (nlvl, flab, largs, TUnit, Propia) m'
                                                    insertf (nm,args,Just s,_,p) m' = 
                                                        do largs <- mapM (\(_,_,at) -> fromTy at) args
                                                           t     <- addpos (getTipoT s) p
                                                           lvl   <- TTr.topLevel
                                                           let flab = genlab nm p -- function label
                                                               escs = P.map snd3 args -- escape lists
                                                               nlvl = TTr.newLevel lvl flab escs -- new level
                                                           insertFunV nm (nlvl, flab, largs, t, Propia) m'

                                                    genlab t p = appends [t, pack "_", pack (show $ line p)]
                                                      
                                                    -- Segunda pasada: inserto las exp que pueden usar las funciones
                                                    inserte (nm,args,Nothing,exp,p) =
                                                        do (lvl,_,_,_,_) <- getTipoFunV nm
                                                           preFunctionDec lvl
                                                           largs   <- mapM (\(n,esc,t) -> (n,,) <$> fromTy t <*> TTr.allocArg esc) args
                                                           i       <- getActualLevel
                                                           (be, et) <- P.foldr (\(n,t,a) -> insertValV n (t,a,i)) (transExp exp) largs
                                                           unless (TUnit ?= et) $ addpos (derror (pack "Tipo de exp no es Unit")) p
                                                           fundec <- TTr.functionDec be lvl IsProc
                                                           posFunctionDec
                                                           return fundec
                                                    inserte (nm,args,Just s,exp,p) =
                                                        do (lvl,_,_,_,_) <- getTipoFunV nm
                                                           preFunctionDec lvl
                                                           largs   <- mapM (\(n,esc,t) -> (n,,) <$> fromTy t <*> TTr.allocArg esc) args
                                                           i       <- getActualLevel
                                                           (be, et) <- P.foldr (\(n,t,a) -> insertValV n (t,a,i)) (transExp exp) largs
                                                           t       <- addpos (getTipoT s) p
                                                           bt      <- tiposIguales et t
                                                           unless bt $ addpos (derror (pack "Tipos no compatibles #2")) p
                                                           fundec <- TTr.functionDec be lvl IsFun
                                                           posFunctionDec
                                                           return fundec
transDecs ((TypeDec xs) : xss)               m = do unless (isJust sorted) $ derror (pack "Se ha encontrado un ciclo.")
                                                    makeRefs sorted' $ undoRefs sorted' $ transDecs xss m
                                                    -- insertar todos los xs con posibles referencias
                                                    -- insertar todos los xs limpiando las referencias
                                                    -- continuar analizando las declaraciones (xss)
                                                   where
                                                    ltipos = P.map (\(x,y,_)->(x,y)) xs
                                                    -- Ordenamos los tipos segun kahnSort
                                                    sorted = kahnSorter ltipos
                                                    sorted' = case sorted of
                                                                    Just x -> x
                                                                    _ -> []
                                                    -- Ponemos para insertar primero los records (as refrecords) y luego el resto
                                                    --  que potencialmente use records (y los veran como refrecords)
                                                    -- Los records se insertan "primero" porque khanSort los ve sin dependencias
                                                    makeRefs :: (Manticore w) => [(Symbol, Ty)] -> w a -> w a
                                                    makeRefs []     m' = m'
                                                    makeRefs (x:xs) m' = do (s',t') <- makeRef x
                                                                            insertTipoT s' t' $ makeRefs xs m'
                                                    -- Cambian todos los Records a RefRecords y Ty a Tipo
                                                    makeRef :: (Manticore w) => (Symbol,Ty) -> w (Symbol, Tipo)
                                                    makeRef (s, RecordTy _) = return (s, RefRecord s)
                                                    makeRef (s, t)          = (s, ) <$> transTy t

                                                    -- Ahora limpiamos las referencias
                                                    undoRefs :: (Manticore w) => [(Symbol,Ty)] -> w a -> w a
                                                    undoRefs nicos = undoRefs' nicos (fst $ unzip nicos)

                                                    undoRefs' :: (Manticore w) => [(Symbol, Ty)] -> [Symbol] -> w a -> w a
                                                    undoRefs' []         _ m' = m'
                                                    undoRefs' ((s,RecordTy flds):xs) nicos m' = aux1 flds s nicos $ undoRefs' xs nicos m'
                                                    undoRefs' (_ : xs) nicos m' = undoRefs' xs nicos m'

                                                    aux1 :: (Manticore w) => [(Symbol,Ty)] -> Symbol -> [Symbol] -> w a -> w a
                                                    aux1 flds s nicos m = 
                                                        do flds' <- mapM transTy tys
                                                           uNicos <- ugen
                                                           insertTipoT s (t' uNicos flds') $ propagarS s (t' uNicos flds') nicos m
                                                               where ordered = List.sortBy (Ord.comparing fst) flds
                                                                     (syms , tys) = unzip ordered
                                                                     t' u f = TRecord (zip3 syms (P.map (undoRef s (t' u f)) f) [0..]) u

                                                    propagarS :: (Manticore w) => Symbol -> Tipo -> [Symbol] -> w a -> w a
                                                    propagarS s t [] m = m
                                                    propagarS s t (s' : ss') m = do tip <- getTipoT s' -- >>= \t' -> insertTipoT s' (undoRef s t t')
                                                                                    let t' = undoRef s t tip
                                                                                    insertTipoT s' t' (propagarS s t ss' m)

                                                    -- Cambia todos los RefRecords al nudo anudado
                                                    undoRef :: Symbol -> Tipo -> Tipo -> Tipo
                                                    undoRef sym dummy (TArray t u)      = TArray (undoRef sym dummy t) u
                                                    undoRef sym dummy (TRecord fs u)    = let syms = P.map fst3 fs
                                                                                              tips = P.map snd3 fs
                                                                                              poss = P.map thd3 fs
                                                                                              undone = P.map (undoRef sym dummy) tips
                                                                                          in  TRecord (zip3 syms undone poss) u
                                                    undoRef sym dummy r@(RefRecord sym') | sym == sym' = dummy
                                                                                         | otherwise   =  r
                                                    undoRef _ _ a                = a


fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x
snd3 :: (a,b,c) -> b
snd3 (_,y,_) = y
thd3 :: (a,b,c) -> c
thd3 (_,_,z) = z

transExp :: (MemM w, Manticore w) => Exp -> w (BExp , Tipo)
-- ** transExp :: (Manticore w) => Exp -> w (() , Tipo)
transExp (VarExp v p) = addpos (transVar v) p
transExp UnitExp{} = (,TUnit) <$> TTr.unitExp
transExp NilExp{} = (,TNil) <$> TTr.nilExp
transExp (IntExp i _) = (,TInt RW) <$> (TTr.intExp i)
transExp (StringExp s _) = (,TString) <$> (TTr.stringExp (pack s))
transExp (CallExp nm args p) = do
  TTr.callArgs (P.length args)
  (lvl,lab,targs,ret,ext) <- getTipoFunV nm
  targs' <- mapM transExp args
  mbs <- zipWithM tiposIguales targs (P.map snd targs')
  unless (and mbs && (P.length targs == P.length targs')) $ addpos (derror (pack "Tipos no compatibles #3")) p
  (,ret) <$> TTr.callExp lab ext (if ret == TUnit then IsProc else IsFun) lvl (P.map fst targs')
transExp (OpExp el' oper er' p) = do -- Esta va /gratis/
        (bl, el) <- transExp el'
        (br, er) <- transExp er'
        case oper of
          EqOp -> if tiposComparables el er EqOp then bOps el er bl br oper
                  else addpos (derror (pack "Error de Tipos. Tipos no comparables")) p
          NeqOp ->if tiposComparables el er EqOp then bOps el er bl br oper
                  else addpos (derror (pack "Error de Tipos. Tipos no comparables")) p
          -- Los unifico en esta etapa porque solo chequeamos los tipos, en la próxima
          -- tendrán que hacer algo más interesante.
          PlusOp -> oOps el er bl br oper
          MinusOp -> oOps el er bl br oper
          TimesOp -> oOps el er bl br oper
          DivideOp -> oOps el er bl br oper
          LtOp -> bOps el er bl br oper
          LeOp -> bOps el er bl br oper
          GtOp -> bOps el er bl br oper
          GeOp -> bOps el er bl br oper
          where oOps l r bl br op = if (?=) l r -- Chequeamos que son comparables
                                       && (?=) l (TInt RO) -- y que además es Entero. [Equiv Tipo es una rel de equiv]
                                    then (, TInt RO) <$> (TTr.binOpIntExp bl op br) 
                                    else addpos (derror (pack "Error en el chequeo de una operacion entera.")) p
                bOps l r bl br op = if (?=) l r -- Chequeamos que son comparables
                                    then case l of
                                        TInt _    -> (, TBool) <$> (TTr.binOpIntRelExp bl op br)
                                        TString   -> (, TBool) <$> (TTr.binOpStrExp bl op br)
                                        TRecord _ u1 -> case r of
                                                            TRecord _ u2 -> if u1 == u2 
                                                                            then (, TBool) <$> (TTr.binOpPtrExp bl op br)
                                                                            else addpos (derror (pack "Error en comparacion de Records")) p
                                                            TNil         -> (, TBool) <$> (TTr.binOpPtrExp bl op br)
                                        TNil         -> case r of
                                                            TRecord{}    -> (, TBool) <$> (TTr.binOpPtrExp bl op br)
                                                            TNil         -> addpos (derror (pack "No se puede comparar Nil con Nil")) p
                                        TArray _ u1  -> case r of
                                                            TArray _ u2  -> if u1 == u2
                                                                            then (, TBool) <$> (TTr.binOpPtrExp bl op br)
                                                                            else addpos (derror (pack "Error en comparacion de Arrays")) p
                                    else addpos (derror (pack "Error en el chequeo de una comparacion.")) p

-- | Recordemos que 'RecordExp :: [(Symbol, Exp)] -> Symbol -> Pos -> Exp'
-- Donde el primer argumento son los campos del records, y el segundo es
-- el texto plano de un tipo (que ya debería estar definido). Una expresión
-- de este tipo está creando un nuevo record.
transExp(RecordExp flds rt p) =
  addpos (getTipoT rt) p >>= \case -- Buscamos en la tabla que tipo es 'rt', y hacemos un análisis por casos.
    trec@(TRecord fldsTy _) -> -- ':: TRecord [(Symbol, Tipo, Int)] Unique'
      do
        -- Especial atención acá.
        -- Tenemos una lista de expresiones con efectos
        -- y estos efectos tiene producirse en orden! 'mapM' viene a mano.
        fldsTys <- mapM (\(nm, cod) -> (nm,) <$> transExp cod) flds -- Buscamos los tipos de cada una de los campos.
        -- como resultado tenemos 'fldsTys :: (Symbol, ( CIr , Tipo))'
        -- Lo que resta es chequear que los tipos  sean los mismos, entre los que el programador dio
        -- y los que tienen que ser según la definición del record.
        let ordered = List.sortBy (Ord.comparing fst) fldsTys
            fldsBs = zip (P.map (fst . snd) ordered) [0..]
        -- asumiendo que no nos interesan como el usuario ingresa los campos los ordenamos.
        _ <- flip addpos p $ cmpZip ( (\(s,(c,t)) -> (s,t)) <$> ordered) fldsTy -- Demon corta la ejecución.
        (, trec) <$> (TTr.recordExp fldsBs) -- Si todo fue bien devolvemos trec.
    _ -> flip addpos p $ derror (pack "Error de tipos.")
transExp(SeqExp es p) = do
       es' <- mapM transExp es
       (, snd $ last es') <$> (TTr.seqExp (P.map fst es'))
transExp(AssignExp var val p) = do (bvar , tvar) <- transVar var
                                   when (tvar == TInt RO) $ addpos (derror (pack ("Int " ++ show var ++ " es read-only."))) p
                                   (bval , tval) <- transExp val
                                   bt <- tiposIguales tvar tval
                                   unless bt $ addpos (derror (pack "Tipos no compatibles #4.")) p
                                   (,TUnit) <$> (TTr.assignExp bvar bval)
transExp(IfExp co th Nothing p) = do
        -- ** (ccond , co') <- transExp co
  -- Analizamos el tipo de la condición
        (bco, co') <- transExp co
  -- chequeamos que sea un entero.
        unless ((?=) co' TBool) $ errorTiposMsg p "En la condición del if->" co' TBool -- Claramente acá se puede dar un mejor error.
        -- ** (cth , th') <- transExp th
  -- Analizamos el tipo del branch.
        (bth, th') <- transExp th
  -- chequeamos que sea de tipo Unit.
        unless ((?=) th' TUnit) $ errorTiposMsg p "En el branch del if->" th' TUnit
  -- Si todo fue bien, devolvemos que el tipo de todo el 'if' es de tipo Unit.
        (, TUnit) <$> (TTr.ifThenExp bco bth)
transExp(IfExp co th (Just el) p) = do
  (bco, condType) <- transExp co
  unless ((?=) condType TBool) $ errorTiposMsg p "En la condición del if ->" condType TBool
  (bth, ttType) <- transExp th
  (bel, ffType) <- transExp el
  C.unlessM (tiposIguales ttType ffType) $ errorTiposMsg p "En los branches." ttType ffType
  -- Si todo fue bien devolvemos el tipo de una de las branches.
  if (?=) ttType TUnit
  then (, ttType) <$> (TTr.ifThenElseExpUnit bco bth bel)
  else (, ttType) <$> (TTr.ifThenElseExp bco bth bel)
transExp(WhileExp co body p) = do
  (bco, coTy) <- addpos (transExp co) p
  unless ((?=) coTy TBool) $ errorTiposMsg p "Error en la condición del While" coTy TBool
  TTr.preWhileforExp
  (bbody, boTy) <- addpos (transExp body) p
  unless ((?=) boTy TUnit) $ errorTiposMsg p "Error en el cuerpo del While" boTy TBool
  wex <- TTr.whileExp bco bbody
  TTr.posWhileforExp
  return (wex, TUnit)
transExp(ForExp nv mb lo hi bo p) = do (elo,tlo) <- transExp lo
                                       unless (esInt tlo) $ addpos (derror (pack "Limite inferior no es entero.")) p
                                       (ehi,thi) <- transExp hi
                                       unless (esInt thi) $ addpos (derror (pack "Limite superior no es entero.")) p
                                       TTr.preWhileforExp
                                       acc <- TTr.allocLocal mb
                                       lev <- TTr.getActualLevel
                                       env <- TTr.simpleVar acc lev
                                       (ebo,tbo) <- insertVRO nv acc lev (transExp bo)
                                       b   <- tiposIguales TUnit tbo
                                       unless b $ addpos (derror (pack "El for retorna algo (y no debe).")) p
                                       fex <- TTr.forExp elo ehi env ebo
                                       TTr.posWhileforExp
                                       return (fex,TUnit)
transExp(LetExp dcs body p) = do (bexps,tipo) <- transDecs dcs (transExp body)
                                 (,tipo) <$> TTr.letExp (init bexps) (last bexps)
transExp(BreakExp p) = addpos ((,TUnit) <$> TTr.breakExp) p
transExp(ArrayExp sn cant init p) = do t <- addpos (getTipoT sn) p
                                       case t of
                                           (TArray t' _) -> do (bcant,cant') <- transExp cant
                                                               unless (esInt cant') $ addpos (derror (pack "Tamanio no es entero.")) p
                                                               (binit,init') <- transExp init
                                                               b2 <- tiposIguales t' init'
                                                               unless b2 $ addpos (derror (pack "Valores iniciales incompatibles.")) p
                                                               (,t) <$> (TTr.arrayExp bcant binit)
                                           _             -> addpos (derror (pack "El array no tiene tipo ahrey")) p


{-
-- Viejo estado, acopla environment y frame! (BORRAR)
data Estado = Est {
                vEnv     :: M.Map Symbol EnvEntry 
                , tEnv   :: M.Map Symbol Tipo
                , level  :: [Level]
                , salida :: [Maybe Label]
                , frag   :: [Frag]
                , maxlvl :: Int
                }
    deriving Show
-- data EstadoG = G {vEnv :: [M.Map Symbol EnvEntry], tEnv :: [M.Map Symbol Tipo]}
--     deriving Show


--
-- Estado Inicial con los entornos
-- * int y string como tipos básicos. -> tEnv
-- * todas las funciones del *runtime* disponibles. -> vEnv
--
initConf :: Estado
initConf = Est
           { tEnv = M.insert (pack "int") (TInt RW) (M.singleton (pack "string") TString)
           , vEnv = M.fromList
                    [(pack "print", Func (TTr.outermost,pack "print",[TString], TUnit, Runtime))
                    ,(pack "flush", Func (TTr.outermost,pack "flush",[],TUnit, Runtime))
                    ,(pack "getchar",Func (TTr.outermost,pack "getchar",[],TString,Runtime))
                    ,(pack "ord",Func (TTr.outermost,pack "ord",[TString],TInt RW,Runtime))
                    ,(pack "chr",Func (TTr.outermost,pack "chr",[TInt RW],TString,Runtime))
                    ,(pack "size",Func (TTr.outermost,pack "size",[TString],TInt RW,Runtime))
                    ,(pack "substring",Func (TTr.outermost,pack "substring",[TString,TInt RW, TInt RW],TString,Runtime))
                    ,(pack "concat",Func (TTr.outermost,pack "concat",[TString,TString],TString,Runtime))
                    ,(pack "not",Func (TTr.outermost,pack "not",[TBool],TBool,Runtime))
                    ,(pack "exit",Func (TTr.outermost,pack "exit",[TInt RW],TUnit,Runtime))
                    ]
           , level  = [TTr.outermost]
           , salida = []
           , frag   = []
           , maxlvl = -1
           }
-}

-- Separamos estado de Environments de estado de Frames:
data Env = Env {
  vEnv     :: M.Map Symbol EnvEntry 
  , tEnv   :: M.Map Symbol Tipo
} deriving Show
data Fra = Fra {
  level  :: [Level]
  , salida :: [Maybe Label]
  , frag   :: [Frag]
  , maxlvl :: Int
} deriving Show

-- Y los juntamos en un estado común
data Estado = Est {
  env :: Env
  , fra :: Fra
} deriving Show

--
-- Estado Inicial con los entornos
-- env: * int y string como tipos básicos. -> tEnv
--      * todas las funciones del *runtime* disponibles. -> vEnv
-- fra: * acumulación de niveles -> level
--      * etiquetas de salida de expresiones -> salida
--      * acumulado de fragmentos (funciones y strings) -> frag
--      * nivel actual/maximo -> maxlvl
--
initConf :: Estado
initConf = Est {
  env = Env { tEnv = M.insert (pack "int") (TInt RW) (M.singleton (pack "string") TString)
              , vEnv = M.fromList
                [(pack "print", Func (TTr.outermost,pack "print",[TString], TUnit, Runtime))
                ,(pack "flush", Func (TTr.outermost,pack "flush",[],TUnit, Runtime))
                ,(pack "getchar",Func (TTr.outermost,pack "getchar",[],TString,Runtime))
                ,(pack "ord",Func (TTr.outermost,pack "ord",[TString],TInt RW,Runtime))
                ,(pack "chr",Func (TTr.outermost,pack "chr",[TInt RW],TString,Runtime))
                ,(pack "size",Func (TTr.outermost,pack "size",[TString],TInt RW,Runtime))
                ,(pack "substring",Func (TTr.outermost,pack "substring",[TString,TInt RW, TInt RW],TString,Runtime))
                ,(pack "concat",Func (TTr.outermost,pack "concat",[TString,TString],TString,Runtime))
                ,(pack "not",Func (TTr.outermost,pack "not",[TBool],TBool,Runtime))
                ,(pack "exit",Func (TTr.outermost,pack "exit",[TInt RW],TUnit,Runtime))
                ]
            }
  , fra = Fra { level  = [TTr.outermost]
              , salida = []
              , frag   = []
              , maxlvl = 0
              }
}


-- Utilizando alguna especie de run de la monada definida, obtenemos algo así
type Monada = ExceptT Symbol (StateT Estado StGen)
  -- StateT Estado (ExceptT Symbol StGen)

instance Demon Monada where
  -- | 'throwE' de la mónada de excepciones.
  derror =  throwE
  -- adder :: w a -> Symbol -> w a
  adder m s = catchE m (throwE . flip append s)

instance Manticore Monada where
  -- | A modo de ejemplo esta es una opción de ejemplo de 'insertValV :: Symbol -> ValEntry -> w a -> w'
    insertValV sym ventry m = do
      -- | Guardamos el estado actual
      oldEst <- get
      -- | Insertamos la variable al entorno (sobrescribiéndolo)
      put (oldEst{ env = (env oldEst){vEnv = M.insert sym (Var ventry) (vEnv (env oldEst)) }})
      -- | ejecutamos la computación que tomamos como argumentos una vez que expandimos el entorno
      a <- m
      -- | Volvemos a poner el entorno viejo
      newEst <- get
      put (newEst{ env = (env oldEst) })
      -- | retornamos el valor que resultó de ejecutar la monada en el entorno expandido.
      return a
  -- | Inserta una Función al entorno
  --   insertFunV :: Symbol -> FunEntry -> w a -> w a
    insertFunV sym fentry m = do
      oldEst <- get
      put (oldEst{ env = (env oldEst){vEnv = M.insert sym (Func fentry) (vEnv (env oldEst)) }})
      a <- m
      newEst <- get
      put (newEst { env = (env oldEst) })
      return a
  -- | Inserta una Variable de sólo lectura al entorno
  --   insertVRO :: Symbol -> Access -> Int -> w a -> w a
    insertVRO sym acc lev m = do
      oldEst <- get
      put (oldEst{ env = (env oldEst){vEnv = M.insert sym (Var (TInt RO,acc,lev)) (vEnv (env oldEst)) }})
      a <- m
      newEst <- get
      put (newEst { env = (env oldEst) })
      return a
  -- | Inserta una variable de tipo al entorno
  --   insertTipoT :: Symbol -> Tipo -> w a -> w a
    insertTipoT sym tentry m = do
      oldEst <- get
      put (oldEst{ env = (env oldEst){tEnv = M.insert sym tentry (tEnv (env oldEst)) }})
      a <- m
      newEst <- get
      put (newEst { env = (env oldEst) })
      return a
  -- | Busca una función en el entorno
  --   getTipoFunV :: Symbol -> w FunEntry
    getTipoFunV sym = do
      est <- get
      maybe (derror (pack ("No se encontro el tipo de la fun "++ show sym ++ " en el map."))) 
            (\case (Func f) -> return f ; _ -> (derror (pack "Undefined Fun"))) (M.lookup sym (vEnv (env est)))
  -- | Busca una variable en el entorno. Ver [1]
  --   getTipoValV :: Symbol -> w ValEntry
    getTipoValV sym = do
      est <- get
      maybe (derror (pack ("No se encontro el tipo de la var " ++ show sym ++ " en el map."))) 
            (\case (Var f) -> return f ; _ -> (derror (pack "Undefined Var"))) (M.lookup sym (vEnv (env est)))
  -- | Busca un tipo en el entorno
  --   getTipoT :: Symbol -> w Tipo
    getTipoT sym = do
      est <- get
      maybe (derror (pack ("No se encontro el tipo "++ show sym ++ " en el map."))) 
            (\t -> return t) (M.lookup sym (tEnv (env est)))
  -- | Funciones de Debugging!
  --   showVEnv :: w a -> w a
    showVEnv w = gets (vEnv . env) >>= flip trace w . show
  --   showTEnv :: w a -> w a
    showTEnv w = gets (tEnv . env) >>= flip trace w . show
    ugen = mkUnique

instance MemM Monada where
    -- | Level management
    -- Es un entero que nos indica en qué nivel estamos actualmente.
    --upLvl :: w () 
    upLvl = do
      st <- get
      put (st{ fra = (fra st){ maxlvl = (maxlvl (fra st) + 1) }})
    --downLvl :: w ()
    downLvl = do
      st <- get
      put (st{ fra = (fra st){ maxlvl = (maxlvl (fra st) - 1) }})
    -- | Salida management.
    -- Esta etiqueta la necesitamos porque es la que nos va permitir saltar a la
    -- salida de un while (Ver código intermedio de While). Usada en el break.
    --pushSalida :: Maybe Label -> w ()
    pushSalida ml = do
      st <- get
      put (st{ fra = (fra st){ salida = ml : (salida (fra st)) }})
    --topSalida :: w (Maybe Label)
    topSalida = do
      st <- get
      case salida (fra st) of
        [] -> derror (pack "Unethical break.")
        _  -> return $ head $ salida (fra st)
    --popSalida :: w ()
    popSalida = do
      st <- get
      put (st{ fra = (fra st){ salida = tail $ salida (fra st) }})
    -- | Level management Cont. El nivel en esta etapa es lo que llamamos el
    -- marco de activación virtual o dinámico (no me acuerdo). Pero es lo que
    -- eventualmente va a ser el marco de activación
    --pushLevel :: Level -> w ()
    pushLevel lvl = do
      st <- get
      put (st{ fra = (fra st){ level = lvl : (level (fra st)), maxlvl = getNlvl lvl }})
    --popLevel  :: w ()
    popLevel = do
      st <- get
      put (st{ fra = (fra st){ level = tail $ level (fra st) }})
    --topLevel  :: w Level
    topLevel = do
      st <- get
      return $ head $ level (fra st)
    -- | Frag management
    -- Básicamente los fragmentos van a ser un efecto lateral de la computación.
    -- Recuerden que los fragmentos son pedazos de código intermedio que se van
    -- a ejecutar. Y estos son un efecto lateral porque todavía no sabemos bien
    -- cómo van a ser ejecutados (eso se decide más adelante)
    --pushFrag  :: Frag -> w ()
    pushFrag fr = do
      st <- get
      put (st{ fra = (fra st){ frag = fr : (frag (fra st)) }})
    --getFrags  :: w [Frag]
    getFrags = do
      st <- get
      return $ frag (fra st)

runMonada :: Monada (BExp, Tipo) -> StGen (Either Symbol (BExp, Tipo))
runMonada =  flip evalStateT initConf . runExceptT

runSeman :: Exp -> StGen (Either Symbol (BExp, Tipo))
runSeman = runMonada . transExp

transProc :: (Manticore w, MemM w) => Exp -> w [Frag]
transProc ex = do
  let pos = Simple {line = 0, col = 0}
  let procExp = LetExp
        [FunctionDec [(pack "_tigermain", [], Just (pack "int"), ex, pos)]]
        (IntExp 0 pos)
        pos
  transExp procExp
  getFrags

runMonadaF :: Monada [Frag] -> StGen (Either Symbol [Frag])
runMonadaF =  flip evalStateT initConf . runExceptT

runFrags :: Exp -> StGen (Either Symbol [Frag])
runFrags e = runMonadaF (transProc e)