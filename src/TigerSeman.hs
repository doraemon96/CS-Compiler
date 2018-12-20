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

-- Le doy nombre al Preludio.
import           Prelude                    as P

-- Debugging. 'trace :: String -> a -> a'
-- imprime en pantalla la string cuando se ejecuta.
import           Debug.Trace                (trace, traceM)

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
    insertVRO :: Symbol -> w a -> w a
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

buscarM :: Symbol -> [(Symbol, Tipo, Int)] -> Maybe Tipo
buscarM s [] = Nothing
buscarM s ((s',t,_):xs) | s == s' = Just t
                        | otherwise = buscarM s xs

-- | __Completar__ 'transVar'.
-- El objetivo de esta función es obtener el tipo
-- de la variable a la que se está __accediendo__.
transVar :: (MemM w, Manticore w) => Var -> w (BExp, Tipo)
-- Leer Nota [1] para SimpleVar
-- ** transVar :: (Manticore w) => Var -> w ( () , Tipo)
transVar (SimpleVar s)      = ((),) <$> getTipoValV s
transVar (FieldVar v s)     = transVar v >>= \case
                                    (_ , TRecord lt _) -> maybe (derror (pack "Not a record field")) (return . ((),)) (buscarM s lt)
                                    _                  -> derror $ pack "Not a record var"
transVar (SubscriptVar v e) = transVar v >>= \case
                                    (_ , TArray t _) -> transExp e >>= \case
                                                            (() , TInt _) -> return ((),t)
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
transTy (NameTy s)      = getTipoT s --TODO chequear (esta bien? Siempre tengo a s en mi manticore?)
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
-- ** transDecs :: (MemM w, Manticore w) => [Dec] -> w a -> w a

transDecs :: (Manticore w) => [Dec] -> w a -> w a
transDecs [] m = m
transDecs ((VarDec nm escap Nothing init p): xs) m = do (_,et) <- transExp init
                                                        insertValV nm et (transDecs xs m)
transDecs ((VarDec nm escap (Just t) init p): xs) m = do (_,et) <- transExp init
                                                         wt     <- addpos (getTipoT t) p
                                                         bt     <- tiposIguales et wt
                                                         if bt then addpos (insertValV nm wt (transDecs xs m)) p
                                                         else addpos (derror (pack "Tipos no compatibles #1")) p
transDecs ((FunctionDec fs) : xs)           m = let fs' = P.map (\ (nm , _, _ ,_ , _) -> nm) fs in
                                                --TODO: agregar pos a la dup dec
                                                if P.length fs' /= P.length (nub fs') then derror (pack "Declaracion duplicada") else
                                                P.foldr insertf (mapM_ inserte fs >> transDecs xs m) fs
                                                   where
                                                    -- Primer pasada: inserto la interfaz de funciones
                                                    insertf (nm,args,Nothing,_,p)  m' =
                                                        do largs <- mapM (\(_,_,at) -> fromTy at) args
                                                           insertFunV nm (0, genlab nm p, largs, TUnit, Propia) m'
                                                    insertf (nm,args,Just s,_,p) m' = 
                                                        do largs <- mapM (\(_,_,at) -> fromTy at) args
                                                           t     <- addpos (getTipoT s) p
                                                           insertFunV nm (0, genlab nm p, largs, t, Propia) m'

                                                    --aux :: (Manticore w) => [Symbol] -> [Tipo] -> w a -> w a
                                                    aux []     []     m'' = m''
                                                    aux (n:ns) (t:ts) m'' = insertValV n t (aux ns ts m'')

                                                    genlab t p = pack $ show t ++ "_" ++ show p

                                                    -- Segunda pasada: inserto las exp que pueden usar las funciones
                                                    inserte (_,args,Nothing,exp,p) =
                                                        do largs <- mapM (\(n,_,t) -> (n,) <$> fromTy t) args
                                                           (_, et) <- P.foldr (uncurry insertValV) (transExp exp) largs
                                                           unless (TUnit ?= et) $ addpos (derror (pack "Tipo de exp no es Unit")) p
                                                    inserte (_,args,Just s,exp,p) =
                                                        do largs <- mapM (\(n,_,t) -> (n,) <$> fromTy t) args
                                                           (_, et) <- P.foldr (uncurry insertValV) (transExp exp) largs
                                                           t      <- addpos (getTipoT s) p
                                                           bt     <- tiposIguales et t
                                                           unless bt $ addpos (derror (pack "Tipos no compatibles #2")) p
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
                                                                     t' u f = TRecord (zip3 syms (P.map (undoRef s (t' u f)) f) [0..]) u --TODO: cambiar nombres

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
transExp (CallExp nm args p) = do (_,_,targs,ret,_) <- getTipoFunV nm
                                  targs' <- mapM transExp args
                                  mbs <- zipWithM tiposIguales targs (P.map snd targs')
                                  unless (and mbs && (P.length targs == P.length targs')) $ addpos (derror (pack "Tipos no compatibles #3")) p
                                  return ((),ret)
transExp (OpExp el' oper er' p) = do -- Esta va /gratis/
        (_ , el) <- transExp el'
        (_ , er) <- transExp er'
        case oper of
          EqOp -> if tiposComparables el er EqOp then bOps el er
                  else addpos (derror (pack "Error de Tipos. Tipos no comparables")) p
          NeqOp ->if tiposComparables el er EqOp then bOps el er
                  else addpos (derror (pack "Error de Tipos. Tipos no comparables")) p
          -- Los unifico en esta etapa porque solo chequeamos los tipos, en la próxima
          -- tendrán que hacer algo más interesante.
          PlusOp -> oOps el er
          MinusOp -> oOps el er
          TimesOp -> oOps el er
          DivideOp -> oOps el er
          LtOp -> bOps el er
          LeOp -> bOps el er
          GtOp -> bOps el er
          GeOp -> bOps el er
          where oOps l r = if (?=) l r -- Chequeamos que son el mismo tipo
                              && (?=) l (TInt RO) -- y que además es Entero. [Equiv Tipo es una rel de equiv]
                           then return ((), TInt RO)
                           else addpos (derror (pack "Error en el chequeo de una comparación entera.")) p
                bOps l r = if (?=) l r -- Chequeamos que son el mismo tipo
                           then return ((), TBool)
                           else addpos (derror (pack "Error en el chequeo de una comparación no entera.")) p

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
        -- asumiendo que no nos interesan como el usuario ingresa los campos los ordenamos.
        _ <- flip addpos p $ cmpZip ( (\(s,(c,t)) -> (s,t)) <$> ordered) fldsTy -- Demon corta la ejecución.
        return ((), trec) -- Si todo fue bien devolvemos trec.
    _ -> flip addpos p $ derror (pack "Error de tipos.")
transExp(SeqExp es p) = fmap last (mapM transExp es)
  -- last <$> mapM transExp es
-- ^ Notar que esto queda así porque no nos interesan los
-- units intermedios. Eventualmente vamos a coleccionar los códigos intermedios y se verá algo similar a:
-- do
--       es' <- mapM transExp es
--       return ( () , snd $ last es')
transExp(AssignExp var val p) = do (_ , tvar) <- transVar var
                                   when (tvar == TInt RO) $ addpos (derror (pack ("Int " ++ show var ++ " es read-only."))) p
                                   (_ , tval) <- transExp val
                                   bt <- tiposIguales tvar tval
                                   unless bt $ addpos (derror (pack "Tipos no compatibles #4.")) p
                                   return ((),TUnit)
transExp(IfExp co th Nothing p) = do
        -- ** (ccond , co') <- transExp co
  -- Analizamos el tipo de la condición
        (_ , co') <- transExp co
  -- chequeamos que sea un entero.
        unless ((?=) co' TBool) $ errorTiposMsg p "En la condición del if->" co' TBool -- Claramente acá se puede dar un mejor error.
        -- ** (cth , th') <- transExp th
  -- Analizamos el tipo del branch.
        (() , th') <- transExp th
  -- chequeamos que sea de tipo Unit.
        unless ((?=) th' TUnit) $ errorTiposMsg p "En el branch del if->" th' TUnit
  -- Si todo fue bien, devolvemos que el tipo de todo el 'if' es de tipo Unit.
        return (() , TUnit)
transExp(IfExp co th (Just el) p) = do
  (_ , condType) <- transExp co
  unless ((?=) condType TBool) $ errorTiposMsg p "En la condición del if ->" condType TBool
  (_, ttType) <- transExp th
  (_, ffType) <- transExp el
  C.unlessM (tiposIguales ttType ffType) $ errorTiposMsg p "En los branches." ttType ffType
  -- Si todo fue bien devolvemos el tipo de una de las branches.
  return ((), ttType)
transExp(WhileExp co body p) = do
  (_ , coTy) <- transExp co
  unless ((?=) coTy TBool) $ errorTiposMsg p "Error en la condición del While" coTy TBool
  (_ , boTy) <- transExp body
  unless ((?=) boTy TUnit) $ errorTiposMsg p "Error en el cuerpo del While" boTy TBool
  return ((), TUnit)
transExp(ForExp nv mb lo hi bo p) = do (elo,tlo) <- transExp lo
                                       unless (esInt tlo) $ addpos (derror (pack "Limite inferior no es entero.")) p
                                       (ehi,thi) <- transExp hi
                                       unless (esInt thi) $ addpos (derror (pack "Limite superior no es entero.")) p
				       (env,tnv) <- transVar (SimpleVar nv) --CONSULTAR
				       TTr.preWhileforExp
                                       (ebo,tbo) <- insertVRO nv (transExp bo)
                                       b <- tiposIguales TUnit tbo
                                       unless b $ addpos (derror (pack "El for retorna algo (y no debe).")) p
                                       (,TUnit) <$> TTr.forExp elo ehi env ebo
transExp(LetExp dcs body p) = transDecs dcs (transExp body)
transExp(BreakExp p) = addpos ((,TUnit) <$> TTr.breakExp) p
transExp(ArrayExp sn cant init p) = do t <- addpos (getTipoT sn) p
                                       case t of
                                           (TArray t' _) -> do (_,cant') <- transExp cant
                                                               unless (esInt cant') $ addpos (derror (pack "Tamanio no es entero.")) p
                                                               (_,init') <- transExp init
                                                               b2 <- tiposIguales t' init'
                                                               unless b2 $ addpos (derror (pack "Valores iniciales incompatibles.")) p
                                                               return ((),t)
                                           _             -> addpos (derror (pack "El array no tiene tipo ahrey")) p

-- Un ejemplo de estado que alcanzaría para realizar todas la funciones es:
data Estado = Est {vEnv :: M.Map Symbol EnvEntry, tEnv :: M.Map Symbol Tipo}
    deriving Show
-- data EstadoG = G {vEnv :: [M.Map Symbol EnvEntry], tEnv :: [M.Map Symbol Tipo]}
--     deriving Show
--
-- Estado Inicial con los entornos
-- * int y string como tipos básicos. -> tEnv
-- * todas las funciones del *runtime* disponibles. -> vEnv
initConf :: Estado
initConf = Est
           { tEnv = M.insert (pack "int") (TInt RW) (M.singleton (pack "string") TString)
           , vEnv = M.fromList
                    [(pack "print", Func (1,pack "print",[TString], TUnit, Runtime))
                    ,(pack "flush", Func (1,pack "flush",[],TUnit, Runtime))
                    ,(pack "getchar",Func (1,pack "getchar",[],TString,Runtime))
                    ,(pack "ord",Func (1,pack "ord",[TString],TInt RW,Runtime))
                    ,(pack "chr",Func (1,pack "chr",[TInt RW],TString,Runtime))
                    ,(pack "size",Func (1,pack "size",[TString],TInt RW,Runtime))
                    ,(pack "substring",Func (1,pack "substring",[TString,TInt RW, TInt RW],TString,Runtime))
                    ,(pack "concat",Func (1,pack "concat",[TString,TString],TString,Runtime))
                    ,(pack "not",Func (1,pack "not",[TBool],TBool,Runtime))
                    ,(pack "exit",Func (1,pack "exit",[TInt RW],TUnit,Runtime))
                    ]
           }

-- Utilizando alguna especie de run de la monada definida, obtenemos algo así
type Monada = ExceptT Symbol (StateT Estado StGen)
  -- StateT Estado (ExceptT Symbol StGen)

instance Demon Monada where
  -- | 'throwE' de la mónada de excepciones.
  derror =  throwE
  -- TODO: Parte del estudiante
  -- adder :: w a -> Symbol -> w a
  adder m s = catchE m (throwE . flip append s)

instance Manticore Monada where
  -- | A modo de ejemplo esta es una opción de ejemplo de 'insertValV :: Symbol -> ValEntry -> w a -> w'
    insertValV sym ventry m = do
      -- | Guardamos el estado actual
      oldEst <- get
      -- | Insertamos la variable al entorno (sobrescribiéndolo)
      put (oldEst{ vEnv = M.insert sym (Var ventry) (vEnv oldEst) })
      -- | ejecutamos la computación que tomamos como argumentos una vez que expandimos el entorno
      a <- m
      -- | Volvemos a poner el entorno viejo
      put oldEst
      -- | retornamos el valor que resultó de ejecutar la monada en el entorno expandido.
      return a
  -- TODO: Parte del estudiante
  -- | Inserta una Función al entorno
  --   insertFunV :: Symbol -> FunEntry -> w a -> w a
    insertFunV sym fentry m = do
      oldEst <- get
      put (oldEst{ vEnv = M.insert sym (Func fentry) (vEnv oldEst) })
      a <- m
      put oldEst
      return a
  -- | Inserta una Variable de sólo lectura al entorno
  --   insertVRO :: Symbol -> w a -> w a
    insertVRO sym m = do
      oldEst <- get
      put (oldEst{ vEnv = M.insert sym (Var (TInt RO)) (vEnv oldEst)})
      a <- m
      put oldEst
      return a
  -- | Inserta una variable de tipo al entorno
  --   insertTipoT :: Symbol -> Tipo -> w a -> w a
    insertTipoT sym tentry m = do
      oldEst <- get
      put (oldEst{ tEnv = M.insert sym tentry (tEnv oldEst)})
      a <- m
      put oldEst
      return a
  -- | Busca una función en el entorno
  --   getTipoFunV :: Symbol -> w FunEntry
    getTipoFunV sym = do
      est <- get
      maybe (derror (pack ("No se encontro el tipo de la fun "++ show sym ++ " en el map."))) 
            (\(Func f) -> return f) (M.lookup sym (vEnv est))
  -- | Busca una variable en el entorno. Ver [1]
  --   getTipoValV :: Symbol -> w ValEntry
    getTipoValV sym = do
      est <- get
      maybe (derror (pack ("No se encontro el tipo de la var " ++ show sym ++ " en el map."))) 
            (\(Var f) -> return f) (M.lookup sym (vEnv est))
  -- | Busca un tipo en el entorno
  --   getTipoT :: Symbol -> w Tipo
    getTipoT sym = do
      est <- get
      maybe (derror (pack ("No se encontro el tipo "++ show sym ++ " en el map."))) 
            (\t -> return t) (M.lookup sym (tEnv est))
  -- | Funciones de Debugging!
  --   showVEnv :: w a -> w a
    showVEnv w = gets vEnv >>= flip trace w . show
  --   showTEnv :: w a -> w a
    showTEnv w = gets tEnv >>= flip trace w . show
    ugen = mkUnique


runMonada :: Monada ((), Tipo)-> StGen (Either Symbol ((), Tipo))
runMonada =  flip evalStateT initConf . runExceptT

runSeman :: Exp -> StGen (Either Symbol ((), Tipo))
runSeman = runMonada . transExp
