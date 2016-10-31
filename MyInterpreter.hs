module MyInterpreter where

import qualified Data.Map as M
import AbsGramatyka
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Reader
import System.IO ( stderr, hPutStrLn )
import TypeCheck

type Lam a = [Loc] -> Interpreter a

data MyVal
    = MyInt Int
    | MyBool Bool
    | MyLam (Lam ())

instance Eq MyVal where
    (MyInt n) == (MyInt m) = n == m
    (MyBool b) == (MyBool c) = b == c
    _ == _ = False

instance Show MyVal where
    show (MyInt n) = show n
    show (MyBool b) = show b
    show (MyLam l) = "lambda"

type Loc = (Int, Type)
type Proc = Loc -> Loc
type Env =  M.Map Ident Loc
type Store = M.Map Loc MyVal

type MyError = ErrorT String IO
type MyStatefulErrorComp = StateT Store MyError
type Interpreter = ReaderT Env MyStatefulErrorComp

showStore :: Store -> String
showStore = show . M.toList

getOp :: EqRel -> Exp -> Exp -> Interpreter MyVal
getOp op e1 e2 = do
                    v1 <- eval e1
                    v2 <- eval e2
                    let realop = (case op of { BEq -> (==); BNeq -> (/=) }) 
                    return $ MyBool $ realop v1 v2
getRel :: OrdRel -> Exp -> Exp -> Interpreter MyVal
getRel op e1 e2 = do
                    v1 <- evalInt e1
                    v2 <- evalInt e2
                    let realop = case op of
                            OLt -> (<)
                            OLet -> (<=)
                            OGt -> (>)
                            OGet -> (>=)
                    return $ MyBool $ realop v1 v2

evalInt :: Exp -> Interpreter Int
evalInt e = do
                v <- eval e
                case v of
                    MyInt n -> return n
                    _ -> throwError "Runtime type mismatch. Int expectation failed."

evalBool :: Exp -> Interpreter Bool
evalBool e = do
                v <- eval e
                case v of
                    MyBool b -> return b
                    _ -> throwError "Runtime type mismatch. Bool expectation failed."

getLoc :: (Exp, Type) -> Interpreter Loc
getLoc (e, t) = do
                    env <- ask
                    if isRefType t then
                        case e of
                            EVar x -> return $ env M.! x
                            _ -> throwError "Value passed instead of reference to a function."
                    else
                        fmap (fmap $ \_ -> t) alloc


eval :: Exp -> Interpreter MyVal
eval (EInt n) = return $ MyInt $ fromInteger n
eval (EVar x) = do
                    env <- ask
                    store <- get
                    loc <- case M.lookup x env of
                                Nothing -> throwError $ show x ++ " was not declared in this scope."
                                Just l -> return l
                    val <- case M.lookup loc store of
                                Nothing -> throwError $ show x ++ " has not been initialised."
                                Just v -> return v
                    return val
eval (ELam (FList xs) _ (Prog ds is)) = do 
                                        --liftIO $ print xs
                                        env <- ask
                                        return $ MyLam $ f env
                                        where   f env locs = local (\_ -> M.union new env) (exec $ SBlock ds is)
                                                    where new = M.fromList $ flip zip locs $ (Ident "if"):xs'
                                                          xs' = map (\(DVar x t) -> x) xs

eval (EFun fid (PList es)) = do
                            --liftIO $ putStrLn $ show fid ++ " ewaluacja!"
                            env <- ask
                            store <- get
                            --liftIO $ print env
                            --liftIO $ print store
                            loc <- case M.lookup fid env of
                                        Nothing -> throwError $ show fid ++ " LOL was not declared in this scope."
                                        Just l@(_, (TFun _ _)) -> return l
                                        _ -> throwError $ show fid ++ " is not a function."
                            fun <- case M.lookup loc store of
                                        Nothing -> throwError $ show fid ++ " has not been initialised."
                                        Just (MyLam f) -> return f
                                        _ -> throwError $ show fid ++ " is not a function."
                            vals <- mapM eval es -- if reference to unassigned or undeclared variable, it will throw error here
                            let (_, (TFun paramsTypes retType)) = loc
                            --let areRef = map isRefType paramsTypes
                            ----liftIO $ print areRef
                            paramRet <- fmap (fmap (\_-> retType)) alloc
                            paramLocs <- liftM2 (:) (return paramRet) $ mapM getLoc $ zip es paramsTypes
                            --liftIO $ print (paramLocs, vals)
                            --paramLocs <- mapM (\_ -> alloc) es
                            modify (M.union $ M.fromList $ zip paramLocs $ (MyInt 12345):vals)
                            fun paramLocs
                            store <- get
                            val <- case M.lookup paramRet store of
                                        Nothing -> return $ MyInt 12345
                                        Just l -> return l
                            return val
eval (EAdd e1 e2) = fmap MyInt $ liftM2 (+) (evalInt e1) (evalInt e2)
eval (ESub e1 e2) = fmap MyInt $ liftM2 (-) (evalInt e1) (evalInt e2)
eval (EMul e1 e2) = fmap MyInt $ liftM2 (*) (evalInt e1) (evalInt e2)
eval (EDiv e1 e2) = do
                        v1 <- evalInt e1
                        v2 <- evalInt e2
                        if v2 == 0 then
                            throwError "Division by zero exception."
                        else
                            return $ MyInt $ div v1 v2
eval (EBool b) = return $ MyBool $ case b of { BTrue -> True; BFalse -> False }
eval (EERel e1 op e2) = getOp op e1 e2
eval (EORel e1 op e2) = getRel op e1 e2
eval (EOr b1 b2) = fmap MyBool $ liftM2 (||) (evalBool b1) (evalBool b2)
eval (EAnd b1 b2) = fmap MyBool $ liftM2 (&&) (evalBool b1) (evalBool b2)
eval (EUn op e) = do
                    val <- eval e
                    case val of
                        MyInt n -> case op of
                                    WPlus -> return $ MyInt n
                                    WMin -> return $ MyInt $ -n
                                    _ -> throwError "Invalid unary operator."
                        MyBool b -> case op of
                                    WNot -> return $ MyBool $ not b
                                    _ -> throwError "Invalid unary operator."
                        _ -> throwError "Invalid unary operator."

alloc :: Interpreter Loc
alloc = do
            locNum <- get
            let nextVal = case locNum M.! ((-1),TInt) of
                            MyInt n -> n
            modify $ M.insert ((-1),TInt) $ MyInt $ nextVal + 1
            return (nextVal, TInt)

isRefType :: Type -> Bool
isRefType (TRef _) = True
isRefType _ = False

typeToRefList :: Type -> [Bool]
typeToRefList (TFun params ret) = map isRefType params
typeToRefList _ = []

decl :: Decl -> Interpreter a -> Interpreter a
decl (DVar x t) int = do
                        l@(newLoc, _) <- alloc
                        local (M.insert x (newLoc, t)) int

execList :: [Stmt] -> Interpreter ()
execList is = foldM (\_ -> exec) () is

exec :: Stmt -> Interpreter ()
exec (SAssign x op e) = do
                        env <- ask
                        store <- get
                        loc <- case M.lookup x env of
                                    Nothing -> throwError $ show x ++ " was not declared in this scope."
                                    Just l -> return l
                        val <- case op of
                                    ASimple -> eval e
                                    otherop -> eval $ f (EVar x) e
                                                    where f = M.fromList [(AAdd, EAdd), (ASub, ESub), (AMul, EMul), (ADiv, EDiv)] M.! otherop
                        modify (M.insert loc val)
exec (SReturn e) = do
                    env <- ask
                    store <- get
                    loc <- case M.lookup (Ident "if") env of
                                Nothing -> throwError $ "Return can not be called outside of a function."
                                Just l -> return l
                    val <- eval e
                    modify (M.insert loc val)
exec (SUnary x op) = exec $ SAssign x AAdd $ EInt $ case op of { UInc -> 1; UDec -> -1 }
exec (SPrint e) = eval e >>= liftIO . putStrLn . show
exec (SIfte b s1 s2) = evalBool b >>= (\v -> if v then exec s1 else exec s2)
exec (SIf b s1) = exec $ SIfte b s1 $ SBlock [] []
exec w@(SWhile b i) = evalBool b >>= (\v -> if v then do { exec i; exec w; } else return ())
exec (SBlock ds is) = foldr decl (execList is) ds

runProgram :: Program -> IO ()
runProgram p@(Prog ds is) = do
                                res <- runTypeCheck p
                                case res of
                                    Left err -> hPutStrLn stderr $ "The type check ended with an error: " ++ err
                                    Right _ -> do
                                        result <- runErrorT $ flip runStateT (M.fromList [((-1,TInt), MyInt 0)]) $ flip runReaderT M.empty $ exec (SBlock ds is)
                                        case result of
                                            Left err -> hPutStrLn stderr $ "The program ended with an error: " ++ err
                                            Right (_, store) -> return ()--putStrLn $ showStore store
