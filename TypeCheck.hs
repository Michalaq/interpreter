module TypeCheck where

import qualified Data.Map as M
import AbsGramatyka
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Reader
import Data.Function

type Env2 =  M.Map Ident Type

type TypeCheckResult = ErrorT String IO
type TypeChecker = ReaderT Env2 TypeCheckResult

checkBoth :: Exp -> Exp -> Type -> TypeChecker Type
checkBoth e1 e2 t = do
                        t1 <- evalType e1
                        t2 <- evalType e2
                        if (t1 == t2) && (t == t1) then
                            return t
                        else
                            throwError $ (show e1) ++ ", " ++ (show e2) ++ ": types do not match."

evalType :: Exp -> TypeChecker Type
evalType (EInt n) = return $ TInt
evalType (EVar x) = do
                    env <- ask
                    t <- case M.lookup x env of
                                Nothing -> throwError $ show x ++ " was not declared in this scope."
                                Just t -> return t
                    return t
evalType (ELam (FList xs) t (Prog ds is)) = do -- returns RETURN type only, should be executed in a modified env regarding parameters
                                            let t' = unpackRef t
                                            let paramTypes = map (\(DVar _ t) -> flattenRef t) xs
                                            let ds' = map (\(DVar x t) -> DVar x $ unpackRef t) xs
                                            retTypes <- evalStmtType $ SBlock (ds ++ ds') is
                                            case retTypes of
                                                [] -> return $ TFun paramTypes TInt
                                                (x:xs) -> if all (==x) xs then
                                                            if x == t' then
                                                                return $ TFun paramTypes x
                                                            else
                                                                throwError "Type returned doesn't match declared type."
                                                          else
                                                            throwError "Ambiguous return type of a function."
                                                        
evalType (EFun fid (PList es)) = do
                            --liftIO $ putStrLn $ show fid ++ " ewaluacja!"
                            env <- ask
                            t@(TFun ps ret) <- case M.lookup fid env of
                                        Nothing -> throwError $ show fid ++ " was not declared in this scope."
                                        Just t@(TFun ps ret) -> return t
                                        _ -> throwError $ show fid ++ " is not a function."
                            types <- mapM evalType es
                            let vars = map (\e -> case e of { EVar _ -> True; _ -> False }) es
                                refs = map (\p -> case p of { TRef _ -> True; _ -> False }) ps
                                basicTypes = map unpackRef ps
                                matchVars = and $ zipWith (>=) vars refs
                                matchTypes = and $ zipWith (==) basicTypes types
                            if matchTypes && matchVars then
                                return ret
                            else
                                throwError $ show fid ++ ": types of actual and formal parameters do not match."
evalType (EBool b) = return $ TBool
evalType (EUn op e) = do
                    t <- evalType e
                    case t of
                        TInt -> case op of
                                    WPlus -> return t
                                    WMin -> return t
                                    _ -> throwError "Invalid unary operator: wrong type."
                        TBool -> case op of
                                    WNot -> return t
                                    _ -> throwError "Invalid unary operator: wrong type."
                        _ -> throwError "Invalid unary operator: wrong type."


evalType e = do
                case e of
                                (EAdd e1 e2) -> checkBoth e1 e2 TInt
                                (ESub e1 e2) -> checkBoth e1 e2 TInt
                                (EMul e1 e2) -> checkBoth e1 e2 TInt
                                (EDiv e1 e2) -> checkBoth e1 e2 TInt
                                --
                                (EERel e1 _ e2) -> (checkBoth e1 e2 TInt >> (return TBool)) `catchError` (\_ -> checkBoth e1 e2 TBool)
                                (EORel e1 _ e2) -> checkBoth e1 e2 TInt >> (return TBool)
                                --
                                (EAnd b1 b2) -> checkBoth b1 b2 TBool
                                (EOr b1 b2) -> checkBoth b1 b2 TBool

declType :: Decl -> TypeChecker a -> TypeChecker a
declType (DVar x t) int = local (M.insert x t) int

evalStmtListType :: [Stmt] -> TypeChecker [Type]
evalStmtListType is = foldM (\acc i -> do { t <- evalStmtType i; return $ t ++ acc }) [] is

unpackRef :: Type -> Type
unpackRef (TRef t) = unpackRef t
unpackRef other = other

flattenRef :: Type -> Type
flattenRef (TRef t@(TRef u)) = flattenRef t
flattenRef other = other

evalStmtType :: Stmt -> TypeChecker [Type]
evalStmtType (SBlock ds is) = foldr declType (evalStmtListType is) ds
evalStmtType (SAssign x op e) = do
                        env <- ask
                        t <- case M.lookup x env of
                                Nothing -> throwError $ show x ++ " was not declared in this scope."
                                Just (TFun ps retType) -> return $ TFun (map flattenRef ps) $ unpackRef retType
                                Just l -> return l
                        case op of
                                ASimple -> do
                                                t2 <- evalType e
                                                if t == t2 then
                                                    return []
                                                else
                                                    throwError $ show t ++ (" = ") ++ (show t2) ++ " - types do not match."
                                otherop -> fmap (\_ -> []) $ evalType $ f (EVar x) e
                                                where f = M.fromList [(AAdd, EAdd), (ASub, ESub), (AMul, EMul), (ADiv, EDiv)] M.! otherop
evalStmtType (SReturn e) = fmap (:[]) $ evalType e
evalStmtType (SUnary x op) = evalStmtType $ SAssign x AAdd $ EInt $ case op of { UInc -> 1; UDec -> -1 }
evalStmtType (SPrint e) = fmap (\_ -> []) $ evalType e
evalStmtType (SIfte b s1 s2) = do
                                t <- evalType b
                                --liftIO $ print t
                                --liftIO $ print b
                                if t == TBool then
                                    liftM2 (++) (evalStmtType s1) (evalStmtType s2)
                                else
                                    throwError "If's condition is not bool."
evalStmtType (SIf b s1) = evalStmtType $ SIfte b s1 $ SBlock [] []
evalStmtType (SWhile b i) = evalStmtType $ SIf b i

runTypeCheck :: Program -> IO (Either String [Type])
runTypeCheck (Prog ds is) = runErrorT $ flip runReaderT M.empty $ evalStmtType (SBlock ds is) 
