module MinHS.Evaluator where
import qualified MinHS.Env as E
import MinHS.Syntax
import MinHS.Pretty
import qualified Text.PrettyPrint.ANSI.Leijen as PP

type VEnv = E.Env Value

data Value = I Integer
           | B Bool
           | Nil
           | Cons Integer Value
           | PartialPrim Op Value
           | PartialCon Value
           | Func Bind
           | Closure VEnv Bind
           -- Others as needed
           deriving (Show)

instance PP.Pretty Value where
  pretty (I i) = numeric $ i
  pretty (B b) = datacon $ show b
  pretty (Nil) = datacon "Nil"
  pretty (Cons x v) = PP.parens (datacon "Cons" PP.<+> numeric x PP.<+> PP.pretty v)
  pretty _ = undefined -- should not ever be used

evaluate :: Program -> Value
evaluate [Bind _ _ _ e] = evalE E.empty e
evaluate bs = evalE E.empty (Let bs (Var "main"))


evalE :: VEnv -> Exp -> Value
-- constant value
evalE _ (Num i)       = I i
evalE _ (Con "True")  = B True
evalE _ (Con "False") = B False
evalE _ (Con "Nil")   = Nil

-- variable expression
evalE env (Var id) = case E.lookup env id of
    Just v -> v
    _      -> error ("Not in scope: " ++ show(id))

-- let expression
evalE env (Let [] e) = evalE env e
evalE env (Let ((Bind id _ [] e1) : xs) e2) = evalE (E.add env (id, (evalE env e1))) (Let xs e2)
evalE env (Let (f@(Bind func _ _ _) : xs) e2) = evalE (E.add env (func, (Func f))) (Let xs e2)

-- mutually recursive bindings
evalE g (Letrec [] e) = evalE g e
evalE g (Letrec bs@(b@(Bind id _ _ e):rest) e') =
  let boundIds = [id' | (Bind id' _ _ _) <- bs]
      freeVars = freeVar e in
    if isIntersect boundIds freeVars then evalE g (Letrec (rest ++ [b]) e')
    else evalE (E.add g (id, (evalE g e))) (Letrec rest e')

-- primitive unary operation
evalE env (App (Prim Neg) e) = let I i = evalE env e in I $ negate $ i

-- primitive binary operations
evalE env (App (App (Prim op) e1) e2) = 
    let I i1 = evalE env e1
        I i2 = evalE env e2
    in case op of
        Add   -> I $ i1 + i2
        Sub   -> I $ i1 - i2
        Mul   -> I $ i1 * i2
        Quot  -> case i2 of
            0 -> error "Exception: divide by zero"
            _ -> I $ quot i1 i2
        Rem   -> case i2 of
            0 -> error "Exception: divide by zero"
            _ -> I $ rem i1 i2
        Gt    -> B $ i1 > i2
        Ge    -> B $ i1 >= i2
        Lt    -> B $ i1 < i2
        Le    -> B $ i1 <= i2
        Eq    -> B $ i1 == i2
        Ne    -> B $ i1 /= i2

-- if statement
evalE env (If guard e1 e2) = 
    case evalE env guard of
        (B True)  -> evalE env e1
        (B False) -> evalE env e2
    
-- list operation
evalE env (App (Prim Head) list) = case evalE env list of
    Nil      -> error "Exception: empty list"
    Cons x _ -> I x
    Func (Bind _ _ [] body) -> evalE env (App (Prim Head) body)

evalE env (App (Prim Tail) list) = case evalE env list of
    Nil       -> error "Exception: empty list"
    Cons _ xs -> xs
    Func (Bind _ _ [] body) -> evalE env (App (Prim Tail) body)

evalE env (App (Prim Null) list) = case evalE env list of
    Nil      -> B True
    Cons _ _ -> B False
    Func (Bind _ _ [] body) -> evalE env (App (Prim Null) body)

evalE env (App (App (Con "Cons") e1) e2) = let I i = evalE env e1 in Cons i (evalE env e2)

-- function abstraction
evalE env (Letfun bind@(Bind func _ [] body)) = evalE (E.add env (func, Func bind)) body
evalE env (Letfun bind@(Bind _ _ _ _ )) = Closure env bind

-- unary function application
-- evalE env (App (Letfun (Bind func _ [] body)) e)       = evalE (E.add env (func, (evalE env body))) (App (Var func) e)
-- evalE env (App (Letfun f@(Bind func _ [arg] body)) e) = evalE (E.add (E.add env (arg, evalE env e)) (func, Func f)) body

-- partial primitive operation
evalE env (App (Prim op) e) = PartialPrim op (evalE env e)
evalE env (App (Con "Cons") e) = PartialCon (evalE env e)

-- partial function application
evalE env a@(App e1 e2) = case evalE env e1 of
    Func f                  -> evalE env (App (Letfun f) e2)
    PartialPrim Add (I i1)  -> I $ i1 + i2
    PartialPrim Sub (I i1)  -> I $ i1 - i2
    PartialPrim Mul (I i1)  -> I $ i1 * i2
    PartialPrim Quot (I i1) -> case i2 of
        0 -> error "Exception: divide by zero"
        _ -> I $ quot i1 i2
    PartialPrim Rem (I i1)  -> case i2 of 
        0 -> error "Exception: divide by zero"
        _ -> I $ rem i1 i2
    PartialCon (I i)        -> Cons i (evalE env e2)
    PartialPrim Gt (B b1)   -> B $ b1 > b2
    PartialPrim Ge (B b1)   -> B $ b1 >= b2
    PartialPrim Lt (B b1)   -> B $ b1 < b2
    PartialPrim Le (B b1)   -> B $ b1 <= b2
    PartialPrim Eq (B b1)   -> B $ b1 == b2
    PartialPrim Ne (B b1)   -> B $ b1 /= b2
    -- function application
    v1@(Closure env' (Bind func _ [arg] body)) -> evalE (E.add (E.add env' (arg, v2)) (func, v1)) body
    -- n-nary function application
    (Closure env' (Bind func (Arrow (TypeCon _) types) args@(arg : remain) body)) -> evalE (E.add env (arg, v2)) (Letfun(Bind func types remain body))
    --_ -> error $ show $ evalE env e1
    where I i2 = evalE env e2 
          B b2 = evalE env e2
          v2 = evalE env e2
          
evalE _ e = error $ show e

isBoundId id bs = case bs of
  [] -> False
  ((Bind id' _ _ _):bs) -> id == id' || isBoundId id bs

freeVar e = case e of
  Var id -> [id]
  App e1 e2 -> freeVar e1 ++ freeVar e2
  If e1 e2 e3 ->freeVar e1 ++ freeVar e2 ++ freeVar e3
  Let bs e -> [ id | id <- freeVar e ++ freeInBinds bs, not (isBoundId id bs)]
  Letfun (Bind n _ p e) -> [ id | id <- freeVar e, not (id `elem` p)]
  Letrec bs e -> [ id | id <- freeVar e ++ freeInBinds bs, not (isBoundId id bs)]
  _ -> []

freeInBinds bs = case bs of
  [] -> []
  (Bind _ _ _ e):rest -> freeVar e ++ freeInBinds bs

isIntersect list1 list2 = case (list1, list2) of
  ([], _) -> False
  (x:xs, ys) -> if x `elem` ys then True else isIntersect xs ys