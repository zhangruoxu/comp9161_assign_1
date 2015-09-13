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

--var expression


-- primitive operations
evalE env (App (App (Prim op) e1) e2) = 
    let I i1 = evalE env e1
        I i2 = evalE env e2
    in case op of
        Add   -> I (i1 + i2)
        Sub   -> I (i1 - i2)
        Mul   -> I (i1 * i2)
        Quot  -> case i2 of
            0 -> error "Exception: divide by zero"
            _ -> I (quot i1 i2)
        Rem   -> case i2 of
            0 -> error "Exception: divide by zero"
            _ -> I (rem i1 i2)
        Gt    -> B (i1 > i2)
        Ge    -> B (i1 >= i2)
        Lt    -> B (i1 < i2)
        Le    -> B (i1 <= i2)
        Eq    -> B (i1 == i2)

-- if statement
evalE env (If guard e1 e2) = 
    case evalE env guard of
        (B True)  -> evalE env e1
        (B False) -> evalE env e2
    
-- list operation
evalE env (App (Prim Head) list) = case evalE env list of
    Nil      -> error "Exception: empty list"
    Cons x _ -> I x

evalE env (App (Prim Tail) list) = case evalE env list of
    Nil       -> error "Exception: empty list"
    Cons _ xs -> xs

evalE env (App (Prim Null) list) = case evalE env list of
    Nil      -> B True
    Cons _ _ -> B False