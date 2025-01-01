{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
import Data.List (union, (\\), find)
import Debug.Trace ()
import Data.Maybe
import Control.Exception

data T = App T T
       | If T T T
       | Succ T
       | Pred T
       | IsZero T
       | Val V
       | Let Label T T
       | Seq T T
       | Alloc T
       | DeRef T
       | Assign T T
  deriving (Show, Eq)

data V = Tru | Fls | Z | SuccNV V | UnitV | Location Loc | Var Label | L Label Type T deriving(Show, Eq)

data Label = A | C | D | E | F | G | H | I | J | K
    | M | O | P | Q | R | S | U | W | X | Y
    deriving (Show, Eq)

data Loc = L0 | L1 | L2 | L3 | L4 | L5 | L6 | L7 | L8 | L9
    deriving (Show, Eq)

data Type = BOOL | NAT | Unit | Arrow Type Type | Ref Type deriving (Show, Eq)

type Gamma = [(Label, Type)]

type Mu = [(Loc, V)]

-- Used in relabeling function
labelSet = [A , C , D , E , F , G , H , I , J
      , K , M  , O
      , P , Q , R , S
      , U  , W , X , Y ]

freeVars :: T -> [Label]
freeVars (Val (Var x)) = [x]
freeVars (App t1 t2) = freeVars t1 `union` freeVars t2
freeVars (Val (L x t term)) = freeVars term \\ [x]


relabel :: T -> Label -> Label -> T
relabel (Val (Var t1)) l l' = if t1 == l then Val (Var l')
                     else Val (Var t1)
relabel (Val (L y t t1)) l l' = if y == l then Val (L l' t (relabel t1 l l'))
                         else Val (L y t t1)
relabel (App termA termB) l l' = App (relabel termA l l') (relabel termB l l')

sub :: T -> Label -> T -> T
sub (Val Tru) x s = Val Tru
sub (Val Fls) x s = Val Fls
sub (Val Z) x s = Val Z
sub (Val UnitV) x s = Val UnitV
sub (Val (Location loc)) x s = (Val (Location loc))
sub (IsZero t1) x s = IsZero (sub t1 x s)
sub (Succ t1) x s = Succ (sub t1 x s)
sub (Pred t1) x s = Pred (sub t1 x s)
sub (Let l t1 t2) x s = Let l (sub t1 x s) ((sub t2 x s))
sub (If t1 t2 t3) x s = If (sub t1 x s) (sub t2 x s) (sub t3 x s)
sub (Seq t1 t2) x s = Seq (sub t1 x s) (sub t2 x s)
sub (Val (L y t t1)) x (Val v)
    | x /= y = Val (L y t (sub t1 x (Val v)))
    | otherwise = Val (L y t t1)
sub (Val (Var y)) x s = if x == y then s
                  else Val (Var y)
sub (App t1 t2) x s = App (sub t1 x s) (sub t2 x s)
sub (Val (L y t t1)) x s 
        | (x /= y) && (y `notElem` freeVars s) = Val (L y t (sub t1 x s))
        | (x /= y) && y `elem` freeVars s = sub (relabel (Val (L y t t1)) y (head(labelSet \\ [y]))) x s
        | otherwise = Val (L y t t1)

isNF :: T -> Bool
isNF (Val (Var _)) = True
isNF (App (Val (L x y z)) _) = False
isNF (App (Val (Var _)) (Val L {})) = False
isNF (App t1 t2) = isNF t1 && isNF t2
isNF (Val (L x t term)) = True
isNF (Val v) = True
isNF (Let _ t1 _) = isNF t1

ssos :: (T, Mu) -> (T, Mu)
ssos (Val v, mu) = (Val v, mu) -- reflexivity
ssos (Succ (Val nv), mu) = (Val (SuccNV nv), mu) -- E-SUCCVAL
ssos (If (Val Tru) t2 t3, mu) = (t2, mu) -- E-IFTRUE
ssos (If (Val Fls) t2 t3, mu) = (t3, mu) -- E-IfFALSE
ssos (If t1 t2 t3, mu) = (If (fst(ssos (t1, mu))) t2 t3, mu) -- E-IF
ssos (Succ t1, mu) = (Succ (fst(ssos (t1, mu))), mu) -- E-SUCC
ssos (Pred (Val Z), mu) = (Val Z, mu)  -- E-PREDZERO
ssos (Pred (Succ nv), mu) = (nv, mu) -- E-PREDSUCC
ssos (Pred t1, mu) = (Pred (fst(ssos (t1, mu))), mu) -- E-PRED
ssos (IsZero (Val Z), mu) = (Val Tru, mu) -- E-ISZEROZERO
ssos (IsZero (Succ nv), mu) = (Val Fls, mu) -- E-ISZEROSUCC
ssos (IsZero t1, mu) = (IsZero (fst(ssos (t1, mu))), mu) -- E-ISZERO
ssos (Val (L x t1 t), mu)
    | isNF t = (Val (L x t1 t), mu)
    | otherwise = (Val (L x t1 (fst(ssos (t, mu)))), mu)
ssos (App (Val (L v t t1)) t2, mu) = (sub t1 v t2, mu) -- E-APPABS
ssos (App t1 t2, mu) -- E-APP1 AND E-APP2
    | not (isNF t1) = (App (fst(ssos (t1, mu))) t2, mu)
    | not (isNF t2) = (App t1 (fst(ssos (t2, mu))), mu)
    | otherwise = (App t1 t2, mu)
ssos (Let x t1 t2, mu) -- E-LET
    | isNF t1 = (sub t1 x t2, mu)
    | otherwise = (Let x (fst(ssos (t1, mu))) t2, mu)
ssos (Seq t1 t2, mu)
    | t1 == Val UnitV = (t2,mu) -- E-SEQNEXT
    | otherwise = (Seq (fst(ssos (t1, mu))) t2, mu) -- E-SEQ
ssos (t, mu) = (t, mu) -- reflexivity

typeCheck :: Gamma -> T -> Maybe Type
typeCheck _ (Val Tru) = Just BOOL -- T-TRUE
typeCheck _ (Val Fls) = Just BOOL -- T-FALSE
typeCheck _ (Val Z) = Just NAT -- T-ZERO
typeCheck _ (Val (SuccNV _)) = Just NAT
typeCheck _ (Val UnitV) = Just Unit -- T-UNIT
typeCheck g (If t1 t2 t3) =  do { -- T-IF
            type1 <- typeCheck g t1;
            type2 <- typeCheck g t2;
            type3 <- typeCheck g t3;
            if (type2 /= type3) || (type1 /= BOOL)
                then Nothing
                else return type3
}
typeCheck g (Succ t1) = do { -- T-SUCC
            type1 <- typeCheck g t1;
            if type1 /= NAT
                then Nothing
                else return NAT
}
typeCheck g (Pred t1) = do { -- T-PRED
            type1 <- typeCheck g t1;
            if type1 /= NAT
                then Nothing
                else return NAT
}
typeCheck g (IsZero t1) = do { -- T-ISZERO
            type1 <- typeCheck g t1;
            if type1 /= NAT
                then Nothing
                else return BOOL
}
typeCheck g (Val (Var a)) -- T-VAR
        | isJust (lookup a g) = lookup a g
        | otherwise = Nothing
typeCheck g (Val (L x t t2)) = do { -- T-ABS
        tType <- typeCheck (g ++ [(x, t)]) t2;
        case tType of
            BOOL -> return (Arrow t tType)
            NAT -> return (Arrow t tType)
            Unit -> return (Arrow t tType)
            Ref _ -> return (Arrow t tType)
            Arrow _ _ -> return (Arrow t tType)
            _ -> Nothing
}
typeCheck g (App x y) = do { -- T-APP
         t1 <- typeCheck g x;
         t2 <- typeCheck g y;
         case t1 of
            (Arrow t11 t12) | t11 == t2 -> return t12
                        | otherwise -> Nothing
            _ -> Nothing
}
typeCheck g (Let x t1 t2) = do { -- T-LET
        type1 <- typeCheck g t1;
        type2 <- typeCheck (g ++ [(x, type1)]) t2;
        case type1 of
            BOOL -> return type2
            NAT -> return type2
            Unit -> return type2
            Ref _ -> return type2
            Arrow _ _ -> return type2
            _ -> Nothing;
}
typeCheck g (Seq t1 t2) = do { -- T-SEQ
        type1 <- typeCheck g t1;
        type2 <- typeCheck g t2;
        if type1 == Unit
            then return type2
            else Nothing
}

eval :: T -> T
eval term
    | term == t1' = term
    | term /= t1' = eval t1'
    where t1' = fst(ssos (term, []))

run :: T -> T
run t
    | isJust (typeCheck [] t) = eval t
    | otherwise = error "Type Checking Failed"

func = App (Val (L X (Arrow NAT BOOL) (App (Val (Var X)) (Val Z)))) (Val (L Y NAT (IsZero (Val (Var Y)))))
