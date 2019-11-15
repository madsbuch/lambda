module Lambda where

-- Just unit as for now
data Info = Info
          | OrigName String
  deriving (Eq, Ord, Show)

data Binding = Binding

data Term
    = TmVar Info Int -- Info, DeBruijn Index
    | TmApp Info Term Term
    | TmAbs Info String Term
    deriving (Eq, Ord)

instance Show Term where
  show (TmVar (OrigName x) i) = x -- Todo, find variable from context
  show (TmApp _ t1 t2) = "(" ++ show t1 ++ ") (" ++ show t2 ++ ")"
  show (TmAbs _ x t) = "(λ" ++ x ++ "." ++ show t ++ ")"

-- | substitute j'th DeBruijn index for term in term resulting in term
--   This function provides sound substitution resulting
--   in Nothing when it can not be produced soundly. This should
--   Be used in conjunction with variable renaming
subs :: Int -> Term -> Term -> Term
subs j s t = walk 0 t
  where
    walk c t'@(TmVar _ x) 
      | x == j + c = shift c s
      | otherwise = t'
    walk c (TmApp ifo t1 t2) = TmApp ifo (walk c t1) (walk c t2)
    walk c (TmAbs ifo x t1) = TmAbs ifo x (walk (c+1) t1)

isValue :: Term -> Bool
isValue (TmAbs _ _ _) = True
isValue (TmVar _ _) = True
isValue _ = False

shift :: Int -> Term -> Term
shift d t = doShift d 0 t
  where
    doShift :: Int -> Int -> Term -> Term
    doShift d c t@(TmVar ifo k)
      | k >= c = TmVar ifo (k + d)
      | otherwise =  TmVar ifo k
    doShift d c (TmAbs ifo x t') = TmAbs ifo x (doShift d (c+1) t')
    doShift d c (TmApp ifo t1 t2) = TmApp ifo (doShift d c t1) (doShift d c t2)

    


step :: Term -> Maybe Term

step (TmApp i t1 t2)
  -- E-APP 1 (TAPL)
  | (not $ isValue t1)  = Just (TmApp i (eval t1) t2)
  -- E-APP 2 (TAPL)
  | (isValue t1) && (not $ isValue t2) = Just (TmApp i t1 (eval t2))
step (TmApp _ (TmAbs _ _ t1) t2)
  -- E-APP-ABS
  | (isValue t2) = let
      t1' = subs 0 t2 t1
      shiftedT1' = shift (-1) t1'
    in Just shiftedT1'

-- We can do nothing as the term is a value
step _ = Nothing

-- | Evaluates a term until it can not be reduced further.
eval :: Term -> Term
eval t = case step t of
    Nothing -> t
    Just t' -> eval t'