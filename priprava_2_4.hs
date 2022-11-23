#FP laboratory 9
data Expr = Num Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Var Char
	  deriving (Eq)

eval::Expr->Int

