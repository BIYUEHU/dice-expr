module Main

import Data.Vect
import Random

data Ty = TyInt | TyBool | TyList Ty | TyFun Ty Ty

implementation Cast Ty Type where
  cast TyInt = Int
  cast TyBool = Bool
  cast (TyList t) = List (cast t)
  cast (TyFun t1 t2) = cast t1 -> cast t2

data HasType : (i : Fin n) -> Vect n Ty -> Ty -> Type where
  Stop : HasType FZ (t :: ctx) t
  Pop  : HasType k ctx t -> HasType (FS k) (u :: ctx) t

data Expr : Vect n Ty -> Ty -> Type where
  Var : HasType i ctx t -> Expr ctx t
  Val : (x : Int) -> Expr ctx TyInt
  Lam : Expr (a :: ctx) t -> Expr ctx (TyFun a t)
  App : Expr ctx (TyFun a t) -> Expr ctx a -> Expr ctx t
  Op  : (cast a -> cast b -> cast c) ->
    Expr ctx a -> Expr ctx b -> Expr ctx c
  If  : Expr ctx TyBool ->
    Lazy (Expr ctx a) ->
    Lazy (Expr ctx a) -> Expr ctx a

data Env : Vect n Ty -> Type where
  Nil  : Env Nil
  (::) : cast a -> Env ctxt -> Env (a :: ctxt)

lookup : HasType i ctxt t -> Env ctxt -> cast t
lookup Stop    (x :: xs) = x
lookup (Pop k) (x :: xs) = lookup k xs

interp : Env ctxt -> Expr ctxt t -> cast t
interp env (Var i)     = lookup i env
interp env (Val x)     = x
interp env (Lam sc)    = \x => interp (x :: env) sc
interp env (App f s)   = interp env f (interp env s)
interp env (Op op x y) = op (interp env x) (interp env y)
interp env (If x t e)  = if interp env x then interp env t else interp env e


dice : Nat -> Nat -> Nat
x `dice` y = 
  let f : Nat -> List Nat
      f (S k) =
        case randomInt 1 (cast y) (initRNG getSeed) of
          (n, _) => (cast n) :: f k
      f 0     = []
  in sum $ f x

add : Expr ctxt (TyFun TyInt (TyFun TyInt TyInt))
add = Lam (Lam (Op (\x => \y => cast $ dice (cast x) (cast y)) (Var Stop) (Var (Pop Stop))))

main : IO ()
main = do
  printLn $ interp [] add 2 1