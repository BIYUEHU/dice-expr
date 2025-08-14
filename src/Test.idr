f1 : {a:Type} -> a -> a

f2 : (forall a. a -> a) -> Int
f2 f = f 1

interface MyEq (ty: Type -> Type) ta where
  constructor MkMyEq
  a : Int
  b: ty Int
  c: ta

interface Foo a where
  constructor MkFoo
  bar : Int

record Foo2 where
  constructor MkFoo2
  bar : Int

Foo String where
  bar = 1

-- foo3 : MyEq List ( Foo String)
-- foo3 = MkMyEq 1 [1,3,4] ""

