import Data.Foldable

namespace Data.Traversable

open Data.Foldable

class Traversable (t : Type -> Type) extends Functor t, Foldable t where
  traverse : {f : Type -> Type} -> [Functor f] -> [Applicative f] -> (a -> f b) -> t a -> f (t b)
  sequence : {f : Type -> Type} -> [Functor f] -> [Applicative f] -> t (f a) -> f (t a) := traverse id

def liftA2 {f : Type -> Type} [Applicative f] : (a -> b -> c) -> f a -> f b -> f c
| f, x, y => f <$> x <*> y

instance : Traversable List where
  traverse f :=
    let cons_f := fun x ys => liftA2 .cons (f x) ys
    List.foldr cons_f $ pure []
