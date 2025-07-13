namespace Data.Foldable

class Foldable (t : Type -> Type) where
  foldr : (a -> b -> b) -> b -> t a -> b
  null : t a -> Bool := foldr (fun _ _ => false) true

instance : Foldable List where
  foldr := List.foldr

instance : Foldable Option where
  foldr
  | _, z, .none => z
  | f, z, .some x => f x z
