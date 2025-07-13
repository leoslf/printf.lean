namespace Data.Ord

instance : Ord Float32 where
  compare a b :=
    if a == b then
      .eq
    else if a < b then
      .lt
    else
      .gt

instance : Ord Float where
  compare a b :=
    if a == b then
      .eq
    else if a < b then
      .lt
    else
      .gt

