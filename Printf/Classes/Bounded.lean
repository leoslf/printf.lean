namespace Printf

universe u

class Bounded (a : Type u) where
  minBound : a
  maxBound : a

instance : Bounded UInt8 where
  minBound := 0
  maxBound := 0xFF

instance : Bounded UInt16 where
  minBound := 0
  maxBound := 0xFFFF

instance : Bounded UInt32 where
  minBound := 0
  maxBound := 0xFFFFFFFF

instance : Bounded UInt64 where
  minBound := 0
  maxBound := 0xFFFFFFFFFFFFFFFF

instance : Bounded Int8 where
  minBound := Int8.minValue
  maxBound := Int8.maxValue

instance : Bounded Int16 where
  minBound := Int16.minValue
  maxBound := Int16.maxValue

instance : Bounded Int32 where
  minBound := Int32.minValue
  maxBound := Int32.maxValue

instance : Bounded Int64 where
  minBound := Int64.minValue
  maxBound := Int64.maxValue

-- instance : Bounded Float32 where
--   minBound := Float32.minValue
--   maxBound := Float32.maxValue
--
-- instance : Bounded Float where
--   minBound := Float.minValue
--   maxBound := Float.maxValue
