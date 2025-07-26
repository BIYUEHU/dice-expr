module Random

import Data.Bits

public export
record RNG where
  constructor MkRNG
  seed : Integer

public export
initRNG : Integer -> RNG
initRNG s = MkRNG s

public export
nextBits : RNG -> (Integer, RNG)
nextBits (MkRNG s) =
  let s1 = s `xor` (s `shiftL` 13)
      s2 = s1 `xor` (s1 `shiftR` 7)
      s3 = s2 `xor` (s2 `shiftL` 17)
      r  = s3 * 2685821657736338717
  in (r, MkRNG s3)

public export
-- %foreign "javascript:lambda:()=>BigInt(Date.now())"
getSeed : Integer
getSeed = 0

public export
randomInt : Integer -> Integer -> RNG -> (Integer, RNG)
randomInt lo hi rng =
  case nextBits rng of
    (bits, rng') =>
      let range = hi - lo + 1
          v     = lo + (bits `mod` range)
      in
        (v, rng')


