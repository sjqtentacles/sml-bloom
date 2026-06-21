(* bloom.sml

   Bloom filter implementation. The bit array is the vendored fixed-capacity
   `Bitset`; the `k` hash functions are derived from SHA-256 (vendored
   `Sha256`). For hash function `i` we digest the byte `chr i` prepended to the
   key, read a 32-bit big-endian window from the front of the digest, and
   reduce it modulo `m` to obtain a bit index. *)

structure Bloom :> BLOOM =
struct
  type t =
    { m     : int          (* bit-array size *)
    , k     : int          (* number of hash functions *)
    , bits  : Bitset.bitset }

  val ln2 = Math.ln 2.0

  fun make n p =
    let
      val () = if n <= 0 then raise Size else ()
      val () = if p <= 0.0 orelse p >= 1.0 then raise Domain else ()
      val nr = Real.fromInt n
      val m  = Real.ceil (~nr * Math.ln p / (ln2 * ln2))
      val m  = if m < 1 then 1 else m
      val k  = Real.round ((Real.fromInt m / nr) * ln2)
      val k  = if k < 1 then 1 else k
    in
      { m = m, k = k, bits = Bitset.empty m }
    end

  (* Big-endian 32-bit unsigned value from the first four bytes of `s`,
     starting at byte offset `off`. Returned as a non-negative int. *)
  fun word32 (s, off) =
    let
      fun byte j = Word.fromInt (Char.ord (String.sub (s, off + j)))
      val w = Word.orb (Word.<< (byte 0, 0w24),
              Word.orb (Word.<< (byte 1, 0w16),
              Word.orb (Word.<< (byte 2, 0w8),
                        byte 3)))
      (* Mask to 32 bits then keep it non-negative as an int. We use the low
         31 bits to stay safely within the host Int range on every compiler. *)
      val low31 = Word.andb (w, 0wx7FFFFFFF)
    in
      Word.toInt low31
    end

  (* Bit index of hash function `i` for `key`, in 0 .. m-1. *)
  fun indexOf m i key =
    let
      val digest = Sha256.digest (String.str (Char.chr (i mod 256)) ^ key)
      val h = word32 (digest, 0)
    in
      h mod m
    end

  fun indices ({ m, k, ... } : t) key =
    List.tabulate (k, fn i => indexOf m i key)

  fun insert (bf as { m, k, bits }) key =
    let
      val bits' =
        List.foldl (fn (idx, b) => Bitset.add b idx) bits (indices bf key)
    in
      { m = m, k = k, bits = bits' }
    end

  fun member (bf as { bits, ... } : t) key =
    List.all (fn idx => Bitset.member bits idx) (indices bf key)

  fun capacity ({ m, ... } : t) = m

  fun bitCount ({ bits, ... } : t) = Bitset.count bits

  fun hashCount ({ k, ... } : t) = k

  fun falsePositiveRate ({ m, k, bits } : t) =
    let
      val fill = Real.fromInt (Bitset.count bits) / Real.fromInt m
    in
      Math.pow (fill, Real.fromInt k)
    end
end
