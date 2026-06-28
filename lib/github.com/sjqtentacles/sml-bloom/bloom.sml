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

  fun sameShape (a : t, b : t) = #m a = #m b andalso #k a = #k b

  fun union (a : t, b : t) =
    if not (sameShape (a, b)) then raise Size
    else { m = #m a, k = #k a, bits = Bitset.union (#bits a, #bits b) }

  fun intersect (a : t, b : t) =
    if not (sameShape (a, b)) then raise Size
    else { m = #m a, k = #k a, bits = Bitset.inter (#bits a, #bits b) }

  fun approxCount ({ m, k, bits } : t) =
    let
      val mr = Real.fromInt m
      val x  = Real.fromInt (Bitset.count bits)
      (* clamp X strictly below m so ln is finite even for a saturated filter *)
      val ratio = Real.min (x / mr, 1.0 - 1.0 / mr)
    in
      ~(mr / Real.fromInt k) * Math.ln (1.0 - ratio)
    end

  (* ---- serialization ----------------------------------------------------
     Layout (before Base16): the ASCII tag "BLM1", then m and k each as four
     big-endian bytes, then each set-bit index as four big-endian bytes, in
     ascending order. The whole thing is hex-encoded so it is a printable,
     compiler-independent string. *)
  fun put32 n =
    let
      val w = Word32.fromInt n
      fun b sh = Char.chr (Word32.toInt (Word32.andb (Word32.>> (w, sh), 0wxFF)))
    in
      String.implode [b 0w24, b 0w16, b 0w8, b 0w0]
    end

  fun get32 (s, off) =
    let
      fun byte j = Word32.fromInt (Char.ord (String.sub (s, off + j)))
      val w = Word32.orb (Word32.<< (byte 0, 0w24),
              Word32.orb (Word32.<< (byte 1, 0w16),
              Word32.orb (Word32.<< (byte 2, 0w8), byte 3)))
    in
      Word32.toInt w
    end

  fun toBytes ({ m, k, bits } : t) =
    let
      val idxs = Bitset.toList bits
      val body = String.concat ("BLM1" :: put32 m :: put32 k
                                 :: List.map put32 idxs)
    in
      Base16.encode body
    end

  fun fromBytes hex =
    case Base16.decode hex of
        NONE => NONE
      | SOME raw =>
          let val len = String.size raw in
            if len < 12 orelse String.substring (raw, 0, 4) <> "BLM1"
               orelse (len - 12) mod 4 <> 0
            then NONE
            else
              let
                val m = get32 (raw, 4)
                val k = get32 (raw, 8)
                val nIdx = (len - 12) div 4
                fun rd i = get32 (raw, 12 + 4 * i)
                val idxs = List.tabulate (nIdx, rd)
              in
                if m < 1 orelse k < 1 orelse List.exists (fn i => i < 0 orelse i >= m) idxs
                then NONE
                else SOME { m = m, k = k, bits = Bitset.fromList m idxs }
              end
          end

  (* ---- counting Bloom filter -------------------------------------------- *)
  type counting =
    { m : int, k : int, counts : int Array.array }

  fun makeCounting n p =
    let
      val { m, k, ... } = make n p
    in
      { m = m, k = k, counts = Array.array (m, 0) }
    end

  fun bumpC (cf : counting) key delta =
    let
      val counts' = Array.tabulate (#m cf, fn i => Array.sub (#counts cf, i))
      val idxs = List.tabulate (#k cf, fn i => indexOf (#m cf) i key)
      val () = List.app (fn idx =>
        let val cur = Array.sub (counts', idx)
            val nv  = cur + delta
        in Array.update (counts', idx, if nv < 0 then 0 else nv) end) idxs
    in
      { m = #m cf, k = #k cf, counts = counts' }
    end

  fun insertC cf key = bumpC cf key 1
  fun deleteC cf key = bumpC cf key ~1

  fun memberC (cf : counting) key =
    List.all (fn i => Array.sub (#counts cf, indexOf (#m cf) i key) > 0)
             (List.tabulate (#k cf, fn i => i))

  fun capacityC  (cf : counting) = #m cf
  fun hashCountC (cf : counting) = #k cf
  fun countSumC  (cf : counting) = Array.foldl op+ 0 (#counts cf)
end
