(* bitset.sml

   Implementation of BITSET.

   A set is a capacity plus a `Word32.word Array.array` of chunks, 32 bits per
   chunk. Bit `i` lives in chunk `i div 32` at bit position `i mod 32`. We use a
   fixed 32-bit chunk (scalar Word32 in a plain Array) rather than the native
   word, because `Word.wordSize` differs across compilers (32 on MLton, 63 on
   Poly/ML) and `Word32Array` is unavailable on Poly/ML.

   Bits in the final chunk that lie beyond `capacity` are kept zero by every
   operation (notably `full` and `complement`), so `count`/`toList`/`equals`
   never observe phantom bits. *)

structure Bitset :> BITSET =
struct
  type word = Word32.word

  val bitsPerWord = 32

  (* capacity, chunks.  length chunks = ceil(capacity / 32) (>= 0). *)
  type bitset = int * word array

  fun capacity (cap, _) = cap

  fun numChunks cap = (cap + bitsPerWord - 1) div bitsPerWord

  (* mask of valid bits in the last chunk; all-ones when capacity is a
     multiple of 32 (or zero). *)
  fun lastMask cap =
      let val r = cap mod bitsPerWord
      in if r = 0 then 0wxFFFFFFFF
         else Word32.<< (0w1, Word.fromInt r) - 0w1
      end

  fun checkSize n = if n < 0 then raise Size else ()

  fun empty cap =
      (checkSize cap; (cap, Array.array (numChunks cap, 0w0)))

  fun full cap =
      let
        val () = checkSize cap
        val nc = numChunks cap
        val a = Array.array (nc, 0wxFFFFFFFF)
      in
        (* trim phantom high bits in the last chunk *)
        if nc > 0
        then Array.update (a, nc - 1, Word32.andb (0wxFFFFFFFF, lastMask cap))
        else ();
        (cap, a)
      end

  fun chunkOf i = i div bitsPerWord
  fun bitOf i = Word.fromInt (i mod bitsPerWord)

  fun member (cap, a) i =
      if i < 0 orelse i >= cap then false
      else Word32.andb (Array.sub (a, chunkOf i), Word32.<< (0w1, bitOf i)) <> 0w0

  fun copy a = Array.tabulate (Array.length a, fn k => Array.sub (a, k))

  fun add (cap, a) i =
      if i < 0 orelse i >= cap then raise Subscript
      else
        let val a' = copy a
            val c = chunkOf i
        in Array.update (a', c, Word32.orb (Array.sub (a', c), Word32.<< (0w1, bitOf i)));
           (cap, a')
        end

  fun remove (cap, a) i =
      if i < 0 orelse i >= cap then raise Subscript
      else
        let val a' = copy a
            val c = chunkOf i
            val clear = Word32.notb (Word32.<< (0w1, bitOf i))
        in Array.update (a', c, Word32.andb (Array.sub (a', c), clear));
           (cap, a')
        end

  fun fromList cap is =
      let val (_, a) = empty cap
      in
        List.app
          (fn i => if i < 0 orelse i >= cap then raise Subscript
                   else let val c = chunkOf i
                        in Array.update (a, c,
                             Word32.orb (Array.sub (a, c), Word32.<< (0w1, bitOf i)))
                        end)
          is;
        (cap, a)
      end

  fun binop f ((c1, a1), (c2, a2)) =
      if c1 <> c2 then raise Size
      else (c1, Array.tabulate (Array.length a1,
                  fn k => f (Array.sub (a1, k), Array.sub (a2, k))))

  fun union (x, y) = binop Word32.orb (x, y)
  fun inter (x, y) = binop Word32.andb (x, y)
  fun diff (x, y) = binop (fn (p, q) => Word32.andb (p, Word32.notb q)) (x, y)

  fun complement (cap, a) =
      let
        val nc = Array.length a
        val a' = Array.tabulate (nc, fn k => Word32.notb (Array.sub (a, k)))
      in
        if nc > 0
        then Array.update (a', nc - 1,
               Word32.andb (Array.sub (a', nc - 1), lastMask cap))
        else ();
        (cap, a')
      end

  (* population count of one chunk via shift-and-mask (portable). *)
  fun popcountWord w =
      let fun go (x, n) =
              if x = 0w0 then n
              else go (Word32.>> (x, 0w1), n + Word32.toInt (Word32.andb (x, 0w1)))
      in go (w, 0) end

  fun count (_, a) =
      Array.foldl (fn (w, acc) => acc + popcountWord w) 0 a

  fun isEmpty (_, a) = Array.all (fn w => w = 0w0) a

  fun foldBits f init (cap, a) =
      let
        fun chunkFold (ci, w, acc) =
            let
              val base = ci * bitsPerWord
              fun go (b, acc) =
                  if b >= bitsPerWord orelse base + b >= cap then acc
                  else
                    let val acc' =
                            if Word32.andb (w, Word32.<< (0w1, Word.fromInt b)) <> 0w0
                            then f (base + b, acc) else acc
                    in go (b + 1, acc') end
            in go (0, acc) end
        fun loop (ci, acc) =
            if ci >= Array.length a then acc
            else loop (ci + 1, chunkFold (ci, Array.sub (a, ci), acc))
      in loop (0, init) end

  fun toList bs = List.rev (foldBits (fn (i, acc) => i :: acc) [] bs)

  fun equals ((c1, a1), (c2, a2)) =
      c1 = c2
      andalso Array.length a1 = Array.length a2
      andalso let fun same k = k >= Array.length a1
                              orelse (Array.sub (a1, k) = Array.sub (a2, k)
                                      andalso same (k + 1))
              in same 0 end
end
