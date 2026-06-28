(* bloom.sig

   A Bloom filter: a space-efficient probabilistic set. Membership queries
   never return false negatives (an inserted element always tests as a member)
   but may return false positives with a tunable probability.

   A filter is created with `make n p` where `n` is the expected number of
   elements and `p` is the desired false-positive rate at that load. The
   backing bit array size `m` and number of hash functions `k` are derived
   from the standard optimal formulas:

     m = ceil(-n * ln p / (ln 2)^2)
     k = max(1, round((m / n) * ln 2))

   All operations are persistent. *)

signature BLOOM =
sig
  type t

  (* `make n p` builds an empty filter sized for `n` expected elements at a
     target false-positive rate `p` (0 < p < 1). *)
  val make : int -> real -> t

  (* Persistently insert a key, returning the updated filter. *)
  val insert : t -> string -> t

  (* Membership test. No false negatives: every inserted key is a member. *)
  val member : t -> string -> bool

  (* The bit-array size `m`. *)
  val capacity : t -> int

  (* Number of currently set bits. *)
  val bitCount : t -> int

  (* Number of hash functions `k`. *)
  val hashCount : t -> int

  (* Estimated false-positive rate given the current fill ratio:
     (setBits / m) ^ k. *)
  val falsePositiveRate : t -> real

  (* Union / intersection of two filters that share the same `m` and `k`
     (raise `Size` otherwise). Union is exact: it contains exactly the keys of
     either input. Intersection is approximate: it never reports a key absent
     from both inputs, but may carry extra bits and so its false-positive rate
     can exceed either input's. *)
  val union     : t * t -> t
  val intersect : t * t -> t

  (* Estimate the number of distinct keys inserted, from the fill ratio:
     -(m/k) * ln(1 - X/m), where X is the set-bit count. *)
  val approxCount : t -> real

  (* Serialize to / from a self-describing byte string. `fromBytes` returns
     NONE on a malformed or truncated encoding. Round-trips m, k and bits. *)
  val toBytes   : t -> string
  val fromBytes : string -> t option

  (* --- Counting Bloom filter ---------------------------------------------
     A counting variant backs each position with a small integer counter
     instead of a single bit, which lets it support deletion. `delete` is
     safe only for keys that were actually inserted; deleting a key that was
     never added can introduce false negatives. *)
  type counting

  val makeCounting   : int -> real -> counting
  val insertC        : counting -> string -> counting
  val deleteC        : counting -> string -> counting
  val memberC        : counting -> string -> bool
  val capacityC      : counting -> int
  val hashCountC     : counting -> int
  (* Total of all counters (= sum over inserts of k, minus deletes). *)
  val countSumC      : counting -> int
end
