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
end
