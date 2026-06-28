# sml-bloom

Bloom filter with optimal sizing and SHA-256 hashing in pure Standard ML.

A Bloom filter is a space-efficient probabilistic set. Membership queries
never produce **false negatives** — every key you insert always tests as a
member — but may produce **false positives** at a tunable rate. This library
sizes the backing bit array and chooses the number of hash functions using the
standard optimal formulas, and derives its `k` independent hashes from SHA-256.

It is built on two sibling libraries:

- [`sml-bitset`](https://github.com/sjqtentacles/sml-bitset) — the packed,
  persistent bit-array backing store.
- [`sml-codec`](https://github.com/sjqtentacles/sml-codec) — SHA-256 for the
  hash functions.

## Installation

```
smlpkg add github.com/sjqtentacles/sml-bloom
smlpkg sync
```

## Usage

```sml
(* Size a filter for 1000 expected elements at a 1% false-positive rate. *)
val bf0 = Bloom.make 1000 0.01

val _ = Bloom.hashCount bf0   (* => 7  (k)            *)
val _ = Bloom.capacity bf0    (* => 9586 (m, in bits) *)
val _ = Bloom.bitCount bf0    (* => 0  (empty)        *)

(* Inserts are persistent: each returns a new filter. *)
val bf =
  List.foldl (fn (k, b) => Bloom.insert b k) bf0
    (List.tabulate (1000, fn i => "item" ^ Int.toString i))

(* No false negatives: every inserted key is a member. *)
val _ = List.all (fn k => Bloom.member bf k)
                 (List.tabulate (1000, fn i => "item" ^ Int.toString i))
(* => true *)

(* A key that was never inserted is (almost always) not a member. *)
val _ = Bloom.member bf "never-inserted"   (* => false (with high probability) *)

(* Estimated false-positive rate from the current fill ratio. *)
val _ = Bloom.falsePositiveRate bf         (* => ~0.0105 *)

(* Set algebra on shape-compatible filters (same m and k). *)
val a = Bloom.insert (Bloom.insert (Bloom.make 100 0.01) "x") "y"
val b = Bloom.insert (Bloom.insert (Bloom.make 100 0.01) "y") "z"
val _ = Bloom.union (a, b)        (* contains x, y, z *)
val _ = Bloom.intersect (a, b)    (* contains y (approximate) *)

(* Estimate how many distinct keys were inserted, from the fill ratio. *)
val _ = Bloom.approxCount bf      (* => ~1000 *)

(* Serialize to a printable, compiler-independent hex string and back. *)
val hex = Bloom.toBytes bf
val SOME bf' = Bloom.fromBytes hex

(* Counting variant supports deletion (use only for keys you inserted). *)
val cf = Bloom.insertC (Bloom.makeCounting 100 0.01) "k"
val _  = Bloom.memberC cf "k"               (* => true  *)
val _  = Bloom.memberC (Bloom.deleteC cf "k") "k"  (* => false *)
```

## API

```sml
signature BLOOM =
sig
  type t
  val make   : int -> real -> t            (* expected n, false-positive rate p *)
  val insert : t -> string -> t
  val member : t -> string -> bool         (* no false negatives *)
  val capacity          : t -> int         (* bit-array size m *)
  val bitCount          : t -> int         (* number of set bits *)
  val hashCount         : t -> int         (* k *)
  val falsePositiveRate : t -> real        (* estimated from fill ratio *)

  (* Set algebra; both filters must share m and k (else Size). Union is exact;
     intersect is approximate (never reports a key absent from both, but may
     carry extra bits). *)
  val union     : t * t -> t
  val intersect : t * t -> t

  (* Estimate distinct inserted keys: -(m/k) · ln(1 - setBits/m). *)
  val approxCount : t -> real

  (* Self-describing hex serialization; fromBytes is NONE on malformed input. *)
  val toBytes   : t -> string
  val fromBytes : string -> t option

  (* Counting Bloom filter: integer counters per slot, supports delete. *)
  type counting
  val makeCounting : int -> real -> counting
  val insertC      : counting -> string -> counting
  val deleteC      : counting -> string -> counting
  val memberC      : counting -> string -> bool
  val capacityC    : counting -> int
  val hashCountC   : counting -> int
  val countSumC    : counting -> int
end
```

### Sizing

Given `n` expected elements and a target false-positive rate `p`:

- bit-array size `m = ceil(-n · ln p / (ln 2)²)`
- hash count `k = max(1, round((m / n) · ln 2))`

Each of the `k` hashes digests `chr(i) ^ key` with SHA-256 and takes a 31-bit
window from the front of the digest, reduced modulo `m`. The estimated
false-positive rate is `(setBits / m) ^ k`.

### Notes on the new operations

- **`union` / `intersect`** require both operands to have identical `m` and `k`
  (raise `Size` otherwise). `union` is exact. `intersect` is conservative: it
  may contain extra set bits relative to a true element-wise intersection, so
  its false-positive rate can exceed either input's.
- **`approxCount`** is a statistical estimate; it is most accurate well below
  saturation and is clamped so a fully saturated filter does not blow up.
- **Counting filter (`delete`)**: deletion is only sound for keys that were
  actually inserted. Deleting a key that was never added (or deleting one more
  time than it was inserted) can introduce false negatives. Counters are
  floored at zero. Serialization (`toBytes`/`fromBytes`) covers the plain
  filter only.

## Testing

```
make test       # MLton
make test-poly  # Poly/ML
```

## License

MIT
