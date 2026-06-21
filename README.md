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
  val falsePositiveRate : t -> real         (* estimated from fill ratio *)
end
```

### Sizing

Given `n` expected elements and a target false-positive rate `p`:

- bit-array size `m = ceil(-n · ln p / (ln 2)²)`
- hash count `k = max(1, round((m / n) · ln 2))`

Each of the `k` hashes digests `chr(i) ^ key` with SHA-256 and takes a 31-bit
window from the front of the digest, reduced modulo `m`. The estimated
false-positive rate is `(setBits / m) ^ k`.

## Testing

```
make test       # MLton
make test-poly  # Poly/ML
```

## License

MIT
