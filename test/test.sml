(* test.sml — tests for sml-bloom. *)

structure Tests =
struct
  open Harness

  (* "item0" .. "item(n-1)" *)
  fun items prefix n = List.tabulate (n, fn i => prefix ^ Int.toString i)

  fun insertAll bf keys = List.foldl (fn (k, b) => Bloom.insert b k) bf keys

  fun run () =
    let
      (* ---- 1. Optimal sizing ---------------------------------------- *)
      val () = section "sizing"
      val bf0 = Bloom.make 1000 0.01
      val () = checkInt "hashCount k = 7" (7, Bloom.hashCount bf0)
      val () = checkInt "capacity m = 9586" (9586, Bloom.capacity bf0)
      val () = checkInt "empty filter bitCount = 0" (0, Bloom.bitCount bf0)

      (* ---- 2. No false negatives ------------------------------------ *)
      val () = section "no-false-negatives"
      val keys = items "item" 1000
      val bf = insertAll bf0 keys
      val allMembers = List.all (fn k => Bloom.member bf k) keys
      val () = check "every inserted key is a member" allMembers
      val () = check "bitCount grew above zero" (Bloom.bitCount bf > 0)
      val () = check "bitCount within capacity"
                     (Bloom.bitCount bf <= Bloom.capacity bf)

      (* ---- 3. Measured false-positive rate -------------------------- *)
      val () = section "measured-fpr"
      val probes = items "x" 10000
      val fpCount =
        List.foldl (fn (k, acc) => if Bloom.member bf k then acc + 1 else acc)
                   0 probes
      val measured = Real.fromInt fpCount / 10000.0
      val () = check ("measured FPR < 0.02 (got "
                      ^ Real.toString measured ^ ")")
                     (measured < 0.02)

      (* ---- 4. Estimated false-positive rate ------------------------- *)
      val () = section "estimated-fpr"
      val est = Bloom.falsePositiveRate bf
      val () = check ("estimated FPR < 0.02 (got "
                      ^ Real.toString est ^ ")")
                     (est < 0.02)
      val () = check "estimated FPR is non-negative" (est >= 0.0)

      (* ---- 5. Empty-filter edge cases ------------------------------- *)
      val () = section "empty-edge"
      val () = checkBool "member of anything on empty = false"
                         (false, Bloom.member bf0 "anything")
      val () = checkBool "member of empty string on empty = false"
                         (false, Bloom.member bf0 "")
      val () = check "empty estimated FPR = 0.0"
                     (Real.== (Bloom.falsePositiveRate bf0, 0.0))

      (* ---- 6. Persistence / independence ---------------------------- *)
      val () = section "persistence"
      val bfA = Bloom.insert bf0 "alpha"
      val () = checkBool "insert returns a new filter with the key"
                         (true, Bloom.member bfA "alpha")
      val () = checkBool "original filter is unchanged by insert"
                         (false, Bloom.member bf0 "alpha")
      val bfB = Bloom.insert bfA "beta"
      val () = check "second insert keeps first key"
                     (Bloom.member bfB "alpha" andalso Bloom.member bfB "beta")
    in
      Harness.run ()
    end
end
