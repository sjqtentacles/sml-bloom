(* demo.sml - a deterministic tour of sml-bloom: inserting and querying a
   Bloom filter, inspecting its size/fill statistics, unioning two filters,
   round-tripping through toBytes/fromBytes, and exercising the counting
   variant's insert/delete symmetry. Every value is a pure function of the
   input key strings, so the output is byte-identical under MLton and
   Poly/ML. *)

fun fmtReal n r =
  let
    val s = Real.fmt (StringCvt.FIX (SOME n)) r
    val s = if String.isPrefix "~" s then "-" ^ String.extract (s, 1, NONE) else s
    val isZero = CharVector.all (fn c => c = #"0" orelse c = #"." orelse c = #"-") s
  in
    if isZero andalso String.isPrefix "-" s then String.extract (s, 1, NONE) else s
  end

val () = print "=== sml-bloom demo ===\n\n"

val keys = ["apple", "banana", "cherry"]

val f = List.foldl (fn (k, bf) => Bloom.insert bf k) (Bloom.make 100 0.01) keys

val () = print "Membership after inserting apple, banana, cherry:\n"
val () =
  List.app
    (fn k => print ("  member \"" ^ k ^ "\" = " ^ Bool.toString (Bloom.member f k) ^ "\n"))
    keys
val () =
  print ("  member \"date\" (never inserted) = "
         ^ Bool.toString (Bloom.member f "date") ^ "\n")

val () = print "\nFilter statistics:\n"
val () = print ("  capacity (m)      = " ^ Int.toString (Bloom.capacity f) ^ "\n")
val () = print ("  bitCount          = " ^ Int.toString (Bloom.bitCount f) ^ "\n")
val () = print ("  hashCount (k)     = " ^ Int.toString (Bloom.hashCount f) ^ "\n")
val () = print ("  falsePositiveRate = " ^ fmtReal 6 (Bloom.falsePositiveRate f) ^ "\n")
val () =
  print ("  approxCount       = " ^ fmtReal 3 (Bloom.approxCount f)
         ^ " (actual inserts: " ^ Int.toString (List.length keys) ^ ")\n")

val () = print "\nUnion of two same-shaped filters:\n"
val fa = List.foldl (fn (k, bf) => Bloom.insert bf k) (Bloom.make 100 0.01) ["apple", "banana"]
val fb = List.foldl (fn (k, bf) => Bloom.insert bf k) (Bloom.make 100 0.01) ["cherry", "date"]
val fu = Bloom.union (fa, fb)
val () =
  List.app
    (fn k => print ("  member \"" ^ k ^ "\" in union = " ^ Bool.toString (Bloom.member fu k) ^ "\n"))
    ["apple", "cherry", "elderberry"]

val () = print "\ntoBytes / fromBytes round trip:\n"
val bytes = Bloom.toBytes f
val f' = valOf (Bloom.fromBytes bytes)
val () = print ("  serialized length = " ^ Int.toString (String.size bytes) ^ " hex chars\n")
val () =
  print ("  member \"apple\" after round trip = "
         ^ Bool.toString (Bloom.member f' "apple") ^ "\n")

val () = print "\nCounting filter: insert \"grape\" twice, then delete it twice:\n"
val cf0 = Bloom.makeCounting 50 0.01
val cf1 = Bloom.insertC cf0 "grape"
val cf2 = Bloom.insertC cf1 "grape"
val () = print ("  countSumC after 2 inserts        = " ^ Int.toString (Bloom.countSumC cf2) ^ "\n")
val cf3 = Bloom.deleteC cf2 "grape"
val () =
  print ("  memberC \"grape\" after 1 of 2 deletes = "
         ^ Bool.toString (Bloom.memberC cf3 "grape") ^ "\n")
val cf4 = Bloom.deleteC cf3 "grape"
val () =
  print ("  memberC \"grape\" after 2 of 2 deletes = "
         ^ Bool.toString (Bloom.memberC cf4 "grape") ^ "\n")
