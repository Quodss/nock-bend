type Result
  = (Ok value)
  | (Err message)
##
(Result/wrap val) = (Result/Ok val)
##
Result/bind (Result/Ok val) nxt = ((undefer nxt) val)
Result/bind (Result/Err message) _nxt = (Result/Err message)
##
##  Nock: Tree u24 -> Tree u24 -> Result (Tree u24)
Nock s (Tree/Node (Tree/Node a b) c) = 
  let (o1, o2) = ((Nock s ![a, b]), (Nock s c))
  with Result {
    ask r1 = o1;
    ask r2 = o2;
    (wrap ![r1, r2])
  }
Nock s (Tree/Node (Tree/Leaf 0) (Tree/Leaf a)) = (Slot s a)
Nock s (Tree/Node (Tree/Leaf 1) a) = (Result/Ok a)
Nock s (Tree/Node (Tree/Leaf 2) (Tree/Node a b)) =
  let (o1, o2) = ((Nock s a), (Nock s b))
  with Result {
    ask r1 = o1;
    ask r2 = o2;
    (Nock r1 r2)
  }
Nock s (Tree/Node (Tree/Leaf 3) a) = 
  with Result {
    ask n = (Nock s a)
    match n {
      Tree/Node: (wrap !0)
      Tree/Leaf: (wrap !1)
    }
  }
Nock s (Tree/Node (Tree/Leaf 4) a) =
  with Result {
    ask n = (Nock s a)
    match n {
      Tree/Node: (Result/Err "increment cell")
      Tree/Leaf: (wrap !(+ 1 n.value))
    }
  }
Nock s (Tree/Node (Tree/Leaf 5) (Tree/Node a b)) =
  let (o1, o2) = ((Nock s a), (Nock s b))
  with Result {
    ask r1 = o1;
    ask r2 = o2;
    if (== (Eq r1 r2) 0) {  # Loobean %.y
      (wrap !0)
    }
    else {
      (wrap !1)
    }
  }
Nock s (Tree/Node (Tree/Leaf 6) (Tree/Node a (Tree/Node b c))) =
  with Result {
    ask cond = (Nock s a)
    match cond {
      Tree/Node: (Result/Err "cell cond")
      Tree/Leaf:
        switch cond.value {
          0: (Nock s b)
          1: (Nock s c)
          _: (Result/Err "loobean out of bounds")
        }
    }
  }
Nock s (Tree/Node (Tree/Leaf 7) (Tree/Node a b)) =
  with Result {
    ask s1 = (Nock s a)
    (Nock s1 b)
  }
Nock s (Tree/Node (Tree/Leaf 8) (Tree/Node a b)) =
  with Result {
    ask v = (Nock s a)
    (Nock ![v, s] b)
  }
Nock s (Tree/Node (Tree/Leaf 9) (Tree/Node (Tree/Leaf a) b)) =
  with Result {
    ask core = (Nock s b)
    ask fml = (Slot core a)
    (Nock core fml)
  }
Nock s (Tree/Node (Tree/Leaf 10) (Tree/Node (Tree/Node (Tree/Leaf a) b) c)) =
  let (rec_mayb, don_mayb) = ((Nock s c), (Nock s b))
  with Result {
    ask rec = rec_mayb
    ask don = don_mayb
    (Edit a don rec)
  }
Nock s (Tree/Node (Tree/Leaf 11) (Tree/Node (Tree/Node a b) c)) =
  let (_, out) = ((Nock s b), (Nock s c))
  out
Nock s (Tree/Node (Tree/Leaf 11) (Tree/Node (Tree/Leaf a) b)) = (Nock s b)
Nock * * = (Result/Err "weird formula")
##
## Slot: Tree u24 -> u24 -> Result (Tree u24)
##
Slot a 0 = (Result/Err "slot 0")
Slot a 1 = (Result/Ok a)
Slot (Tree/Node a b) 2 = (Result/Ok a)
Slot (Tree/Node a b) 3 = (Result/Ok b)
Slot a b =
  if (== (% b 2) 0) {
    with Result {
      ask n = (Slot a (/ b 2))
      match n {
        Tree/Leaf: (Result/Err "no head")
        Tree/Node: (wrap n.left)
      }
    }
  }
  else {
    with Result {
      ask n = (Slot a (/ b 2))
      match n {
        Tree/Leaf: (Result/Err "no tail")
        Tree/Node: (wrap n.right)
      }
    }
  }
##
##  Edit: u24 -> Tree u24 -> Tree u24 -> (Result (Tree u24))
Edit 0 * * = (Result/Err "edit 0")
Edit 1 don rec = (Result/Ok don)
Edit axis don rec = 
  with Result {
    if (== (% axis 2) 0) {
      ask n = (Slot rec (+ axis 1))
      (Edit (/ axis 2) ![don, n] rec)
    }
    else {
      ask n = (Slot rec (- axis 1))
      (Edit (/ axis 2) ![n, don] rec)
    }
  }
##
## Eq: Tree u24 -> Tree u24 -> Loobean
Eq (Tree/Leaf a) (Tree/Leaf b) = (^ 1 (== a b))            # Boolean to loobean
Eq (Tree/Node a b) (Tree/Node c d) = (| (Eq a c) (Eq b d)) # | is conjunction for loobeans
Eq * * = 1 # %.n
##
main =
  let call_fml = ![!9, ![!2, ![!0, !1]]]
  let trap_turn = ![![!7, ![![!8, ![![!9, ![!4, ![!0, !3]]], ![!9, ![!2, ![!10, ![![!6, ![![!7, ![![!0, !3], ![!1, ![!100, ![!101, ![!102, ![!103, ![!104, ![!105, ![!106, ![!107, ![!108, ![!109, ![!110, ![!111, ![!112, ![!113, ![!114, ![!115, ![!116, ![!117, ![!118, ![!119, ![!120, ![!121, ![!122, ![!123, ![!124, ![!125, ![!126, ![!127, ![!128, ![!129, ![!130, ![!131, ![!132, ![!133, ![!134, ![!135, ![!136, ![!137, ![!138, ![!139, ![!140, ![!141, ![!142, ![!143, ![!144, ![!145, ![!146, ![!147, ![!148, ![!149, ![!150, ![!151, ![!152, ![!153, ![!154, ![!155, ![!156, ![!157, ![!158, ![!159, ![!160, ![!161, ![!162, ![!163, ![!164, ![!165, ![!166, ![!167, ![!168, ![!169, ![!170, ![!171, ![!172, ![!173, ![!174, ![!175, ![!176, ![!177, ![!178, ![!179, ![!180, ![!181, ![!182, ![!183, ![!184, ![!185, ![!186, ![!187, ![!188, ![!189, ![!190, ![!191, ![!192, ![!193, ![!194, ![!195, ![!196, ![!197, ![!198, ![!199, ![!200, !0]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]], ![!7, ![![!0, !3], ![!9, ![!10, ![!0, !3]]]]]]], ![!0, !2]]]]]]], ![!1, !0]]], ![![![!8, ![![!1, ![!0, ![![!0, !15], ![!0, ![!0, !0]]]]], ![![!1, ![!8, ![![!1, ![!6, ![![!5, ![![!1, !0], ![!0, !28]]], ![![!1, !0], ![![!8, ![![!0, !29], ![!9, ![!2, ![!10, ![![!6, ![!0, !120]], ![!0, !2]]]]]]], ![!9, ![!2, ![!10, ![![!28, ![!0, !57]], ![!0, !1]]]]]]]]]], ![!9, ![!2, ![!0, !1]]]]]], ![!0, !1]]]], ![![!8, ![![!1, !0], ![![!1, ![!6, ![![!5, ![![!1, !0], ![!0, !6]]], ![![!0, !0], ![!8, ![![!1, !0], ![!8, ![![!1, ![!6, ![![!5, ![![!0, !30], ![!4, ![!0, !6]]]], ![![!0, !6], ![!9, ![!2, ![!10, ![![!6, ![!4, ![!0, !6]]], ![!0, !1]]]]]]]]], ![!9, ![!2, ![!0, !1]]]]]]]]]]], ![!0, !1]]]], ![![!1, ![!7, ![![!8, ![![!9, ![!4, ![!0, !3]]], ![!9, ![!2, ![!10, ![![!6, ![![!7, ![![!0, !3], ![!1, ![!100, ![!101, ![!102, ![!103, ![!104, ![!105, ![!106, ![!107, ![!108, ![!109, ![!110, ![!111, ![!112, ![!113, ![!114, ![!115, ![!116, ![!117, ![!118, ![!119, ![!120, ![!121, ![!122, ![!123, ![!124, ![!125, ![!126, ![!127, ![!128, ![!129, ![!130, ![!131, ![!132, ![!133, ![!134, ![!135, ![!136, ![!137, ![!138, ![!139, ![!140, ![!141, ![!142, ![!143, ![!144, ![!145, ![!146, ![!147, ![!148, ![!149, ![!150, ![!151, ![!152, ![!153, ![!154, ![!155, ![!156, ![!157, ![!158, ![!159, ![!160, ![!161, ![!162, ![!163, ![!164, ![!165, ![!166, ![!167, ![!168, ![!169, ![!170, ![!171, ![!172, ![!173, ![!174, ![!175, ![!176, ![!177, ![!178, ![!179, ![!180, ![!181, ![!182, ![!183, ![!184, ![!185, ![!186, ![!187, ![!188, ![!189, ![!190, ![!191, ![!192, ![!193, ![!194, ![!195, ![!196, ![!197, ![!198, ![!199, ![!200, !0]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]], ![!7, ![![!0, !3], ![!9, ![!10, ![!0, !3]]]]]]], ![!0, !2]]]]]]], ![!1, !0]]]], ![!0, !1]]]], ![![![!8, ![![!1, ![!0, !0]], ![![!1, ![!8, ![![!1, ![!6, ![![!5, ![![!0, !28], ![!4, ![!0, !29]]]], ![![!1, !0], ![![!0, !28], ![!9, ![!2, ![!10, ![![!28, ![!4, ![!0, !28]]], ![!0, !1]]]]]]]]]], ![!9, ![!2, ![!0, !1]]]]]], ![!0, !1]]]], ![!8, ![![!8, ![![!1, !0], ![![!1, ![!8, ![![!0, !6], ![!8, ![![!5, ![![!0, !14], ![!0, !2]]], ![!0, !6]]]]]], ![!0, !1]]]], ![![!1, ![!8, ![![!1, !0], ![![!1, ![!8, ![![!6, ![![!3, ![!0, !6]], ![![![!8, ![![!0, !30], ![!9, ![!2, ![!10, ![![!6, ![!0, !28]], ![!0, !2]]]]]]], ![!8, ![![!7, ![![!0, !7], ![!8, ![![!9, ![!5, ![!0, !7]]], ![!9, ![!2, ![!10, ![![!6, ![!0, !14]], ![!0, !2]]]]]]]]], ![!9, ![!2, ![!10, ![![!6, ![!0, !29]], ![!0, !2]]]]]]]], ![!6, ![![!5, ![![!1, !0], ![!0, !6]]], ![![!1, !0], ![!0, !0]]]]]]], ![!8, ![![!5, ![![!0, !14], ![!0, !2]]], ![!0, !6]]]]]], ![!0, !1]]]]], ![!0, !1]]]]], !0]]]
  (Nock trap_turn call_fml)