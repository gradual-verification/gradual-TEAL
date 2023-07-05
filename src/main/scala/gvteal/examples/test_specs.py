#@ requires true;
#@ requires True if curr.next == None else acc(curr.next.next) and acc(curr.next.val) and acc(curr.next.deleted) and curr.next.val >= curr.val and orderedListSegWithPrev(curr.next.next, None, curr.next.val, -1);
#@ loop_invariant acc(q);
#@ ensures true;
#@ fold orderedListSegWithPrev(a, c, aPrev, cVal);
#@ unfold orderedListSegWithPrev(a, c, aPrev, cVal);