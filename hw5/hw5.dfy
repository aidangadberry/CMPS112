-- Assignment 5 - 3/14/16 - Aidan Gadberry agadberr


-- #1
method Min(a: int, b: int) returns (c: int)
  ensures ((c == a && c < b) || (c == b && c <= a))
{
  if (a < b){
    return a;
  } else {
    return b;
  }
}

method TestMin()
{
  var m := Min(12,5);
  assert m == 5;
  var n := Min(23,42);
  assert n == 23;
}

-- #2
method Search(arr: array<int>, element: int) returns (idx: int)
  requires arr != null
  ensures 0 <= idx ==> idx < arr.Length && arr[idx] == element
  ensures idx < 0 ==> forall k: int :: 0 <= k < arr.Length ==>  arr[k] != element 
  ensures -1 <= idx
{
  var n := 0;
  while (n < arr.Length)
    invariant 0 <= n <= arr.Length
    invariant forall k :: 0 <= k < n ==> arr[k] != element
  {
    if (arr[n] == element) {
      return n;
    }
    n := n + 1;
  }
  return -1;
}

method TestSearch()
{
  var arr := new int[3];
  arr[0] := 23;
  arr[1] := 21;
  arr[2] := 22;
  var s := Search(arr, 21);
  assert s == 1;
  var t := Search(arr, 20);
  assert t == -1;
}

-- #3
method BinarySearch(arr: array<int>, element: int) returns (idx: int)
  requires arr != null
  requires forall j, k :: 0 <= j < k < arr.Length ==> arr[j] <= arr[k]
  ensures 0 <= idx ==> idx < arr.Length && arr[idx] == element
  ensures idx < 0 ==> forall k: int :: 0 <= k < arr.Length ==>  arr[k] != element 
  ensures -1 <= idx
{
  if (arr.Length == 0) {
    return -1;
  }
  var left := 0;
  var right := arr.Length;
  while (left < right)
    invariant 0 <= left <= right <= arr.Length
    invariant forall k :: 0 <= k < arr.Length && !(left <= k < right) ==> arr[k] != element
  {
    var mid := (left + right) / 2;
    if (arr[mid] == element) {
      return mid;
    }
    if (arr[mid] < element) {
      left := mid + 1;
    } else {
      right := mid;
    }
  }
  return -1;
}

method TestBinarySearch()
{
  var arr := new int[3];
  arr[0] := 21;
  arr[1] := 22;
  arr[2] := 23;
  var s := BinarySearch(arr, 22);
  assert s == 1;
  var t := BinarySearch(arr, 24);
  assert t == -1;
}