main(x,y,z)
[
  x := y
  x := 0
  z := ~y
  y := x + y
  x := M[y]
  x := M[0]
  a := CALL main(x,y,z)
  M[0] := x
  M[x] := x
  LABEL l1
  GOTO l1
  RETURN z
  LABEL l2
  IF x < y THEN l1 ELSE l2
]
main(x,y,z)
[
  x := y
  x := 0
  z := ~y
  y := x + y
  x := M[y]
  x := M[0]
  a := CALL main(x,y,z)
  M[0] := x
  M[x] := x
  LABEL l1
  GOTO l1
  RETURN z
  LABEL l2
  IF x < y THEN l1 ELSE l2
]
