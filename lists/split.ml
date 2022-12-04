let split l x =
  let rec first l n = match (l, n) with
    | ([], _) -> []
    | (_, 0) -> []
    | (h :: t, n') -> h :: (first t (n'-1)) in
  let rec second l n = match (l, n) with
    | ([], _) -> []
    | (l', 0) -> l'
    | (_::t, n') -> second t (n'-1) in
  ((first l x), (second l x))