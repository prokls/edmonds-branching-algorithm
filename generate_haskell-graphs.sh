#!/bin/zsh

for g in tests/*.in.graph;
do
  BASE="${g:t:r:r}"
  ./haskell/edmonds "$g" > "tmp.out.graph";
  python3 utils/visualize_graph.py -n "$BASE" "tmp.out.graph" > "$BASE"
  dot -Tpng "$BASE" > "haskell-graphs/$BASE.png"
  rm "tmp.out.graph" "$BASE"
done

