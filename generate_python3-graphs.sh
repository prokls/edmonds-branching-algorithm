#!/bin/zsh

for g in tests/*.in.graph;
do
  BASE="${g:t:r:r}"
  python3 python3/edmonds.py "$g" > "tmp.out.graph";
  python3 utils/visualize_graph.py -n "$BASE" "tmp.out.graph" > "$BASE"
  dot -Tpng "$BASE" > "python3-graphs/$BASE.png"
  rm "tmp.out.graph" "$BASE"
done

