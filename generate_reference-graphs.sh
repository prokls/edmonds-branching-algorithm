#!/bin/zsh

for g in tests/*.in.graph;
do
  BASE="${g:t:r:r}"
  python3 utils/bruteforce.py -b "$g" > "tmp.out.graph";
  python3 utils/visualize_graph.py -n "$BASE" "tmp.out.graph" > "$BASE"
  dot -Tpng "$BASE" > "reference-graphs/$BASE.png"
  rm "tmp.out.graph" "$BASE"
done

