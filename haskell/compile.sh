# or: runhaskell edmonds.hs

echo "[ ] Compiling ..."
ghc -o edmonds edmonds.hs -threaded -prof -fprof-auto -fprof-cafs
echo "[X] Compiling ..."

echo "[ ] Running ..."
./edmonds "$@" +RTS
echo "[X] Running ..."
