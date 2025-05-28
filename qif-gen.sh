# /bin/sh

rm -f *.out.*

cmd=dist/build/qif-enc/qif-enc

for qif in qifs/qifs/*.qif; do
  for cap in 0 256 512 4096; do
    for blk in 0 100; do
      for ack in 0 1; do
        echo "$cmd $qif $cap $blk $ack"
        $cmd $qif $cap $blk $ack
      done
    done
  done
done

mv *.out.* qifs/encoded/qpack-05/haskell-quic
