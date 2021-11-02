# /usr/bin/sh

size=$1

cmd=dist/build/qif/qif

for i in qifs/encoded/qpack-05/*/*.out.$size.*.? ; do
  qif=qifs/qifs/`basename $i | sed 's/\..*$//'`.qif
  echo $cmd $size $i $qif
  $cmd $size $i $qif
done
