# /usr/bin/sh

cmd=dist/build/qif-dec/qif-dec

for size in 0 256 512 4096; do
  for i in qifs/encoded/qpack-05/*/*.out.$size.*.? ; do
    qif=qifs/qifs/`basename $i | sed 's/\..*$//'`.qif
    echo $i
    $cmd $size $i $qif
    if [ $? -ne 0 ]; then
      echo $cmd $size $i $qif
    fi
  done
done
