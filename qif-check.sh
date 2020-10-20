# /usr/bin/sh

cmd=util/dist/build/qif/qif
for i in qifs/encoded/qpack-06/*/*.4096.* ; do
  qif=qifs/qifs/`basename $i | sed 's/\..*$//'`.qif
  echo $cmd $i $qif
  $cmd $i $qif
done
