# /usr/bin/sh

cmd=util/dist/build/qif/qif
for i in `cat ~/list`; do
  qif=qifs/qifs/`basename $i | sed 's/\..*$//'`.qif
  echo $cmd $i $qif
  $cmd $i $qif
done
