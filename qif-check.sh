# /bin/sh

cmd=dist/build/qif-dec/qif-dec

for enc in qifs/encoded/qpack-05/*/*.out.*.*.? ; do
  cap=`basename $enc | sed -e 's/^[-a-zA-Z0-9]*\.out\.\([0-9]*\)\.[0-9]*\.[01]$/\1/g'`
  qif=qifs/qifs/`basename $enc | sed 's/\..*$//'`.qif
  printf "%s " $enc # excluding \n
  $cmd $cap $enc $qif
  if [ $? -ne 0 ]; then
    echo $cmd $cap $enc $qif
  fi
done
