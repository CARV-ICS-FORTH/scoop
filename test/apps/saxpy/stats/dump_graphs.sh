for ((bs=1; bs<=16; bs=bs*2))
do
  for((spu=1; spu<=6; spu=spu+1))
  do
    for((n=8; n<=24; n=n+8))
    do
      echo "${n} ${bs} ${spu} "
      ../saxpy37 ${n} ${bs} ${spu} > /dev/null
      ../saxpy37 ${n} ${bs} ${spu} > saxpy_dump_${n}_${spu}_${bs}.txt
    done
  done
done
