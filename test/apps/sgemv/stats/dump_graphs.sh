for ((bs=16; bs<=64; bs=bs*2))
do
  for((spu=1; spu<=6; spu=spu+1))
  do
      echo "${n} ${bs} ${spu} "
      ../sgemv37 64 512 ${bs} ${spu} > /dev/null
      ../sgemv37 64 512 ${bs} ${spu} > sgemv_dump_32_${spu}_${bs}.txt
  done
done
