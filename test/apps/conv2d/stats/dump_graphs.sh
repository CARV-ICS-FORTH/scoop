for((spu=1; spu<=6; spu=spu+1))
do
  echo "${spu} "
  ../conv2d37 ${spu} > /dev/null
  ../conv2d37 ${spu} > conv2d_dump_${spu}.txt
done
