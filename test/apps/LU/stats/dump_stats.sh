exp_dir="."
splash_dir="/home1/private/tzenakis/tpcapps/splash/lu-time/contiguous_blocks"

for ((n=256; n<=4096; n=2*n))
do
  for ((b=8; b<=32; b=2*b))
  do
    for((spu=1; spu<=6; spu=spu+1))
    do
      dump_file="${exp_dir}/LU_dump_${n}_${spu}_${b}.txt"
      command="-n${n} -a${spu} -b${b}"
      echo "${command} > ${dump_file}"
      ../LU ${command} > ${dump_file}
      #numactl --membind=1 --cpunodebind=1 ${command} > ${dump_file}
    done
#${splash_dir}/LU -n${n} -b${b} > "LUS_dump_${n}_${b}.txt"
    echo
  done
done

