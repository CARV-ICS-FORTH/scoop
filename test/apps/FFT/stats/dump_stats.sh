exp_dir="."
splash_dir="/home1/private/tzenakis/tpcapps/splash/fft-time"

for ((n=10; n<=22; n=n+2))
do
  for((spu=1; spu<=6; spu=spu+1))
  do
    dump_file="${exp_dir}/FFT_dump_${n}_${spu}.txt"
    command="-m${n} -a${spu}"
    echo "${command} > ${dump_file}"
    ../FFT ${command} > ${dump_file}
    #numactl --membind=1 --cpunodebind=1 ${command} > ${dump_file}
  done
  echo "SPLASH/FFT -m${n}"
  ${splash_dir}/FFT -m${n} > "FFTS_dump_${n}.txt"
done

