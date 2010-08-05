#a simple script to help me move arround the final files
#for ps3 runs

###################### benchamrk ###############################
cd test && ./cil.sh -DSTATISTICS --with-stats ./benchmark.c && cp final* ../../libtpc && cd ..

###################### pp_sync_test ###############################
#cd test
#./cil.sh ./tests/pp_sync_test.c
#cp final* ../../libtpc/tests
#cd ..

###################### CONV2D ###############################
#cd ./test/apps/conv2d
##./cil.sh --out-name=conv2d conv2d_ppe.c 
#./cil.sh -DSTATISTICS --with-stats --out-name=conv2d conv2d_ppe.c 
#cp * ../../../../libtpc/tests/conv2d
#cd ../../../

###################### Cholesky ###############################
# cd ./test/apps/cholesky-multifile
# ./cil.sh --out-name=cholesky cholesky_kernels.c 
# #./cil.sh -DSTATISTICS --with-stats --out-name=cholesky cholesky_kernels.c 
# cp -R * ../../../../libtpc/tests/cholesky
# cd ../../../

###################### FFT ###############################
# cd ./test/apps/FFT
# ./cil.sh --out-name=fft fft_in.c 
# # #./cil.sh -DSTATISTICS --with-stats --out-name=fft fft_in.c 
# cp -R * ../../../../libtpc/tests/fft
# cd ../../../

