#a simple script to help me move arround the final files
#for ps3 runs

#cd test
#./cil.sh ./tests/pp_sync_test.c
#cp final* ../../libtpc/tests
#cd ..

cd ./test/apps/conv2d
./cil.sh --out-name=conv2d conv2d_ppe.c 
#./cil.sh -DSTATISTICS --with-stats --out-name=conv2d conv2d_ppe.c 
cp * ../../../../libtpc/tests/conv2d
cd ../../../

