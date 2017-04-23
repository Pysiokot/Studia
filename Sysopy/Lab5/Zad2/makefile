all: 
	gcc master.c -o master
	gcc slave.c -lm -o slave

runMaster:
	./master dane 600

runSlave:
	./slave dane 10000000 100