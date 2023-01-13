overlap : overlap.f90 mt19937-64.o
	f95 overlap.f90 mt19937-64.o -o overlap -O3

mt19937-64.o : mt19937-64.f95
	f95 -c -O3 mt19937-64.f95

clean:
	rm mt19937_64.mod
	rm mt19937-64.o
	rm overlap
