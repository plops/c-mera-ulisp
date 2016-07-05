# https://gcc.gnu.org/ml/gcc-help/2003-08/msg00128.html
DEADCODESTRIP := #-Wl,-static -fdata-sections -ffunction-sections -Wl,--gc-sections
#-Wl,-s
WARN := -Wswitch-default -Wfloat-equal -Winline -Wundef -Wnested-externs  -Wstrict-aliasing=3 -Wall -Wextra -pedantic -std=c99 -Wshadow -Wpointer-arith -Wcast-qual -Wstrict-prototypes -Wshift-negative-value -Wshift-overflow=2 -Wtautological-compare -Wnull-dereference -Wduplicated-cond  -Wmisleading-indentation 

#CFLAGS := -Os -fomit-frame-pointer #-fno-omit-frame-pointer -ffloat-store -fno-common -fstrict-aliasing

#CFLAGS := -Og -ggdb3 -fno-omit-frame-pointer -ffloat-store -fno-common -fstrict-aliasing -fsanitize=address -fsanitize=bounds-strict -march=native -fdelete-null-pointer-checks  -fsanitize=undefined

CFLAGS := -Og -ggdb3 -fno-omit-frame-pointer -ffloat-store -fno-common -fstrict-aliasing -march=native  

ulisp-interp: ulisp.c
	gcc  $(CFLAGS) $(DEADCODESTRIP) $(WARN)  ulisp.c  -o ulisp-interp

#--coverage -pg

ulisp.s: ulisp.c
	gcc -S -Wall -Wextra -O2  ulisp.c
	cat ulisp.s

clean:
	rm ulisp-interp
