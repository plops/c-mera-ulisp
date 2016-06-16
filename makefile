ulisp: ulisp.c
	gcc -Og -ggdb3 -pg --coverage -fno-omit-frame-pointer -ffloat-store -fno-common -fstrict-aliasing -Wswitch-default -Wfloat-equal -Winline -Wundef -Wnested-externs  -Wstrict-aliasing=3 -Wall -Wextra -pedantic -std=c99 -Wshadow -Wpointer-arith -Wcast-qual -Wstrict-prototypes  ulisp.c  -o ulisp

ulisp.s: ulisp.c
	gcc -S -Wall -Wextra -O2  ulisp.c
	cat ulisp.s

clean:
	rm ulisp
