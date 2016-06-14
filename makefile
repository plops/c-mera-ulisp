ulisp: ulisp.c
	gcc -Og -Wall -Wextra ulisp.c -o ulisp

ulisp.s: ulisp.c
	gcc -S -Wall -Wextra -O2  ulisp.c
	cat ulisp.s
