int putchar(int c);

struct Point { int x; int y; };

int fib(int n) {
    if (n <= 1) return n;
    return fib(n - 1) + fib(n - 2);
}

int main(void) {
    struct Point p;
    p.x = fib(7);
    p.y = fib(8);
    // fib(7) = 13, fib(8) = 21
    putchar(48 + p.x / 10);
    putchar(48 + p.x % 10);
    putchar(32);
    putchar(48 + p.y / 10);
    putchar(48 + p.y % 10);
    putchar(10);
    return 0;
}
