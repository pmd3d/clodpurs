int putchar(int c);

struct Point {
    int x;
    int y;
};

int main(void) {
    struct Point p;
    p.x = 42;
    p.y = 99;
    int sum = p.x + p.y;
    // 141 -> '1' '4' '1'
    putchar(48 + sum / 100);
    putchar(48 + (sum / 10) % 10);
    putchar(48 + sum % 10);
    putchar(10);
    return 0;
}
