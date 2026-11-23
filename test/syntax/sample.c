/* A sample of C for the tests: every line parses. */
#include <stdio.h>
#include <stdlib.h>
#include "local.h"

#define MAX_ITEMS 64
#define SQUARE(x) ((x) * (x))

#ifdef DEBUG
#  define LOG(...) fprintf(stderr, __VA_ARGS__)
#else
#  define LOG(...) ((void) 0)
#endif

typedef unsigned long size_type;

struct point {
    int x, y;
    const char *label;
};

typedef struct {
    struct point *items;
    size_type count;
} point_list;

enum shade { LIGHT = 1, DARK = 2, BOTH = LIGHT | DARK };

union number {
    long whole;
    double real;
};

static int total = 0;
extern const char *program_name;

/* Add every point's coordinates to TOTAL; return how many there were. */
static size_type add_all(const point_list *list, int (*weigh)(const struct point *))
{
    size_type i;
    for (i = 0; i < list->count; i++) {
        const struct point *p = &list->items[i];
        total += weigh ? weigh(p) : p->x + p->y;
    }
    return list->count;
}

int main(int argc, char **argv)
{
    char buffer[MAX_ITEMS] = { 0 };
    double ratio = 3.5e-2;
    unsigned char mask = 0xFFu;
    point_list list = { NULL, 0 };

    if (argc > 1 && argv[1][0] == '-') {
        LOG("option %s\n", argv[1]);
    } else if (argc == 1) {
        puts("no arguments");
    }

    while (mask >>= 1)
        total++;

    switch (argc) {
    case 0:
        break;
    default:
        snprintf(buffer, sizeof buffer, "%d \"items\"\t%f", argc, ratio);
        break;
    }

    // line comments are C99
    goto done;
done:
    return add_all(&list, NULL) > SQUARE(2) ? EXIT_FAILURE : EXIT_SUCCESS;
}
