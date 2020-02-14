#include <stdio.h>
#include <string.h>
#include <stdlib.h>

struct string {
    size_t length;
    char str[];
};

void printInt(int i) {
    printf("%d\n", i);
}

void printString(struct string *str) {
    fwrite(str->str, sizeof(char), str->length, stdout);
    putchar('\n');
}

struct string *concat(struct string *ls, struct string *rs) {
    struct string *res = malloc(sizeof(size_t) + ls->length + rs->length);
    res->length = ls->length + rs->length;
    memcpy(res->str, ls->str, ls->length);
    memcpy(res->str + ls->length, rs->str, rs->length);
    return res;
}

int readInt(void) {
    int i;
    scanf("%d", &i);
    return i;
}

struct string *readString(void) {
    char *tmp;
    size_t n = 0;

    scanf(" ");
    n = getline(&tmp, &n, stdin);

    if (tmp[n - 1] == '\n') {
                --n;
    }

    struct string *str = malloc(sizeof(size_t) + n);
    str->length = n;
    memcpy(str->str, tmp, n);

    free(tmp);
    return str;
}

void* alloc(unsigned sz) {
    return calloc(sz, 1);
}

void* clone_object(void *data, size_t size) {
    void *res = malloc(size);
    memcpy(res, data, size);
    return res;
}

void error(void) {
    fprintf(stderr, "error\n");
    exit(1);
}

