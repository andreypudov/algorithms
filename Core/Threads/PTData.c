/*
 * The Laboratory of Algorithms
 *
 * The MIT License
 *
 * Copyright 2011-2015 Andrey Pudov.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the 'Software'), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

#include "PTData.h"
#include <stdlib.h>

void array_init(array_t **array, int size) {
    if (*array == NULL) {
        *array = (array_t*) malloc(sizeof(array_t));
    }

    pthread_mutex_init(&((*array)->mutex),NULL);
    (*array)->data = (void**) malloc(sizeof(void*) * size);

    for(int index = 0; index < size; ++index) {
        (*array)->data[index] = NULL;
    }

    (*array)->size  = size;
    (*array)->after = 0;
}

void array_resize(array_t **array, int size) {
    (*array)->data = (void**) realloc((*array)->data, sizeof(void*) * size);
    (*array)->size = size;

    for(int index = (*array)->after; index < size; ++index) {
        (*array)->data[index] = NULL;
    }
}

void array_delete(array_t *array) {
    free(array->data);
    free(array);
}

void varray_init(varray_t **array, int size) {
    if (*array == NULL) {
        *array = (varray_t*) malloc(sizeof(varray_t));
    }

    pthread_mutex_init(&((*array)->mutex),NULL);
    (*array)->data = (volatile void**) malloc(sizeof(void*) * size);

    for(int index = 0; index < size; ++index) {
        (*array)->data[index] = NULL;
    }

    (*array)->size = size;
    (*array)->after = 0;
}

void varray_resize(varray_t **array, int size) {
    (*array)->data = (volatile void**)realloc((*array)->data,sizeof(volatile void*)*size);
    (*array)->size = size;

    for(int index = (*array)->after; index < size; ++index) {
        (*array)->data[index] = NULL;
    }
}

void varray_delete(varray_t *array) {
    free(array->data);
    free(array);
}

int is_valid(array_t *array, int id) {
    return ((id >= 0) && (id < array->after) && (array->data[id] != NULL)) ? 1 : 0;
}
