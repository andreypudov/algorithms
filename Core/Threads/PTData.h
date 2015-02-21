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

#ifndef PT_DATA_H_
#define PT_DATA_H_

#include <pthread.h>

#define INIT_SIZE    4
#define FT_OK        0
#define FT_EINIT    -1
#define FT_EINVALID -2

#define false 0
#define true  1

typedef struct array_tag {
    void **data;
    int size;
    int after;
    pthread_mutex_t mutex;
} array_t;

typedef struct varray_tag {
    volatile void **data;
    int size;
    int after;
    pthread_mutex_t mutex;
} varray_t;


int is_initialized;

array_t *threads;
array_t *mutexes;
/*array_t *conditions;
array_t *barriers;
array_t *rwlocks;*/

array_t *thread_attributes;
array_t *mutex_attributes;
/*array_t *condition_attributes;
array_t *barrier_attributes;
array_t *rwlock_attributes;*/

array_t *thread_keys;
array_t *once_ctrls;
//varray_t *spinlocks;

void array_init(array_t **array, int size);
void array_resize(array_t **array, int size);
void array_delete(array_t *array);

void varray_init(varray_t **array, int size);
void varray_resize(varray_t **array, int size);
void varray_delete(varray_t *array);

int is_valid(array_t *arr, int id);

#endif
