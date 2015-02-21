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
#include "PThreads.h"
#include <stdlib.h>
#include <string.h>
#include <pthread.h>

void thread_init(int *info) {
    pthread_t stid;
    static int init  = 0;
           int index = 0;
    *info = FT_OK;

    if (init) {
        *info = FT_EINIT;
        return;
    }

    threads = NULL;
    mutexes = NULL;

    thread_attributes = NULL;
    mutex_attributes  = NULL;

    thread_keys = NULL;
    once_ctrls  = NULL;

    array_init(&threads, INIT_SIZE);
    array_init(&mutexes, INIT_SIZE);

    array_init(&thread_attributes, INIT_SIZE);
    array_init(&mutex_attributes,  INIT_SIZE);

    array_init(&thread_keys, INIT_SIZE);
    array_init(&once_ctrls,  INIT_SIZE);

    /* allocate and store the thread master id */
    threads->data[0] = (pthread_t*) malloc(sizeof(pthread_t));
    stid = pthread_self();

    memcpy(threads->data[0], &stid, sizeof(pthread_t));
    threads->after++;

    init = true;
    is_initialized = init;
}

void thread_alloc(int *thread_id, int *info) {
    if (is_initialized == false) {
        *info = FT_EINIT;
        return;
    }

    pthread_mutex_lock(&(threads->mutex));
    if (threads->after == threads->size) {
        array_resize(&threads, threads->size * 2);
    }

    threads->data[threads->after] = (pthread_t*) malloc(sizeof(pthread_t));

    *thread_id = threads->after;
    threads->after++;

    pthread_mutex_unlock(&(threads->mutex));
}

void thread_create(int *thread_id, int *attribute_id, void *(**routine)(void *),
                   void *argument, int* info) {
    pthread_attr_t *attribute;
    int index = 0;
        *info = FT_OK;

    if (is_initialized == false) {
        *info = FT_EINIT;
        return;
    }

    if (is_valid(threads, *thread_id) == false) {
        *info = FT_EINVALID;
        return;
    }

    pthread_mutex_lock(&(threads->mutex));
    if (*attribute_id == -1) {
        attribute = (pthread_attr_t*) malloc(sizeof(pthread_attr_t));
        pthread_attr_init(attribute);
        pthread_attr_setdetachstate(attribute, PTHREAD_CREATE_JOINABLE);
    } else {
        if (is_valid(thread_attributes, *attribute_id) == false) {
            pthread_mutex_unlock(&(threads->mutex));
            *info = FT_EINVALID;
            return;
        }

        attribute = thread_attributes->data[*attribute_id];
    }

    *info = pthread_create(threads->data[*thread_id], attribute, (*routine), argument);
    if (*attribute_id == -1) {
        free(attribute);
    }

    if (*info) {
        pthread_mutex_unlock(&(threads->mutex));
        return;
    }

    pthread_mutex_unlock(&(threads->mutex));
}

void thread_join(int *thread_id, void **value_pointer, int *info) {
    *info = FT_OK;

    if (is_initialized == false) {
        *info = FT_EINIT;
        return;
    }

    pthread_mutex_lock(&(threads->mutex));
    if (is_valid(threads, *thread_id) == false) {
        *info = FT_EINVALID;
        return;
    }

    *info = pthread_join(*((pthread_t*) (threads->data[*thread_id])), value_pointer);
    if (*info) {
        pthread_mutex_unlock(&(threads->mutex));
        return;
    }

    free(threads->data[*thread_id]);
    threads->data[*thread_id] = NULL;

    pthread_mutex_unlock(&(threads->mutex));
}

void thread_cancel(int *thread_id, int *info) {
    *info = FT_OK;

    if (is_initialized == false) {
        *info = FT_EINIT;
        return;
    }

    if (is_valid(threads, *thread_id) == false) {
        *info = FT_EINVALID;
        return;
    }

    *info = pthread_cancel(*((pthread_t*) (threads->data[*thread_id])));
}

void thread_exit(void *value_pointer) {
    pthread_exit(value_pointer);
}

void thread_destroy(int* info) {
    for(int id = 1; id < threads->after; ++id) {
        thread_cancel(&id, info);
    }
    array_delete(threads);
    array_delete(thread_attributes);
    array_delete(thread_keys);
    array_delete(once_ctrls);

    for(int id = 0; id < mutexes->after; ++id) {
        thread_mutex_destroy(&id, info);
    }
    array_delete(mutexes);
    array_delete(mutex_attributes);

    *info = FT_OK;
}

void thread_mutex_init(int *mutex_id, int *attribute_id, int *info) {
    pthread_mutexattr_t *attribute;
    *info = FT_OK;

    if (is_initialized == false) {
        *info = FT_EINIT;
        return;
    }

    pthread_mutex_lock(&(mutexes->mutex));
    if (mutexes->after == mutexes->size) {
        array_resize(&mutexes,mutexes->size * 2);
    }

    mutexes->data[mutexes->after] = (pthread_mutex_t*) malloc(sizeof(pthread_mutex_t));

    if (*attribute_id == -1) {
        attribute = NULL;
    } else {
        attribute = mutex_attributes->data[*attribute_id];
    }

    *info = pthread_mutex_init((pthread_mutex_t*) (mutexes->data[mutexes->after]), attribute);

    if (*info) {
        pthread_mutex_unlock(&(mutexes->mutex));
        return;
    }

    *mutex_id = mutexes->after;
    mutexes->after++;

    pthread_mutex_unlock(&(mutexes->mutex));
}

void thread_mutex_lock(int *mutex_id, int *info) {
    *info = FT_OK;

    if (is_initialized == false) {
        *info = FT_EINIT;
        return;
    }

    if (is_valid(mutexes, *mutex_id) == false) {
        *info = FT_EINVALID;
        return;
    }

    *info = pthread_mutex_lock((pthread_mutex_t*)(mutexes->data[*mutex_id]));
}

void thread_mutex_unlock(int *mutex_id, int *info) {
    *info = FT_OK;

    if (is_initialized == false) {
        *info = FT_EINIT;
        return;
    }

    if (is_valid(mutexes,*mutex_id) == false) {
        *info = FT_EINVALID;
        return;
    }

    *info = pthread_mutex_unlock((pthread_mutex_t*)(mutexes->data[*mutex_id]));
}

void thread_mutex_destroy(int *mutex_id, int *info) {
    *info = FT_OK;

    if (is_initialized == false) {
        *info = FT_EINIT;
        return;
    }

    pthread_mutex_lock(&(mutexes->mutex));
    if (is_valid(mutexes, *mutex_id) == false) {
        pthread_mutex_unlock(&(mutexes->mutex));
        *info = FT_EINVALID;
        return;
    }

    *info = pthread_mutex_destroy(((pthread_mutex_t*) (mutexes->data[*mutex_id])));
    if (*info) {
        pthread_mutex_unlock(&(mutexes->mutex));
        return;
    }

    free(mutexes->data[*mutex_id]);
    mutexes->data[*mutex_id] = NULL;

    pthread_mutex_unlock(&(mutexes->mutex));
}
