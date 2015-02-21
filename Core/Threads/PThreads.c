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

#include <stdio.h>

void thread_init(int *info) {
    pthread_t stid;
    static int init  = 0;
           int index = 0;
    *info = FT_OK;

    if (init) {
        *info = FT_EINIT;
        return;
    }

    threads    = NULL;
    mutexes    = NULL;
    /*conditions = NULL;
    barriers   = NULL;
    rwlocks    = NULL;*/

    thread_attributes    = NULL;
    mutex_attributes     = NULL;
    /*condition_attributes = NULL;
    barrier_attributes   = NULL;
    rwlock_attributes    = NULL;*/

    thread_keys = NULL;
    once_ctrls  = NULL;
    //spinlocks   = NULL;

    array_init(&threads,    INIT_SIZE);
    array_init(&mutexes,    INIT_SIZE);
    /*array_init(&conditions, INIT_SIZE);
    array_init(&barriers,   INIT_SIZE);
    array_init(&rwlocks ,   INIT_SIZE);*/

    array_init(&thread_attributes,    INIT_SIZE);
    array_init(&mutex_attributes,     INIT_SIZE);
    /*array_init(&condition_attributes, INIT_SIZE);
    array_init(&barrier_attributes,   INIT_SIZE);
    array_init(&rwlock_attributes,    INIT_SIZE);*/


    array_init(&thread_keys, INIT_SIZE);
    array_init(&once_ctrls,  INIT_SIZE);
    //varray_init(&spinlocks,  INIT_SIZE);

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
    if (is_valid(threads,*thread_id) == false) {
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

    /*
    for(int id = 0; id < conditions->after; ++id) {
        thread_condition_destroy(&id, info);
    }
    array_delete(conditions);
    array_delete(condition_attributes);

    for(int id = 0; id < barriers->after; ++id) {
        thread_barrier_destroy(&id, info);
    }
    array_delete(barriers);
    array_delete(barrier_attributes);

    for(int id = 0; id < spinlocks->after; ++id) {
        thread_spin_destroy(&id, info);
    }
    varray_delete(spinlocks);

    for(int id = 0; id < rwlocks->after; ++id) {
        thread_rwlock_destroy(&id, info);
    }

    array_delete(rwlocks);
    array_delete(rwlock_attributes);
    */

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

/*
void thread_condition_init(int *condition_id, int *attribute_id, int *info) {
    pthread_condattr_t *attribute;
    *info = FT_OK;

    if (is_initialized == false) {
        *info = FT_EINIT;
        return;
    }

    pthread_mutex_lock(&(conditions->mutex));
    if (conditions->after == conditions->size) {
        array_resize(&conditions, conditions->size * 2);
    }

    conditions->data[conditions->after] = (pthread_cond_t*) malloc(sizeof(pthread_cond_t));
    if (*attribute_id == -1) {
        attribute = NULL;
    } else {
        attribute = condition_attributes->data[*attribute_id];
    }

    *info = pthread_cond_init((pthread_cond_t*)(conditions->data[conditions->after]), attribute);
    if (*info) {
        pthread_mutex_unlock(&(conditions->mutex));
        return;
    }

    *condition_id = conditions->after;
    conditions->after++;

    pthread_mutex_unlock(&(conditions->mutex));
}

void thread_condition_destroy(int *condition_id, int *info) {
    *info = FT_OK;

    if (is_initialized == false) {
        *info = FT_EINIT;
        return;
    }

    pthread_mutex_lock(&(conditions->mutex));
    if (is_valid(conditions, *condition_id) == false) {
        pthread_mutex_unlock(&(conditions->mutex));
        *info = FT_EINVALID;
        return;
    }

    *info = pthread_cond_destroy(((pthread_cond_t*) (conditions->data[*condition_id])));
    if (*info) {
        pthread_mutex_unlock(&(conditions->mutex));
        return;
    }

    free(conditions->data[*condition_id]);
    conditions->data[*condition_id] = NULL;

    pthread_mutex_unlock(&(conditions->mutex));

}

void thread_barrier_init(int *barrier_id, int *attribute_id, int *count, int *info) {
    pthread_barrierattr_t *attribute;
    *info = FT_OK;

    if (is_initialized == false) {
        *info = FT_EINIT;
        return;
    }

    pthread_mutex_lock(&(barriers->mutex));
    if (barriers->after == barriers->size) {
        array_resize(&barriers,barriers->size * 2);
    }

    barriers->data[barriers->after] = (pthread_barrier_t*) malloc(sizeof(pthread_barrier_t));
    if (*attribute_id == -1) {
        attribute = NULL;
    } else {
        attribute = barrier_attrs->data[*attribute_id];
    }

    *info = pthread_barrier_init((pthread_barrier_t*) (barriers->data[barriers->after]), attribute, *count);
    if (*info) {
        pthread_mutex_unlock(&(barriers->mutex));
        return;
    }

    *barrier_id = barriers->after;
    barriers->after++;

    pthread_mutex_unlock(&(barriers->mutex));
}

void thread_barrier_destroy(int *barrier_id, int *info) {
    *info = FT_OK;

    if (is_initialized == false) {
        *info = FT_EINIT;
        return;
    }

    pthread_mutex_lock(&(barriers->mutex));
    if (is_valid(barriers, *barrier_id) == false) {
        pthread_mutex_unlock(&(barriers->mutex));
        *info = FT_EINVALID;
        return;
    }

    *info = pthread_barrier_destroy(((pthread_barrier_t*) (barriers->data[*barrier_id])));
    if (*info) {
        pthread_mutex_unlock(&(barriers->mutex));
        return;
    }

    free(barriers->data[*barrier_id]);
    barriers->data[*barrier_id] = NULL;

    pthread_mutex_unlock(&(barriers->mutex));
}

void thread_spin_init(int *spinlock_id, int *pshared, int *info) {
    *info = FT_OK;

    if (is_initialized == false) {
        *info = FT_EINIT;
        return;
    }

    pthread_mutex_lock(&(spinlocks->mutex));
    if (spinlocks->after == spinlocks->size) {
        varray_resize(&spinlocks,spinlocks->size * 2);
    }

    spinlocks->data[spinlocks->after] = (pthread_spinlock_t*) malloc(sizeof(pthread_spinlock_t));
    *info = pthread_spin_init((pthread_spinlock_t*)(spinlocks->data[spinlocks->after]), *pshared);
    if (*info) {
        pthread_mutex_unlock(&(spinlocks->mutex));
        return;
    }

    *spinlock_id = spinlocks->after;
    spinlocks->after++;

    pthread_mutex_unlock(&(spinlocks->mutex));
}

void thread_spin_destroy(int *spinlock_id, int *info) {
    *info = FT_OK;

    if (is_initialized == false) {
        *info = FT_EINIT;
        return;
    }

    pthread_mutex_lock(&(spinlocks->mutex));
    if (is_valid(spinlocks,*spinlock_id) == false) {
        pthread_mutex_unlock(&(spinlocks->mutex));
        *info = FT_EINVALID;
        return;
    }

    *info = pthread_spin_destroy(((pthread_spinlock_t*) (spinlocks->data[*spinlock_id])));
    if (*info) {
        pthread_mutex_unlock(&(spinlocks->mutex));
        return;
    }

    free((void *)spinlocks->data[*spinlock_id]);
    spinlocks->data[*spinlock_id] = NULL;

    pthread_mutex_unlock(&(spinlocks->mutex));
}

void thread_rwlock_init(int *rwlock_id, int *attribute_id, int *info) {
    pthread_rwlockattr_t *attribute;
    *info = FT_OK;

    if (is_initialized == false) {
        *info = FT_EINIT;
        return;
    }

    pthread_mutex_lock(&(rwlocks->mutex));
    if (rwlocks->after == rwlocks->size) {
        array_resize(&rwlocks, rwlocks->size * 2);
    }

    rwlocks->data[rwlocks->after] = (pthread_rwlock_t*) malloc(sizeof(pthread_rwlock_t));
    if (*attribute_id == -1) {
        attribute = NULL;
    } else {
        attribute = rwlock_attributes->data[*attribute_id];
    }

    *info = pthread_rwlock_init((pthread_rwlock_t*) (rwlocks->data[rwlocks->after]) ,attribute);

    if (*info) {
        pthread_mutex_unlock(&(rwlocks->mutex));
        return;
    }

    *rwlock_id = rwlocks->after;
    rwlocks->after++;

    pthread_mutex_unlock(&(rwlocks->mutex));
}

void thread_rwlock_destroy(int *rwlock_id, int *info) {
    *info = FT_OK;

    if (is_initialized == false) {
        *info = FT_EINIT;
        return;
    }

    pthread_mutex_lock(&(rwlocks->mutex));
    if (is_valid(rwlocks, *rwlock_id) == false) {
        pthread_mutex_unlock(&(rwlocks->mutex));
        *info = FT_EINVALID;
        return;
    }

    *info = pthread_rwlock_destroy(((pthread_rwlock_t*) (rwlocks->data[*rwlock_id])));
    if (*info) {
        pthread_mutex_unlock(&(rwlocks->mutex));
        return;
    }

    free(rwlocks->data[*rwlock_id]);
    rwlocks->data[*rwlock_id] = NULL;

    pthread_mutex_unlock(&(rwlocks->mutex));
}
*/
