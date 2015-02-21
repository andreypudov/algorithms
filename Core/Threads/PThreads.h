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

void thread_init(int *info);
void thread_alloc(int *thread_id, int *info);
void thread_create(int *thread_id, int *attribute_id, void *(**routine)(void *),
                   void *argument, int* info);
void thread_join(int *thread_id, void **value_pointer, int *info);
void thread_cancel(int *thread_id, int *info);
void thread_destroy(int* info);

void thread_mutex_init(int *mutex_id, int *attribute_id, int *info);
void thread_mutex_lock(int *mutex_id, int *info);
void thread_mutex_unlock(int *mutex_id, int *info);
void thread_mutex_destroy(int *mutex_id, int *info);

/*
void thread_condition_init(int *condition_id, int *attribute_id, int *info);
void thread_condition_destroy(int *condition_id, int *info);

void thread_barrier_init(int *barrier_id, int *attribute_id, int *count, int *info);
void thread_barrier_destroy(int *barrier_id, int *info);

void thread_spin_init(int *spinlock_id, int *pshared, int *info);
void thread_spin_destroy(int *spinlock_id, int *info);

void thread_rwlock_init(int *rwlock_id, int *attribute_id, int *info);
void thread_rwlock_destroy(int *rwlock_id, int *info);
*/
