#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Aug 30 14:08:10 2018

@author: israeldiego
"""


# Running Median Heap Problem
class MedianHeap:
    def __init__(self):
        self.min_heap = MinHeap()
        self.max_heap = MaxHeap()
        
    def peek(self):
        if self.min_heap.size() == self.max_heap.size():
            return (self.min_heap.peek() + self.max_heap.peek()) // 2.0
        if self.min_heap.size() > self.max_heap.size():
            return self.min_heap.peek()
        return self.max_heap.peek()
    
    def push(self, value):
        if(self.min_heap.is_empty()):
            self.min_heap.push(value)
            return
        if value < self.peek():
            self.max_heap.push(value)
        else:
            self.min_heap.push(value)
        # verify heap size difference is <= 1    
        if self.min_heap.size() > self.max_heap.size() + 1:
            self.max_heap.push(self.min_heap.pop())
        elif self.max_heap.size() > self.min_heap.size() + 1:
            self.min_heap.push(self.max_heap.pop())
        
    def pop(self):
        pass

class BaseHeap:
    def __init__(self):
        self.values = [None]
        
    def peek(self):
        return self.values[1]
    
    def is_empty(self):
        return len(self.values) == 1
    
    def size(self):
        return len(self.values) - 1
    
    def push(self, value):
        self.values.append(value)
        # verify less than parent
        child_index = len(self.values) - 1
        while True: #if you don't know what the conditional should be
            parent_index = child_index // 2
            if parent_index == 0: # at top of heap
                return
            if self.heapify_complete(parent_index, value): # stop swapping
                return
            # swap, and update child_index
            self.values[parent_index],self.values[child_index] = self.values[child_index], self.values[parent_index]
            child_index = parent_index
            
    def __repr__(self):
        return 'Heap(' + str(self.values) + ')'
    
    def pop(self):
        answer = self.values[1]
        last_value = self.values.pop()
        if len(self.values) < 2:
            return answer
        self.values[1] = last_value # update top of heap
        if len(self.values) <= 2: # check if we need to do more work
            return answer
        parent_index = 1 # otherwise 
        while True:
            left_child_index = 2 * parent_index
            if left_child_index >= len(self.values):
                return answer
            right_child_index = left_child_index + 1
            if right_child_index >= len(self.values) or \
                self.use_left_index(left_child_index):
                    swap_index = left_child_index
            else:
                swap_index = right_child_index
            if self.heapify_complete(parent_index, self.values[swap_index]):
                return answer
            self.values[swap_index], self.values[parent_index] = \
                self.values[parent_index], self.values[swap_index]
            parent_index = swap_index  
        return answer
    
class MaxHeap(BaseHeap):
    def heapify_complete(self, parent_index, value):
        return self.values[parent_index] >= value
    def use_left_index(left_child_index):
        return self.values[left_child_index] >= self.values[left_child_index + 1]
    
    

class MinHeap(BaseHeap):
    def heapify_complete(self, parent_index, value):
        return self.values[parent_index] <= value
    
    def use_left_index(left_child_index):
        return self.values[left_child_index] <= self.values[left_child_index + 1]


data = [6, 3, 1, 4, 5, 9, 2, 7]
mh = MedianHeap()
for d in data:
    mh.push(d)
    print(mh.peek())

print(mh)

#while len(mh.values) > 1:
#    print(mh.pop())

# Problem, Given Data, periodically check what the median of data is