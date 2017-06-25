#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Sun Jun 25 11:53:22 2017

@author: yuy
"""
import os
path='/Users/yuy/Documents/learning/algorithms/week6/'
os.chdir(path)
#class Solution(object):
def twoSum(nums, target):
    #hash用于建立数值到下标的映射
    hash = {}
    #循环nums数值，并添加映射
    for i in range(len(nums)):
        if target - nums[i] in hash:
            return [hash[target - nums[i]] + 1, i + 1]
        hash[nums[i]] = i
    #无解的情况
    return [-1, -1]

lst=[]
with open('prob-2sum.txt') as f:
    for l in f.readlines():
        #print l
        lst.append(int(l.strip('\n')))
        #lst.append(l.split())

count=0
for t in range(-10000,10001):
    result=twoSum(lst,t)
    if t % 100 ==0:
        print t
    if result!=[-1,-1]:
         #print t,result
         count=count+1

print count