# -*- coding: utf-8 -*-
"""
Created on Mon May 08 10:02:46 2017

@author: yuy
"""


import os
#import heapq
#path='C://Users//Yu Yu//Documents//learning//Algorithms Design and Analysis//V. week 4//'
path='H://training_materials//algorithms//'
#os.chdir('H://training_materials//algorithms//')
import pandas as pd
import numpy as np
os.chdir(path)
import sys
sys.setrecursionlimit(10000)

lst=[]
with open('SCC.txt') as f:
    for l in f.readlines():
        if l=='\n':
            continue
        line=l.split('\t')
#        print(line),len(line)                
        if len(line)==1:
            line=l.split()
#            print(line)
        line2=[]
        for i in line:
 #           print(i)
            if i=='':
                pass
            elif i[-1]=='\n' and len(i)>2:
                i=i.strip('\n')
 #               print(i)
                line2.append(int(i))
            elif i=='\n':
                pass
            else:
                line2.append(int(i))
#            print(line2)
        lst.append(line2)
#reverse the graph     
df = pd.DataFrame(lst, columns=['start','end'])
set1=set(df['end'])
#df.set_index('start',inplace=True)

vertex_list=set1.union(set((df['start'])))#this list is used to contain all vertex

#if len(vertex_list)>len(lst):
   # print 'some vertex are missing from directed graph'

record = pd.DataFrame({'visited':False,'leader':None,'finish_time':None},index=list(vertex_list))


#sort the beginning and end of a vertex link with other vertex
df.sort_values(by='end',axis=0,inplace=True)
df.reset_index(inplace=True,drop=True)

def set_record(df,record):
    record['visited']=False
    record['leader']=None
    rr=df['end'].value_counts(sort=False).astype(int)
    rr.sort_index(axis=0,inplace=True)
    new=rr.cumsum()
    col=pd.DataFrame(new,index=new.index)
    col.rename(columns={'end':'to'},inplace=True)
    record=record.merge(col,how='left',left_index=True,right_index=True)
    col2=col.shift(periods=1)
    col2.rename(columns={'to':'from'},inplace=True)
    record=record.merge(col2,how='left',left_index=True,right_index=True)
    record['to']=record['to']-1
    record.ix[1,'from']=0
   # record['from']=np.where(np.isnan(record['from']), None, record['from'])
   # record['to']=np.where(np.isnan(record['to']), None, record['to'])
    record['from']=record['from'].fillna(-1.0).astype(int)
    record['to']=record['to'].fillna(-1.0).astype(int)
    return record
#
record=set_record(df,record)
#note: to make the database work, I fill missing values in from and to with -1. 

def dfs_loop(df,record,order):
    global t,s
    t=0
    s=None
    for i in order:
        if i % 100==0:
            print i
        if record.ix[i,'visited']==False:
       #     print i,'has not been visited yet'
            s=i
            DFS(df,record,i)

def DFS(df,record,i):
    global t,s
   # print 'depth first search starting with',i
    start=record.ix[i,'from']
    end=record.ix[i,'to']
    record.ix[i,'visited']=True
    if start!=-1:
        record.ix[i,'leader']=s
        for j in range(start,end+1):
          #  print 'j is',j
            nn=df.ix[j,'start']
            if record.ix[nn,'visited']==False:
              #  print 'explore neighbour',nn
                DFS(df,record,nn)
    t=t+1
    #print 'set finish time for ',i,'as',t
    record.ix[i,'finish_time']=t     
       
dfs_loop(df,record,sorted(vertex_list,reverse=True))

df_original=df.rename(columns={'start':'end','end':'start'})
df_original.sort_values(by='end',axis=0,inplace=True)
df_original.reset_index(inplace=True,drop=True)

record=set_record(lst,df_original,record)
finish_order=record['finish_time']
new_list=finish_order.sort_values(ascending=False).index.tolist()
dfs_loop(df_original,record,new_list)

final_list=record['leader'].value_counts()