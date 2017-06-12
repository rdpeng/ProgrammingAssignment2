# -*- coding: utf-8 -*-
"""
Created on Mon May 08 10:02:46 2017
@author: yuy
"""


import os
import time
#import heapq
#path='C://Users//Yu Yu//Documents//learning//Algorithms Design and Analysis//V. week 4//'
#path='H://training_materials//algorithms//'
#os.chdir('H://training_materials//algorithms//')
path='/Users/yuy/Documents/learning/algorithms/week4/'

#import pandas as pd
import numpy as np
os.chdir(path)

#target = open(path+'log first leg.txt', 'w')    

start_time = time.time()
lst=[]
with open('sample1.txt','r') as f:
    for l in f.readlines():
        #print(l)
        line=l.split('\r')
        for k in line:
            lst.append(k.split('\t'))
        
#with open('SCC.txt') as f:
#    for l in f.readlines():
#        lst.append(l.split())
        
may = np.asarray(lst)
may = may.astype(np.int)

print("--- %s seconds ---" % (time.time() - start_time))     
print 'read in data'
#reverse the graph     
#df = pd.DataFrame(lst, columns=['start','end'])
may_rev=may[:,np.argsort([1,0])]

set1=set(may[:,0])
vertex_set=set1.union(set(may[:,1]))#this list is used to contain all vertex
#vsl=list(vertex_set)
#the vertex_set contains all vertexes in the graph
n=len(vertex_set)
#record columns are: 0: vertex, 1:'visited', 2:'leader',3:'finish_time',4:'index from',5:'index to'
record = np.column_stack((np.array(list(vertex_set)),np.zeros([n,5],dtype=int)))

#sort the reversed adjacency list 
may_rev=may_rev[may_rev[:,0].argsort()]

#this function figures out how many outward edges there are from a vertex
def set_record(array0,record):
    #set the visited and leader records to 0
    record[:,1]=0
    record[:,2]=0
    record[:,4]=-1
    record[:,5]=-1
    runner=0
    past_runner=0
    for i in range(array0.shape[0]):
        
        vertex=array0[i,0]

        while vertex>record[runner,0]:
            runner=runner+1
        if vertex==record[runner,0] and vertex!=array0[i-1,0]:
            record[runner,4]=i
            record[past_runner,5]=i-1
            past_runner=runner
        if i==array0.shape[0]-1:
            record[runner,5]=i
    return record

record1=set_record(may_rev,record)
print 'initialized records '

def dfs_loop(array0,record,order):
    global t,s
    t=0
    s=None
    for i in order:
        #if i % 100==0:
        print i
        if record[i-1,1]==0:
            DFS(array0,record,i)
    

def DFS(array0,record,i):
    global t,s,new
    S=[]
    pred=[]
    skp=[]
    S.append(i)
    s=i
    #s=i
    while len(S)>0:
        u=S.pop(0)
        pred.append(u)
        #p=pred.pop()
        print 'pop',u,'from S'
        if record[u-1,1]==0:
            print u,'has not been visited yet, set it as visited'
            record[u-1,1]=1
            
   # print 'depth first search starting with',i
            start=record[u-1,4]
            end=record[u-1,5]
       #     print u,' outgoing vertex starts from',start, 'and end at ',end
        #record.ix[i,'visited']=True
            if start==-1:
                t=t+1 
                #target.write('the edge has no outgoing vertex, set finish time for '+str(u)+' as '+str(t)+'\n')

                print 'the edge has no outgoing vertex, set finish time for ',u,'as',t
                record[u-1,3]=t 
                record[u-1,2]=u
                print 'set leader for ',u,'as',u
            else:
                #record.ix[u,'leader']=s
                print 'search through all its outgoing vertex'
                for j in range(start,end+1):
                  #  print 'j is',j-1
                    nn=array0[j,1]
                    print 'neighbor is ',nn
                    if record[nn-1,1]==0 and nn not in S:
                        print 'put neighbour',nn,'into stack and its predecesor',u,'into another stack'
                        S.append(nn)
                        #pred.append(u)
                        print 'S is',S,'pred is ',pred
                    else:
                        print 'neighbor',nn,'has been visited'
                        skp.append(nn)
                while len(skp)>0:
                        nn=skp.pop(0)   
                        brk=0
                        #s=u
                        while len(pred)>0 and brk==0:
                            new=pred.pop(0)  
                            ss=record[new-1,4]
                            ee=record[new-1,5]
                            if ss!=-1 and ee!=-1:
                                for check in range(ss,ee+1):
                                    print 'check whether ',new,'has remaining edges to visit'
                                    cn=array0[check,1]
                                    
                                    if cn in S:
                                        print new,'has remaining edge in S to visit, put it back in the qeue'
                                        pred.append(new)
                                        brk=1
                                        
                                        break
                                    #else:
                                     #   print 'no, ',new,'is done, we set finish time as',t,' and leader as',s
                                        #record[new-1,3]=t
                                        #record[new-1,2]=s
                                        #t=t+1
                if record[new-1,3]==0:
                    t=t+1
                                #target.write('set finish time for '+str(new)+' as '+str(t)+'\n')
                    print 'set finish time for ',new,'as',t
                    record[new-1,3]=t 
                    record[new-1,2]=s
                    print 'set leader for ',new,'as',s
    #return record
 
       
       
dfs_loop(may_rev,record1,sorted(list(vertex_set),reverse=True))
#target.close()
print 'finish the first loop'

df_original=df.rename(columns={'start':'end','end':'start'})
df_original.sort_values(by='end',axis=0,inplace=True)
df_original.reset_index(inplace=True,drop=True)

record=record.drop(['to','from'],axis=1)
record=set_record(df_original,record)

target = open(path+'log second leg.txt', 'w')   
finish_order=record['finish_time']
new_list=finish_order.sort_values(ascending=False).index.tolist()
dfs_loop(df_original,record,new_list)
target.close()
if record[record.leader.isnull()].shape[0]>0:
    record.ix[record.leader.isnull(),'leader']=record[record.leader.isnull()].index.values[0]
final_list=record['leader'].value_counts()
print final_list[0:4]