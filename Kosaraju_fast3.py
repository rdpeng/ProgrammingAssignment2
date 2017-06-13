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
from scipy.stats import itemfreq

os.chdir(path)

#target = open(path+'log first leg.txt', 'w')    



#this function figures out how many outward edges there are from a vertex
def set_record(array0,record):
    #set the visited and leader records to 0
    record[:,1]=0
    record[:,2]=0
    record[:,3]=0
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
#    skp=[]
    S.append(i)
    s=i
    #s=i
    while len(S)>0:
        u=S.pop(0)
        pred.append(u)
        print 'pop',u,' from S and add ',u,'to the traveled path'
        if record[u-1,1]==0:
            print u,'has not been visited yet, set it as visited'
            record[u-1,1]=1
            #find all the edges going out of the current vertex
            start=record[u-1,4]
            end=record[u-1,5]
       #     print u,' outgoing vertex starts from',start, 'and end at ',end
            if start==-1:
                t=t+1 
                #target.write('the edge has no outgoing vertex, set finish time for '+str(u)+' as '+str(t)+'\n')
                print 'the edge has no outgoing vertex, set finish time for ',u,'as',t,'leader for ',u,'as',u
                record[u-1,3]=t 
                record[u-1,2]=u
            else:
                #record.ix[u,'leader']=s
                print 'search through all its outgoing vertex'
                for j in range(start,end+1):
                  #  print 'j is',j-1
                    nn=array0[j,1]
                    print 'neighbor is ',nn
                    if record[nn-1,1]==0 and nn not in S:#if nn hasn't been explored, go to it next
                        print 'put neighbour',nn,'into stack'
                        S.append(nn)
                        print 'S is',S,'pred is ',pred
                    else:
                        print 'neighbor',nn,'has been visited'
    #after all queue items are visited, we started going back 
    while len(pred)>0:
        t=t+1
        new=pred.pop()
        print 'set end time to',new,'as ',t,'leader as',s
        record[new-1,3]=t
        record[new-1,2]=s

def main():
    start_time = time.time()
    lst=[]
    with open('sample5.txt','r') as f:
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
    record1=set_record(may_rev,record)
    print 'initialized records '
    
    dfs_loop(may_rev,record1,sorted(list(vertex_set),reverse=True))
    #target.close()
    print 'finish the first loop'
    finish_order=record1[record1[:,3].argsort()[::-1],0]
    record2=set_record(may,record)
    dfs_loop(may,record2,finish_order)
    
    fq=itemfreq(record2[:,2])
    n=5
    idx=fq[:,1].argsort()[::-1][:n]
    final_list=fq[idx][:,1]
    print final_list[0:4]
    
if __name__=='__main__':
    main()