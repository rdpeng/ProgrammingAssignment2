#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Fri Jun 16 20:40:26 2017

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
#from scipy.stats import itemfreq
from collections import deque,defaultdict
import operator
os.chdir(path)

#target = open(path+'log first leg.txt', 'w')    



#this function figures out how many outward edges there are from a vertex
def set_record(array0):
   graph=defaultdict(list)
   for i in range(array0.shape[0]):
        graph[array0[i,0]].append(array0[i,1])
   return graph



def dfs_loop(graph,order):
    global t,s,finish_times_seen,finish_times,leaders,visited,outgoing_set
    t=0
    s=None
    finish_times_seen=set()
    visited=set()
    finish_times=defaultdict(list)
    leaders=defaultdict(list)
    outgoing_set=set(graph.keys())
    for item in order:
        leaders.setdefault(str(item),[]).append(0)
  #  print leaders.items()
    for i in order:
        if i not in visited:
            DFS(graph,i)
    

def DFS(graph,i):
    global t,s,finish_times_seen,finish_times,leaders,visited,outgoing_set
    S_set=set()    
    S=deque()
    pred=deque()
#    skp=[]
    S.append(i)
    S_set.add(i)
    s=i
    #s=i
    while len(S)>0:
        u=S.pop()
        S_set.remove(u)
        pred.append(u)
     #   print 'pop',u,' from S and add ',u,'to the traveled path, S is',S,'pred is',pred
        if u not in visited:#record[u-1,1]==0:
       #     print u,'has not been visited yet, set it as visited'
            #record[u-1,1]=1
            visited.add(u)
            #find all the edges going out of the current vertex
       #     print u,' outgoing vertex starts from',start, 'and end at ',end
            if u not in outgoing_set:
                t=t+1 
                #target.write('the edge has no outgoing vertex, set finish time for '+str(u)+' as '+str(t)+'\n')
              #  print 'the edge has no outgoing vertex, set finish time for ',u,'as',t,'leader for ',u,'as',u
                finish_times_seen.add(u)
                pred.remove(u)
               # print 'now the pred list is',pred
                finish_times[u].append(t)
                leaders[str(u)][0]+=1
                continue
            else:
               # print 'search through all its outgoing vertex'
               list0=graph[u]
               for j in list0:
                  #  print 'j is',j-1
                  #  print 'neighbor is ',nn
                    if j not in visited and j not in S_set:#if nn hasn't been explored, go to it next
                     #   print 'put neighbour',nn,'into stack'
                        S.append(j)
                        S_set.add(j)
                     #   print 'S is',S,'pred is ',pred
                    else:
                      #  print 'neighbor',nn,'has been visited'
                        brk=0
                        while len(pred)>0 and brk!=1:
                            last=pred.pop()
                           # print 'retrieve to last step ',last,' and check its neighbours'
                            for k in graph[last]:
                                if k not in visited:
                                   # print 'neighbor',nk,'has not been visited'
                                    if k not in S_set:
                                        S.append(k)
                                        S_set.add(k)
                                    pred.append(last)
                                    brk=1
                                    break
                            if brk==1:
                               # print 'some vertex has not been visited, continue'
                                break
                            else:
                                if last not in finish_times_seen:
                                    t=t+1
                                 #   print 'all neighbors have been visited, set end time to',last,'as',t,'leader as',s
                                    finish_times_seen.add(last)
                                    finish_times[last].append(t)
                                    leaders[str(s)][0]+=1
   #after all queue items are visited, we started going back 
    while len(pred)>0:
        
        new=pred.pop()
        if new not in finish_times_seen:
            t=t+1
    #        print 'set end time to',new,'as ',t,'leader as',s
            finish_times_seen.add(new)
            finish_times[new].append(t)
            leaders[str(new)][0]+=1

def main():
    start_time = time.time()
    lst=[]
#    with open('sample5.txt','r') as f:
#        for l in f.readlines():
#            #print(l)
#            line=l.split('\r')
#            for k in line:
#                lst.append(k.split('\t'))
            
    with open('SCC.txt') as f:
        for l in f.readlines():
            lst.append(l.split())
            
    may = np.asarray(lst)
    may = may.astype(np.int)
    
    print("--- %.2f seconds ---" % (time.time() - start_time))     
    print 'read in data'
    mid_time1 = time.time()
    #reverse the graph     
    #df = pd.DataFrame(lst, columns=['start','end'])
    may_rev=may[:,np.argsort([1,0])]
    
    set1=set(may[:,0])
    vertex_set=set1.union(set(may[:,1]))#this list is used to contain all vertex
   
    #sort the reversed adjacency list 
    may_rev=may_rev[may_rev[:,0].argsort()]        
    graph1=set_record(may_rev)
    print("--- %.2f seconds ---" % (time.time() - mid_time1))  
    print 'initialized records '
    mid_time2=time.time()
    dfs_loop(graph1,sorted(list(vertex_set),reverse=True))

    print("--- %.2f seconds ---" % (time.time() - mid_time2))
    print 'finish the first loop'
 #   print 'leaders are',leaders
    mid_time3=time.time()
    sorted_x = sorted(finish_times.items(), key=operator.itemgetter(1),reverse=True)
    finish_order=[]    
    for i in sorted_x:
        finish_order.append(i[0])
#    print 'finish order is'
#    for i in finish_order:
#        print i
    graph2=set_record(may)
    dfs_loop(graph2,finish_order)

    sorted_y = sorted(leaders.items(), key=operator.itemgetter(1),reverse=True)  
    count=0
    first_five=[]
    for i in sorted_y:
        first_five.append(i[1][0])    
        count=count+1
        if count>4:
            break
        
    print first_five
    print ("--- %.2f seconds ---" % (time.time() - mid_time3))
    return sorted_y
if __name__=='__main__':
    final_list=main()