# -*- coding: utf-8 -*-
"""
Created on Mon Jun 12 17:02:47 2017

@author: yuy
"""
import os
import time
#import heapq
#path='C://Users//Yu Yu//Documents//learning//Algorithms Design and Analysis//V. week 4//'
#path='H://training_materials//algorithms//'
os.chdir('H://training_materials//algorithms//')
#path='/Users/yuy/Documents/learning/algorithms/week4/'

#import pandas as pd
import numpy as np
from scipy.stats import itemfreq
from collections import deque,defaultdict
import operator
#os.chdir(path)

#target = open(path+'log first leg.txt', 'w')    



#this function figures out how many outward edges there are from a vertex
def set_record(array0,record):
    #set the visited and leader records to 0
   # record[:,1]=0
    #record[:,2]=0
    #record[:,3]=0
    record[:,1]=-1
    record[:,2]=-1
    runner=0
    past_runner=0
    for i in range(array0.shape[0]):
        
        vertex=array0[i,0]

        while vertex>record[runner,0]:
            runner=runner+1
        if vertex==record[runner,0] and vertex!=array0[i-1,0]:
            record[runner,1]=i
            record[past_runner,2]=i-1
            past_runner=runner
        if i==array0.shape[0]-1:
            record[runner,2]=i
    return record



def dfs_loop(array0,record,order):
    global t,s,finish_times_seen,finish_times,leaders,visited
    t=0
    s=None
    finish_times_seen=set()
    visited=set()
    finish_times=defaultdict(list)
    leaders=defaultdict(list)
    for item in order:
        leaders.setdefault(str(item),[]).append(0)
  #  print leaders.items()
    for i in order:
        #if i % 100==0:
      #  print i
        if i not in visited:
            DFS(array0,record,i)
    

def DFS(array0,record,i):
    global t,s,finish_times_seen,finish_times,leaders,visited
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
      #  print 'pop',u,' from S and add ',u,'to the traveled path, S is',S,'pred is',pred
        if u not in visited:#record[u-1,1]==0:
       #     print u,'has not been visited yet, set it as visited'
            #record[u-1,1]=1
            visited.add(u)
            #find all the edges going out of the current vertex
            start=record[u-1,1]
            end=record[u-1,2]
       #     print u,' outgoing vertex starts from',start, 'and end at ',end
            if start==-1:
                t=t+1 
                #target.write('the edge has no outgoing vertex, set finish time for '+str(u)+' as '+str(t)+'\n')
             #   print 'the edge has no outgoing vertex, set finish time for ',u,'as',t,'leader for ',u,'as',u
                finish_times_seen.add(u)
                pred.remove(u)
               # print 'now the pred list is',pred
                finish_times[u].append(t)
                leaders[str(u)][0]+=1
                #record[u-1,3]=t 
                #record[u-1,2]=u
                continue
            else:
                #record.ix[u,'leader']=s
               # print 'search through all its outgoing vertex'
                for j in range(start,end+1):
                  #  print 'j is',j-1
                    nn=array0[j,1]
                  #  print 'neighbor is ',nn
                    if nn not in visited and nn not in S_set:#if nn hasn't been explored, go to it next
                     #   print 'put neighbour',nn,'into stack'
                        S.append(nn)
                        S_set.add(nn)
                     #   print 'S is',S,'pred is ',pred
                    else:
                      #  print 'neighbor',nn,'has been visited'
                        brk=0
                        while len(pred)>0 and brk!=1:
                            last=pred.pop()
                           # print 'retrieve to last step ',last,' and check its neighbours'
                            last_start=record[last-1,1]
                            last_end=record[last-1,2]
                            for k in range(last_start,last_end+1):
                                nk=array0[k,1]
                                if nk not in visited:#record[nk-1,1]==0 :
                                   # print 'neighbor',nk,'has not been visited'
                                    if nk not in S_set:
                                        S.append(nk)
                                        S_set.add(nk)
                                    pred.append(last)
                                    brk=1
                                    break
                            if brk==1:
                               # print 'some vertex has not been visited, continue'
                                break
                            else:
                                #if len(pred)>0:
                                #    new=pred.pop()
                                if last not in finish_times_seen:
                                    t=t+1
                                 #   print 'all neighbors have been visited, set end time to',last,'as',t
                                    finish_times_seen.add(last)
                                    finish_times[last].append(t)
                                    leaders[str(s)][0]+=1
                                    #record[last-1,3]=t
                                    #record[last-1,2]=s
    #after all queue items are visited, we started going back 
    while len(pred)>0:
        
        new=pred.pop()
        if new not in finish_times_seen:
            t=t+1
       #     print 'set end time to',new,'as ',t,'leader as',s
            finish_times_seen.add(new)
            finish_times[new].append(t)
            leaders[str(new)][0]+=1
            #record[new-1,3]=t
            #record[new-1,2]=s

def main():
    start_time = time.time()
    lst=[]
#    with open('sample5.txt','r') as f:
#        for l in f.readlines():
#      #      print l
#            line=l.split()
#       #     print line
#            if line !=[]:
#                lst.append(line)
            
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
    #vsl=list(vertex_set)
    #the vertex_set contains all vertexes in the graph
    n=len(vertex_set)
    #record columns are: 0: vertex, 1:'visited', 2:'index from',3:'index to'
    record = np.column_stack((np.array(list(vertex_set)),np.zeros([n,2],dtype=int)))
    
    #sort the reversed adjacency list 
    may_rev=may_rev[may_rev[:,0].argsort()]        
    record1=set_record(may_rev,record)
    print("--- %.2f seconds ---" % (time.time() - mid_time1))  
    print 'initialized records '
    mid_time2=time.time()
#    leaders=[]
    dfs_loop(may_rev,record1,sorted(list(vertex_set),reverse=True))
    #target.close()
    print("--- %.2f seconds ---" % (time.time() - mid_time2))
    print 'finish the first loop'
    mid_time3=time.time()
    sorted_x = sorted(finish_times.items(), key=operator.itemgetter(1),reverse=True)
    finish_order=[]    
    for i in sorted_x:
        finish_order.append(i[0])
    #finish_order=sorted_x.keys()#record1[record1[:,3].argsort()[::-1],0]
 #   print 'finish order is'
  #  for i in finish_order:
  #      print i
#    leaders=[]
    record2=set_record(may,record)
#    finish_order.reverse()
    dfs_loop(may,record2,finish_order)

    sorted_y = sorted(leaders.items(), key=operator.itemgetter(1),reverse=True)  
    count=0
    first_five=[]
    for i in sorted_y:
        first_five.append(i[1][0])    
        count=count+1
        if count>4:
            break
        
#    fq=itemfreq(record2[:,2])
#    n=5
#    idx=fq[:,1].argsort()[::-1][:n]
#    final_list=fq[idx][:,1]
    print first_five
    print ("--- %.2f seconds ---" % (time.time() - mid_time3))
    return sorted_y
if __name__=='__main__':
    final_list=main()