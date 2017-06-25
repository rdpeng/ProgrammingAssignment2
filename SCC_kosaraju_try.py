# -*- coding: utf-8 -*-
"""
Created on Mon May 08 10:02:46 2017

@author: yuy
"""


import os
#import heapq
path='C://Users//Yu Yu//Documents//learning//Algorithms Design and Analysis//V. week 4//'
#path='H://training_materials//algorithms//'
#os.chdir('H://training_materials//algorithms//')
import pandas as pd
import numpy as np
os.chdir(path)

target = open(path+'log first leg.txt', 'w')    

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
        
print 'read in data'
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
    for k in vertex_list:
        if not pd.isnull(record.ix[k,'to']): 
            record.ix[k,'from']=0
            break
   # record['from']=np.where(np.isnan(record['from']), None, record['from'])
   # record['to']=np.where(np.isnan(record['to']), None, record['to'])
    record['from']=record['from'].fillna(-1.0).astype(int)
    record['to']=record['to'].fillna(-1.0).astype(int)
    return record
#
record=set_record(df,record)
#note: to make the database work, I fill missing values in from and to with -1. 
print 'initialized records '

def dfs_loop(df,record,order):
    global t,s
    t=0
    s=None
    for i in order:
        if i % 100==0:
            print i
        if record.ix[i,'visited']==False:
            DFS(df,record,i)
#    if record['finish_time'].isnull().values.any():
#        for i in record[record['finish_time'].isnull()].index.values:
#            t=t+1
#            record.ix[i,'finish_time']=t
#            record.ix[i,'leader']=i

def DFS(df,record,i):
    global t,s,new
    S=[]
    pred=[]
    skp=[]
    S.append(i)
    s=i
    #s=i
    while len(S)>0:
        u=S.pop()
        pred.append(u)
        #p=pred.pop()
    #    print 'pop',u,'from S'
        if record.ix[u,'visited']==False:
            record.ix[u,'visited']=True
            
   # print 'depth first search starting with',i
            start=record.ix[u,'from']
            end=record.ix[u,'to']
        #record.ix[i,'visited']=True
            if start==-1:
                t=t+1 
                target.write('the edge has no outgoing vertex, set finish time for '+str(u)+' as '+str(t)+'\n')

              #  print 'the edge has no outgoing vertex, set finish time for ',u,'as',t
                record.ix[u,'finish_time']=t 
                record.ix[u,'leader']=u
      #          print 'set leader for ',u,'as',u
            else:
                #record.ix[u,'leader']=s
                for j in range(start,end+1):
                  #  print 'j is',j
                    nn=df.ix[j,'start']
                    if record.ix[nn,'visited']==False and nn not in S:
       #                 print 'put neighbour',nn,'into stack and its predecesor',u,'into another stack'
                        S.append(nn)
                        #pred.append(u)
        #                print 'S is',S,'pred is ',pred
                    else:
                  #      print 'neighbor',nn,'has been visited'
                        skp.append(nn)
                while len(skp)>0:
                        nn=skp.pop()   
                        brk=0
                        #s=u
                        while len(pred)>0 and brk==0:
                            new=pred.pop()  
                            ss=record.ix[new,'from']
                            ee=record.ix[new,'to']
                            if ss !=-1 and ee !=-1:
                                for check in range(ss,ee+1):
                   #                 print 'check whether ',new,'has remaining edges to visit'
                                    cn=df.ix[check,'start']
                                    
                                    if cn in S:
                       #                 print new,'has remaining edge in S to visit, put it back in the qeue'
                                        pred.append(new)
                                        brk=1
                                        
                                        break
                                    #else:
                                    #    print 'no, ',new,'is done, we should claim it finished and assign finishing time'
                            if pd.isnull(record.ix[new,'leader']):
                                t=t+1
                                target.write('set finish time for '+str(new)+' as '+str(t)+'\n')

                          #      print 'set finish time for ',new,'as',t
                                record.ix[new,'finish_time']=t 
                                record.ix[new,'leader']=s
                        #        print 'set leader for ',new,'as',s
      #  else:  
         #   print u,'is visited'              
            #t=t+1
            #print 'set finish time for ',p,'as',t
            #record.ix[p,'finish_time']=t     
       
       
dfs_loop(df,record,sorted(vertex_list,reverse=True))
target.close()
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