from rpy2.robjects.packages import importr
from rpy2.robjects import r
import rpy2.robjects as robjects
import pandas as pd
import math

import warnings
warnings.filterwarnings("ignore") #全篇忽略warning

#pacman = importr('pacman')
##=== 准备运行策略所需的数据 =========
pr=pd.read_csv("ModifiedData.csv")  #pr为dataframe


#=======牛市熊市择时策略引入===========
# 1.将股票市场简单划分为牛市熊市，依据为若价格高于过去n天（n取20）移动平均值，就是牛市（type==1)，否则看做是熊市(type==0)
# 2.择时策略：处于牛市时，当下一个交易日是周M1则买入，下一个交易日是周M2则卖出；处于熊市时，当下个交易日是周N1则买入，下个交易日是周N2则卖出
# 3.超参数组合 n,M1,M2,N1,N2

#TTR=importr('TTR')
#Weekday=["星期一","星期二","星期三","星期四","星期五"]

#================择时策略函数 buysigTiming()===================
import numpy as np

pr_list=pr.values.tolist()  #读CSV文件转为list
pr_array=np.array(pr_list)

from datetime import datetime

#将数据中的日期标注星期
temp={}
week=[]
for i in range(len(pr['date'])):
    temp[i]=datetime.strptime(pr['date'][i],'%Y/%m/%d')
    week.append(temp[i].strftime("%w"))

#type(week): list[str] change into list[int](week_new)
week_new=list(map(int,week))

#====================数据处理结束，以下为择时策略函数（可替换部分）===================

#def buysigTiming(hyperPars=[]):
#    stg=pd.DataFrame()
#    #columns=["Type","buySig","sellSig"]
#    Type=[]
#    buySig=[]
#    sellSig=[]
#    MA=[]
#    for i in range(len(pr)):
#        MA=pr['op'].rolling(window=hyperPars[4]).mean()  
#        #print(MA)                     #0-18个数据没有
#        if pr['cl'][i]>MA[i]:
#            Type.append(1)
#        else:  
#            Type.append(0)        

#        #若下一交易日为M1、N1（且符合选定的市场行情）则买入；
#        if ((Type[i]==1 and week_new[i]==hyperPars[0])or(Type[i]==0 and week_new[i]==hyperPars[2])):
#            buySig.append(1)
#        else:
#            buySig.append(0)

#        #为M2、N2则卖出,分别打上买入和卖出标记
#        if ((Type[i]==1 and week_new[i]==hyperPars[1])or(Type[i]==0 and week_new[i]==hyperPars[3])):
#            sellSig.append(1)
#        else:
#            sellSig.append(0)

#    stg['date']=pr['date']
#    stg['Type']=Type
#    stg['week']=week_new
#    stg['op']=pr['op']
#    stg['cl']=pr['cl']
#    stg['buySig']=buySig
#    stg['sellSig']=sellSig   

#    return stg

##策略函数测试
##该策略即：处于牛市时，下一交易日为周一则买入，为周四则卖出;
##处于熊市时，下一交易日为周二则买入，为周三则卖出
#hyperPars=[1,4,2,3,20]
##buysigTiming(hyperPars)
#stgy=buysigTiming(hyperPars)  

#=======================基于布林带的择时策略======================
#1.当收盘价从下轨线(lower)突破时（前一天收盘价应当位于lower下方），买入股票
#2.当收盘价从上轨线（upper)跌破时（前一天收盘价应当位于upper上方），卖出股票
#超参数：period,nbdevup,nbdevdn
#=====================布林带(Bollinger Band)====================
import talib
def getBBands(df,period=10,nbdevup=2,nbdevdn=2):
    close=df['cl']
    upper,middle,lower=talib.BBANDS(
        close.values,
        timeperiod=period,
        nbdevup=nbdevup,
        nbdevdn=nbdevdn,
        matype=0
        )
    data = dict(upper=upper, middle=middle, lower=lower)
    df = pd.DataFrame(data, index=df.index, columns=['upper', 'middle', 'lower']).dropna()

    return df

stgy=getBBands(pr)
stg= pr.join(stgy, how='outer')

def buysigTiming(hyperPars=[]):
    sig=pd.DataFrame()
    buySig=[]
    sellSig=[]
    #i=hyperPars[0]      #timeperiod前hyperPars[0]数据为空，舍去  
    for i in range(9,len(stg)):
        #当收盘价从下轨线(lower)突破时（前一天收盘价应当位于lower下方），买入股票
        if((stg['cl'][i-1]<stg['lower'][i])and(stg['cl'][i]>stg['lower'][i])):
            buySig.append(1)
        else:
            buySig.append(0)
        #当收盘价从上轨线（upper)跌破时（前一天收盘价应当位于upper上方），卖出股票
        if((stg['cl'][i-1]>stg['upper'][i])and(stg['cl'][i]<stg['upper'][i])):
            sellSig.append(1)
        else:
            sellSig.append(0)
        
    sig['buySig']=buySig
    sig['sellSig']=sellSig 

#==============================择时策略函数结束（可替换部分结束）====================
###############写入文件##################
#np.savetxt('stgy.txt',stgy,delimiter='   ',fmt='%s')


#===============查准率评估策略函数 evaluateppv()=====================

def evaluateppv(stgy):
    stgy_buy=stgy.loc[stgy['buySig']==1]          #提取dataframe中的元素
    stgy_buy=stgy_buy.reset_index(drop=True)      #将标签重新排序
    stgy_sell=stgy.loc[stgy['sellSig']==1]
    stgy_sell=stgy_sell.reset_index(drop=True)
    u=pd.DataFrame()
    u=pd.concat([stgy_buy['op'],stgy_sell['cl']],axis=1)   #横向合并op,cl
    #print(u)
    #根据卖出当天收盘价与买入当天开盘价取对数后作差计算盈利
    profit=pd.DataFrame()
    profit['profit']=u['cl'].map(lambda x:math.log(x))-u['op'].map(lambda x:math.log(x))
    profit=profit.dropna(axis=0,how='any')
    ppv=profit[profit['profit']>0].count() / len(profit)
    #print("ppv=",ppv)
    return ppv

#ppv=evaluateppv(stgy)
#print(ppv)
#================夏普比率评估函数evaluateSR()========================

def evaluateSR(stgy=[]):
    stgy_buy=stgy.loc[stgy['buySig']==1]          #提取dataframe中的元素
    stgy_buy=stgy_buy.reset_index(drop=True)      #将标签重新排序
    stgy_sell=stgy.loc[stgy['sellSig']==1]
    stgy_sell=stgy_sell.reset_index(drop=True)
    u=pd.DataFrame()
    u=pd.concat([stgy_buy['op'],stgy_sell['cl']],axis=1)   #横向合并op,cl
    
    #根据卖出当天收盘价与买入当天开盘价计算盈利率
    Rate=pd.DataFrame()
    Rate['Rate']=u.apply(lambda x:(x['cl']-x['op'])/x['op'],axis=1)
    Rate=Rate.dropna(axis=0,how='any')

    #计算夏普比率
    ERp=Rate['Rate'].sum()  #计算投资预期收益率
    P=np.std(Rate,ddof=1)   #计算收益率标准差
    Rf=0.03                 #无风险利率选择国债利率（此处取3%）
    SR=(ERp-Rf)/P

    return SR

# 以 M1,M2,N1,N2,n 为策略超参数生成参数搜索空间
M1=list(range(1,3,1))
N1=list(range(1,3,1))
M2=list(range(1,3,1))
N2=list(range(1,3,1))
n=list(range(15,17,1))

from itertools import product
parSpace=[]
for item in product(M1,N1,M2,N2,n):
    parSpace.append(item)

parSpace=np.array(parSpace)
parSpace_new=[]
for i in range(len(parSpace)):
    if ((parSpace[i,0]!=parSpace[i,2]) and (parSpace[i,1]!=parSpace[i,3])):
        parSpace_new.append(parSpace[i])
parSpace_new=np.array(parSpace_new)



#=============进行网格搜索的Search()函数，输出标定指标最优策略==========
def Search(parSpace=[]):
    #每个参数组合都检验，能暴力选出ppv最好的参数组合
    ppv=[]
    for i in range(len(parSpace)):  
        print("M1=",parSpace[i,0],"N1=",parSpace[i,1],"M2=",parSpace[i,2],"N2=",parSpace[i,3],"n=",parSpace[i,4])
        ppv.append(evaluateSR(buysigTiming(parSpace[i])))
    ppv=np.array(ppv)
    #查看参数组合(Search)以及其所对应的ppv值
    Search=pd.DataFrame(columns=["M1","N1","M2","N2","n","ppv"])
    Search['M1']=parSpace[:,0]
    Search['N1']=parSpace[:,1]
    Search['M2']=parSpace[:,2]
    Search['N2']=parSpace[:,3]
    Search['n']=parSpace[:,4]
    Search['ppv']=ppv
     #查看最优参数组合(Best)
    Best=[]
    for i in range(len(Search)):
        ppv_max=Search['ppv'].max()
        if Search['ppv'][i]==ppv_max:
            Best.append(Search[i:i+1])
    return Best

#Best=Search(parSpace_new)
#print(Best)

 #================= 使用 CSCV 计算 PBO ==================================
hyperParSpace=np.array(parSpace_new)
S=10
fgs=r['combn'](S,math.floor(S/2))

np.savetxt('fgs.txt',fgs,delimiter='   ',fmt='%s')

#X=pd.DataFrame()
ppv_J=[]
ppv_Jc=[]
temp=[]
N=[]
for i in range(len(parSpace_new)):
    print("M1=",parSpace_new[i,0],"N1=",parSpace_new[i,1],"M2=",parSpace_new[i,2],"N2=",parSpace_new[i,3],"n=",parSpace_new[i,4])


    stg=buysigTiming(parSpace_new[i])          #首先在全数据上生成买入卖出信号
    ar=np.arange(1,len(stg)+1,1)#相当于1:nrow(pr)
    stg['group']=pd.cut(ar,S,labels=False)#在全数据上加上组标签（0-9）
    for m in range(len(stg)):                  #修改成（1-10）   有warning
        stg['group'][m]=stg['group'][m]+1

    #我不知道为什么要加这一段，因为不加会报错
    #from rpy2.robjects import pandas2ri
    #pandas2ri.activate()
    #robjects.globalenv['dataframe']=stg 

    fgs_array=np.array(fgs)
    stg_list=stg.values.tolist()  #读CSV文件转为list
    stg_array=np.array(stg_list)


    for j in range(len(fgs_array[0])):
        J_tubes=pd.DataFrame()
        Jc_tubes=pd.DataFrame()
        temp.append(parSpace_new[i,:])
        #不同N代表不同的训练集选择
        N.append(j+1)
        k=0
        ########生成训练集##############
        for k in range(len(fgs_array)):
            J_tubes=J_tubes.append(stg.loc[stg['group']==fgs_array[k,j]])
        #J_tubes=np.array(J_tubes)
        ############生成验证集###########
        Jc_tubes=Jc_tubes.append(J_tubes)
        Jc_tubes=Jc_tubes.append(stg)
        Jc_tubes=Jc_tubes.drop_duplicates(subset=['date','Type','week','op','cl','buySig','sellSig','group'],keep=False)
        ##########计算ppv##################
        ppv_J.append(evaluateppv(J_tubes))
        ppv_Jc.append(evaluateppv(Jc_tubes))

X=pd.DataFrame(temp,columns=["M1","M2","N1","N2","n"],dtype=np.float64)
X['N']=N
X['ppv_J']=np.array(ppv_J)
X['ppv_Jc']=np.array(ppv_Jc)
#np.savetxt('ppv_J.txt',ppv_J,delimiter='   ',fmt='%s')
#np.savetxt('ppv_Jc.txt',ppv_Jc,delimiter='   ',fmt='%s')
print(X)
np.savetxt('X.txt',X,delimiter='   ',fmt='%s')
np.savetxt('stg.txt',stg,delimiter='   ',fmt='%s')

i=0
lambdaa=[]
Ld=pd.DataFrame(parSpace_new,columns=["M1","N1","M2","N2","n"])
while i <len(X):
    t=pd.DataFrame()
    pctank=pd.DataFrame()
    t=X.iloc[i:(i+len(fgs_array[0])),:]
    t['rank_b']=list(range(len(fgs_array[0])))  #原序号
    fg=t['ppv_J'].argmax()     # 确定训练集中最大的ppv对应的行号
    pcrank=t.sort_values(by="ppv_Jc",ascending=True)   #按ppvJc从小到大排序
    pcrank['rank_a']=list(range(1,len(fgs_array[0])+1))   #排序后序号
    W=pcrank.loc[pcrank['rank_b']==fg]
    w=float((W['rank_a']-1)/(len(fgs_array[0])-1))
    w=0.999 if w==1 else(0.001 if w==0 else w)
    lambdaa.append(math.log(w/(1-w)))           # 针对每一个训练集，计算样本外测试集的ppv相对排名，并计算lambda
    i=i+len(fgs_array[0])
Ld['lambda']=lambdaa


#from statsmodels.distributions.empirical_distribution import ECDF
#ecdf=ECDF(np.array(lambdaa))
#PBO=ecdf(0)

