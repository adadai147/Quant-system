library(pacman)
p_load(data.table,tidyverse,TTR,magrittr,parallel)


##=== 准备运行策略所需的数据 =========
pr <- fread('HQfd.csv') # 读取深证ETF 原始数据(优矿基金数据下载)
pr <- pr[,.(date=tradeDate,cl=closePrice,
            op=openPrice,adj=accumAdjFactor)] # 裁出所需数据
pr <- pr[,c('cl','op'):={
  list(cl*adj,op*adj)
}]
# 将价格转为后复权价格，保证计算的一致性


#=======牛市熊市择时策略引入===========
# 1.将股票市场简单划分为牛市熊市，依据为若价格高于过去n天（n取20）移动平均值，就是牛市（type == 1)，否则看做是熊市(type == 0)
# 2.择时策略：处于牛市时，当下一个交易日是周M1则买入，下一个交易日是周M2则卖出；处于熊市时，当下个交易日是周N1则买入，下个交易日是周N2则卖出
# 3.超参数组合 n,M1,M2,N1,N2
library(TTR)
#载入计算移动平均值的包TTR
Weekday <- c("星期一","星期二","星期三","星期四","星期五")
buysigTiming <- function(x,hyperPars=c(n,M1,M2,N1,N2)){
  #择时策略函数
  d1 <- x[,date]
  dmonth <- as.Date(d1,"%Y/%m/%d")
  week <- weekdays(dmonth)      #将数据中的日期标注星期
  
  stg <- x[,{
    MA <- SMA(x$op,hyperPars[1])        #计算出n日的移动平均值
    type <- ifelse(cl > MA,1,0)         #标记牛市熊市
    
    buySig <- ifelse((type==1 & week==hyperPars[2])||(type==0 & week ==hyperPars[4]),1,0)  #若下一交易日为M（且符合选定的市场行情）则买入；
    sellSig <- ifelse((type==1 & week==hyperPars[3])||(type==0 & week ==hyperPars[5]),1,0) #为N则卖出,分别打上买入和卖出标记
  list(date,type,week,op,cl,buySig,sellSig)}
  ]
  return(stg)
  
}
#策略函数测试
stgy <- buysigTiming(pr,hyperPars=c(20,Weekday[1],Weekday[4],Weekday[2],Weekday[3]))
stgy                         #该策略即：处于牛市时，下一交易日为周一则买入，为周四则卖出;
                                       #处于熊市时，下一交易日为周二则买入，为周三则卖出

#构造buysig与sellsig分别为1的行所构成的矩阵
loc<-(which(stgy[,buySig]==1))
loc
stgyb<-stgy[loc,]
loc<-(which(stgy[,sellSig]==1))
loc
stgys<-stgy[loc,]
profit<-(sum(stgys$cl)-sum(stgyb$op))#根据卖出当天收盘价与买入当天开盘价总和之差计算盈利

# 编制策略的评估函数 ======
evaluate <- function(stg, data=pr) 
{
  # data: 评估策略所需要用到的数据
  Y <- merge(data,stg,by='date',all.x=T) #将数据和策略给出的买入信号合并
  Y <- Y[,{
    performance <- ifelse(buySig>0, profit, 0)
    list(date,performance)
  }] #添加买入信号下，按信号投资的实际收益率
  ppv <- Y[,][,.(y=sum(performance>0)/.N)]$y # 使收益率为正的信号所占的比例：查准率
  ppv
}
# 以 M,N 为策略超参数，暴力网格搜索，优化得到ppv最大的策略
M <- seq(1,5,by=1)
N <- seq(1,5,by=1)


parSpace <- expand_grid(M,N) %>% data.table #生成参数搜索空间，一行对应一套参数
parSpace <- parSpace[M!=N,] #筛选出type为1的参数组合

Search <- parSpace[,{
  cat('M=',M,' N=',N,'\n')
  ppv <- evaluate(buysigTiming(x=pr,hyperPars=c(20,Type=1,M=M,N=N)), data=pr)
  ppv
},by=.(M,N)] #每个参数组合都检验，能暴力选出ppv最好的参数组合
