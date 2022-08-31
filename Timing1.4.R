library(pacman)
library(gcookbook)
library(ggplot2)
p_load(data.table,tidyverse,TTR,magrittr,parallel,plyr)


##=== 准备运行策略所需的数据 =========
pr <- fread('HQfd.csv') # 读取深证ETF 原始数据(优矿基金数据下载)
pr <- pr[,.(date=tradeDate,cl=closePrice,
            op=openPrice,adj=accumAdjFactor)] # 裁出所需数据
pr <- pr[,c('cl','op'):={
  list(cl*adj,op*adj)
}]
# 将价格转为后复权价格，保证计算的一致性


#=======牛市熊市择时策略引入===========
# 1.将股票市场简单划分为牛市熊市，依据为若价格高于过去n天（n取20）移动平均值，就是牛市（type==1)，否则看做是熊市(type==0)
# 2.择时策略：处于牛市时，当下一个交易日是周M1则买入，下一个交易日是周M2则卖出；处于熊市时，当下个交易日是周N1则买入，下个交易日是周N2则卖出
# 3.超参数组合 n,M1,M2,N1,N2


library(TTR)

Weekday <- c("星期一","星期二","星期三","星期四","星期五")
#================择时策略函数 buysigTiming()===================
buysigTiming <- function(x,hyperPars=c(M1,M2,N1,N2,n)){
  d1 <- x[,date]
  dmonth <- as.Date(d1,"%Y/%m/%d")
  week <- weekdays(dmonth)      #将数据中的日期标注星期
  
  stg <- x[,{
    MA <- SMA(x$op,hyperPars[5])        #计算出n日的移动平均值
    type <- ifelse(cl > MA,1,0)         #标记牛市熊市
    
    buySig <- ifelse((type==1 & week==Weekday[hyperPars[1]]) | (type==0 & week ==Weekday[hyperPars[3]]),1,0) 
    #若下一交易日为M1、N1（且符合选定的市场行情）则买入,；
    sellSig <- ifelse((type==1 & week==Weekday[hyperPars[2]]) | (type==0 & week ==Weekday[hyperPars[4]]),1,0) 
    #为M2、N2则卖出,分别打上买入和卖出标记
    list(date,type,week,op,cl,buySig,sellSig)}
  ]
  return(stg)
  
}
#策略函数测试
stgy <- buysigTiming(x=pr,hyperPars=c(1,4,2,3,20))
stgy                         #该策略即：处于牛市时，下一交易日为周一则买入，为周四则卖出;
                             #处于熊市时，下一交易日为周二则买入，为周三则卖出

#===============查准率评估策略函数 evaluateppv()=====================
#输入策略，输出其PPV
library("plyr")
evaluateppv <- function(stgy){
  loc<-(which(stgy[,'buySig']==1))
  stgyb<-stgy[loc,]
  
  loc<-(which(stgy[,'sellSig']==1))
  stgys<-stgy[loc,]
  
  list1 <- list()
  list1[[1]] <- data.frame(t(stgyb$op))
  list1[[2]] <- data.frame(t(stgys$cl))
  u <- rbind.fill(list1)
  u <- t(u)
  profit <- na.omit(log(u[,2])-log(u[,1])) #根据卖出当天收盘价与买入当天开盘价取对数后作差计算盈利
  ppv <- length(profit[profit>0])/length(profit)  #计算查准率
  
  return(ppv)
}
#===============绘制收益曲线函数drafting()=====================
#输入策略，输出其收益曲线（和股指曲线比较）

drafting <- function(stgy)
{
  loc<-(which(stgy[,'buySig']==1))
  stgyb<-stgy[loc,]
  
  loc<-(which(stgy[,'sellSig']==1))
  stgys<-stgy[loc,]
  
  list1 <- list()
  list1[[1]] <- data.frame(t(stgyb$op))
  list1[[2]] <- data.frame(t(stgys$cl))
  u <- rbind.fill(list1)
  u <- t(u)
  profit <- na.omit(u[,2]-u[,1]) #根据卖出当天收盘价与买入当天开盘价作差计算累计收益
  
  d1 <- stgyb$date
  dmonth <- as.Date(d1,"%Y/%m/%d")
  Time <- dmonth#提取出buysig为1的日期
  
  Data <- data.frame(Time, profit)# 赋值数据
  
  Time <- factor(Time)
  profit <- factor(profit)#变量因子化
  p <- ggplot(Data, aes(x=Time, y=profit, group=1)) + geom_line(linetype="dotted")#使用绘图函数
  return(p)
}
#================夏普比率评估函数evaluateSR()========================
evaluateSR <- function(stgy){
  loc<-(which(stgy[,'buySig']==1))
  stgyb<-stgy[loc,]
  
  loc<-(which(stgy[,'sellSig']==1))
  stgys<-stgy[loc,]
  
  list1 <- list()
  list1[[1]] <- data.frame(t(stgyb$op))
  list1[[2]] <- data.frame(t(stgys$cl))
  u <- rbind.fill(list1)
  u <- t(u)
  Rate <- na.omit(((u[,2])-(u[,1]))/u[,1])
   #根据卖出当天收盘价与买入当天开盘价计算盈利率
  
  #计算夏普比率
  ERp <- sum(Rate) #计算投资预期收益率
  P <- sd(Rate)    #计算收益率标准差
    Rf <- 0.03     #无风险利率选择国债利率（此处取3%）
    SR <- (ERp-Rf)/P
  
  return(SR)
}


# 以 M1,M2,N1,N2,n 为策略超参数生成参数搜索空间
M1 <- seq(1,5,by=1)
N1 <- seq(1,5,by=1)
M2 <- seq(1,5,by=1)
N2 <- seq(1,5,by=1)
n <- seq(10,30,by=1)

parSpace <- expand_grid(M1,N1,M2,N2,n) %>% data.table #生成参数搜索空间，一行对应一套参数
parSpace <- parSpace[(M1!=M2)&(N1!=N2),]              #筛选出符合条件的参数组合


#=============进行网格搜索的Search()函数，输出标定指标最优策略==========
#输入超参组合，输出最优组合
Search <-function(parSpace){
  Search <- parSpace[,{
    cat('M1=',M1,' N1=',N1,' M2=',M2,' N2=',N2, ' n=',n,'\n')
    ppv <- evaluateppv(buysigTiming(x=pr,hyperPars=c(M1=M1,N1=N1,M2=M2,N2=N2,n=n)))
    ppv
  },by=.(M1,N1,M2,N2,n)] #每个参数组合都检验，能暴力选出ppv最好的参数组合
  setnames(Search,'V1','ppv')
  Search  #查看参数组合以及其所对应的ppv值
  Best <- Search[which.max(ppv)]
  return(Best)    #查看最优参数组合
}
Search(parSpace)
 

#================= 使用 CSCV 计算 PBO ==================================
hyperParSpace <- parSpace
S=10L  #分十组

fgs <- combn(S,floor(S/2),simplify = F) # 模型训练集的分组序号组合
fulldata <- pr %>% mutate(group=cut(1:nrow(pr),S) %>% as.integer()) #在标记买卖信号的全数据上加上组标签

X <- hyperParSpace[,{
  cat(' M1=',M1,' M2=',M2,' N1=',N1,'N2=',N2,'n=',n,'\n')
  stg <-buysigTiming(x=pr,
                     hyperPars=c(M1=M1,N1=N1,M2=M2,N2=N2,n=n)) #首先在全数据上生成买入卖出信号
  fulldata <- stg %>% mutate(group=cut(1:nrow(pr),S) %>% as.integer()) #在标记买卖信号的全数据上加上组标签
  J.tubes <-fgs %>% map(~fulldata[group %in% .,]) # 生成含有所有训练集组合的列表
  Jc.tubes <- J.tubes %>% map(~ setdiff(fulldata,.)) #用差集的方式生成含有所有验证集组合的列表
  ppv.J <- map_dbl(J.tubes,~evaluateppv(.))
  ppv.Jc <- map_dbl(Jc.tubes,~evaluateppv(.))
  list(N=1:length(J.tubes),ppv.J=ppv.J,ppv.Jc=ppv.Jc)#不同N代表不同的训练集选择
},by=.(M1,N1,M2,N2,n)] # 针对每一个超参组合，计算全部训练集J和对应的全部验证集Jc上的ppv结果

write.csv(X,"outcome.csv") #将训练结果写入csv文件

Ld <- X[,{
  fg <- which.max(ppv.J) # 确定训练集中最大的ppv对应的行号
  w <- ppv.Jc %>% percent_rank() %>% `[`(fg)
  w <- ifelse(w==1,0.999,ifelse(w==0,0.001,w))
  lambda <- log(w/(1-w))
  list(lambda=lambda)         
},by=.(M1,N1,M2,N2,n)]# 针对每一个训练集，计算样本外测试集的ppv相对排名，并计算lambda

write.csv(Ld,"Rank.csv") #将排名结果写入csv文件

Fn.cdf <- Ld$lambda %>% ecdf #构建lambda的经验分布函数
PBO <- Fn.cdf(0) # 根据经验分布函数，得到过拟合概率


