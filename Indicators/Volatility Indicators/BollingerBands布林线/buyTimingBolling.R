library(pacman)
library(gcookbook)
library(ggplot2)
library(reticulate)
p_load(data.table,tidyverse,TTR,magrittr,parallel,plyr)


##=== 准备运行策略所需的数据 =========
pr <- fread('HQfd.csv') # 读取深证ETF 原始数据(优矿基金数据下载)
pr <- pr[,.(date=tradeDate,cl=closePrice,
            op=openPrice,adj=accumAdjFactor)] # 裁出所需数据
pr <- pr[,c('cl','op'):={
  list(cl*adj,op*adj)
}]
# 将价格转为后复权价格，保证计算的一致性


library(TTR)

Weekday <- c("星期一","星期二","星期三","星期四","星期五")

#=======================基于布林带的择时策略======================
#1.当收盘价从下轨线(lower)突破时（前一天收盘价应当位于lower下方），买入股票
#2.当收盘价从上轨线（upper)跌破时（前一天收盘价应当位于upper上方），卖出股票
#超参：period,factor
#=====================布林带(Bollinger Band)====================

buyTimingBolling <- function(x,hyperPars=c(per,fac)){
  py$period=hyperPars[1]
  py$factor=hyperPars[2]
  py_run_file("C:/量化投资/2022.10-11/BollingerBands布林线/Bolling_py/Bolling_py.py")
  stg <- x[,{
    sellSig=py$hband
    buySig=py$lband
    
    list(date,op,cl,buySig,sellSig)
    
  }]
  return(stg)
  
}
#策略函数测试
#stgy <- buysigBolling(x=pr,hyperPars=c(20,2))
#stgy                        


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



#超参：period,factor
per <- seq(10,12,by=1)
fac <- seq(1,2,by=1)

parSpace <- expand_grid(per,fac) %>% data.table #生成参数搜索空间，一行对应一套参数

#================= 使用 CSCV 计算 PBO ==================================
hyperParSpace <- parSpace
S=10L  #分十组

fgs <- combn(S,floor(S/2),simplify = F) # 模型训练集的分组序号组合
fulldata <- pr %>% mutate(group=cut(1:nrow(pr),S) %>% as.integer()) #在标记买卖信号的全数据上加上组标签

X <- hyperParSpace[,{
  cat(' per=',per,' fac=',fac,'\n')
  stg <-buyTimingBolling(x=pr,
                     hyperPars=c(per=per,fac=fac)) #首先在全数据上生成买入卖出信号
  fulldata <- stg %>% mutate(group=cut(1:nrow(pr),S) %>% as.integer()) #在标记买卖信号的全数据上加上组标签
  J.tubes <-fgs %>% map(~fulldata[group %in% .,]) # 生成含有所有训练集组合的列表
  Jc.tubes <- J.tubes %>% map(~ setdiff(fulldata,.)) #用差集的方式生成含有所有验证集组合的列表
  ppv.J <- map_dbl(J.tubes,~evaluateppv(.))
  ppv.Jc <- map_dbl(Jc.tubes,~evaluateppv(.))
  list(N=1:length(J.tubes),ppv.J=ppv.J,ppv.Jc=ppv.Jc)#不同N代表不同的训练集选择
},by=.(per,fac)] # 针对每一个超参组合，计算全部训练集J和对应的全部验证集Jc上的ppv结果

#write.csv(X,"outcome.csv") #将训练结果写入csv文件

Ld <- X[,{
  fg <- which.max(ppv.J) # 确定训练集中最大的ppv对应的行号
  w <- ppv.Jc %>% percent_rank() %>% `[`(fg)
  w <- ifelse(w==1,0.999,ifelse(w==0,0.001,w))
  lambda <- log(w/(1-w))
  list(lambda=lambda)         
},by=.(per,fac)]# 针对每一个训练集，计算样本外测试集的ppv相对排名，并计算lambda

write.csv(Ld,"Rank.csv") #将排名结果写入csv文件

Fn.cdf <- Ld$lambda %>% ecdf #构建lambda的经验分布函数
PBO <- Fn.cdf(0) # 根据经验分布函数，得到过拟合概率

