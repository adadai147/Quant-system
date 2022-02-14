################## DEMO #######################
######### authored by Lin Li ##################
### Calculation of PBO by CSCV algorithm ######

############# 加载所需的R 包
library(pacman)
p_load(data.table,tidyverse,TTR,magrittr,parallel)


##=== 准备运行策略所需的数据 =========
pr <- fread('HQfd.csv') # 读取深证ETF 原始数据(优矿基金数据下载)
pr <- pr[,.(date=tradeDate,cl=closePrice,
            op=openPrice,adj=accumAdjFactor)] # 裁出所需数据
pr <- pr[,c('cl','op'):={
  list(cl*adj,op*adj)
}]# 将价格转为后复权价格，保证计算的一致性

#===== 设计一个简单策略 ============================
# 简单的投资策略为：MACD的快线上穿慢线信号确认后的
# 第二天开盘买入，在第三天的收盘卖出，不计任何交易
# 成本, 其中MACD用简单移动平均计算

# 编制策略函数 ========
buysigSimpleMACD <- function(x=pr,
                             hyperPars=c(nFast=12,nSlow=26,nSig=9)){
  # x: 生成策略所需要用到的数据
  stg <- x[,{
    macd <-  cl %>% MACD(nFast=hyperPars[1],
                         nSlow=hyperPars[2],
                         nSig=hyperPars[3]) # 计算出MACD的快线和慢线，第一列快线，第二列慢线
    DIFF <- macd[,1]
    DEA <-  macd[,2]
    prime <- DIFF-DEA # MACD 快线减慢线形成的柱
    buysig <- ifelse(prime>0 & prime*shift(prime,1)<0,1,0) # 如果柱是向上，并且快线上穿慢线就给出买入信号
    list(date,buysig)
  }] 
  return(stg)
} # 生成策略，针对每个交易日，有买入信号是 sig 是1，否则 sig 是0

# 策略函数测试 =========
stg <- buysigSimpleMACD(pr,hyperPars=c(10,20,8))


# 编制策略的评估函数 ======
evaluate <- function(stg, data=pr){
  # data: 评估策略所需要用到的数据
  Y <- merge(data,stg,by='date',all.x=T) #将数据和策略给出的买入信号合并
  Y <- Y[,{
    performance <- ifelse(buysig>0, (log(shift(cl,-2))-log(shift(op,-1)))*100,0)
    list(date,buysig,performance)
  }] #添加买入信号下，按信号投资的实际收益率
  ppv <- Y[buysig==1,][,.(y=sum(performance>0)/.N)]$y # 使收益率为正的信号所占的比例：查准率
  ppv
}

setnames(Search,'V1','ppv')
# 策略评估函数的测试 =========
ppv <- evaluate(buysigSimpleMACD(x=pr,hyperPars=c(10,20,8)), data=pr)
ppv

# 以 nFast, nSlow, nSig 为策略超参数，暴力网格搜索，优化得到ppv最大的策略
nFast <- seq(1,50,by=2)
nSlow <- seq(1,100,by=2)
nSig <- seq(1,50,by=3)

parSpace <- expand_grid(nFast,nSlow,nSig) %>% data.table #生成参数搜索空间，一行对应一套参数
parSpace <- parSpace[nSlow>nFast,] #筛选出 nSlow 比 nFast 大的参数组合

Search <- parSpace[,{
  cat('nFast=',nFast,' nSlow=',nSlow,' nSig=',nSig,'\n')
  ppv <- evaluate(buysigSimpleMACD(x=pr,hyperPars=c(nFast=nFast,nSlow=nSlow,nSig=nSig)), data=pr)
  ppv
},by=.(nFast,nSlow,nSig)] #每个参数组合都检验，能暴力选出ppv最好的参数组合



###### 使用 CSCV 计算 PBO ##############
##### 以下为演示，超参搜索空间并不大, 未编成函数 ####
hyperParSpace <- expand_grid(nFast=c(9,15,20),nSlow=c(20,30,40),nSig=c(7,10,13)) %>% data.table
S=10L

fgs <- combn(S,floor(S/2),simplify = F) # 模型训练集的分组序号组合
fulldata <- pr %>% mutate(group=cut(1:nrow(pr),S) %>% as.integer()) #在全数据上加上组标签

X <- hyperParSpace[,{
  cat('nFast=',nFast,' nSlow=',nSlow,' nSig=',nSig,'\n')
  stg <-buysigSimpleMACD(x=pr,
                         hyperPars=c(nFast=nFast,nSlow=nSlow,nSig=nSig)) #首先在全数据上生成买入信号
  J.tubes <-fgs %>% map(~fulldata[group %in% .,]) # 生成训练集
  Jc.tubes <- J.tubes %>% map(~ setdiff(fulldata,.)) #用差集的方式生成验证集
  ppv.J <- map_dbl(J.tubes,~evaluate(stg,data=.))
  ppv.Jc <- map_dbl(Jc.tubes,~evaluate(stg,data=.))
  list(n=1:length(J.tubes),ppv.J=ppv.J,ppv.Jc=ppv.Jc)
},by=.(nFast,nSlow,nSig)] # 针对每一个超参组合，计算全部训练集J和对应的全部验证集Jc上的ppv结果


## 上面相同的过程，使用并行计算提高速度 ##===========
tubes <- hyperParSpace %>% split(hyperParSpace[[1]] %>% factor)
cores <- detectCores() # 检查计算机有几个核
cl.cores = makeCluster(cores) # 将每个核配置成并行运算，生成运算集群
clusterExport(cl.cores,
              varlist=c('pr','fgs','fulldata',
                        'hyperParSpace','buysigSimpleMACD','evaluate')) #将这些数据装入集群，否则集群找不到这些数据
Ylist <- parLapply(cl=cl.cores,tubes,function(s){
  library(pacman)
  p_load(tidyverse,magrittr,data.table,TTR)
  Y.s <- hyperParSpace[,{
    cat('nFast=',nFast,' nSlow=',nSlow,' nSig=',nSig,'\n')
    stg <-buysigSimpleMACD(x=pr,
                           hyperPars=c(nFast=nFast,nSlow=nSlow,nSig=nSig)) #首先在全数据上生成买入信号
    J.tubes <-fgs %>% map(~fulldata[group %in% .,]) # 生成训练集
    Jc.tubes <- J.tubes %>% map(~ setdiff(fulldata,.)) #用差集的方式生成验证集
    ppv.J <- map_dbl(J.tubes,~evaluate(stg,data=.))
    ppv.Jc <- map_dbl(Jc.tubes,~evaluate(stg,data=.))
    list(n=1:length(J.tubes),ppv.J=ppv.J,ppv.Jc=ppv.Jc)
  },by=.(nFast,nSlow,nSig)]
  return(Y.s)
})
stopCluster(cl.cores)
Y <- do.call('rbind',c(Ylist,fill=T)) %>% data.table

Ld <- Y[,{
  fg <- which.max(ppv.J) # 确定训练集中最大的ppv对应的行号
  w <- ppv.Jc %>% percent_rank() %>% `[`(fg)
  w <- ifelse(w==1,0.999,ifelse(w==0,0.001,w))
  lambda <- log(w/(1-w))
  list(lambda=lambda)         
},by=.(n)]# 针对每一个训练集，计算样本外测试集的ppv相对排名，并计算lambda

Fn.cdf <- Ld$lambda %>% ecdf #构建lambda的经验分布函数
PBO <- Fn.cdf(0) # 根据经验分布函数，得到过拟合概率

##### 根据脚本演示，编制PBO计算函数 ###########
cores <- detectCores() # 检查计算机有几个核
BOP.simpleMACD <- function(hyperParSpace,S=10L,data=pr,cores=cores,level=0.5){
  #BOP ：与PBO做区分，Backtest Overfitting Probability
  #level: 判定过拟合时的分位数，通常用50% 分位数
  fgs <- combn(S,floor(S/2),simplify = F) # 模型训练集的分组序号组合
  fulldata <- data %>% mutate(group=cut(1:nrow(data),S) %>% as.integer()) #在全数据上加上组标签
  
  tubes <- hyperParSpace %>% split(hyperParSpace[[1]] %>% factor)
  cl.cores = makeCluster(cores) # 将每个核配置成并行运算，生成运算集群
  clusterExport(cl.cores,
                varlist=c('data','fgs','fulldata',
                          'hyperParSpace','buysigSimpleMACD','evaluate')) #将这些数据装入集群，否则集群找不到这些数据
  Ylist <- parLapply(cl=cl.cores,tubes,function(s){
    library(pacman)
    p_load(tidyverse,magrittr,data.table,TTR)
    Y.s <- hyperParSpace[,{
      cat('nFast=',nFast,' nSlow=',nSlow,' nSig=',nSig,'\n')
      stg <-buysigSimpleMACD(x=data,
                             hyperPars=c(nFast=nFast,nSlow=nSlow,nSig=nSig)) #首先在全数据上生成买入信号
      J.tubes <-fgs %>% map(~fulldata[group %in% .,]) # 生成训练集
      Jc.tubes <- J.tubes %>% map(~ setdiff(fulldata,.)) #用差集的方式生成验证集
      ppv.J <- map_dbl(J.tubes,~evaluate(stg,data=.))
      ppv.Jc <- map_dbl(Jc.tubes,~evaluate(stg,data=.))
      list(n=1:length(J.tubes),ppv.J=ppv.J,ppv.Jc=ppv.Jc)
    },by=.(nFast,nSlow,nSig)]
    return(Y.s)
  }) 
  stopCluster(cl.cores)
  Y <- do.call('rbind',c(Ylist,fill=T)) %>% data.table
  
  Ld <- Y[,{
    fg <- which.max(ppv.J) # 确定训练集中最大的ppv对应的行号
    w <- ppv.Jc %>% percent_rank() %>% `[`(fg)
    w <- ifelse(w==1,0.999,ifelse(w==0,0.001,w))
    b <- level/(1-level)
    lambda <- log(w/(b*(1-w)))
    list(lambda=lambda)         
  },by=.(n)]# 针对每一个训练集，计算样本外测试集的ppv相对排名，并计算lambda
  
  Fn.cdf <- Ld$lambda %>% ecdf #构建lambda的经验分布函数
  PBO <- Fn.cdf(0) # 根据经验分布函数，得到过拟合概率
  return(list(PBO,lambda=Ld$lambda))
}


nFast <- seq(2,50,length.out=cores) %>% floor 
nSlow <- seq(10,80,by=8)
nSig <- seq(5,50,by=5)
hyperParSpace <- expand_grid(nFast,nSlow,nSig) %>% data.table
hyperParSpace <- hyperParSpace[,nSlow:=nSlow+nFast]
S=10L


#下面运算，超参组合有720个，
#使用并行，8核CPU，大约运行1个小时左右
BOP <- BOP.simpleMACD(hyperParSpace,S=10L,data=pr,cores=cores) 



