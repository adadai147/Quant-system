import pandas as pd
import ta
cl=r.pr['cl']
nf=int(nf)
ns=int(ns)
cycle=int(cycle)
smooth1=int(smooth1)
smooth2=int(smooth2)

stc = ta.trend.STCIndicator(cl,nf,ns,cycle,smooth1,smooth2).stc()
