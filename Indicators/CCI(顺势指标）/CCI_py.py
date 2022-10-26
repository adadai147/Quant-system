import pandas as pd
import ta
high=r.pr['high']
low=r.pr['low']
cl=r.pr['cl']
n=int(n)
c=int(c)
cci = ta.trend.CCIIndicator(high,low,cl,n,c).cci()
