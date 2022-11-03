import pandas as pd
import ta

cl=r.pr['cl']
n=int(n)
ema = ta.trend.EMAIndicator(cl,n).ema_indicator()

