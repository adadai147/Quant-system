import pandas as pd
import ta

high=r.pr['high']
low=r.pr['low']
cl=r.pr['cl']
n1=int(n1)
n2=int(n2)
n3=int(n3)
a = ta.trend.IchimokuIndicator(high,low,n1,n2,n3).ichimoku_a()
b = ta.trend.IchimokuIndicator(high,low,n1,n2,n3).ichimoku_b()
base_line = ta.trend.IchimokuIndicator(high,low,n1,n2,n3).ichimoku_base_line()
conversion_line = ta.trend.IchimokuIndicator(high,low,n1,n2,n3).ichimoku_conversion_line()
