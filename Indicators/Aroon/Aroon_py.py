import pandas as pd
import ta
cl=r.pr['cl']
n=int(n)
aroon_down = ta.trend.AroonIndicator(cl,n).aroon_down()
aroon_up = ta.trend.AroonIndicator(cl,n).aroon_up()
aroon_indicator = ta.trend.AroonIndicator(cl,n).aroon_indicator()
