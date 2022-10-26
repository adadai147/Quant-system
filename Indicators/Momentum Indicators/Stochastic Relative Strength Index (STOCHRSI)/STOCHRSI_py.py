import pandas as pd
import ta
rsi=ta.momentum.StochRSIIndicator(r.pr['cl'],int(n),int(para1),int(para2)).stochrsi()
