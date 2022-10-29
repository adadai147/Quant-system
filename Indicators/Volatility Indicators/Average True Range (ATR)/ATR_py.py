import pandas as pd
import ta

atr=ta.volatility.AverageTrueRange(r.pr['hp'],r.pr['lp'],r.pr['cl'],int(period)).average_true_range()
