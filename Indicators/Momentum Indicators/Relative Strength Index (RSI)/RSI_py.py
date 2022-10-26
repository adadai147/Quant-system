import pandas as pd
import ta
rsi=ta.momentum.RSIIndicator(r.pr['cl'],int(n)).rsi()
