import pandas as pd
import ta
tsi=ta.momentum.TSIIndicator(r.pr['cl'],int(slow),int(fast)).tsi()
