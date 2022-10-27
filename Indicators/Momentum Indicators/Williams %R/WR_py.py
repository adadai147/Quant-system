import pandas as pd
import ta
wr=ta.momentum.williams_r(r.pr['high'],r.pr['low'],r.pr['cl'],int(lbp))
