import pandas as pd
import ta
roc = ta.momentum.ROCIndicator(r.pr['cl'],int(n)).roc()
