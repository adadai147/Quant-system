import pandas as pd
import ta
ppo=ta.momentum.PercentagePriceOscillator(r.pr['cl'],int(slow),int(fast),int(sign)).ppo_hist()
