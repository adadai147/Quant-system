import pandas as pd
import ta
sto=ta.momentum.StochasticOscillator(r.pr['high'],r.pr['low'],r.pr['cl'],int(n1),int(n2)).stoch()
