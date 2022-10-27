import pandas as pd
import ta
uo=ta.momentum.UltimateOscillator(r.pr['high'],r.pr['low'],r.pr['cl'],int(short),int(medium),int(long),4/7,2/7,1/7).ultimate_oscillator()
