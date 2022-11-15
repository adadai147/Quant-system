import pandas as pd
import ta

hband=ta.volatility.KeltnerChannel(r.pr['hp'],r.pr['lp'],r.pr['cl'],int(window),int(window_atr),int(multiplier)).keltner_channel_hband()
lband=ta.volatility.KeltnerChannel(r.pr['hp'],r.pr['lp'],r.pr['cl'],int(window),int(window_atr),int(multiplier)).keltner_channel_lband()
