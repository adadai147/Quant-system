import pandas as pd
import ta

hband=ta.volatility.BollingerBands(r.pr['cl'],int(period),int(factor)).bollinger_hband_indicator()
lband=ta.volatility.BollingerBands(r.pr['cl'],int(period),int(factor)).bollinger_lband_indicator()
