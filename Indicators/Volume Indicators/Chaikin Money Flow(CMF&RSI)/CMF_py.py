import pandas as pd
import ta
cmf=ta.volume.ChaikinMoneyFlowIndicator(r.pr['high'],r.pr['low'],r.pr['cl'],r.pr['vol'],int(n1)).chaikin_money_flow() 
rsi=ta.momentum.rsi(r.pr['cl'],int(n2))
