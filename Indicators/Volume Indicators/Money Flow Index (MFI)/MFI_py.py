import pandas as pd
import ta
mfi=ta.volume.MFIIndicator(r.pr['high'],r.pr['low'],r.pr['cl'],r.pr['vol'],int(n)).money_flow_index()
