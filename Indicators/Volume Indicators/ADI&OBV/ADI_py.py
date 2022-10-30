import pandas as pd
import ta
adi=ta.volume.acc_dist_index(r.pr['high'],r.pr['low'],r.pr['cl'],r.pr['vol'])
obv=ta.volume.on_balance_volume(r.pr['cl'],r.pr['vol'])
