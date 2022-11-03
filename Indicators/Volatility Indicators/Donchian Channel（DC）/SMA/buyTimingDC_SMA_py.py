import pandas as pd
import ta

mband=ta.volatility.DonchianChannel(r.pr['hp'],r.pr['lp'],r.pr['cl'],int(period)).donchian_channel_mband()
