import pandas as pd
import ta

#hband=ta.volatility.DonchianChannel(r.pr['hp'],r.pr['lp'],r.pr['cl'],int(period)).donchian_channel_hband()
#lband=ta.volatility.DonchianChannel(r.pr['hp'],r.pr['lp'],r.pr['cl'],int(period)).donchian_channel_lband()
mband=ta.volatility.DonchianChannel(r.pr['hp'],r.pr['lp'],r.pr['cl'],int(period)).donchian_channel_mband()