import pandas as pd
import ta
fi=ta.volume.force_index(r.pr['cl'],r.pr['vol'],int(n1))
cci=ta.trend.CCIIndicator(r.pr['high'],r.pr['low'],r.pr['cl'],int(n2),0.015).cci()
