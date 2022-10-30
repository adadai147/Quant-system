import pandas as pd
import ta
vpt=ta.volume.VolumePriceTrendIndicator(r.pr['cl'],r.pr['vol']).volume_price_trend()
