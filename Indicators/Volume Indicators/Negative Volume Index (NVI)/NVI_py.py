import pandas as pd
import ta
nvi=ta.volume.NegativeVolumeIndexIndicator(r.pr['cl'],r.pr['vol']).negative_volume_index() 
