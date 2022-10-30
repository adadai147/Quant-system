import pandas as pd
import ta
eom=ta.volume.ease_of_movement(r.pr['high'],r.pr['low'],r.pr['vol'],int(n))
