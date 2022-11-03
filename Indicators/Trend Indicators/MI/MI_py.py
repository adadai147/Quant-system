import pandas as pd
import ta
high=r.pr['high']
low=r.pr['low']
nf=int(nf)
ns=int(ns)

mass = ta.trend.MassIndex(high,low,nf,ns).mass_index()
