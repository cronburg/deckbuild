#!/usr/bin/env python
import pickle
from pylab import *

# Optional percentages on y-axis:
#to_percentage = lambda y, pos: str(round( ( y / float(len(data)) ) * 100.0, 2)) + '%'
#plt.gca().yaxis.set_major_formatter(FuncFormatter(to_percentage))

data = pickle.load(open("data.pkl",'r'))

for t in [24,25,26,27]:
  hist_data = data[t] #param0[where(turn == t)[0]]
  weights = np.ones_like(hist_data) / len(hist_data)
  
  hist(hist_data, bins=80, color='g', weights=weights)
  xlim(0.0, 1.0)
  ylim(0.0, 0.12)
  
  xlabel("param0 (Village-to-Chancellor buy ratio)")
  ylabel("probability")
  title("P(param0 | turns == %02d)" % (t,))
  savefig("MH-village-chancellor-%02dturns.png" % (t,))
  close()

