import numpy as np
from expmmq import *

# considerando dia0 como 14/03 (primeiro caso em JF). 
# ref: https://www.worldometers.info/coronavirus/coronavirus-cases/#total-cases
dia = np.array([0,1,2,3,4,5,6,7])
casos = np.array([156653, 169593, 182490, 198238, 218822, 244933, 275597, 305036])
c0,c1 = exp_mmq(dia, casos)
print(c0,c1)