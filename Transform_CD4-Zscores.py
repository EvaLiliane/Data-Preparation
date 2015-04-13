
import pandas as pd
import numpy as np


def CD4toZscore(cd4,age):

  x = np.nan
  
  #cd4,age = indata[0], indata[1]
  a1 = 0.62987
  a2 = -1.4017
  a3 = 0.83364
  a4 = -0.23954
  a5 = 0.57091
  b1 = -0.49740
  b2 = 0.75867
  b3 = 0.23400
  c1 = 0.26709
  c2 = 0.26079
  c3 = 0.43891
  
  x = (age**a5-1)/a5
  m = (a1*x-a2)*np.exp(-a3*x+a4)
  l = b1+b2*np.exp(-b3*x)
  s = c1+c2*np.exp(-c3*x)
  
  return (((((cd4/1000)**l)-1)/l)-m)/s
  
  
#def   

if __name__ == "__main__":

  
#  dat = pd.io.parsers.read_csv("/home/evaliliane/Documents/PhD/Codes/NewData/IeDEA_Children_Data2014-10-06.csv")
  dat = pd.io.parsers.read_csv("ChildrenData_Time_Slope_Base.csv")
  print (dat.columns)
  #dat['zscore'] = pd.concat(dat['age'],dat['labv']).map(CD4toZscore)
  l = []
  niter = len(dat.patient)
  for i in range(niter):
    zsc = CD4toZscore(dat.lab_v[i],dat.age[i])
    print(i)
    l.append(zsc)
  dat['zscore'] = l  
  #dat['zscore'] = CD4toZscore(dat['age'],dat['labv'])
  dat.to_csv("ChildrenZscores.csv")
  print (len(dat.age), niter, len(dat.zscore), len(dat.lab_v))
