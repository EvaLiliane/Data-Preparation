# python regression.py "/home/evaliliane/Documents/PhD/Python/labdata_2014-07-10_OUT_.csv"

import pandas as pd
import numpy as np
import statsmodels.formula.api as sm
import matplotlib.pyplot as plt
import os, sys
import datetime

def main(argv):
    '''
# Written by Eva Ujeneza.
# Heavily based on a code initially written by David Matten on 2014-09-10.
# This script transforms the file (given as a passed argument) to calculate the slope of a biomarker values, for a patient.
# NA values for biomarker values are ignored.
# The input file is a .csv file, with columns  "","merge_no","cohort","patient","lab_dmy","lab_id","lab_v","unit_txt","lab_t","rna_l","tb_drug","drug_res", "diff".
# The input file should contain a heading row.
# The output file is a .csv file, with the same input headings, and the additional heading: "slope"
# The output file is written to the same directory as the input file.
    '''
    # Confirm the file exists.
#{{{
    if len(argv) != 2:
        sys.exit("Only supply the path to file: labdata_2014-07-10.csv")
    fn = argv[1]
    if not os.path.exists(fn):
        sys.exit("it appears the supplied file path does not exist.")
#}}}

    df = pd.io.parsers.read_csv(fn)
    # df = pd.io.parsers.read_csv("labdata_2014-07-10_OUT_.csv", low_memory=False)
    
    grouped = df.groupby(['patient', 'lab_id'])
    print "constructing dictionary"
    dct = {}
    n_groups = len(df.groupby(['patient', 'lab_id'], sort=False))
    i,p = 0,0
    printed = False
    for name, group in df.groupby(['patient', 'lab_id'], sort=False):
    	
        i += 1
        p1 = int(i*1.0 / n_groups * 100.0)
        if (p1 != p) and (p1%5==0):
            p = p1
            print str(p) + "% done"
        # calculate slope
        tim = ["diff"]
        nam = ["lab_v"]
        n = len(group[tim])
        nd = group[tim].count()['diff']
        nv = group[nam].count()['lab_v']
        
        #df['A'].count()
        if (nd == 0 or nv == 0):
          #print nd, nv, n, name[0]
        #x = sm.add_constant(group[tim])
         #print group
          m = "NA"
         # print group
        else:
         model = sm.ols(formula='group[nam] ~ group[tim]', data=df, missing='none') # , subset = whiteside['Insul']=="Before")
         fitted = model.fit()
         #print "Model fitting OK"
         #print fitted.summary()
         m = fitted.params[1]
         #print m
        #m = group.lab_dmy.dropna().min()
        n = name[0] + "_" + name[1]
        dct[n] = m
    print "finished constructing dictionary"
    out_fn = os.path.split(fn)[0] +"/"+ os.path.split(fn)[1][:-4] + "_OUT1.csv"

    fw = open(out_fn, "w")
    print "writing to file."
    headings = ",merge_no,cohort,patient,lab_dmy,lab_id,lab_v,unit_txt,lab_t,rna_l,tb_drug,drug_res,diff, slope\n"
    fw.write(headings)
#    i,p = 0,0
#    n = len(df)
    for row in df.iterrows():
#        i += 1
#        p1 = int(i*1.0 / n * 100.0)
#        if p1 != p and p1%5==0:
#            p = p1
#            print str(p) + "% done"
        slope = np.nan
        # What does the below mean ??
        if not pd.isnull(row[1][4]):
            k = row[1][3] + "_" + row[1][5]
            slope = dct[k]
            # diff = (pd.to_datetime(row[1][4]) - pd.to_datetime(dct[k])).days
        # What does the below mean ??
        l = [str(i) for i in row[1]] + [str(slope)]
        
        x = ",".join(l)
        x= x.replace("nan","NA")
        fw.write(x + "\n")

    fw.close()
    print "finished writing to file."

if __name__ == "__main__":

    main(sys.argv)
    print "end"

