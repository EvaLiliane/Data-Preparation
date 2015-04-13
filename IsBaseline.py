# python IsBaseline.py "/home/evaliliane/Documents/PhD/Python/AdultData_Time_Slope.csv" 1
# python IsBaseline.py "/home/evaliliane/Documents/PhD/Python/ChildrenData_Time_Slope.csv" 2

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
    if len(argv) != 3:
        sys.exit("Only supply the path to file: labdata_2014-07-10.csv")
    fn = argv[1]
    if not os.path.exists(fn):
        sys.exit("it appears the supplied file path does not exist.")
#}}}

    if int(argv[2]) == 1 : # Adults Dataset
      print int(argv[2])
      headings = ',patient,cohort,birth_dmy,birth_y,birth_m,gender,frsvis_dmy,entry,entry_other,mode,haart,haart_dmy,\
        fhv_stage_who,exp_y,mtct_y,pep_y,tb_fhv,wks_tb_fhv,preg_fhv,last_contact_dmy,last_contact_t,outcome,outcome_dmy,\
        outcome_y,outcome_m,death_txt,deliv_m,weight_birth,age,lab_dmy,lab_id,lab_v,unit_txt,rna_dmy,nviral,viral,\
        rna_unit_txt,weight,height,diff, slope,suppress, base\n'
      dif = 40
    if int(argv[2]) == 2 : # Children Dataset
      headings = ',patient,cohort,birth_dmy,birth_y,birth_m,gender,frsvis_dmy,entry,entry_other,mode,haart,haart_dmy,\
        fhv_stage_who,exp_y,mtct_y,pep_y,tb_fhv,wks_tb_fhv,preg_fhv,last_contact_dmy,last_contact_t,outcome,outcome_dmy,\
        outcome_y,outcome_m,death_txt,deliv_m,weight_birth,age,lab_dmy,lab_id,lab_v,unit_txt,rna_dmy,nviral,viral,\
        rna_unit_txt,visit_dmy,weight,height,diff, slope,suppress, base\n'
      dif = 41


    df = pd.io.parsers.read_csv(fn)
    print df.shape
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
        m = group.lab_dmy.dropna().min()        
        n = name[0] + "_" + name[1]
        #print n
        dct[n] = m
    print ("finished constructing dictionary")
    out_fn = os.path.split(fn)[0] +"/"+ os.path.split(fn)[1][:-4] + "_Base.csv"

    fw = open(out_fn, "w")
    print "writing to file."
    #headings = ",merge_no,cohort,patient,lab_dmy,lab_id,lab_v,unit_txt,lab_t,rna_l,tb_drug,drug_res,diff, slope\n"
    fw.write(headings)
#    i,p = 0,0
#    n = len(df)
    for row in df.iterrows():
#        i += 1
#        p1 = int(i*1.0 / n * 100.0)
#        if p1 != p and p1%5==0:
#            p = p1
#            print str(p) + "% done"
        base = 0
        if not pd.isnull(row[1][32]) and (row[1][dif] == 0):
            #print row[1][1], row[1][31], row[1][30], row[1][32], row[1][dif]
            k = row[1][1] + "_" + row[1][31]
            base = 1
        l = [str(i) for i in row[1]] + [str(base)]
        x = ",".join(l)
        x= x.replace("nan","NA")
        fw.write(x + "\n")

    fw.close()
    print "finished writing to Base file."

if __name__ == "__main__":

    main(sys.argv)
    print "end"

