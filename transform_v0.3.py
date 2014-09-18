# Written by David Matten. 2014-09-10.
# For Eva Ujeneza.
# This script transforms the file (given as a passed argument) to calculate the difference in days between the minimum visit date, for a patient for a biomarker, and the visit date for a given row.
# The input file is a .csv file, with columns  "","merge_no","cohort","patient","lab_dmy","lab_id","lab_v","unit_txt","lab_t","rna_l","tb_drug","drug_res".
# The input file must contain this heading row - as pandas uses the headings to look up columns
# The output file is a .csv file, with the same input headings, and the additional heading: "diff
# The output file is written to the same directory as the input file.

import os, sys
import datetime
import pandas as pd
import numpy as np

def main(argv):
    '''
# Written by David Matten. 2014-09-10.
# For Eva Ujeneza.
# This script transforms the file (given as a passed argument) to calculate the difference in days between the minimum visit date, for a patient for a biomarker, and the visit date for a given row.
# In the case of a missing visit date, a NA is used.
# The input file is a .csv file, with columns  "","merge_no","cohort","patient","lab_dmy","lab_id","lab_v","unit_txt","lab_t","rna_l","tb_drug","drug_res".
# The input file should not contain a heading row.
# The output file is a .csv file, with the same input headings, and the additional heading: "diff
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
        dct[n] = m
    print "finished constructing dictionary"
    out_fn = os.path.split(fn)[0] +"/"+ os.path.split(fn)[1][:-4] + "_OUT1.csv"

    fw = open(out_fn, "w")
    print "writing to file."
    headings = ",merge_no,cohort,patient,lab_dmy,lab_id,lab_v,unit_txt,lab_t,rna_l,tb_drug,drug_res,diff\n"
    fw.write(headings)
#    i,p = 0,0
#    n = len(df)
    for row in df.iterrows():
#        i += 1
#        p1 = int(i*1.0 / n * 100.0)
#        if p1 != p and p1%5==0:
#            p = p1
#            print str(p) + "% done"
        diff = np.nan
        if not pd.isnull(row[1][4]):
            k = row[1][3] + "_" + row[1][5]
            diff = (pd.to_datetime(row[1][4]) - pd.to_datetime(dct[k])).days
        l = [str(i) for i in row[1]] + [str(diff)]
        x = ",".join(l)
        x= x.replace("nan","NA")
        fw.write(x + "\n")

    fw.close()
    print "finished writing to file."

if __name__ == "__main__":

    main(sys.argv)
    print "end"

