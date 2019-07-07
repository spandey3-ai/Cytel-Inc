__author__ = 'O8288'
'''
This code extracts all required data from ORACALE databases. All files are properly named and are read in Merge_Data.py
Please ignore all commented lines
'''
import cx_Oracle

import cx_Oracle
# import pyodbc
import csv
import pandas as pd
def getMSSQLData(sqlStr=None,qry=True,insert=False):
    '''
    #test on SQL Server
    #str_Desc= "SELECT [SUSPENSE CODE],[DESCRIPTION] FROM [Revenue_Services].[dbo].[SuspenseCodeDesc]"
    #rows=getMSSQLData(str_Desc)
    #print rows[0][0]
    '''
    server = 'Tjaxoprsrch'
    connStr = ' DRIVER={SQL Server}'
    connStr += ';SERVER=' + server
    # connStr += ';DATABASE=' + db
    # connStr += ';UID=' + user
    # connStr += ';PWD=' + pwd
    connStr +='; Trusted_Connection=yes;'
    conn = pyodbc.connect(connStr)
    cursor = conn.cursor()
    if qry:
        cursor.execute(sqlStr)
        rows = cursor.fetchall()
        return rows
    elif insert:
        cursor.execute(sqlStr,insert)
        return cursor.commit()
    else:
        cursor.execute(sqlStr)
        return cursor.commit()

def getOracleData(filename,conn_str,qry=True,insert=False,sqlStr=None):
    '''
    #use [tnsping tmsp] to look for server name & port
    #e.g. test on Oracle Server
    rows=getOracleData("SELECT * FROM CSA_OWN.F_SWITCH_ANALYTICS_RESULT_DAY")
    print rows[0][0]
    '''
    # conn_str = u'NMD_USER/NMD_USER@vlx3013:1521/NMDP'
    # conn_str = u'TPRS_USER/TPRS_USER@ORACL03P:1521/TMSP'
    # print username
    # print (username+"/"+username+"@")
    # +host+":"+port+"/"+database)
    # conn_str = username+"/"+username+"@"+host+":"+port+"/"+database
    print " conn_str ", conn_str
    print " file ", filename
    # raw_input("Proceed??")
    connection = cx_Oracle.connect(conn_str)
    cursor = connection.cursor()
    print sqlStr
    cursor.execute(sqlStr)
    rows = cursor.fetchall()
    print rows
    columns = [i[0] for i in cursor.description]
    #count = cursor.fetchall()[0][0]
    fp = open(filename,'wb')
    myfile = csv.writer(fp)
    myfile.writerow(columns)
    myfile.writerows(rows)
    fp.close()
    cursor.close()
    connection.close()
    return rows

username = "TPRS_USER"
password= "TPRS_USER"
host = "ORACL03P"
port = "1521"
database = "TMSP"
conn_str = username+"/"+username+"@"+host+":"+port+"/"+database


#FIRST QUERY - Get Aurora data
S1 = "select  * from MV_WHOLE_MILEPOST_DTPM "
op= getOracleData(filename="APRIL_19th_2016_AURORA_DATA.csv",conn_str= conn_str, sqlStr=S1)


#SECOND QUERY - Tie Installation history for entire network
S2 = "select  WHOLE_MILEPOST_I , BEGIN_ENGINEER_MILEPOST_I , END_ENGINEER_MILEPOST_I , TRACK_TYPE_C, TIE_YEAR_M, TIE_Q , TRACK_SEGMENT_PREFIX_I from _)" \
     " GROUP BY  WHOLE_MILEPOST_I, TIE_YEAR_M, BEGIN_ENGINEER_MILEPOST_I , END_ENGINEER_MILEPOST_I ,TRACK_TYPE_C, TIE_YEAR_M, TIE_Q , TRACK_SEGMENT_PREFIX_I "
op= getOracleData(filename="APRIL_19th_2016_TIE_HISTORY.csv",conn_str=conn_str, sqlStr=S2)


#THIRD QUERY -Features for the Predictive Model
S5= "SELECT distinct  T2.WHOLE_MILEPOST_I AS WHOLE_MILEPOST_I, T2.BEGIN_ENGINEER_MILEPOST_I AS BEGIN_ENGINEER_MILEPOST_I, T2.END_ENGINEER_MILEPOST_I AS END_ENGINEER_MILEPOST_I, T2.DIVISION_C as DIVISION_C , T2.SUBDIVISION_C as SUBDIVISION_C, T2.TRACK_TYPE_C AS TRACK_TYPE_C , T2.TRACK_SEGMENT_PREFIX_I, T2.DETERIORATION_ZONE_X as DETERIORATION_ZONE_X , T2.DETERIORATION_RATE_M as DETERIORATION_PERCENT, T2.FREIGHT_SPEED_M as FREIGHT_SPEED_M , T2.LENGTH_OF_MILE_M AS LENGTH_OF_MILE_M , T2.CURVE_DELTA_SCORE_M AS CURVE_DELTA_SCORE_M, T2.CURVE_DELTA_M AS CURVE_DELTA_M ,T2.GAGE_DPM_M AS GAGE_DPM_M , T2.MGT_M AS MGT_M, T2.HAZMAT_X AS HAZMAT_X, T2.THRESHOLD_M AS THRESHOLD_M, T2.TOTAL_TIE_PER_MILE_Q as TOTAL_TIE_PER_MILE_Q, T2.CRITICAL_AREA_M AS CRITICAL_AREA_M FROM  TPRS_OWN.MASTERTRACK_WHOLE_MILEPOST T2, TIE_DETERIORATION_ANALYSIS T1 WHERE T2.WHOLE_MILEPOST_I = T1.WHOLE_MILEPOST_I"
op= getOracleData(filename="APRIL_20th_2016_CSX_NETWORK_MILE_DATA.csv",conn_str=conn_str, sqlStr=S5)


