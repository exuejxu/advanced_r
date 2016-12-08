#!/usr/bin/env python

#import csv
#import operator
import datetime

TMPERATURE_READINGS = "/tmp/temperature-readings.csv"
TBL_FORMAT = "{0:<8}{1:<10}{2:<8}"
SORT_KEY = lambda (year, (sn, temp)):temp
#FIELD_NAMES = ['station_number','date','time','temperature','quality']

f = open(TMPERATURE_READINGS, "rb")
#reader = csv.reader(f, delimiter=';')


start_time = datetime.datetime.now()
max_temp = dict()
for row in f:
    rl = row.split(';')
    sn = rl[0]
    temp = float(rl[3])
    year = rl[1].split('-')[0]
    if year in max_temp:
        if temp > max_temp[year][1]:
            max_temp[year] = (sn, temp)
    else:
        max_temp[year] = (sn, temp)

sorted_max_temp = sorted(max_temp.items(), key=SORT_KEY, reverse=True)
end_time = datetime.datetime.now()
duration = end_time - start_time

print TBL_FORMAT.format('Year', 'Station', 'Max')
for r in sorted_max_temp:
    if int(r[0]) >1949 and int(r[0]) < 2015:
        print TBL_FORMAT.format(r[0], r[1][0], r[1][1])

print "Time to calulate the max temperature: {0}".format(duration)

# reset file pointer
f.seek(0)

min_temp = dict()
for row in f:
    rl = row.split(';')
    sn = rl[0]
    temp = float(rl[3])
    year = rl[1].split('-')[0]
    if year in min_temp:
        if temp < min_temp[year][1]:
            min_temp[year] = (sn, temp)
    else:
        min_temp[year] = (sn, temp)

sorted_min_temp = sorted(max_temp.items(), key=SORT_KEY)

print "******************************************"
print TBL_FORMAT.format('Year', 'Station', 'Min')
for r in sorted_min_temp:
    if int(r[0]) >1949 and int(r[0]) < 2015:
        print TBL_FORMAT.format(r[0], r[1][0], r[1][1])


f.close()
