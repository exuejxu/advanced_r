# BDA 2. Assignment 1

from pyspark import SparkContext
from pyspark.sql import SQLContext, Row
from pyspark.sql import functions as F

sc = SparkContext()
sqlContext = SQLContext(sc)

tempRDD = sc.textFile("/user/x_matka/data/temperature-readings-small.csv")

tempRDD = tempRDD.map(lambda a: a.split(";"))

tempRDD = tempRDD.filter(lambda x: float(x[3]) >= 10 and int(x[1][0:4]) >= 1950 and int(x[1][0:4])<=2014)

tempReadings = tempRDD.map(lambda p: Row(station=p[0], year=p[1][0:7], temp=float(p[3])))

# Inferring the schema and registeringthe DataFrameas a table (Way I, slide 6)

schemaTempReadings = sqlContext.createDataFrame(tempReadings)

schemaTempReadings.registerTempTable("tempReadings")

# SQL-queries. SQL-queries is saved as a RDD

tempMax = sqlContext.sql("SELECT count(value) as value FROM tempReadings ORDER by value")

print tempMax.take(10)


# Save result when needed, otherwise print result

#tempMax.saveAsTextFile("/user/x_matka/lab2_assignment1_max")

#tempMin.saveAsTextFile("/user/x_matka/lab2_assignment1_min")

