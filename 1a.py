from pyspark import SparkContext
sc= SparkContext(appName="Station Min temperature1")

lines = sc.textFile("/user/x_yumli/temperature-readings.csv").cache()

lines = lines.map(lambda a: a.split(";"))

lines = lines.filter(lambda x: int(x[1][0:4]) >= 1950 and int(x[1][0:4])<=2014)

temperatures = lines.map(lambda x: (x[1][0:4], (x[0], float(x[3]))))

#minTemperatures = temperatures.groupByKey(lambda a,b: a if(a[1]>b[1]) else b)

minTemperatures = temperatures.reduceByKey(lambda a,b: a if(a[1]>=b[1]) else b)

#minTemperatures = minTemperatures.map(lambda a: (a[0], min(a[1])))

minTemperaturesSorted = minTemperatures.sortBy(ascending=False,keyfunc=lambda k: k[1][1])

minTemperaturesSorted.saveAsTextFile("/user/x_yumli/station_min1")
