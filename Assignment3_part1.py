from pyspark import SparkContext
sc = SparkContext(appName="Monthly temp")

lines = sc.textFile("/user/x_matka/data/temperature-readings-small.csv").cache()

lines = lines.map(lambda a: a.split(";"))

lines = lines.filter(lambda x: int(x[1][0:4]) >= 1960 and int(x[1][0:4]) <= 2014)

temperatures = lines.map(lambda x: ((x[1][0:10], x[0]), (float(x[3]), float(x[3]))))

temperatures = temperatures.reduceByKey(lambda a, b: (max(a[0], b[0]), min(a[1], b[1])))
print("================================================")
print temperatures.take(10)

temperatures = temperatures.map(lambda x: ((x[0][0][0:7], x[0][1]), sum((float(x[1][0]), float(x[1][1])))))
print("================================================")
print temperatures.take(10)

temperatures = temperatures.groupByKey()
temperatures = temperatures.map(lambda x: (x[0], sum(x[1])/(len(x[1])*2)))
print("================================================")
print temperatures.take(10)

#temperatures = temperatures.groupByKey()
#temperatures = temperatures.map(lambda x: (x[0], float(x[1][0:5])))

#print("================================================")
#print temperatures.take(10)
#temperatures = temperatures.reduceByKey(lambda a, b: a+b)

#temperatures = temperatures.reduceByKey(lambda a, b: (a[0]+a[1], b[0]+b[1]))

# Test

#temperatures = temperatures.map(lambda x: ((x[0][0:10], x[1]), (float(x[2][0])+float(x[2][1]))))

#temperatures = temperatures.reduceByKey(lambda a, b: a+b)

#temperatures.saveAsTextFile("/user/x_matka/Assignment3")

