from pyspark import SparkConf,SparkContext
import sys
import itertools, functools

# At first are the configuration settings so we can run it through the terminal
# then we set up configuration for the PySpark job
if len(sys.argv) != 3:
    print('Usage: ' + sys.argv[0] + ' <in> <out>') # [0] will be the .py file
    sys.exit(1)
inputlocation = sys.argv[1] # [1] will be the input file(i.e. the txt files here)
outputlocation = sys.argv[2] # [2] where we save the output of the program

# Set up the configuration and job context
conf = SparkConf().setAppName('Triangles') 
sc = SparkContext(conf = conf) 

# Read Files ----------------------------------------------------------------------------------------------------
data = sc.textFile(inputlocation)

# Split line, convert to ints, format as (key,value) pairs where node n is the key, and its friends are the values
data = data.map(lambda line: [num for num in line.split()]).map(lambda numList: [int(num) for num in numList]).map(lambda numList: (numList[0], numList[1:]))

# Get all combinations of pairs of friends for each key. (n choose 2)
combs = data.map(lambda w: [w[0], list(itertools.combinations(w[1], 2))])

# Create all possible triangles
triangles = combs.flatMapValues(lambda x: x).map(lambda w: (w[0], w[1][0], w[1][1]))

# Sort each tuple
triangles = triangles.map(lambda w: tuple(sorted(w)))

# Set up to be used as dictionary and count by key/ filter out the "fake" triangles that only show up once
triangles = triangles.map(lambda w: (w, 0))
triangles = triangles.countByKey()
triangles = list(filter(lambda x: x[1] > 1, triangles.items()))

# Convert list to RDD again in order to sort output and convert data to string
triangles = sc.parallelize(triangles)
data_final = triangles.map(lambda w: w[0]).collect()
data_final = sorted(data_final)

# Convert to space-separated
data_final = sc.parallelize(data_final)
data_final = data_final.map(str).map(lambda w: w.strip('(),'))
data_final = data_final.map(lambda w: w.replace(',', ''))

# Save Output ----------------------------------------------------------------------------------------------------
data_final.saveAsTextFile(outputlocation)
sc.stop() # Let Spark know that the job is done.