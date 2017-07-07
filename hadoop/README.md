# Hadoop Random Sample

This is a very basic Hadoop job to take large text data files (text, csv or anything with single line entries) and create a random sample for testing. 

# Development Setup

The assumption is that Hadoop is installed locally for development, the project is built in Eclipse but a jar file has been saved in the `dist` folder should you wish to run it without looking at the code. 

Your build properties will need access to the following jar files within the Hadoop distribution:

```
hadoop-common-2.x.0.jar
hadoop-mapreduce-client-core-2.x.0.jar
commons-cli-1.2.jar
```

These are found in the $HADOOP_HOME/share/hadoop/common and the $HADOOP_HOME/share/hadoop/mapreduce/ folders. 

# Running The Job on Hadoop

You can run the job locally from the command line. The .5 is the percentage of the sample you wish to keep from the large data file.

```
$HADOOP_HOME/bin/hadoop jar /path/to/hadooprandomsample.jar uk.co.ons.p4.hadoop.randomsample.RandomSample .5 /path/to/inputdata /path/to/outputdata
```
