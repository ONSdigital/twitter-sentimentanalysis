#!/bin/bash

export HADOOP_HOME=/usr/local/hadoop-2.8.0
export HADOOP_LIBS=$HADOOP_HOME/share/hadoop/tools/lib
INPUT_PATH=/Users/jasonbell/Desktop/hadoopin
OUTPUT_PATH=/Users/jasonbell/Desktop/hadoopout
RHADOOP_HOME=/Users/jasonbell/work/ons/twitter-sentimentanalysis/hadoop/RHadoop



$HADOOP_HOME/bin/hadoop jar $HADOOP_LIBS/hadoop-streaming-2.8.0.jar \
-mapper $RHADOOP_HOME/RHadoopMap.r \
-reducer $RHADOOP_HOME/RHadoopReduce.r \
-input $INPUT_PATH -output $OUTPUT_PATH 
