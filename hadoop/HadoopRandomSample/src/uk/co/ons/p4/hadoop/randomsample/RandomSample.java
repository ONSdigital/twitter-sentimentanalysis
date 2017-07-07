package uk.co.ons.p4.hadoop.randomsample;

import java.io.IOException;
import java.util.Random;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.NullWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.apache.hadoop.util.GenericOptionsParser;

public class RandomSample {

	public static class SRSMapper extends Mapper<Object, Text, NullWritable, Text> {

		private Random rands = new Random();
		private Double percentage;

		@Override
		protected void setup(Context context) throws IOException, InterruptedException {
			// retrieve the percentage that is passed in via the configuration
			// like this: conf.set("filter_percentage", .5); for .5%
			String strPercentage = context.getConfiguration().get("percentage");
			percentage = Double.parseDouble(strPercentage) / 100.0;
		}

		@Override
		public void map(Object key, Text value, Context context) throws IOException, InterruptedException {
			// there's no need for a reducer so we use the NullWritable
			// All we're doing is reading lines in and if the random number
			// is less than our percentage weight we write the line to the output. 
			if (rands.nextDouble() < percentage) {
				context.write(NullWritable.get(), value);
			}
		}
	}

	public static void main(String[] args) throws Exception {
		Configuration conf = new Configuration();
		String[] otherArgs = new GenericOptionsParser(conf, args).getRemainingArgs();
		if (otherArgs.length != 3) {
			System.err.println("Usage: Randomsample <percentage> <inputdir> <outputdir>");
			System.exit(2);
		}
		conf.set("percentage", otherArgs[0]);

		Job job = new Job(conf, "Hadoop-Random-Sample");
		job.setJarByClass(RandomSample.class);
		job.setMapperClass(SRSMapper.class);
		job.setOutputKeyClass(NullWritable.class);
		job.setOutputValueClass(Text.class);
		job.setNumReduceTasks(0); // Set number of reducers to zero
		FileInputFormat.addInputPath(job, new Path(otherArgs[1]));
		FileOutputFormat.setOutputPath(job, new Path(otherArgs[2]));
		System.exit(job.waitForCompletion(true) ? 0 : 1);
	}
}
