package nl.uva.cpp;

import org.apache.hadoop.conf.*;
import org.apache.hadoop.fs.*;
import org.apache.hadoop.io.*;
import org.apache.hadoop.mapreduce.*;
import org.apache.hadoop.mapreduce.lib.input.*;
import org.apache.hadoop.mapreduce.lib.output.*;
import org.apache.hadoop.util.*;

import org.apache.hadoop.mapreduce.lib.input.NLineInputFormat;

public class WordCountTool extends Configured implements Tool {

	@Override
	public int run(String[] args) throws Exception {
		Configuration conf = this.getConf();
		Job job = Job.getInstance(conf);
		conf.set("mapreduce.input.keyvaluelinerecordreader.key.value.separator", "\n\n");
		job.setJarByClass(this.getClass());

		// Set the input and output paths for the job, to the paths given
		// on the command line.
		FileInputFormat.addInputPath(job, new Path(args[0]));
		FileOutputFormat.setOutputPath(job, new Path(args[1]));

		// Use our mapper and reducer classes.
		job.setMapperClass(WordCountMapper.class);
		job.setReducerClass(WordCountReducer.class);

		// Our input file is a text file.
		job.setInputFormatClass(TextInputFormat.class);
		job.getConfiguration().setInt("mapreduce.input.lineinputformat.linespermap", 4);

		// Our output is a mapping of text to integers. (See the tutorial for
		// some notes about how you could map from text to text instead.)
		job.setOutputKeyClass(Text.class);
		job.setOutputValueClass(IntWritable.class);

		// Limit the number of reduce/map classes to what was specified on
		// the command line.
		int numTasks = Integer.valueOf(args[2]);
		job.setNumReduceTasks(numTasks);
		job.getConfiguration().setInt("mapred.max.split.size", 750000 / numTasks);
		// This limits the number of running mappers, but not the total.
		// job.getConfiguration().setInt("mapreduce.job.running.map.limit", numTasks);

		// Run the job!
		return job.waitForCompletion(true) ? 0 : 1;
	}
}
