/* Steven Raaijmakers (10804242) & Marcus van Bergen (10871993) */
/* The reducer gets (hashtag, sentiment) and outputs a hashtags average
sentiment and its standard deviation */

package nl.uva.cpp;

import java.io.IOException;
import org.apache.hadoop.io.*;
import org.apache.hadoop.mapreduce.*;

/* added: */
import java.lang.Math;

public class WordCountReducer extends Reducer<Text, IntWritable, Text, Text> {

	@Override
	public void reduce(Text key, Iterable<IntWritable> values, Context context)
			throws IOException, InterruptedException {
		/* Smart way to calculate mean and stdev in one for loop
		Src: http://bit.ly/2gyNaa3 */
		double sum1 = 0;
		double sum2 = 0;
		double count = 0;

		for (IntWritable val : values) {
			sum1 += val.get();
			sum2 += val.get() * val.get();
			count++;
		}

		double mean = sum1 / count;
		double var = (count * sum2 - sum1 * sum1) / (count * count);
		double std = Math.sqrt(var);

		/* Output as a string */
		String value = count + "\t" + "\t" + mean + "\t" + std;
		context.write(key, new Text(value));
	}
}
