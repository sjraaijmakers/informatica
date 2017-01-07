/* Steven Raaijmakers (10804242) & Marcus van Bergen (10871993) */
/* Program uses hadoop to count hashtags in tweets */

package nl.uva.cpp;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import java.io.IOException;
import java.util.StringTokenizer;

import org.apache.hadoop.io.*;
import org.apache.hadoop.mapreduce.*;

/* added: */
import java.lang.String;
import java.text.Normalizer;


public class WordCountMapper extends Mapper<LongWritable, Text, Text, IntWritable> {

	final static Pattern HASHTAG = Pattern.compile("(?<=^|(?<=[^a-zA-Z0-9-_\\.]))#([A-Za-z0-9_]+[A-Za-z]+)");
	private final static IntWritable one = new IntWritable(1);
	private Text word = new Text();

	static enum Counters {
		INPUT_WORDS
	}

	/* Function strips accents of s (xixicocô will be xixicoco)
	src: http://bit.ly/2gG43OE */
	public static String stripAccents(String s){
		s = Normalizer.normalize(s, Normalizer.Form.NFD);
		s = s.replaceAll("[\\p{InCombiningDiacriticalMarks}]", "");
		return s;
	}

	@Override
	public void map(LongWritable key, Text value, Context context) throws IOException, InterruptedException {
		String line = value.toString();

		// Remove everything except tweet context:
		String tweet_context = line.substring(line.indexOf("W")+1);

		// Match context to hashtag
		Matcher matcher = HASHTAG.matcher(stripAccents(tweet_context.toLowerCase()));

		// Find hashtags
		int count = 0;
		while(matcher.find()){
			String token = matcher.group();

			// Send to reducer
			word.set(token);
			context.write(word, one);
			context.getCounter(Counters.INPUT_WORDS).increment(1);
		}
	}
}
