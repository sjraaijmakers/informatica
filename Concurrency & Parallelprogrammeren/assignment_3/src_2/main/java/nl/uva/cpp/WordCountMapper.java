/* Steven Raaijmakers (10804242) & Marcus van Bergen (10871993) */
/* Program uses hadoop to analyse tweets; assigns a certain sentiment to each
hashtag */

package nl.uva.cpp;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import java.io.IOException;
import java.util.StringTokenizer;

import org.apache.hadoop.io.*;
import org.apache.hadoop.mapreduce.*;
import java.lang.String;

/* Added imports: */
import java.text.Normalizer;

import me.champeau.ld.UberLanguageDetector;
import java.util.Properties;
import edu.stanford.nlp.ling.CoreAnnotations;
import edu.stanford.nlp.pipeline.Annotation;
import edu.stanford.nlp.pipeline.StanfordCoreNLP;
import edu.stanford.nlp.neural.rnn.RNNCoreAnnotations;
import edu.stanford.nlp.sentiment.SentimentCoreAnnotations;
import edu.stanford.nlp.trees.Tree;
import edu.stanford.nlp.util.CoreMap;

public class WordCountMapper extends Mapper<LongWritable, Text, Text, IntWritable> {

	final static Pattern HASHTAG = Pattern.compile("(?<=^|(?<=[^a-zA-Z0-9-_\\.]))#([A-Za-z0-9_]+[A-Za-z]+)");
	private final static IntWritable one = new IntWritable(1);
	private Text word = new Text();
	private static StanfordCoreNLP pipeline;

	static enum Counters {
		INPUT_WORDS
	}

	/* Class constructor: loads pipe */
	public WordCountMapper(){
		String parseModelPath = "edu/stanford/nlp/models/lexparser/englishPCFG.ser.gz";
		String sentimentModelPath = "edu/stanford/nlp/models/sentiment/sentiment.ser.gz";

		Properties props = new Properties();
		props.setProperty("annotators", "tokenize, ssplit, parse, sentiment");
		props.put("parse.model", parseModelPath);
		props.put("sentiment.model", sentimentModelPath);
		pipeline = new StanfordCoreNLP(props);
	}

	/* Performs sentiment analysis on text */
	private int findSentiment(String text){
		int mainSentiment = 0;
		if(text != null && text.length () > 0){
			int longest = 0;
			Annotation annotation = pipeline.process(text);
			for(CoreMap sentence : annotation.get(CoreAnnotations.SentencesAnnotation.class)){
				// ’ AnnotatedTree ’ is ’ SentimentAnnotatedTree ’ in newer versions
				Tree tree = sentence.get(SentimentCoreAnnotations.AnnotatedTree.class);
				int sentiment = RNNCoreAnnotations.getPredictedClass(tree);
				String partText = sentence.toString();
				if(partText.length () > longest){
					mainSentiment = sentiment;
					longest = partText.length();
				}
			}
		}
		return mainSentiment ;
	}

	/* Function strips accents of s (xixicocô will be xixicoco)
	src: http://bit.ly/2gG43OE */
	public static String stripAccents(String s){
	    s = Normalizer.normalize(s, Normalizer.Form.NFD);
	    s = s.replaceAll("[\\p{InCombiningDiacriticalMarks}]", "");
	    return s;
	}

	/* Removes: #, @USER, http://link.com */
	private String cleanTweet(String u){
		return u.replaceAll("(@[A-Za-z0-9]+)|([^0-9A-Za-z \\t])|(\\w+:\\/\\/\\S+)", "").replaceAll("\\s+", " ");
	}

	@Override
	public void map(LongWritable key, Text value, Context context) throws IOException, InterruptedException {
		String line = value.toString();

		/* Remove everything except CONTEXT + standardize context: */
		String tweet_context = line.substring(line.indexOf("W")+1);
		tweet_context = stripAccents(tweet_context.toLowerCase());

		/* Match CONTEXT to HASHTAG regex */
		Matcher matcher = HASHTAG.matcher(tweet_context);

		/* If tweet contains any hashtags: perform analysis */
		if(matcher.find()){
			/* Remove mentions and hashtags (to perform better analysis) */
			String clean_context = cleanTweet(tweet_context);

			/* Detect language: */
			String lang = UberLanguageDetector.getInstance().detectLang(clean_context);
			if(lang.equals("en")){
				IntWritable sentiment = new IntWritable(findSentiment(clean_context));

				/* Assign sentiment to all hashtags in current tweet */
				do{
					String token = matcher.group();
					word.set(token);
					context.write(word, sentiment);
					context.getCounter(Counters.INPUT_WORDS).increment(1);
				}
				while(matcher.find());
			}
		}
	}
}
