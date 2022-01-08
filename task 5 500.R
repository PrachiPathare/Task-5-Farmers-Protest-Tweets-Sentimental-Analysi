options(warn = -1)
#loading library
library(tm)
library(SnowballC)
library(textstem)
library(ggplot2)
library(wordcloud)
library(syuzhet)
library(plyr)
library(DataExplorer)
library(tidyverse)


#reading file
tweets = read.csv("tweets.csv", stringsAsFactors = F)

#reading first 500 rows
my_tweets = head(tweets,500)
my_tweets = my_tweets[c(2,3)]
View(my_tweets)


#making corpus
corpus <- iconv(my_tweets$renderedContent)
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

#Cleaning the data

#lowering the upper case letters
corpus = tm_map(corpus, tolower)
inspect(clean_data[1:5])

#removing punctuation
corpus = tm_map(corpus, removePunctuation)
inspect(corpus[1:5])

#removing numbers and whitespaces
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, stripWhitespace)
inspect(corpus[1:5])

#removing URLs
removeURL = function(x) gsub('http[[:alnum:]]*','', x)
corpus = tm_map(corpus, content_transformer(removeURL))
inspect(corpus[1:5])

#removing some other symbol
cleantext = content_transformer(function(x, pattern){return(gsub(pattern, "", x))})
corpus = tm_map(corpus, cleantext, "ðÿ‘‡")
corpus = tm_map(corpus, cleantext, "â€¦")
corpus = tm_map(corpus, cleantext, "à¤¸à¤°")
corpus = tm_map(corpus, cleantext, "à¤•à¤¾à¤°")
corpus = tm_map(corpus, cleantext, "¥")
corpus = tm_map(corpus, cleantext, "‡")
corpus = tm_map(corpus, cleantext, "à¤")
corpus = tm_map(corpus, cleantext, "à¤•")
corpus = tm_map(corpus, cleantext, "•")
corpus = tm_map(corpus, cleantext, "¦")
corpus = tm_map(corpus, cleantext, "à")
corpus = tm_map(corpus, cleantext, "\u008d°–‹²‹")
corpus = tm_map(corpus, cleantext, "‹")
corpus = tm_map(corpus, cleantext, "‚")
corpus = tm_map(corpus, cleantext, "¿")
corpus = tm_map(corpus, cleantext, "€°")
corpus = tm_map(corpus, cleantext, "¹²")
corpus = tm_map(corpus, cleantext, "¾")
corpus = tm_map(corpus,cleantext, "¨")
corpus = tm_map(corpus,cleantext, "—")
corpus = tm_map(corpus,cleantext, "š©ˆ°…ª©\u0081©€œ©€¤°©")
corpus = tm_map(corpus,cleantext, "ðÿœðÿšœ")
corpus = tm_map(corpus,cleantext, "ð")
corpus = tm_map(corpus,cleantext, "ÿ‘‰itâ€™s")
corpus = tm_map(corpus,cleantext, ",")
corpus = tm_map(corpus,cleantext, "ÿ")
corpus = tm_map(corpus,cleantext, "ÿ˜")
corpus = tm_map(corpus,cleantext, "â€œyoâ€\u009d")
corpus = tm_map(corpus,cleantext, "ÿ’")
corpus = tm_map(corpus,cleantext, "©")
corpus = tm_map(corpus,cleantext, "â€“")
corpus = tm_map(corpus,cleantext, "â€™")
corpus = tm_map(corpus,cleantext, "â€”")
corpus = tm_map(corpus,cleantext, "¶")
corpus = tm_map(corpus,cleantext, "µ°")
corpus = tm_map(corpus,cleantext, "§€¸")
corpus = tm_map(corpus,cleantext, "â˜º")
corpus = tm_map(corpus,cleantext, "\u009dª\u009d²\u009d\u009d² \u009d™\u009d®\u009d\u009dº\u009d²\u009d\u009d˜€ \u009d¡\u009d¼\u009d˜\u0081 \u009d§ã©\u009d\u009dã³\u009d\u009d¶\u009d˜€\u009d˜\u0081")
corpus = tm_map(corpus,cleantext, "â€œarguing¸")
corpus = tm_map(corpus,cleantext, "âœš¸")
corpus = tm_map(corpus,cleantext, "ÿ›‘¸")
corpus = tm_map(corpus,cleantext, "®€¸")
corpus = tm_map(corpus,cleantext, "\u009d ¹ˆ¸")
corpus = tm_map(corpus,cleantext, "¹ˆ")
corpus = tm_map(corpus,cleantext, "tã©rrã³ristâœš")
#inspect(corpus[88])
corpus = tm_map(corpus,cleantext," ˜")
corpus = tm_map(corpus,cleantext,"˜˜˜")
corpus = tm_map(corpus,cleantext,"â€")
corpus = tm_map(corpus,cleantext,"›‘")
corpus = tm_map(corpus,cleantext,"âœš")
corpus = tm_map(corpus,cleantext,"tãrrã³rist")
#inspect(corpus[101:200])
corpus = tm_map(corpus,cleantext,"âž¡ï¸")
corpus = tm_map(corpus,cleantext,"\u008f")
corpus = tm_map(corpus,cleantext,"œ\u008f")
corpus = tm_map(corpus,cleantext,"˜s ")
corpus = tm_map(corpus,cleantext,"¤¬")
corpus = tm_map(corpus,cleantext,"³")
corpus = tm_map(corpus,cleantext,"‘†‘†")
corpus = tm_map(corpus,cleantext,"‘†")
corpus = tm_map(corpus,cleantext,"ªœ¬¹°¯£¯ªœ¸")
#corpus = tm_map(corpus,cleantext,"\u008d\u008f")
corpus = tm_map(corpus,cleantext,"®")
corpus = tm_map(corpus,cleantext,"º")
corpus = tm_map(corpus,cleantext,"²")
corpus = tm_map(corpus,cleantext,"¤œ¬")
corpus = tm_map(corpus,cleantext,"™")
corpus = tm_map(corpus,cleantext,"\u008f")
corpus = tm_map(corpus,cleantext,"‘€‘")
corpus = tm_map(corpus,cleantext,"˜¼")
#inspect(corpus[201:300])
corpus = tm_map(corpus,cleantext,"¸§€€")
corpus = tm_map(corpus,cleantext,"˜‘‰")
corpus = tm_map(corpus,cleantext,"¹âš”ï¸§’")
corpus = tm_map(corpus,cleantext,"£")
corpus = tm_map(corpus,cleantext," ¸ ")
corpus = tm_map(corpus,cleantext," „ ")
corpus = tm_map(corpus,cleantext," â– ")
corpus = tm_map(corpus,cleantext,"œ¼‘œ¼")
corpus = tm_map(corpus,cleantext,"œâ\u009d¤ï¸")
corpus = tm_map(corpus,cleantext,"‘‰")
corpus = tm_map(corpus,cleantext,"¼")
corpus = tm_map(corpus,cleantext,"¹")
corpus = tm_map(corpus, cleantext, "¸¸ ")
inspect(corpus[1:5])



#removing stop words
corpus = tm_map(corpus, removeWords, stopwords('english'))
inspect(corpus[1:5])
dictCorpus = corpus

#stemming the document
corpus = tm_map(corpus,stemDocument)

#tokenize the corpus
corpusTokenized = lapply(corpus,scan_tokenizer)
# stem complete each token vector
myTokensStemCompleted = lapply(corpusTokenized, stemCompletion,dictCorpus)
#creating data frame
cleandata = data.frame(text = sapply(myTokensStemCompleted, paste, collapse = " "), stringsAsFactors = F)
View(cleandata)



#creating document term metrix
dtm = DocumentTermMatrix(corpus)
dtm_m = as.matrix(dtm)
View(dtm_m)

#sorting word by decreasing order

freq = colSums(as.matrix(dtm))
freq_df = data.frame(freq)
View(freq_df)


ord = order(freq, decreasing = TRUE)
freq[head(ord)]
freq[tail(ord)]

# finding the terms atleast occuring 50 times in corpus
findFreqTerms(dtm, lowfreq = 50)

#finding correlations (how many times word in given list occur with which word more than 25%)
findAssocs(dtm, terms = c("farmersprotest","stophateagainstfarm","farmer","bjp", "hate", "corpor"), corlimit = 0.25)

#getting histogram
m_f = data.frame(term = names(freq),occurrences = freq)
head(m_f,5)
ggplot(subset(m_f, freq > 50), aes(term, occurrences))+
  geom_bar(stat = "identity") +
  labs(x = 'words', y = 'frquency') +
  guides(scale = "none")

#getting word cloud
set.seed(1234)
wordcloud(names(freq),freq, min.freq = 5, max.words = 100, colors = brewer.pal(8,"Dark2"))
#inspect(corpus[4])



#sentimental analysis whether senti is positive or not
corpus <- iconv(my_tweets$renderedContent)
s = get_sentiment(corpus, method = "syuzhet")
s_df = data.frame(s)  

#overall sentiment
overall_sentiment = colSums(s_df)
overall_sentiment

#classifying the tweet as positive negative or netural
polarity = function(x){
  i = x[1]
  if(i < 0){print("negative")}
  else if(i == 0){print("neutral")}
  else{print("positive")}
}
output = apply(s_df, 1 ,polarity)


#final output
final_output = data.frame(c(my_tweets,cleandata,s_df,output_df))
View(final_output)


