library(twitteR)
library(ggplot2)
library(gridExtra)
library(plyr)
library(tm)
library(tm.plugin.webmining)
library(wordcloud)

# Your constants
CONSUMER_KEY = ''
CONSUMER_SECRET = ''
ACCESS_TOKEN = ''
ACCESS_SECRET = ''
HASHTAG = ''

# Authenticate with your Twitter API app
setup_twitter_oauth(consumer_key = CONSUMER_KEY,
                    consumer_secret = CONSUMER_SECRET,
                    access_token = ACCESS_TOKEN,
                    access_secret = ACCESS_SECRET)


# Search Twitter for your conference hashtag
tweets <- searchTwitter(HASHTAG, n=9999)
tweets_wc = tweets

# Convert the list to a data frame
tweets <- twListToDF(tweets)
tweets <- unique(tweets)

# Make a table of the number of tweets per user
user.tweets <- as.data.frame(table(tweets$screenName))
names(user.tweets) <- c("User", "Tweets")

# Order the table by number of tweets per user
user.tweets <- user.tweets[with(user.tweets, order(-Tweets)), ]

# Plot tweets per user for users with x or more tweets
p1 <- ggplot(data=user.tweets[user.tweets$Tweets >= 2, ], aes(x=reorder(User, Tweets), y=Tweets)) +
             geom_bar(stat='identity') +
             coord_flip() +
             scale_y_continuous("Tweets") +
             scale_x_discrete("User") +
             labs(title = paste(HASHTAG, " tweets per user")) +
             theme_bw() +
             theme(axis.title = element_text(face="bold"), axis.text.y = element_text(size=6))

# Plot tweets per day
p2 <- ggplot(data=tweets, aes(x=created)) +
             geom_bar(binwidth=60*60*24) +
             scale_x_datetime("Date-time") +
             scale_y_continuous("Tweets") +
             labs(title=paste(HASHTAG, " tweets per day")) +
             theme_bw() +
             theme(axis.title = element_text(face="bold"))

# Plot number of tweeters
tweets$dom = strftime(tweets$created, format="%d")
tweeters = ddply(tweets, ~dom, summarise, count=length(unique(screenName)))
p3 <- ggplot(data=tweeters, aes(x=dom)) + 
             geom_histogram(aes(weight=count)) +
             scale_x_discrete("Day of month") +
             scale_y_continuous("Tweeters") +
             labs(title=paste(HASHTAG, " tweeters per day")) +
             theme_bw() +
             theme(axis.title = element_text(face="bold"))

# Plot both plots
grid.arrange(p1, p2, nrow=1, ncol=2)


## Tweet wordcloud
# get the text from tweets
tweet_txt = sapply(tweets_wc, function(x) x$getText())
# remove retweet entities
tweet_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweet_txt)
# remove at people
tweet_txt = gsub("@\\w+", "", tweet_txt)
# remove punctuation
# tweet_txt = gsub("[[:punct:]]", "", tweet_txt)
# remove html links
tweet_txt = gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", tweet_txt)

# remove unnecessary spaces
#tweet_txt = gsub("[ \t]{2,}", "", tweet_txt)
#tweet_txt = gsub("^\\s+|\\s+$", "", tweet_txt)

tweet_corpus = Corpus(VectorSource(tweet_txt))

#inspect(tweet_corpus[1:15])
tdm = TermDocumentMatrix(tweet_corpus,
                         control = list(wordLengths=c(5, Inf), removePunctuation = TRUE,
                                        stopwords = c("through","thenew10","between","should","really","agree","around","looking","the","I","theyre","using","heres","about","their","english"),
                                        tolower = TRUE))

m = as.matrix(tdm)
word_freqs = sort(rowSums(m), decreasing=TRUE)
dm = data.frame(word=names(word_freqs), freq=word_freqs)
#freq[2:100] is used b/c the #1 frequency item is SO frequent it messes up the scale
wordcloud(dm$word, dm$freq[2:100], random.order=FALSE, colors=brewer.pal(8, "Dark2"))

