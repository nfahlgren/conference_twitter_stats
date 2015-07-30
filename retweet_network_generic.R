library(twitteR)
library(ggplot2)
library(gridExtra)
library(plyr)
library(tm)
library(tm.plugin.webmining)
library(wordcloud)
library(igraph)

# Your constants
CONSUMER_KEY = 'Your Consumer Key'
CONSUMER_SECRET = 'Your Consumer Secret Key'
ACCESS_TOKEN = 'Your Access Token'
ACCESS_SECRET = 'Your Access Secret'
HASHTAG = '#plantbiology15'

# Authenticate with your Twitter API app
setup_twitter_oauth(consumer_key = CONSUMER_KEY,
                    consumer_secret = CONSUMER_SECRET,
                    access_token = ACCESS_TOKEN,
                    access_secret = ACCESS_SECRET)

### Retweet Network

# Search Twitter for your conference hashtag
tweets <- searchTwitter(HASHTAG, n=9999)

# Convert the list to a data frame
tweets <- twListToDF(tweets)
retweets <-subset(tweets,isRetweet==TRUE)
retweets.tweet<-retweets$text
retweets.user<-retweets$screenName
user.retweets<-data.frame(retweets.user,retweets.tweet)
names(user.retweets) <- c("User", "text")
user.retweets <- user.retweets[with(user.tweets, order(-Tweets)), ]

#get out who was retweeted
retweeters<-as.character(user.retweets$text)
retweeters_split<-sapply(strsplit(retweeters,split=":"),"[",1)
retweeters_split1<-as.character(retweeters_split)
retweeters_split2<-sapply(strsplit(retweeters_split1,split=" "),"[",2)
user.retweets$retweeted<-NA
user.retweets$retweeted<-substring(retweeters_split2,2)

#alphabetize
retweet.matrix<-data.frame(user.retweets$User,user.retweets$retweeted)
names(retweet.matrix) <- c("retweeter", "tweeter")
retweet.alpha <- retweet.matrix[order(retweet.matrix$retweeter),]
retweet.alpha1 <- retweet.alpha[order(retweet.alpha$tweeter),]

#make a weighted adjacency table
weights<-table(as.character(interaction(retweet.alpha1)))
weight1<-data.frame(weights) 
splitpairs1<-data.frame(do.call('rbind', strsplit(as.character(weight1$Var1),'.',fixed=TRUE)))

#split the names to make a clean adjacency table
freq_table<-data.frame(splitpairs1$X1,splitpairs1$X2,weight1$Freq)
names(freq_table) <- c("retweeter", "tweeter","weight")
freq_matrix=as.matrix(freq_table)
g=graph.edgelist(freq_matrix[,1:2])
E(g)$weight=as.numeric(freq_matrix[,3])
adj=get.adjacency(g,attr='weight',sparse=FALSE) 

#graph
twitter_net <- graph.adjacency(adj, mode = "undirected")
plot.igraph(twitter_net, vertex.size=3, vertex.label.family="sans",
            vertex.color="deepskyblue", vertex.label.cex=0.4, 
            vertex.label.dist=0.25, vertex.label.color="black",
            edge.color="deepskyblue")

