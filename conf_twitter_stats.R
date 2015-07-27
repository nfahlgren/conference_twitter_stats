library(twitteR)
library(ggplot2)
library(gridExtra)

# Your constants
CONSUMER_KEY = ''
CONSUMER_SECRET = ''
ACCESS_TOKEN = ''
ACESS_SECRET = ''
HASHTAG = ''

# Authenticate with your Twitter API app
setup_twitter_oauth(consumer_key = CONSUMER_KEY,
                    consumer_secret = CONSUMER_SECRET,
                    access_token = ACCESS_TOKEN,
                    access_secret = ACESS_SECRET)


# Search Twitter for your conference hashtag
tweets <- searchTwitter(HASHTAG, n=9999)

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

# Plot both plots
grid.arrange(p1, p2, nrow=1, ncol=2)
