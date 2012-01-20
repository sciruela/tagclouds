library(twitteR)
library(Snowball)

cities<-c("vienna","toronto","paris","new york","london","tokyo","berlin","copenhagen","hong kong","barcelona")



for (city in cities){
	rdmTweets <- searchTwitter(city, n=1500)
	n <- length(rdmTweets)
	rdmTweets[1:5]

	df <- do.call("rbind", lapply(rdmTweets, as.data.frame))
	dim(df)

	library(tm)

	myCorpus <- Corpus(VectorSource(df$text))

	myCorpus <- tm_map(myCorpus, tolower)

	myCorpus <- tm_map(myCorpus, removePunctuation)

	myCorpus <- tm_map(myCorpus, removeNumbers)

	myStopwords <- c(stopwords('spanish'), stopwords('english'),"available", "via")
	idx <- which(myStopwords == "r")

	myStopwords <- myStopwords[-idx]
	myCorpus <- tm_map(myCorpus, removeWords, myStopwords)

	dictCorpus <- myCorpus

	myCorpus <- tm_map(myCorpus, stemDocument)

	inspect(myCorpus[1:3])


	myCorpus <- tm_map(myCorpus, stemCompletion, dictionary=dictCorpus)

	inspect(myCorpus[1:3])

	myDtm <- TermDocumentMatrix(myCorpus, control = list(minWordLength = 1))
	inspect(myDtm[266:270,31:40])

	findFreqTerms(myDtm, lowfreq=10)


	findAssocs(myDtm, 'smart', 0.30)


 
	findAssocs(myDtm, 'green', 0.30)

	library(wordcloud)
	require(RColorBrewer)
	m <- as.matrix(myDtm)

	v <- sort(rowSums(m), decreasing=TRUE)
	myNames <- names(v)
	k <- which(names(v)=="turistas")
	myNames[k] <- "turismo"
	d <- data.frame(word=myNames, freq=v)
	pal2 <- brewer.pal(8,"Set2")
	pdf(paste("D:/tagclouds/",city,".pdf"))
	wordcloud(d$word, d$freq, min.freq=5, max.words=Inf, random.order=F, rot.per=.3, colors=pal2)
	dev.off()
}
