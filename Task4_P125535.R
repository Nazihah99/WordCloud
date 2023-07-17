library(wordcloud)
library(wordcloud2)
library(tm)
library(RColorBrewer)
library(ggplot2)
library(stringr)

###THANK YOU, NEXT by Ariana Grande###
###Theme: Breakup

#1. Import and transform data text as corpus
text=readLines("Thank.csv" )
docs=Corpus(VectorSource(text))
inspect(docs) 

#2. Data Cleaning/Preprocessing
toSpace=content_transformer(function(x, pattern) gsub(pattern, "",x))
#Convert the text to lower case 
docs=tm_map(docs,content_transformer(tolower)) 
#Correct the short form
docs=tm_map(docs, content_transformer(gsub), pattern = "i'd", replacement = "i would")
docs=tm_map(docs, content_transformer(gsub), pattern = "wasn't", replacement = "was not")
docs=tm_map(docs, content_transformer(gsub), pattern = "i'm", replacement = "i am")
docs=tm_map(docs, content_transformer(gsub), pattern = "cause", replacement = "because")
docs=tm_map(docs, content_transformer(gsub), pattern = "i've", replacement = "i have")
docs=tm_map(docs, content_transformer(gsub), pattern = "that's", replacement = "that is")
docs=tm_map(docs, content_transformer(gsub), pattern = "ain't", replacement = "am not")
docs=tm_map(docs, content_transformer(gsub), pattern = "'bout", replacement = "about")
docs=tm_map(docs, content_transformer(gsub), pattern = "nothin", replacement = "nothing")
docs=tm_map(docs, content_transformer(gsub), pattern = "we're", replacement = "we are")
docs=tm_map(docs, content_transformer(gsub), pattern = "havin", replacement = "having")
docs=tm_map(docs, content_transformer(gsub), pattern = "gon", replacement = "going to")
docs=tm_map(docs, content_transformer(gsub), pattern = "she's", replacement = "she is")
docs=tm_map(docs, content_transformer(gsub), pattern = "i'll", replacement = "i will")
docs=tm_map(docs, content_transformer(gsub), pattern = "wanna", replacement = "want to")
inspect(docs)
#Eliminate stopwords
docs2=tm_map(docs,removeWords, stopwords("english"))
#Eliminate punctuation
docs2=tm_map(docs2,removePunctuation) 
#Eliminate extra white space
docs2=tm_map(docs2, stripWhitespace) 
inspect(docs2)
#Lemmatization 
library(textstem)
docs2=tm_map(docs2,lemmatize_strings)
inspect(docs2)


#3. Document term matrix and frequency
dtm=DocumentTermMatrix(docs2)
inspect(dtm)
freq=colSums(as.matrix(dtm))
#The number of words involved excluding the stopwords
length(freq) 
ord=order(freq,decreasing=T)
#Top 5 terms with the highest frequency
freq[head(ord,5)]
#Tabulate the frequency
wf=data.frame(names(freq),freq)
names(wf)=c("word","freq")
rownames(wf)=NULL
wf=wf[order(-freq),]
wf
sum(wf$freq)
head(wf,10)
#Plot
Subs=head(wf,10)
ggplot(Subs,aes(x=reorder(word,-freq),y=freq,fill=word))+geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=45,hjust=1))+ggtitle("The Most Words in Thank You, Next")+
  xlab("Word")+ylab("Frequency")

#4. Word Cloud
wordcloud2(wf,size=0.8,shape="star",fontWeight = 'normal',backgroundColor = "white")



###BELIEVER by Imagine Dragons###
###Theme: Pain

#Import and transform data text as corpus
text=readLines("Believer.csv") 
docs=Corpus(VectorSource(text))
inspect(docs) 

#1. Data Cleaning/Preprocessing
toSpace=content_transformer(function(x, pattern) gsub(pattern, "",x))
#Convert the text to lower case 
docs=tm_map(docs,content_transformer(tolower)) #convert the text to lower case
#Correct the short form
docs=tm_map(docs, content_transformer(gsub), pattern = "i'ma", replacement = "i am going to")
docs=tm_map(docs, content_transformer(gsub), pattern = "i'm", replacement = "i am")
docs=tm_map(docs, content_transformer(gsub), pattern = "don't", replacement = "do not")
docs=tm_map(docs, content_transformer(gsub), pattern = "it's", replacement = "it is")
docs=tm_map(docs, content_transformer(gsub), pattern = "gon'", replacement = "going to")
docs=tm_map(docs, content_transformer(gsub), pattern = "imaginin'", replacement = "imagining")
docs=tm_map(docs, content_transformer(gsub), pattern = "what's", replacement = "what is")
docs=tm_map(docs, content_transformer(gsub), pattern = "happenin'", replacement = "happening")
docs=tm_map(docs, content_transformer(gsub), pattern = "can't", replacement = "cannot")
docs=tm_map(docs, content_transformer(gsub), pattern = "let's", replacement = "let us")
docs=tm_map(docs, content_transformer(gsub), pattern = "bloomin'", replacement = "blooming")
docs=tm_map(docs, content_transformer(gsub), pattern = "losin'", replacement = "losing")
docs=tm_map(docs, content_transformer(gsub), pattern = "you're", replacement = "you are")
docs=tm_map(docs, content_transformer(gsub), pattern = "'til", replacement = "until")
docs=tm_map(docs, content_transformer(gsub), pattern = "hol'", replacement = "hold")
inspect(docs)
#Eliminate stopwords
docs2=tm_map(docs,removeWords, stopwords("english"))
#Eliminate punctuation
docs2=tm_map(docs2,removePunctuation) 
#Eliminate extra white space
docs2=tm_map(docs2, stripWhitespace) 
inspect(docs2) 
#Lemmatization 
library(textstem)
docs2=tm_map(docs2,lemmatize_strings)
inspect(docs2)
#docs2=tm_map(docs2, content_transformer(gsub), pattern = "believer", replacement = "believe")
#docs2=tm_map(docs2, content_transformer(gsub), pattern = "breather", replacement = "breath")
#docs2=tm_map(docs2, content_transformer(gsub), pattern = "leader", replacement = "lead")

#2. Document term matrix and frequency
dtm=DocumentTermMatrix(docs2)
inspect(dtm)
freq=colSums(as.matrix(dtm))
#The number of words involved excluding the stopwords
length(freq) 
ord=order(freq,decreasing=T)
#Top 5 terms with the highest frequency
freq[head(ord,5)]
#Tabulate the frequency
wf=data.frame(names(freq),freq)
names(wf)=c("word","freq")
rownames(wf)=NULL
wf=wf[order(-freq),]
wf
sum(wf$freq)
head(wf,10)

#Plot
Subs=head(wf,10)
ggplot(Subs,aes(x=reorder(word,-freq),y=freq,fill=word))+geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=45,hjust=1))+ggtitle("The Most Words in Believer")+
  xlab("Word")+ylab("Frequency")

#3. Word Cloud
wordcloud2(wf,size=0.7,shape="circle",fontWeight = 'normal',backgroundColor = "white")



###STRONGER(WHAT DOESN'T KILL YOU) by Kelly Clarkson###
###Theme: Inspiration

#Import and transform data text as corpus
text=readLines("Stronger.csv") 
docs=Corpus(VectorSource(text))
inspect(docs) 

#1. Data Cleaning/Preprocessing
toSpace=content_transformer(function(x, pattern) gsub(pattern, "",x))
#Convert the text to lower case 
docs=tm_map(docs,content_transformer(tolower)) #convert the text to lower case
#Correct the short form
docs=tm_map(docs, content_transformer(gsub), pattern = "don't", replacement = "do not")
docs=tm_map(docs, content_transformer(gsub), pattern = "you've", replacement = "you have")
docs=tm_map(docs, content_transformer(gsub), pattern = "i'll", replacement = "i will")
docs=tm_map(docs, content_transformer(gsub), pattern = "don't", replacement = "do not")
docs=tm_map(docs, content_transformer(gsub), pattern = "'cause", replacement = "because")
docs=tm_map(docs, content_transformer(gsub), pattern = "you're", replacement = "you are")
docs=tm_map(docs, content_transformer(gsub), pattern = "doesn't", replacement = "does not")
docs=tm_map(docs, content_transformer(gsub), pattern = "i'm", replacement = "i am")
docs=tm_map(docs, content_transformer(gsub), pattern = "didn't", replacement = "did not")
docs=tm_map(docs, content_transformer(gsub), pattern = "i'd", replacement = "i would")
docs=tm_map(docs, content_transformer(gsub), pattern = "thinkin'", replacement = "thinking")
docs=tm_map(docs, content_transformer(gsub), pattern = "'bout", replacement = "about")
#Eliminate stopwords
docs2=tm_map(docs,removeWords, stopwords("english"))
#Eliminate punctuation
docs2=tm_map(docs2,removePunctuation) 
#Eliminate extra white space
docs2=tm_map(docs2, stripWhitespace) 
inspect(docs2) 
#Lemmanization
library(textstem)
docs2=tm_map(docs2,lemmatize_strings)
inspect(docs2)

#2. Document term matrix and frequency
dtm=DocumentTermMatrix(docs2)
inspect(dtm)
freq=colSums(as.matrix(dtm))
#The number of words involved excluding the stopwords
length(freq) 
ord=order(freq,decreasing=T)
#Top 5 terms with the highest frequency
freq[head(ord,5)]
#Tabulate the frequency
wf=data.frame(names(freq),freq)
names(wf)=c("word","freq")
rownames(wf)=NULL
wf=wf[order(-freq),]
wf
sum(wf$freq)
head(wf,10)
#Plot
Subs=head(wf,10)
ggplot(Subs,aes(x=reorder(word,-freq),y=freq,fill=word))+geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=45,hjust=1))+ggtitle("The Most Words in Stronger (What Doesn't Kill You)")+
  xlab("Word")+ylab("Frequency")

#3. Word Cloud
figpath="kill.png"
wordcloud2(wf,figPath = figpath,size=0.5,brewer.pal(5,"Dark2"))

