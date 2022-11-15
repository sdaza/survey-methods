
# Students' survey
# Let's explore your answers!


# load libraries
library(data.table)
library(ggplot2)
library(wordcloud)
library(tm)
library(RColorBrewer)

# Functions

create_wordcloud = function(text, my_stopwords=NULL, max_words=200) {

    text = Corpus(VectorSource(text))
    text = suppressWarnings(tm_map(text, removePunctuation))
    text = suppressWarnings(tm_map(text, stripWhitespace))
    text = suppressWarnings(tm_map(text, content_transformer(tolower)))
    text = suppressWarnings(tm_map(text, removeWords, stopwords("english")))
    text = suppressWarnings(tm_map(text, removeWords, my_stopwords))

    dtm = TermDocumentMatrix(text) 
    matrix = as.matrix(dtm) 
    words = sort(rowSums(matrix),decreasing=TRUE) 
    df = data.frame(word = names(words),freq=words)

    set.seed(1234)
    suppressWarnings(
        wordcloud(words = df$word, freq = df$freq, min.freq = 1, 
        max.words=max_words, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
    )
}

# read data

dat = fread("../data/fstudents.csv")
names(dat) = c('time', 'interest', 'years_design', 
    'years_analysis', 'power', 'languages', 'working', 'position', 'eposition')
print(paste('number of rows:', nrow(dat)))

str(dat)

# Experience / knowledge

prop.table(table(dat$years_analysis))

prop.table(table(dat$years_design))

print(round(mean(dat$power), 3))

print(round(mean(sample(0:5, 10000, replace=TRUE)), 3))

prop.table(table(dat$working))

# Wordclouds

create_wordcloud(dat$interest, c('survey', 'surveys', 'data', 'stop'))

# set figure size using options in R
create_wordcloud(dat$eposition, c('one', 'years', 'like')
