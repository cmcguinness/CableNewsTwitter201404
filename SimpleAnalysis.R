##
##      Simple Analysis for Cable News Twitter Data
##
##      This is a set of R functions that show how to use the Cable News Twitter data
##
##      You can either call doAnalysis() to have them all run, or call any individual
##      functions as you see fit.
##

##      doAnalysis
##
##      Call all the functions, one after another, or you pick 'em!
doAnalysis <- function () {

        message("Enter one of: ")
        message("1 - Do all analyses")
        message("2 - Top words")
        message("3 - Top hashtags")
        message("4 - Top URLs")
        message("5 - Top users mentioned")
        message("6 - Gender analysis by network")
        message("0 - Why oh why did I run this?")
        choice <- readline("Enter 0-6: ")
        
        if (choice == "0") return(invisible(""))
        
        if (choice == "1" || choice == "2") {
                doList("words.csv","words", stopwords = TRUE)
        }
        
        if (choice == "1" || choice == "3") {
                doList("tags.csv","hashtags")
        }

        if (choice == "1" || choice == "4") {
                doList("urls.csv","urls")
        }
        
        if (choice == "1" || choice == "5") {
                doList("users.csv","users mentioned")
        }
        
        if (choice == "1" || choice == "6") {
                doGenders()
        }
        
        invisible("")
        
        
}

## doGenders    Process the genders CSV file
##
## Since the list of genders is different (there are 3 - Male, Female, and Unknown),
## we don't need to do all the top ten kind of things with it...
doGenders <- function () {
        genders <- read.csv("genders.csv")
        
        message("This is a count of tweets (including RTs) broken down by gender of sender.")
        message("Note that the determination of gender is a guestimate, and a large number")
        message("of tweets have no easy way to determine gender.  Consider this exerimental.")
        message("")
        message("Count.M = Tweets identified as from a male, .F from a female, .U from unknown.")
        message("")
        print(reshape(genders, direction="wide", idvar = "Network", timevar = "Gender"))
        invisible("")
}

## doList:      process a CSV file to show the top 10s...
##
## With the exception of the gender file, all of the CSV files contain rows of the format
##
##      network,item,count
##
## The item is a word, a hashtag, a user, or a URL, depending upon which file it is.
## Regardless, the files can all be processed in the same way.

doList <- function (filename, categoryname, stopwords = FALSE) {
        items <- read.csv(filename)
        
        #       Special magic for words -- we want to remove the stop words
        if (stopwords) {
                source("TwitterstopWords.R")
                items <- items[! isStopWord(items[[2]]),]
        }
        
        #       The data in the CSV file is stored as Network,item,Count
        #       To operate on the data on a per network basis, we first
        #       use the split function to break our one data frame which
        #       has all the data into a list of data frames, one element
        #       per network...
        networkSplit <- split(items, items$Network)
        
        #       Now let's work on the overall list of top items  First, sort...
        items <- items[order(-items$Count),]
        
        #       Then change the row names (as a cosmetic detail)
        row.names(items) <- 1:length(items$Count)
        
        #       And print the first 10 items out...
        message("The top ten ", categoryname, " across all networks:")
        print(head(items,10))
        
        ### message("")
        ### message("The top ten ", categoryname, " by network")
        
        
        #       Now, let's iterate over the list of 
        for (network in names(networkSplit)) {
                message("")
                
                message("Top ten ", categoryname, " for ",network)
                
                networkItems <- networkSplit[[network]];
                networkItems <- networkItems[order(-networkItems$Count),]
                row.names(networkItems) <- 1:length(networkItems$Count)
                print(head(networkItems[,c(2,3)],10))
        }
        
}