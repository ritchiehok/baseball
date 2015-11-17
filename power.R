power <- function(years, players = 50) {
    ## Data from http://www.seanlahman.com/baseball-archive/statistics/
    rawdata <- read.csv("batting.csv")
    data <- rawdata[,c("playerID", "yearID", "stint", "X2B", "HR")]
    
    ## Subsets rows that match the given years 
    data <- data[data$yearID %in% years, ]
    
    ## Aggregates doubles and homeruns then places them in a data frame
    doubles <- aggregate(X2B ~ playerID, data, sum)
    homeruns <- aggregate(HR ~ playerID, data, sum)
    power <- data.frame(doubles$playerID, doubles$X2B, homeruns$HR)
    colnames(power) <- c("playerID", "2B", "HR")
    
    ## Sorts players from most doubles to least
    ordpower <- power[order(power[,"2B"], decreasing = TRUE),]
    head(ordpower, players)
}

## ratio of doubles, ratio of homeruns
## subsetting player data
## data[data$playerID == "name",]
## peak_power