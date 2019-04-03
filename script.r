# Final Project
library("ggplot2")
# Plot #1
fname <- file.choose()
catfname <- file.choose()
trend.data <- read.csv(fname, header = TRUE, stringsAsFactors = FALSE)
category  <- read.csv(catfname, header = TRUE, stringsAsFactors = FALSE)
View(trend.data)
View(category)

# Change the Category ID collected to Category Names
trend.data[["categoryId"]] <- category[match(trend.data[['categoryId']], category[['categoryId']] ) , 'categoryname']
category.pop <- tapply(trend.data$view_count
                           , list(trend.data$categoryId) #change the order
                           , sum)
View(category.pop)

category.pop <- sort(category.pop,decreasing=F)

barplot(category.pop, ylim = c(0,150000000), xlab = "Categories", ylab= "View Count",
        main = "View Count for Each Category in Trending on Youtube"
        ,las=2,  cex.axis=.75, cex.names=.5)

# Plot 2 Likes and DisLikes in each category. Is a video controverisal?
ratio <- tapply(trend.data$likes
                , list(trend.data$categoryId) #change the order
                , sum)

dislike.ratio <- tapply(trend.data$dislikes
                    , list(trend.data$categoryId) #change the order
                    , sum)

par(mfrow=c(1,2))
barplot(ratio, ylim = c(0,9000000), ylab ="Likes Count"
        ,main = "Popular Categories and \n the like ratio", las =2, cex.names =.75)
barplot(dislike.ratio, ylim = c(0,900000), ylab ="Dislikes Count"
        ,main = "Popular Categories and \n the dislike ratio", las =2, cex.names = .75)

# Plot 3 Do youtubers or  sponsored content dominate the trending page?
nontubers <- trend.data$subs < 1000000 # if 1,000,000 subscribers or more they are sponsored

youtubers <- length(nontubers[nontubers == F])
notsponsored <- length(nontubers[nontubers == T])

slices <- c(youtubers, notsponsored)
labels <- c("'Large channels", "Smaller Channels")
pie(slices, labels = labels, main = "What types of channels dominate the Trending Page")


# Plot 4 Heat Map of the World And concentrated Viewership

# Plot 5 viewership over a week which categories perform best