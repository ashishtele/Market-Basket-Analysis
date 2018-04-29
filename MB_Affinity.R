rm(list = ls())

# Loading libraries

load_lb <- function()
{
  library(readxl)
  library(tidyr)
  library(dplyr)
  library(rpart)
  library(tree)
  library(MASS)
  require(data.table)
  require(Matrix)
  library(Amelia)
  library(mice)
  library(readr)
  library(ggplot2)
  library(caret)
  library(lubridate)
  library(plyr)
}

load_lb()

# Loading required libraries
library(arules)
library(arulesViz)

# Importing the file
OnRetail <- read_excel("E:\\Study\\R Projects\\Market Basket/OnlineRetail.xlsx")

# File structure
glimpse(OnRetail)

col_cl <- sapply(OnRetail, function(x) class(x))
col_cl
col_name <- colnames(OnRetail)

percent <- function(x, digits = 1, format = "f", ...) 
{
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

# Function to determine missing and empty values in columns
countMissing <- function(x,y)
{
  ## calculate counts
  if (mode(x) == "character") emptyStrings = sum(x=="", na.rm=TRUE) else emptyStrings = 0
  if (mode(x) == "numeric") missing1 = sum(x=="", na.rm=TRUE) else missing1 = 0
  missing <- sum(is.na(x)) + missing1
  totalRows = NROW(x)
  nonMissing = totalRows - missing - emptyStrings
  
  ## present results
  cat(" #         Column Name: ",y,"\n", sep="")
  cat("#           TOTAL ROWS: ", totalRows, " (", percent(totalRows/NROW(x)), ")\n", sep="")
  cat("# Missing Values (NAs): ", missing, " (", percent(missing/NROW(x)), ")\n", sep="")
  cat("  # Empty Strings (\"\"): ", emptyStrings, " (", percent(emptyStrings/NROW(x)), ")\n", sep="")
  cat("   # Non-missing Value: ", nonMissing, " (", percent(nonMissing/NROW(x)), ")\n", sep="")
  cat("    Mode & Class: ", mode(x), ", ", class(x), "\n", sep="")
}

for(i in 1:length(col_name))
{
  countMissing(OnRetail[[i]],col_name[i])
}

############################# Data preprocessing ########################

OnRetail <- OnRetail[complete.cases(OnRetail),]
OnRetail %>% mutate(Description = as.factor(OnRetail$Description),
                    Country = as.factor(OnRetail$Country),
                    Date = as.Date(OnRetail$InvoiceDate),
                    Time = hour(hms(format(OnRetail$InvoiceDate,"%H:%M:%S"))),
                    InvoiceNo = as.numeric(as.character(OnRetail$InvoiceNo))) -> OnRetail

glimpse(OnRetail)

# Shopping time distribution

OnRetail %>% 
  ggplot(aes(x=Time)) +
  geom_histogram(stat = "count", fill = "indianred")


# no of items bought

OnRetail %>%
  group_by(InvoiceNo) %>%
  summarize(n_items = mean(Quantity)) %>% 
  ggplot(aes(x=n_items)) +
  geom_histogram(fill = "blue", bins = 100000) +
  coord_cartesian(xlim=c(0,80))


# Best sellers

OnRetail %>%
  group_by(StockCode,Description) %>%
  summarize(cnt = n()) %>%
  arrange(desc(cnt)) -> top
top <- head(top,10)  
top

top %>% 
  ggplot(aes(x=reorder(Description,cnt), y=cnt)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip()

## Formatting the data table
sorted_r <- OnRetail[order(OnRetail$CustomerID),]

items <- ddply(sorted_r,c("CustomerID","Date"),
               function(dt) paste(dt$Description,collapse = ","))
head(items,1)
items$CustomerID <- NULL
items$Date <- NULL
colnames(items) <- c("items")

write.csv(items,"E:\\Study\\R Projects\\Market Basket\\transactions.csv",
          row.names = TRUE, quote = FALSE)

tr <- read.transactions("E:\\Study\\R Projects\\Market Basket\\transactions.csv",
                        format = 'basket', sep=',')
summary(tr)
# density : total no. of non emplty cells in matrix
# : tot no. of items purchased/total no. of possible items in the matrix

itemFrequencyPlot(tr, topN = 20)
  
# the default min are high

m1 <- apriori(tr, parameter = list(support=0.001,confidence=0.8))
m1 <- sort(m1,by='confidence', decreasing = TRUE)
summary(m1)

inspect(m1[1:10])
top_p <- m1[1:5]

plot(top_p,method="graph")
