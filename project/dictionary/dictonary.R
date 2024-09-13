
setwd("/website/project/dictionary")

# The package "Tidyverse" includes "stringr" which is an essential package with functions that handle the most common string manipulation
library(tidyverse)

# I work with YouTube dataset (yt_text.csv)

yt <-read.csv(file="yt_text.csv", sep="\t", colClasses=c("channel_url"="character", "channel_name"="character", 
                                                         "video_link"="character", "text"="character"))

# Make the text lower case 

yt <- yt %>% mutate(text2=tolower(text))
head(yt$text, 10)
head(yt$text2, 10)

## Extracting URLs such as "https://www."

pattern1= "\\b(?:https?|ftp):\\/\\/\\S+\\b" 

yt_pattern1 <-yt %>% mutate(urls1=str_extract_all(text2, pattern1)) %>% unnest(urls1) 

#str_detect # True or False 
#str_extract # NA or the value 
#str_extract_all # extracting all values 
#str_count # count the number of times the value appears 

# use the urltools package to deal with extracting more url related patterns
#install.packages("urltoos")
#library(urltools)
#yt_pattern1 <- yt_pattern1 %>% mutate(domain_name=domain(urls1))

##  remove special characters (non-alphabetic characters) such as *
pattern2 <-"[^[:alnum:]]"
yt <-yt %>% mutate(text3=str_replace_all(text2, pattern2, " "))
head(yt$text2, 10)
head(yt$text3, 10)

# create a variable that indicates whether the pattern is present
yt <-yt %>% mutate(affiliate=str_detect(text2, "affiliate"))
yt <-yt %>% mutate(sponsor=str_detect(text2, "sponsor"))

# To find an exact match, wrap the keyword with "\\b" and "\\b"
# place \\b in the beginning as well as in end of the string 
yt <- yt %>% mutate(sponsor2=str_detect(text2, "\\bsponsored\\b")) # To find an exact match, wrap the keyword with "\\b" and "\\b"

# Count the number of times words appear
yt <- yt %>% mutate(affiliate=str_count(text2, "affiliate"))
yt <- yt %>% mutate(sponsor=str_count(text2, "sponsor"))  #this will capture "sponsorship" and "sponsored"
yt <- yt %>% mutate(sponsored=str_count(text2, "\\bsponsored\\b")) # this will find "sponsored" only 

# You can filter only rows that contain "sponsor"
yt_sponsor <- yt %>% filter(sponsor==TRUE)

# You can filter only rows that mentioned "affiliate" more than once 
yt_affiliate <- yt %>% filter(affiliate >=1)

#What if you have a list of words for pattern matching
words <-c("affiliate", "affiliation", "sponsor", "disclaimer")
yt <- yt %>% mutate(my_count=rowSums(sapply(words, function(x) str_count(yt$text2, x))))


###############################################################################################
########################### Practice #########################################################
##############################################################################################

# import the datasets
pub <- read.csv(file="prolific_open_dat_2023.csv", sep="\t")

# lowercase 
pub$high_quality_news <- tolower(pub$high_quality_news)

# remove special characters
pub <-pub %>% mutate(high_quality_news=str_replace_all(high_quality_news, "[^[:alnum:]]", " "))
pub <-pub %>% filter(high_quality_news!="")

#pub <- pub %>% mutate(ads=str_detect(high_quality_news, "\\bads\\b"))
#pub2 <- pub %>% filter(ads==T)

# Create a dictionary 
bias <- c("bias", "sides", "balance", "opinion", "objective", "political", "partisan", "fair", "neutral")

# count the public data 
pub <- pub %>% mutate(bias_count=rowSums(sapply(bias, function(x) str_count(pub$high_quality_news, x))))

# what percentages of responses contain at least one bias related keywords 
pub <- pub %>% mutate(bias_mention=ifelse(bias_count>0, "Yes", "No")) #table(pub$bias_mention)

##########################################################################################
########################## Practice #######################################################

threat <- read.csv(file="threat.txt", header=T)
