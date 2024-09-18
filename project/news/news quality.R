
# The package "Tidyverse" includes "stringr" which is an essential package with functions that handle the most common string manipulation
library(tidyverse)
library(dplyr)
library(stringr)

########################
#######clean data#######
########################
# read the file as lines

file_path <- "~/Desktop/dictionary/prolific_open_dat_2023.csv"
lines <- readLines(file_path)

# separate each line by the delimiter (assuming tab-separated values here)
#  adjust the 'sep' argument to match your delimiter (e.g., "\t" for tab)
cleaned_data <- strsplit(lines, "\t")

#determine the maximum number of fields per line to ensure consistency
max_length <- max(sapply(cleaned_data, length))

# ensure all rows have the same number of columns by padding shorter rows with NA
cleaned_data <- lapply(cleaned_data, function(x) {
  length(x) <- max_length
  return(x)
})

#convert the list back into a data frame  
cleaned_data_df <- as.data.frame(do.call(rbind, cleaned_data), stringsAsFactors = FALSE)

# check how many columns the cleaned data has
num_columns <- ncol(cleaned_data_df)

# adjust the number of column names to match the actual number of columns
# add more column names if necessary
expected_columns <- c("PROLIFIC_PID", "high_quality_news", "low_quality_news", "gender", 
                      "political_leaning", "party", "education", "hispanic", "race", 
                      "income", "born", "age")

# make sure the number of column names matches the number of columns
if (length(expected_columns) < num_columns) {
  additional_columns <- paste0("column", seq(length(expected_columns) + 1, num_columns))
  colnames(cleaned_data_df) <- c(expected_columns, additional_columns)
} else {
  colnames(cleaned_data_df) <- expected_columns[1:num_columns]
}

# remove the first n rows (for example, the first 1 row)
pub <- cleaned_data_df[-c(1), ]  # Modify the "1" to the number of rows you want to remove

# view the cleaned data
head(pub)

# save the csv
write.csv(pub, "clean_news.csv", row.names = FALSE)

###############################
####### data processing #######
###############################
# load necessary libraries
library(dplyr)
library(stringr)
library(ggplot2)
###############################
###### high quality news ######
###############################

# define the dictionary for high-quality news characteristics
high_quality_news_dict <- list(
  accuracy = c("accurate", "factual", "correct", "true", "reliable", "verifiable", "precise"),
  objectivity = c("objective", "impartial", "neutral", "unbiased", "balanced", "fair", "non-partisan"),
  clarity = c("clear", "concise", "understandable", "straightforward", "coherent", "simple", "readable"),
  thoroughness = c("thorough", "detailed", "comprehensive", "in-depth", "complete", "exhaustive"),
  transparency = c("transparent", "open", "accountable", "honest", "disclosure", "candid"),
  source_credibility = c("credible", "reliable sources", "verified", "sourced", "referenced", "cited", "attributed")
)

# create a function to count keyword occurrences for each category
count_keywords <- function(text_column, keywords) {
  rowSums(sapply(keywords, function(keyword) str_count(text_column, keyword)))
}

# convert all text in 'high_quality_news' to lowercase
pub$high_quality_news <- tolower(pub$high_quality_news)

# remove special characters and replace them with spaces
pub <- pub %>% 
  mutate(high_quality_news = str_replace_all(high_quality_news, "[^[:alnum:]]", " ")) %>%
  filter(high_quality_news != "")

# apply the dictionary to count the keywords related to each characteristic
pub <- pub %>% 
  mutate(
    accuracy_count = count_keywords(high_quality_news, high_quality_news_dict$accuracy),
    objectivity_count = count_keywords(high_quality_news, high_quality_news_dict$objectivity),
    clarity_count = count_keywords(high_quality_news, high_quality_news_dict$clarity),
    thoroughness_count = count_keywords(high_quality_news, high_quality_news_dict$thoroughness),
    transparency_count = count_keywords(high_quality_news, high_quality_news_dict$transparency),
    source_credibility_count = count_keywords(high_quality_news, high_quality_news_dict$source_credibility)
  )

# create a column indicating if any high-quality characteristic is mentioned
pub <- pub %>%
  mutate(
    high_quality_mention = ifelse(rowSums(select(., contains("_count"))) > 0, "Yes", "No")
  )

# view the percentage of responses mentioning at least one high-quality characteristic
high_quality_percentage <- table(pub$high_quality_mention) / nrow(pub) * 100

# print the percentage
print(high_quality_percentage)

# view the first few rows with counts for each characteristic
head(pub)



##########################
######visualization#######
##########################

# to visualize the percentage of respondents mentioning each of the high-quality characteristics (accuracy, objectivity, clarity, thoroughness, transparency, and source credibility)

# calculate the percentage of respondents who mentioned each characteristic
percentage_data <- pub %>%
  summarise(
    accuracy = mean(accuracy_count > 0) * 100,
    objectivity = mean(objectivity_count > 0) * 100,
    clarity = mean(clarity_count > 0) * 100,
    thoroughness = mean(thoroughness_count > 0) * 100,
    transparency = mean(transparency_count > 0) * 100,
    source_credibility = mean(source_credibility_count > 0) * 100
  )

# convert the data to a long format for plotting
percentage_data_long <- tidyr::gather(percentage_data, key = "Characteristic", value = "Percentage")

##### plot 1
#  bar plot for high-quality news characteristics
ggplot(percentage_data_long, aes(x = reorder(Characteristic, Percentage), y = Percentage, fill = Characteristic)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +  # Add border to bars
  scale_fill_brewer(palette = "Set2") +  # Use a color palette
  labs(
    title = "High-Quality News Characteristics",
    x = "Characteristic",
    y = "Percentage of Respondents (%)"
  ) +
  theme_minimal(base_size = 15) +  # Increase base text size for readability
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),  # Tilt x-axis labels for readability
    axis.title.x = element_text(face = "bold"),  # Bold x-axis title
    axis.title.y = element_text(face = "bold"),  # Bold y-axis title
    plot.title = element_text(hjust = 0.5, face = "bold"),  # Center and bold the title
    legend.position = "none"  # Remove the legend for a cleaner look
  ) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), vjust = -0.5, size = 4)  # Add percentage labels above bars

# Rename characteristics for a cleaner presentation
percentage_data_long$Characteristic <- factor(percentage_data_long$Characteristic, 
                                              levels = c("accuracy", "objectivity", "clarity", 
                                                         "thoroughness", "transparency", 
                                                         "source_credibility"),
                                              labels = c("Accuracy", "Objectivity", "Clarity", 
                                                         "Thoroughness", "Transparency", 
                                                         "Source Credibility"))

# plot 2: NYT-style plot for high-quality news characteristics
ggplot(percentage_data_long, aes(x = reorder(Characteristic, Percentage), y = Percentage, fill = Characteristic)) +
  geom_bar(stat = "identity", color = "white", size = 0.5, width = 0.7) +  # Thin borders, narrow bars
  labs(
    title = "High-Quality News Characteristics",
    subtitle = "Percentage of respondents who mentioned each characteristic",
    x = NULL,  # Remove x-axis title for a cleaner look
    y = "Percentage of Respondents"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 50)) +  # Y-axis in percentage
  scale_fill_manual(values = c("Accuracy" = "#5B9BD5", "Objectivity" = "#ED7D31", "Clarity" = "#A5A5A5", 
                               "Thoroughness" = "#FFC000", "Transparency" = "#70AD47", 
                               "Source Credibility" = "#4472C4")) +  # Soft, neutral colors
  theme_minimal() +
  theme(
    plot.title = element_text(family = "serif", face = "bold", size = 18, hjust = 0.5, color = "#333333"),
    plot.subtitle = element_text(family = "serif", size = 12, hjust = 0.5, color = "#666666"),  # Subtitle below title
    axis.text.x = element_text(family = "sans", size = 12, color = "#333333", angle = 45, hjust = 1, vjust = 1),  # Clean axis labels
    axis.text.y = element_text(family = "sans", size = 12, color = "#333333"),
    axis.title.y = element_text(family = "sans", size = 14, face = "bold", color = "#333333"),
    panel.grid.major.y = element_line(color = "#CCCCCC", size = 0.5),  # Light horizontal grid lines
    panel.grid.major.x = element_blank(),  # Remove vertical gridlines
    panel.grid.minor = element_blank(),  # No minor gridlines
    axis.ticks.x = element_blank(),  # Remove x-axis ticks
    axis.line.x = element_line(color = "#333333", size = 0.5),  # Subtle x-axis line
    legend.position = "none"  # Hide the legend
  ) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), vjust = -0.5, size = 4, family = "sans", color = "#333333")  # Add percentage labels


##### news by political party######

# filter out unnamed or missing political party rows, but keep "Other"
pub_party_summary <- pub %>%
  filter(Political_Party != "" & !is.na(Political_Party)) %>%  # Remove unnamed or missing categories
  group_by(Political_Party) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# NYT style plot for political party distribution
ggplot(pub_party_summary, aes(x = Political_Party, y = percentage, fill = Political_Party)) +
  geom_bar(stat = "identity", color = "white", size = 0.5, width = 0.7) +  # Use white borders and a slimmer bar width
  labs(
    title = "High-Quality News Mentions by Political Party",
    subtitle = "Percentage of respondents who mentioned high-quality news characteristics",
    x = NULL,  # Remove x-axis title for a cleaner look
    y = "Percentage of Respondents"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 75)) +  # Y-axis as percentages
  scale_fill_manual(values = c("Democrat" = "#5B9BD5", "Independent" = "#ED7D31", "Other" = "#A5A5A5", "Republican" = "#FF6361")) +  # Soft, neutral tones
  theme_minimal() +
  theme(
    plot.title = element_text(family = "serif", face = "bold", size = 18, hjust = 0.5, color = "#333333"),
    plot.subtitle = element_text(family = "serif", size = 12, hjust = 0.5, color = "#666666"),  # Subtitle for context
    axis.text.x = element_text(family = "sans", size = 12, color = "#333333"),  # Clean, readable axis labels
    axis.text.y = element_text(family = "sans", size = 12, color = "#333333"),
    axis.title.y = element_text(family = "sans", size = 14, face = "bold", color = "#333333"),
    panel.grid.major.y = element_line(color = "#CCCCCC", size = 0.5),  # Soft horizontal grid lines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    panel.grid.major.x = element_blank(),  # No vertical gridlines for cleaner look
    axis.ticks.x = element_blank(),  # Remove x-axis ticks
    axis.line.x = element_line(color = "#333333", size = 0.5),  # Add subtle x-axis line
    legend.position = "none"  # Hide the legend
  ) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), vjust = -0.5, size = 4, family = "sans", color = "#333333")  # Add percentage labels above bars

########low quality news##########

# define a dictionary for low-quality news characteristics
low_quality_dict <- list(
  sensationalism = c("sensational", "exaggerated", "overhyped", "shock", "clickbait"),
  bias = c("biased", "partisan", "one-sided", "unfair", "slanted"),
  misinformation = c("misinformation", "false", "inaccurate", "unverified", "rumor"),
  opinionated = c("opinion", "subjective", "personal", "unbalanced", "opinionated"),
  ads = c("ads", "advertisement", "commercials", "sponsored", "promo")
)
# clean the low-quality news text column (assuming pub contains 'low_quality_news')
pub$low_quality_news <- tolower(pub$low_quality_news)  # Convert to lowercase
pub$low_quality_news <- str_replace_all(pub$low_quality_news, "[^[:alnum:]]", " ")  # remove special characters

# count the occurrences of each characteristic in the low-quality news column
pub <- pub %>%
  mutate(
    sensationalism_count = rowSums(sapply(low_quality_dict$sensationalism, function(x) str_count(low_quality_news, x))),
    bias_count = rowSums(sapply(low_quality_dict$bias, function(x) str_count(low_quality_news, x))),
    misinformation_count = rowSums(sapply(low_quality_dict$misinformation, function(x) str_count(low_quality_news, x))),
    opinionated_count = rowSums(sapply(low_quality_dict$opinionated, function(x) str_count(low_quality_news, x))),
    ads_count = rowSums(sapply(low_quality_dict$ads, function(x) str_count(low_quality_news, x)))
  )

# create binary indicators for mentions of each characteristic
pub <- pub %>%
  mutate(
    sensationalism_mention = ifelse(sensationalism_count > 0, 1, 0),
    bias_mention = ifelse(bias_count > 0, 1, 0),
    misinformation_mention = ifelse(misinformation_count > 0, 1, 0),
    opinionated_mention = ifelse(opinionated_count > 0, 1, 0),
    ads_mention = ifelse(ads_count > 0, 1, 0)
  )

# summarize the percentage of respondents mentioning each low-quality characteristic
low_quality_summary <- pub %>%
  summarise(
    sensationalism_percentage = mean(sensationalism_mention) * 100,
    bias_percentage = mean(bias_mention) * 100,
    misinformation_percentage = mean(misinformation_mention) * 100,
    opinionated_percentage = mean(opinionated_mention) * 100,
    ads_percentage = mean(ads_mention) * 100
  )

# convert to long format for easier plotting
low_quality_summary_long <- pivot_longer(low_quality_summary, cols = everything(), 
                                         names_to = "Characteristic", values_to = "Percentage")

# rename the characteristics for better presentation
low_quality_summary_long$Characteristic <- factor(low_quality_summary_long$Characteristic,
                                                  levels = c("sensationalism_percentage", "bias_percentage", 
                                                             "misinformation_percentage", "opinionated_percentage", 
                                                             "ads_percentage"),
                                                  labels = c("Sensationalism", "Bias", "Misinformation", "Opinionated", "Ads"))

# create a bar plot for low-quality news characteristics
ggplot(low_quality_summary_long, aes(x = reorder(Characteristic, Percentage), y = Percentage, fill = Characteristic)) +
  geom_bar(stat = "identity", color = "white", size = 0.5, width = 0.7) +  # Thin borders, narrow bars
  labs(
    title = "Low-Quality News Characteristics",
    subtitle = "Percentage of respondents mentioning each characteristic",
    x = NULL,  # Remove x-axis title for a cleaner look
    y = "Percentage of Respondents"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 50)) +  # Y-axis in percentage
  scale_fill_manual(values = c("Sensationalism" = "#F8766D", "Bias" = "#ED7D31", "Misinformation" = "#A5A5A5", 
                               "Opinionated" = "#FFC000", "Ads" = "#70AD47")) +  # Soft, neutral colors
  theme_minimal() +
  theme(
    plot.title = element_text(family = "serif", face = "bold", size = 18, hjust = 0.5, color = "#333333"),
    plot.subtitle = element_text(family = "serif", size = 12, hjust = 0.5, color = "#666666"),
    axis.text.x = element_text(family = "sans", size = 12, color = "#333333", angle = 45, hjust = 1, vjust = 1),  # Clean axis labels
    axis.text.y = element_text(family = "sans", size = 12, color = "#333333"),
    axis.title.y = element_text(family = "sans", size = 14, face = "bold", color = "#333333"),
    panel.grid.major.y = element_line(color = "#CCCCCC", size = 0.5),  # Light horizontal grid lines
    panel.grid.major.x = element_blank(),  # Remove vertical gridlines
    panel.grid.minor = element_blank(),  # No minor gridlines
    axis.ticks.x = element_blank(),  # Remove x-axis ticks
    axis.line.x = element_line(color = "#333333", size = 0.5),  # Subtle x-axis line
    legend.position = "none"  # Hide the legend
  ) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), vjust = -0.5, size = 4, family = "sans", color = "#333333")  # Add percentage labels

###### low quality by party ######

# Step 1: Filter and summarize the data for low-quality news mentions by political party
pub_low_quality_summary <- pub %>%
  filter(Political_Party != "" & !is.na(Political_Party)) %>%  # Remove unnamed or missing categories
  group_by(Political_Party) %>%
  summarise(
    low_quality_count = sum(sensationalism_mention | bias_mention | misinformation_mention | opinionated_mention | ads_mention),
    total_count = n()
  ) %>%
  mutate(percentage = (low_quality_count / total_count) * 100)

# Step 2: Create a bar plot for low-quality news mentions by political party in the same style as the high-quality plot
ggplot(pub_low_quality_summary, aes(x = Political_Party, y = percentage, fill = Political_Party)) +
  geom_bar(stat = "identity", color = "white", size = 0.5, width = 0.7) +  # Use white borders and a slimmer bar width
  labs(
    title = "Low-Quality News Mentions by Political Party",
    subtitle = "Percentage of respondents who mentioned low-quality news characteristics",
    x = NULL,  # Remove x-axis title for a cleaner look
    y = "Percentage of Respondents"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 75)) +  # Y-axis as percentages
  scale_fill_manual(values = c("Democrat" = "#5B9BD5", "Independent" = "#ED7D31", "Other" = "#A5A5A5", "Republican" = "#FF6361")) +  # Soft, neutral tones
  theme_minimal() +
  theme(
    plot.title = element_text(family = "serif", face = "bold", size = 18, hjust = 0.5, color = "#333333"),
    plot.subtitle = element_text(family = "serif", size = 12, hjust = 0.5, color = "#666666"),  # Subtitle for context
    axis.text.x = element_text(family = "sans", size = 12, color = "#333333"),  # Clean, readable axis labels
    axis.text.y = element_text(family = "sans", size = 12, color = "#333333"),
    axis.title.y = element_text(family = "sans", size = 14, face = "bold", color = "#333333"),
    panel.grid.major.y = element_line(color = "#CCCCCC", size = 0.5),  # Soft horizontal grid lines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    panel.grid.major.x = element_blank(),  # No vertical gridlines for a cleaner look
    axis.ticks.x = element_blank(),  # Remove x-axis ticks
    axis.line.x = element_line(color = "#333333", size = 0.5),  # Add subtle x-axis line
    legend.position = "none"  # Hide the legend
  ) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), vjust = -0.5, size = 4, family = "sans", color = "#333333")  # Add percentage labels above bars






