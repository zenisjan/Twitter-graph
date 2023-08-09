library ("readxl")
library ("purrr")
library ("data.table")
library ("datasets")
library ("dplyr")
library ("methods")
library ("ggplot2")
library ("gridExtra")
library ("scales")
library ("dichromat")

options(scipen = 999)

###########################################
#
# README
# - Change dir path
# - Check the names of datasets and change them based on the real names
#
###########################################

dir="~/Documents/UK/Diplomka/R/"
setwd(dir)


###########################################
#
# Make plot for every searched hashtag
#
###########################################


tweet_count <- read.csv("tweet_count_region.csv")

# list of all hashtags
list_of_hashtags <- data.table(unique(tweet_count$query))

# Retype string to Date
tweet_count$date <- as.Date(tweet_count$date, "%Y-%m-%d")

# make charts for all hashtags
charts_tweets_by_hashtag <- function(data, hashtag){
  
  df <- data %>%
    filter(query == hashtag)
  
  plot <- ggplot(df, aes(x = as.Date(date), y = count, group = query, color= query)) + 
    geom_line() + scale_x_date(date_breaks = "2 months", date_minor_breaks = "1 month",
                               date_labels = "%B") + labs(x = 'Date', y = 'Number of tweets', title = sprintf('Hashtag - %s', hashtag), 
                                                          subtitle = 'Plot subtitle')  
  return(plot)
}

# Call the function pass the dataset with all tweets and parametr for every hashtag
plot_list_hashtags <- map(list_of_hashtags$V1, ~ charts_tweets_by_hashtag(tweet_count, .x)) 
plot_list_hashtags[[1]]

#plot two graphs per page and save it
ml2 <- marrangeGrob(plot_list_hashtags, nrow = 2, ncol = 1)
ggsave("MultipageHashtags.pdf", ml2)

###########################################
#
#All tweets #ukraine and all Attacks by Russia and Ukraine
#
###########################################

tweet_count <- read.csv("tweet_count_all.csv")

# Retype string to Date
tweet_count$date <- as.Date(tweet_count$date, "%Y-%m-%d")

tweet_count_adjusted <- tweet_count %>%
  filter (date > "2022-01-01") %>%
  rename (Hashtag = query) %>%
  mutate(Hashtag = case_when(
    grepl("kraine", Hashtag) ~ "Ukraine",
    grepl("ussia", Hashtag) ~ "Russia"))



plot_all_tweets <- ggplot(tweet_count_adjusted, aes(x = as.Date(date), y = count, group = Hashtag, color= Hashtag)) + geom_line(linetype = "solid",linewidth = 0.8) +
  scale_x_date(date_breaks = "2 months", date_minor_breaks = "1 month", date_labels = "%B") +
  labs(x = 'Date', y = 'Number of Tweets', title = 'Number of Tweets per hashtag', subtitle = '#Russia and #Ukraine')  +
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ","))
  #scale_y_continuous(labels = fancy_scientific)

plot_all_tweets

###########################################
#
# Number of Tweets by language of the Tweet
#
###########################################


tweet_language_count <- read.csv("tweet_language.csv")

tweet_language_count_adj <- tweet_language_count %>%
  filter(!grepl('en|de|fr', tweet_language)) %>%
  #filter(grepl('en|de|fr', tweet_language)) %>%
  filter(count > 1000)

plot_language_tweets <- ggplot(tweet_language_count_adj, aes(x = count, y= reorder(tweet_language,count), fill=factor(tweet_language))) + geom_bar(stat="identity", width = 1) +
  labs(x = 'Languages', y = 'Number of Tweets', title = 'Number of Tweeets by language')  +
  scale_x_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",")) +
  #scale_x_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",")) +
  labs(y='Language',x='Count') +
  guides(fill=guide_legend(title="Languages"))

plot_language_tweets

###########################################
#
# Number authors with number of followers
#
###########################################

#Fine mode in DF column
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}

tweet_followers <- read.csv("Authors_16_3.csv")

#Change data type
tweet_followers$author_created_date <- as.Date(tweet_followers$author_created_date, "%Y-%m-%d")
tweet_followers$author_followers_count <- as.numeric(tweet_followers$author_followers_count)
tweet_followers$author_tweet_count <- as.numeric(tweet_followers$author_tweet_count)

#After invasion accounts only
tweet_followers_adjusted <- tweet_followers %>%
  #filter (author_created_date > "2021-02-24") %>%
  filter (author_created_date > "2022-02-24")

tweet_followers_adjusted$author_followers_group <- cut(tweet_followers_adjusted$author_followers_count,
              breaks=c(-1,10,50, 100, 250, 500, 1000,1000000000),
              labels=c('0-10','11-50', '51-100', '101-250', '251-500', '501-1000', '1001+'))

#occurences <-tweet_followers_adjusted %>%
#  group_by(author_tweet_count) %>%
#  tally()

#calculate average
average_tweets <- tweet_followers_adjusted  %>%
  group_by(author_followers_group) %>%
  summarise_at(vars(author_tweet_count), list(average_tweets = mean))

#calculate mean
median_tweets  <- tweet_followers_adjusted  %>%
  group_by(author_followers_group) %>%
  summarize(median = median(author_tweet_count, na.rm = TRUE))

#group by all the data
tweet_followers_adjusted <- tweet_followers_adjusted %>%
  group_by(author_followers_group) %>%
  tally()

#join everything into one dataframe
tweet_followers_adjusted <- left_join(tweet_followers_adjusted, average_tweets, by = "author_followers_group")
tweet_followers_adjusted <- left_join(tweet_followers_adjusted, median_tweets, by = "author_followers_group")

floor(tweet_followers_adjusted$median)
describe(tweet_followers_adjusted)

portion_median <- tweet_followers_adjusted %>%
  mutate(portion_median = average_tweets/median_tweets)

#rounding
tweet_followers_adjusted$median_tweets <- round(tweet_followers_adjusted$median,0)

#plot with bar chart and two point charts
plot_followers_adjusted <- ggplot(tweet_followers_adjusted) +
  geom_bar(aes(x = n, y= reorder(author_followers_group,n), fill=factor(author_followers_group)), stat="identity", width = 1) +
  geom_point(aes(x= average_tweets, y=reorder(author_followers_group,average_tweets), group = author_followers_group), stat="identity",color="black",  size=3) +
  geom_point(aes(x= median_tweets, y=reorder(author_followers_group,median_tweets), group = author_followers_group), stat="identity",color="red",  size=3) +
  labs(title = 'Authors created after invastion by number of followers')  +
  scale_x_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",")) +
  labs(y='Number of Followers',x='Count') +
  guides(fill=guide_legend(title="Followers"))

plot_followers_adjusted


###########################################
#
# Hashtag count
#
###########################################

hashtag_count <- read.table("Hashtags_count.csv", sep="\t", header=TRUE)

#Change data type

hashtag_count$count <- as.numeric(hashtag_count$count)

hashtag_count_adjusted <- hashtag_count %>%
  filter(!grepl('^Ukraine$|^$', hashtags)) %>%
  filter(count > 15200)

plot_hashtags <- ggplot(hashtag_count_adjusted) +
  geom_bar(aes(x = count, y= reorder(hashtags,count), fill=factor(hashtags)), stat="identity", width = 1) +
  labs(x = 'Occurance', y = 'Hashtags', title = 'Occurance of TOP30 hashtags mentioned in Tweets #Ukraine')  +
  scale_x_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",")) +
  guides(fill=guide_legend(title="Hashtags"))

plot_hashtags
  
###########################################
#
#Weapons donation chart -  number of donation per day
#
###########################################

weapons_donation <- read_excel("donations.xlsx")

weapons_donation_adjusted <- weapons_donation %>%
  group_by(`date`) %>%
  filter (date > "2022-02-01") %>%
  filter (date < "2023-02-01") %>%
  tally()

plot_weapons_donation_count <- ggplot(weapons_donation_adjusted, aes(x = as.Date(date))) +
  geom_bar(aes(x = as.Date(date), y = n, fill = "Per day"), stat = 'identity', width = 2) +
  scale_fill_manual(values = "blue") +
  geom_line(aes(y = cumsum(n)/30, color = 'Aggregated'), size = 1) +
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "7 day", date_labels = "%b") +
  scale_y_continuous(name = 'Volume of donations', sec.axis = sec_axis(~.*30, name= 'Accumulated donations')) +
  guides(fill=guide_legend(title="Donations"), colour = guide_legend(title = "Donations")) +
  labs(x = 'Date', title = 'Volume of weapons donations per day') +
  theme(axis.title.y.right=element_text(colour='black'), axis.ticks.y.right=element_line(colour = 'black'), axis.text.y.right=element_text(colour='black'))

plot_weapons_donation_count

###########################################
#
#Weapons donation chart -  amount of money donated per day
#
###########################################

weapons_donation <- read_excel("donations.xlsx")

weapons_donation$value_usd <- as.numeric(weapons_donation$value_usd) 
weapons_donation$value_usd <- round(weapons_donation$value_usd, digits = 0) 

weapons_donation_adjusted <- weapons_donation[!duplicated(weapons_donation$ID),]

weapons_donation_adjusted <- weapons_donation_adjusted %>%
  group_by(date) %>%
  summarise(value_usd = sum(value_usd))

plot_weapons_donation_count <- ggplot(weapons_donation_adjusted, aes(x = as.Date(date))) +
  geom_bar(aes(x = as.Date(date), y = value_usd, fill = "Per day"), stat = 'identity', width = 2) +
  scale_fill_manual(values = "blue") +
  geom_line(aes(y = cumsum(value_usd)/4, color = 'Aggregated'), size = 1) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  guides(fill=guide_legend(title="Donations"), colour = guide_legend(title = "Donations")) +
  labs(x = 'Date', y = 'Value of donations in USD', title = 'Value of Weapons Donations per Day in USD') +
  scale_y_continuous(labels = scales::label_number_si(), sec.axis = sec_axis(~.*4, name= 'Accumulated donations in USD', labels = scales::label_number_si()))  +
  theme(axis.title.y.right=element_text(colour='black'), axis.ticks.y.right=element_line(colour = 'black'), axis.text.y.right=element_text(colour='black'))


plot_weapons_donation_count 
  
###########################################
#
# Weapons donations count and number of Tweets
#
###########################################
  
weapons_donation <- read_excel("donations.xlsx")
tweet_count <- read.csv("tweet_count_all.csv")

weapons_donation_adjusted <- weapons_donation %>%
  group_by(`date`) %>%
  filter (date > "2022-02-01") %>%
  filter (date < "2023-03-01") %>%
  tally() %>%
  rename(count = n)

#Tweet count based on the searched hashtag
tweet_count_adjusted <- tweet_count %>%
  mutate(type = "Tweet") %>%
  filter (date > "2022-04-01") %>%
  filter (date < "2023-03-01") %>%
  mutate(query = case_when(
    grepl("ukraine", query) ~ "Ukraine",
    grepl("russia", query) ~ "Russia"
  ))

tweet_count_adjusted$date <- as.Date(tweet_count_adjusted$date, "%Y-%m-%d")

multiplicator = 1500

plot_all_tweets_donations <- ggplot() +
  #geom bar created independently from ukraine_conflict_adjusted
  #geom_bar(data = weapons_donation_adjusted, aes(x = as.Date(date), y = count*multiplicator, fill = "Count"), width=1.5, stat = 'identity') +

  geom_line(data = weapons_donation_adjusted, aes(x = as.Date(date), y = cumsum(count)*multiplicator,color = "Count"), size=1) +

  # geom point plot created independently from tweet_count_adjusted
  geom_point(data = tweet_count_adjusted, aes(x = as.Date(date), y = count, group = query, colour = query)) +
  # change colors of fill
  scale_color_manual(values = c("blue", "#F8766D", "#03bfc4")) + 
  # change design x axis
  scale_x_date(date_breaks = "2 months", date_minor_breaks = "1 month", date_labels = "%B") +
  labs(x = 'Date', y = 'Number of Tweets', title = sprintf('Number of Tweets and Donations per Day'), subtitle = 'From April 2022 to February 2023') +
  guides(color = guide_legend(title = "Tweets &\nDonations")) +
  # change design of the secondary Y axis and add different big mark design "."
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ","), sec.axis = sec_axis(~./multiplicator, name= 'Number of Donations', labels = comma_format(big.mark = ".", decimal.mark = ",")))

plot_all_tweets_donations
  
###########################################
#
# Weapons donations amount and number of Tweets
#
###########################################

weapons_donation <- read_excel("donations.xlsx")
tweet_count <- read.csv("tweet_count_all.csv")

weapons_donation$value_usd <- as.numeric(weapons_donation$value_usd) 
weapons_donation$value_usd <- round(weapons_donation$value_usd, digits = 0) 
weapons_donation_adjusted <- weapons_donation[!duplicated(weapons_donation$ID),]

weapons_donation_adjusted <- weapons_donation_adjusted %>%
  group_by(date) %>%
  summarise(value_usd = sum(value_usd)) %>%
  filter (date > "2022-02-01") %>%
  filter (date < "2023-03-01") %>%
  rename(value = value_usd)

#Tweet count based on the searched hashtag
tweet_count_adjusted <- tweet_count %>%
  mutate(type = "Tweet") %>%
  filter (date > "2022-04-01") %>%
  filter (date < "2023-03-01") %>%
  mutate(query = case_when(
    grepl("ukraine", query) ~ "Ukraine",
    grepl("russia", query) ~ "Russia"
  )) %>%
  mutate(count = count) %>%
  mutate(count = floor(count))

tweet_count_adjusted$date <- as.Date(tweet_count_adjusted$date, "%Y-%m-%d")

multiplicator = 67000

plot_all_tweets_donations <- ggplot() +
  geom_point(data = tweet_count_adjusted, aes(x = as.Date(date), y = count, group = query, color = query)) +

  geom_line(data = weapons_donation_adjusted, aes(x = as.Date(date), y = cumsum(value)/multiplicator,color = "Value"), size=1) +
  
  scale_color_manual(values=c("#F8766D", "#03bfc4", "blue"))+

  # geom point plot created independently from tweet_count_adjusted
  # change design x axis
  scale_x_date(date_breaks = "2 months", date_minor_breaks = "1 month", date_labels = "%B") +
  labs(x = 'Date', y = 'Tweets per day', title = sprintf('Number of Tweets and Value of Donations in USD'), subtitle = 'From April 2022 to February 2023') +
  guides(color = guide_legend(title = "Tweets & \nDonations")) +
  # change design of the secondary Y axis and add different big mark design "."
  scale_y_continuous(labels = scales::label_number_si(), sec.axis = sec_axis(~.*multiplicator, name= 'Value of donations in USD', labels = scales::label_number_si()))

plot_all_tweets_donations

###########################################
#
# Weapons donations number and number of Attacks
#
###########################################

weapons_donation <- read_excel("donations.xlsx")
ukraine_conflict <- read_excel("Ukraine_Black_Sea_2020_2023_Jan20.xlsx")

weapons_donation_adjusted <- weapons_donation %>%
  group_by(`date`) %>%
  filter (date > "2022-02-01") %>%
  filter (date < "2023-02-01") %>%
  tally() %>%
  rename(count = n)

# Add column Attack Actor
ukraine_conflict_adjusted <- ukraine_conflict %>%
  mutate(query = case_when(
    grepl("kraine", ACTOR1) ~ "Ukraine",
    grepl("ussia|ovoross", ACTOR1) ~ "Russia",
    !grepl("kraine|ussia|ovoross", ACTOR1) ~ "Unknown")) %>%
  filter(!grepl('Unknown',query)) %>%
  rename(date = EVENT_DATE) %>%
  rename(type = ADMIN1) %>%
  filter (date > "2022-02-01") %>%
  filter (date < "2023-02-01") %>%
  group_by(date, query, type) %>%
  tally() %>%
  rename(count = n) %>%
  mutate(count = count)

ukraine_conflict_adjusted$date <- as.Date(ukraine_conflict_adjusted$date, "%Y-%m-%d")
weapons_donation_adjusted$count <- as.numeric(weapons_donation_adjusted$count)

multiplicator = 0.14

plot_all_tweets_donations <- ggplot() +
  #geom bar created independently from ukraine_conflict_adjusted
  geom_bar(data =  ukraine_conflict_adjusted, aes(x = as.Date(date), y = count, group = query, fill = query), width=1, stat = 'identity') +
  scale_fill_manual(values=c("green","blue")) + 
  # change colors of fill
  # geom point plot created independently from tweet_count_adjusted
  geom_line(data = weapons_donation_adjusted, aes(x = as.Date(date), y = cumsum(count)*multiplicator, color = "Count"), size = 1) +
  # change design x axis
  scale_color_manual(values=c("red"))+
  scale_x_date(date_breaks = "2 months", date_minor_breaks = "1 month", date_labels = "%B") +
  labs(x = 'Date', y = 'Number of Attacks', title = sprintf('Number of Attacks and Donations per day'), subtitle = 'From February 2022 to February 2023') +
  guides(fill=guide_legend(title="Attacks"), color = guide_legend(title = "Donations")) +
  # change design of the secondary Y axis and add different big mark design "."
  scale_y_continuous(sec.axis = sec_axis(~./multiplicator, name= 'Number of Donation', labels = comma_format(big.mark = ".", decimal.mark = ",")))

plot_all_tweets_donations

###########################################
#
# Weapons donations amount and number of Attacks
#
###########################################
  
weapons_donation <- read_excel("donations.xlsx")
ukraine_conflict <- read_excel("Ukraine_Black_Sea_2020_2023_Jan20.xlsx")

weapons_donation$value_usd <- as.numeric(weapons_donation$value_usd) 
weapons_donation$value_usd <- round(weapons_donation$value_usd, digits = 0) 
weapons_donation_adjusted <- weapons_donation[!duplicated(weapons_donation$ID),]

weapons_donation_adjusted <- weapons_donation_adjusted %>%
  group_by(date) %>%
  summarise(value_usd = sum(value_usd)) %>%
  filter (date > "2022-02-01") %>%
  filter (date < "2023-03-01") %>%
  rename(value = value_usd)

# Add column Attack Actor
ukraine_conflict_adjusted <- ukraine_conflict %>%
  mutate(query = case_when(
    grepl("kraine", ACTOR1) ~ "Ukraine",
    grepl("ussia|ovoross", ACTOR1) ~ "Russia",
    !grepl("kraine|ussia|ovoross", ACTOR1) ~ "Unknown")) %>%
  filter(!grepl('Unknown',query)) %>%
  rename(date = EVENT_DATE) %>%
  rename(type = ADMIN1) %>%
  filter (date > "2022-02-01") %>%
  filter (date < "2023-03-01") %>%
  group_by(date, query, type) %>%
  tally() %>%
  rename(count = n) %>%
  mutate(count = count)

ukraine_conflict_adjusted$date <- as.Date(ukraine_conflict_adjusted$date, "%Y-%m-%d")

ukraine_conflict_adjusted$date <- as.Date(ukraine_conflict_adjusted$date, "%Y-%m-%d")

multiplicator = 0.0000000018

plot_all_tweets_donations <- ggplot() +
  # geom bar created independently from ukraine_conflict_adjusted
  geom_bar(data = ukraine_conflict_adjusted, aes(x = as.Date(date), y = count, group = query, fill = query), width=1, stat = 'identity') +
  scale_fill_manual(values=c("green","blue")) + 
  # change colors of fill
  # geom point plot created independently from tweet_count_adjusted
  geom_line(data = weapons_donation_adjusted, aes(x = as.Date(date), y = cumsum(value)*multiplicator, color = "Value"), size = 1) +
  scale_color_manual(values=c("red"))+
  # change design x axis
  scale_x_date(date_breaks = "2 months", date_minor_breaks = "1 month", date_labels = "%B") +
  labs(x = 'Date', y = 'Number of Attacks', title = sprintf('Number of Attacks and Accumulated Value of Donations'), subtitle = 'From April 2022 to February 2023') +
  guides(fill=guide_legend(title="Attacks"), color = guide_legend(title = "Donations")) +
  # change design of the secondary Y axis and add different big mark design "."
  scale_y_continuous(sec.axis = sec_axis(~./multiplicator, name= 'Value of Donation in USD', labels = scales::label_number_si()))

plot_all_tweets_donations

######################################################################################
#
#Combined all tweets and attacks in region based on date
#
######################################################################################

ukraine_conflict <- read_excel("Ukraine_Black_Sea_2020_2023_Jan20.xlsx")
tweet_count <- read.csv("tweet_count_region.csv")

# List of unique regions
list_regions <- data.table(unique(ukraine_conflict$ADMIN1)) %>%
  filter(V1 != "NA") %>%
  rename(Region = V1)

#REMOVE THIS BEFORE FINAL VERSION
tweet_count$date <- as.Date(tweet_count$date, "%d.%m.%Y")

multiplicator = 2500

# Add column Attack Actor
ukraine_conflict_adjusted <- ukraine_conflict %>%
  mutate(query = case_when(
    grepl("kraine", ACTOR1) ~ "Ukraine",
    grepl("ussia|ovoross", ACTOR1) ~ "Russia",
    !grepl("kraine|ussia|ovoross", ACTOR1) ~ "Unknown")) %>%
  filter(!grepl('Unknown',query)) %>%
  rename(date = EVENT_DATE) %>%
  rename(type = ADMIN1) %>%
  filter (date > "2022-04-01") %>%
  filter (date < "2023-02-01") %>%
  group_by(date, query, type) %>%
  tally() %>%
  rename(count = n)

ukraine_conflict_adjusted$type[ukraine_conflict_adjusted$type == 'Zaporizhia'] <- 'Zaporizhzhia'

#ukraine_conflict_adjusted = ukraine_conflict_adjusted[,c(1,4,2,3)]

#Tweet count based on the searched hashtag
tweet_count_adjusted <- tweet_count %>%
  mutate(type = "Tweet") %>%
  filter (date > "2022-04-01") %>%
  filter (date < "2023-02-01") %>%
  mutate(count = count/multiplicator) %>%
  mutate(count = floor(count))

tweet_count_adjusted$query[tweet_count_adjusted$query == 'Zaporizhia'] <- 'Zaporizhzhia'

tweet_count_adjusted$date <- as.Date(tweet_count_adjusted$date, "%Y-%m-%d")
ukraine_conflict_adjusted$date <- as.Date(ukraine_conflict_adjusted$date, "%Y-%m-%d")

#combined_data = rbind(tweet_count_adjusted, ukraine_conflict_adjusted)

region = "Zaporizhzhia"

attacks_region <- ukraine_conflict_adjusted %>%
  filter(type == region)

tweets_region <- tweet_count_adjusted %>%
  filter(query == region)

plot_region <- ggplot() +
  # geom bar created independently from ukraine_conflict_adjusted
  geom_bar(data = attacks_region, aes(x = as.Date(date), y = count, group = query, fill = query), width=1, stat = 'identity') +
  # change colors of fill
  scale_fill_manual(values=c("green","blue")) +
  # geom point plot created independently from tweet_count_adjusted
  geom_point(data = tweets_region, aes(x = as.Date(date), y = count, group = query, color = query)) +
  # change design x axis
  scale_x_date(date_breaks = "2 months", date_minor_breaks = "1 month", date_labels = "%B") +
  labs(x = 'Date', y = 'Number of events', title = sprintf('Number of Tweets and Attacks in %s region', region), subtitle = 'From April 2022 to February 2023') +
  guides(fill=guide_legend(title="Attacks"), color = guide_legend(title = "Tweets")) +
  # change design of the secondary Y axis and add different big mark design "."
  scale_y_continuous(sec.axis = sec_axis(~.*multiplicator, name= 'Tweets per day', labels = comma_format(big.mark = ".", decimal.mark = ",")))

plot_region


###########################################
#
#All tweets #ukraine and all Attacks by Russia and Ukraine
#
###########################################

ukraine_conflict <- read_excel("Ukraine_Black_Sea_2020_2023_Jan20.xlsx")
tweet_count <- read.csv("tweet_count_all.csv")

# Add column Attack Actor
ukraine_conflict_adjusted <- ukraine_conflict %>%
  mutate(query = case_when(
    grepl("kraine", ACTOR1) ~ "Ukraine",
    grepl("ussia|ovoross", ACTOR1) ~ "Russia",
    !grepl("kraine|ussia|ovoross", ACTOR1) ~ "Unknown")) %>%
  filter(!grepl('Unknown',query)) %>%
  rename(date = EVENT_DATE) %>%
  rename(type = ADMIN1) %>%
  filter (date > "2022-04-01") %>%
  filter (date < "2023-02-01") %>%
  group_by(date, query, type) %>%
  tally() %>%
  rename(count = n) %>%
  mutate(count = count*1)

#ukraine_conflict_adjusted = ukraine_conflict_adjusted[,c(1,4,2,3)]

multiplicator = 5000

#Tweet count based on the searched hashtag
tweet_count_adjusted <- tweet_count %>%
  mutate(type = "Tweet") %>%
  filter (date > "2022-04-01") %>%
  filter (date < "2023-02-01") %>%
  mutate(query = case_when(
    grepl("ukraine", query) ~ "Ukraine",
    grepl("russia", query) ~ "Russia"
  )) %>%
  mutate(count = count/multiplicator)

tweet_count_adjusted$date <- as.Date(tweet_count_adjusted$date, "%Y-%m-%d")
ukraine_conflict_adjusted$date <- as.Date(ukraine_conflict_adjusted$date, "%Y-%m-%d")

#combined_data <- rbind(tweet_count_adjusted, ukraine_conflict_adjusted)
#combined_data <- merge(x=tweet_count_adjusted,y=ukraine_conflict_adjusted, by="date", all=TRUE)

plot_all_tweets_attacks <- ggplot() +
  # geom bar created independently from ukraine_conflict_adjusted
  geom_bar(data = ukraine_conflict_adjusted, aes(x = as.Date(date), y = count, group = query, fill = query), width=1, stat = 'identity') +
  # change colors of fill
  scale_fill_manual(values=c("green","blue")) +
  # geom point plot created independently from tweet_count_adjusted
  geom_point(data = tweet_count_adjusted, aes(x = as.Date(date), y = count, group = query, color = query)) +
  # change design x axis
  scale_x_date(date_breaks = "2 months", date_minor_breaks = "1 month", date_labels = "%B") +
  labs(x = 'Date', y = 'Number of events', title = sprintf('Number of Tweets and Attacks across all Ukraine'), subtitle = 'From April 2022 to February 2023') +
  guides(fill=guide_legend(title="Attacks"), color = guide_legend(title = "Tweets")) +
  # change design of the secondary Y axis and add different big mark design "."
  scale_y_continuous(sec.axis = sec_axis(~.*multiplicator, name= 'Tweets per day', labels = comma_format(big.mark = ".", decimal.mark = ",")))

plot_all_tweets_attacks
