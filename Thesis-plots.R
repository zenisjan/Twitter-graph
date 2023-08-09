library ("readxl")
library ("purrr")
library ("data.table")
library ("datasets")
library ("dplyr")
library ("methods")
library ("ggplot2")
library ("gridExtra")

options(scipen = 999)

dir="~/Documents/UK/Diplomka/R/"

setwd(dir)

ukraine_conflict <- read_excel("Ukraine_Black_Sea_2020_2023_Jan20.xlsx")

twitter_author_registration <- read.csv("Twitter_registration_date.csv")

ukraine_conflict <- ukraine_conflict %>%
  mutate(ACTOR = case_when(
    grepl("kraine", ACTOR1) ~ "Ukraine",
    grepl("ussia|ovoross", ACTOR1) ~ "Russia",
    !grepl("kraine|ussia|ovoross", ACTOR1) ~ "Unknown"
  ))


###########################################
#
#All attacks grouped by date
#
###########################################

attacks_by_date <- ukraine_conflict %>% 
  group_by(EVENT_DATE) %>% 
  filter (EVENT_DATE > "2022-01-01") %>%
  tally()

#All events on timeline
graph_attacts_by_date <- ggplot(attacks_by_date, aes(x = EVENT_DATE, y = n, color = n)) + geom_line()

graph_attacts_by_date + labs(x = 'Date', y = 'Number of events', title = 'Russo - Ukrainian war', 
                               subtitle = 'Number of events in time')  + geom_smooth()


###########################################
#
#Plot all attacks from Russia and Ukraine grouped by date
#
###########################################

#Group by all Ukrainian attacts by the date
attacks_by_ukraine_date <- ukraine_conflict %>% 
  group_by(EVENT_DATE, ACTOR) %>%
  filter(ACTOR == "Ukraine") %>%
  filter (EVENT_DATE > "2022-01-01") %>%
  tally()

#Rename n to Ukraine_n
attacks_by_ukraine_date %>%
  rename(Ukraine_n = n)

#Group by all Russia attacts by the date
attacks_by_russia_date <- ukraine_conflict %>% 
  group_by(EVENT_DATE, ACTOR) %>%
  filter(ACTOR == "Russia") %>%
  filter (EVENT_DATE > "2022-01-01") %>%
  tally()

attacks_by_russia_date %>%
  rename(Russia_n = n)

#Merged and joined attacks by both adversaries
merged_attacks_by_date <- merge(attacks_by_ukraine_date,attacks_by_russia_date, by="EVENT_DATE", all=TRUE)

joined_attacks_by_date <- rbind(attacks_by_ukraine_date,attacks_by_russia_date)

#All events on timeline by Actor
graph_by_actor_and_date <- ggplot(joined_attacks_by_date, aes(x = EVENT_DATE, y = n, group = ACTOR, color = ACTOR )) + geom_line()
  
graph_by_actor_and_date + labs(x = 'Date', y = 'Number of events', title = 'Russo - Ukrainian war', 
                             subtitle = 'Number of events in time by adversary')

###########################################
#
#All attacks grouped by date
#
###########################################

# List of All regions for plot
list_regions <- data.table(unique(add_column_actor$ADMIN1)) %>%
  filter(V1 != "NA") %>%
  rename(Region = V1)

# Dataset gruped by data, actor and administrative region from 2022
attacks_by_region_date <- add_column_actor %>%
  rename(date = EVENT_DATE) %>%
  filter (date > "2022-01-01") %>%
  group_by(date, ACTOR, ADMIN1) %>%
  tally()

# Function that has two parameters a dataset and a specific region for ploting 
chart_attacks_by_region_date <- function(data, region){
  
  df <- data %>% 
    filter(ADMIN1 == region)
  
  plot <- ggplot(df, aes(x = as.Date(date), y = n, group = ACTOR, color= ACTOR)) + 
    geom_line() + labs(x = 'Date', y = 'Number of events', title = sprintf('Region - %s', region), 
                subtitle = 'Number of events in time by adversary')
  
  return(plot)
}

# A map function that loops through all regions and call the function for charting
plot_list_regions <- map(list_regions$Region, ~ chart_attacks_by_region_date(attacks_by_region_date, .x)) 
plot_list_regions[[1]]

ml1 <- marrangeGrob(plot_list_regions, nrow = 2, ncol = 1)
ggsave("AttacksByRegion.pdf", ml1)


###########################################
#
#Twitter Author registration date
#
###########################################

twitter_author_registration <- twitter_author_registration %>%
  filter (author_created_date > "2022-01-01")
  

plot_author_registration <- ggplot(twitter_author_registration, aes(x = as.Date(author_created_date), y = count, color = count)) + geom_line() +
  scale_x_date(date_breaks = "1 months", date_labels = "%B") +
  labs(x = 'Date', y = 'Twitter Author Registration', title = 'Number of Registration per Day')

ggsave("Author_registration.pdf", plot_author_registration)


###########################################
#
#Weapons donation chart -  number of donation per day
#
###########################################

weapons_donation_adjusted <- weapons_donation %>%
  group_by(`Announcement Date`) %>%
  rename(date = `Announcement Date`) %>%
  filter (date > "2022-02-01") %>%
  filter (date < "2023-02-01") %>%
  tally()

plot_weapons_donation_count <- ggplot(weapons_donation_adjusted, aes(x = as.Date(date), y = n, color = n)) + geom_line() +
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "7 day", date_labels = "%b") +
  labs(x = 'Date', y = 'Number of donations', title = 'Number of weapons donations per day')

plot_weapons_donation_count

###########################################
#
#Weapons donation chart -  amount of money donated per day
#
###########################################

weapons_donation <- read_excel("donations.xlsx")

weapons_donation$value_usd <- as.numeric(weapons_donation$value_usd) 
weapons_donation$value_usd <- round(weapons_donation$value_usd, digits = 0) 

weapons_donation_adjusted <- weapons_donation %>%
  group_by(date) %>%
  summarise(value_usd = sum(value_usd))

class(weapons_donation_adjusted$donation)


plot_weapons_donation_count <- ggplot(weapons_donation_adjusted, aes(x = as.Date(date), y = value_usd, color = value_usd)) + geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(x = 'Date', y = 'Value of donations', title = 'Value of Weapons Donations per Day in USD') +
  scale_y_continuous(labels = scales::label_number_si()) +
  scale_color_continuous(labels = scales::unit_format(unit = "B", scale = 1e-9))

plot_weapons_donation_count
