# Packages used in the code
library(ggplot2)
library(dplyr)

# Read the header file
Head <- read.csv("indiv_header_file.csv")

# Start debugging
# Debugging quote issue
test_5 <- readLines("itcont.txt", n = 5)
#Data_5 <- read.table("itcont.txt", sep = "|", nrow = 5) # first 5 rows
#Data_1000 <- read.table("itcont.txt", sep = "|", nrow = 1000) # first 1000 rows
test_1000 <- readLines("itcont.txt", n = 1000)
test_1000[647]
split_str <- strsplit(test_1000, "|", fixed = T)
sapply(split_str, length)
# quote issue is fixed

# However, we cannot find error in line 647, so we try another way (count fields) to find the error.
count_1 <- count.fields(textConnection(test_1000), sep = "|")
which(is.na(count_1))
test_1000[37]
# We find error here, the first one is in line 37.
# We can see that the O'Neill have the apostrophe, which causes error here. We can fix that.

# Debugging the comment character issue
# We find another error in line 3166, try to fix it. It is the character "#" which cause the error.
# We use the same way as before to find the error, we find that the number of element in line 3166 is 12 instead of 21.  
#Data_10000 <- read.table("itcont.txt", sep = "|", quote = "", nrow = 10000) # first 10000 rows
test_10000 <- readLines("itcont.txt", n = 10000)
count_2 <- count.fields(textConnection(test_10000), sep = "|", quote = "")
which(count_2 != 21)
count_3 <- count.fields(textConnection(test_10000), sep = "|", quote = "",comment.char = "")
which(count_3 != 21)
# comment character issue is fixed

# Read the full dataset. Here we also add colclasses=xx to keep the format of all column types.
# We also read the column names from the header csv file which was imported just now.
Data <- read.table("itcont.txt", sep = "|", quote = "", comment.char = "", 
                   colClasses = c(rep("character", 14), 
                                  "numeric", 
                                  rep("character", 2), 
                                  "numeric", 
                                  rep("character", 2), 
                                  "numeric"))
colnames(Data) <- colnames(Head)
Data$TRANSACTION_DT <- as.Date(Data$TRANSACTION_DT, format = "%m%d%Y") # converting the transaction date to a Date object
#str(Data)

# Using saveRDS to save the full dataset for future use.
#saveRDS(Data, "Full_Dataset.rds")

# I also find a 2020 population dataset, and we only choose to plot 52 states, and omit other places.
population <- read.csv("Population.csv", header = T)[, -1]
population$Abbv <- state.abb[match(population$State, state.name)] # connect abbv with full name
population$Abbv[population$State == "District of Columbia"] <- "DC"
population$Abbv[population$State == "Puerto Rico"] <- "PR"
population$Growth <- population$Growth + 1
population$Pop2016 <- round(population$Pop2018 / population$Growth)
population <- population[, c(2, 9, 10)] # select only state abbv name and population of 2016 and 2020

# Contribution: 2015-present: greater than $200.
# Each row could be an individual contribution from a person/company to a campaign, super-PAC, etc. since the transaction type is different.
# First plot the number of contributions by day.
#table(Data$TRANSACTION_DT)
#ggplot(Data, aes(x = TRANSACTION_DT)) + 
  #stat_count(geom = "line", aes(y = ..count..))

# We find that the plot is very hard to see when plot by day since the number of contribution is so huge. So we try to plot the number of contributions by month.

#ggplot(Data) + 
  #aes(x = format(TRANSACTION_DT, "%Y-%m")) + 
  #geom_bar() + 
  #theme(axis.text.x = element_text(angle = 90, hjust = 1))

# We find that the most contributions are range from 2019-01-01 to 2020-10-01 (to current time), so I decide to narrow the range.

Narrow_date <- Data[Data$TRANSACTION_DT >= "2019-01-01" & Data$TRANSACTION_DT <= "2020-10-01",]
Narrow_date <- na.omit(Narrow_date, cols = "TRANSACTION_DT")
nrow(Narrow_date) / nrow(Data)
sum(Narrow_date$TRANSACTION_AMT) / sum(Data$TRANSACTION_AMT)

# Deal with the dataset, only choose 52 states. This is to connect with the population data
Narrow_date_52 <- Narrow_date[Narrow_date$STATE %in% population$Abbv, ]
nrow(Narrow_date_52) / nrow(Data)
sum(Narrow_date_52$TRANSACTION_AMT) / sum(Data$TRANSACTION_AMT)

# Find the Contributions which are not included in itemization total, using the narrowed dataset. This is the MEMO_CD column.
Cont_not_in <- Narrow_date_52[Narrow_date_52$MEMO_CD == "X", ]
nrow(Cont_not_in) / nrow(Narrow_date_52) # percentage of number of contribution not in total
sum(Cont_not_in$TRANSACTION_AMT) / sum(Narrow_date_52$TRANSACTION_AMT) # percentage of amount of contribution not in total
# we can include those, will not influence a lot.

#ggplot(Narrow_date_52) + 
  #aes(x = format(TRANSACTION_DT, "%Y-%m-%d")) + 
  geom_bar() + 
  #theme(axis.text.x = element_text(angle = 90, hjust = 1))
# plot by day make it hard to see

# plot Number of Contributions by Month in 2020 election   
ggplot(Narrow_date_52) + 
  aes(x = format(TRANSACTION_DT, "%Y-%m")) + 
  geom_bar() + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  ggtitle("Number of Contributions by Month in 2020 election") + 
  xlab("Date by Month") + 
  ylab("Number of Contributions")

# Plot the number of contributions by state.
# I choose to use the narrowed dataset instead of the full dataset.
ggplot(Narrow_date_52) + 
  aes(x = STATE) + 
  geom_bar() + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  ggtitle("Number of Contributions by State in 2020 election") + 
  xlab("States") + 
  ylab("Number of Contributions")

# merge two datasets to get the per capita data
States_52 <- as.data.frame(table(Narrow_date_52$STATE))
States_per_capita <- merge(States_52, population, by.x = "Var1", by.y = "Abbv", all.y = T)
States_per_capita$per_capita_20 <- States_per_capita$Freq / States_per_capita$Pop
colnames(States_per_capita) <- c("State", 
                                 "NoC_20", 
                                 "Population_20", 
                                 "Population_16", 
                                 "Per_capita_20")

# Plot the number of contributions by state per capita
ggplot(States_per_capita) + 
  aes(x = State, y = Per_capita_20) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  ggtitle("Number of Contributions by State per capita in 2020 election") + 
  xlab("States") + 
  ylab("Number of Contributions per capita")

# Begin Discover other aspects
# plot the amount of contribution by month in 2020 election
# we need to get the sum of transaction amount at first, group by the transaction date
amount_by_month <- Narrow_date_52 %>% 
  group_by(TRANSACTION_DT) %>%
  summarise(TRANSACTION_AMT = sum(TRANSACTION_AMT))

ggplot(amount_by_month) + 
  aes(x = format(TRANSACTION_DT, "%Y-%m"), y = TRANSACTION_AMT) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  ggtitle("Amount of Contributions by Month in 2020 election") + 
  xlab("Date by Month") + 
  ylab("Amount of Contributions")

# discover contribution number and amount by different entity type
Narrow_date_52$ENTITY_TP[Narrow_date_52$ENTITY_TP == ""] <- "Not Recorded" # fill in the blank cell with not recorded

# Also need to sum the contribution amount, group by entity type
entity_amount <- Narrow_date_52 %>% 
  group_by(ENTITY_TP) %>%
  summarise(TRANSACTION_AMT = sum(TRANSACTION_AMT))

colnames(entity_amount) <- c("Entity_type", "Amount_of_Contribution")

# number of contribution
entity_number <- as.data.frame(table(Narrow_date_52$ENTITY_TP))
colnames(entity_number) <- c("Entity_type", "Number_of_Contribution")
entity_number <- entity_number[order(-entity_number$Number_of_Contribution), ]
entity_number

# amount of contribution
entity_amount <- entity_amount[order(-entity_amount$Amount_of_Contribution), ]
entity_amount

# discover contribution number and amount by different Transaction type
# the processing steps are rather similar with the entity type
# number of contribution
transaction_type_number <- as.data.frame(table(Narrow_date_52$TRANSACTION_TP))
colnames(transaction_type_number) <- c("transaction_type", "Number_of_Contribution")
transaction_type_number <- transaction_type_number[order(-transaction_type_number$Number_of_Contribution), ]
transaction_type_number

# amount of contribution
transaction_type_amount <- Narrow_date_52 %>% 
  group_by(TRANSACTION_TP) %>%
  summarise(TRANSACTION_AMT = sum(TRANSACTION_AMT))

colnames(transaction_type_amount) <- c("transaction_type", "Amount_of_Contribution")
transaction_type_amount <- transaction_type_amount[order(-transaction_type_amount$Amount_of_Contribution), ]
transaction_type_amount

# Now compare the number and amount of contribution between 2016 and 2020 election
# import 2016 dataset. same processing steps as 2020
Data_16 <- read.table("itcont_16.txt", sep = "|", quote = "", comment.char = "", 
                      colClasses = c(rep("character", 14), 
                                     "numeric", 
                                     rep("character", 2), 
                                     "numeric", 
                                     rep("character", 2), 
                                     "numeric"))
colnames(Data_16) <- colnames(Head)
Data_16$TRANSACTION_DT <- as.Date(Data_16$TRANSACTION_DT, format = "%m%d%Y")

# select from 2015-01-01 to 2016-12-31, same reason as before
Narrow_date_16 <- Data_16[Data_16$TRANSACTION_DT >= "2015-01-01" & Data_16$TRANSACTION_DT <= "2016-12-31", ]
Narrow_date_16 <- na.omit(Narrow_date_16, cols = "TRANSACTION_DT")
nrow(Narrow_date_16) / nrow(Data_16)
sum(Narrow_date_16$TRANSACTION_AMT) / sum(Data_16$TRANSACTION_AMT)

# Deal with the dataset, only choose 52 states.
Narrow_date_52_16 <- Narrow_date_16[Narrow_date_16$STATE %in% population$Abbv, ]
nrow(Narrow_date_52_16) / nrow(Data_16)
sum(Narrow_date_52_16$TRANSACTION_AMT) / sum(Data_16$TRANSACTION_AMT)

# number of contributions in 16 and 20
nrow(Narrow_date_52_16)#16
nrow(Narrow_date_52)#20

# amount of contributions in 16 and 20
sum(Narrow_date_52_16$TRANSACTION_AMT)#16
sum(Narrow_date_52$TRANSACTION_AMT)#20

# plot the number of contributions by month.
ggplot(Narrow_date_52_16) + 
  aes(x = format(TRANSACTION_DT, "%Y-%m")) + 
  geom_bar() + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  ggtitle("Number of Contributions by Month in 2016 election") + 
  xlab("Date by Month") + 
  ylab("Number of Contributions")

# combination of 16 and 20 number of contribution by state
state_16 <- as.data.frame(table(Narrow_date_52_16$STATE))
colnames(state_16) <- c("State", "NoC_16")
state_20 <- as.data.frame(table(Narrow_date_52$STATE))
colnames(state_20) <- c("State", "NoC_20")

# plot the contrast of two year - number of contribution
ggplot() + 
  geom_point(data = state_16, aes(x = State, y = NoC_16, colour = "Year_2016")) + 
  geom_point(data = state_20, aes(x = State, y = NoC_20, colour = "Year_2020")) + 
  scale_color_manual(name = "Year of Election", values = c(Year_2016 = "red", Year_2020 = "blue")) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position = c(0.8, 0.8), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  ggtitle("Number of Contributions by State in both 2016 and 2020 election") + 
  xlab("States") + 
  ylab("Number of Contributions")

# plot the contrast of two year - amount of contribution
# first need to sum the contribution amount
amount_16 <- Narrow_date_52_16 %>% 
  group_by(STATE) %>%
  summarise(TRANSACTION_AMT = sum(TRANSACTION_AMT))
colnames(amount_16) <- c("State", "Amount_16")

amount_20 <- Narrow_date_52 %>%
  group_by(STATE) %>%
  summarise(TRANSACTION_AMT = sum(TRANSACTION_AMT))
colnames(amount_20) <- c("State", "Amount_20")

# second need to merge into one dataset by state
amount_comb <- merge(amount_16, amount_20, by.x = "State", by.y = "State")

ggplot() + 
  geom_point(data = amount_comb, aes(x = State, y = Amount_16, colour = "Year_2016")) +
  geom_point(data = amount_comb, aes(x = State, y = Amount_20, colour = "Year_2020")) + 
  scale_color_manual(name = "Year of Election", values = c(Year_2016 = "red", Year_2020 = "blue")) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position = c(0.8, 0.8), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  ggtitle("Amount of Contributions by State in both 2016 and 2020 election") + 
  xlab("States") + 
  ylab("Amount of Contributions")

# plot the contrast of two year - number of contribution per capita
# we need the 2016 population estimated before and merge data to calculate per capita data
NoC_comb <- merge(state_16, States_per_capita, by.x = "State", by.y = "State")
NoC_comb$Per_capita_16 <- NoC_comb$NoC_16 / NoC_comb$Population_16

ggplot() + 
  geom_point(data = NoC_comb, aes(x = State, y = Per_capita_16, colour = "Year_2016")) + 
  geom_point(data = NoC_comb, aes(x = State, y = Per_capita_20, colour = "Year_2020")) + 
  scale_color_manual(name = "Year of Election", values = c(Year_2016 = "red", Year_2020 = "blue")) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position = c(0.8, 0.8), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  ggtitle("Number of Contributions by State per capita in both 2016 and 2020 election") + 
  xlab("States") + 
  ylab("Number of Contributions per capita")

# plot the contrast of two year - amount of contribution per capita
# first merge the data in order to have two year sum of amount and population
# second get the per capita data
amount_comb <- merge(amount_comb, NoC_comb[, c(1, 4, 5)], by.x = "State", by.y = "State")
amount_comb$pc16 <- amount_comb$Amount_16 / amount_comb$Population_16
amount_comb$pc20 <- amount_comb$Amount_20 / amount_comb$Population_20

ggplot() + 
  geom_point(data = amount_comb, aes(x = State, y = pc16, colour = "Year_2016")) + 
  geom_point(data = amount_comb, aes(x = State, y = pc20, colour = "Year_2020")) + 
  scale_color_manual(name = "Year of Election", values = c(Year_2016 = "red", Year_2020 = "blue")) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position = c(0.8, 0.8), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  ggtitle("Amount of Contributions by State per capita in both 2016 and 2020 election") + 
  xlab("States") + 
  ylab("Amount of Contributions per capita")

# Finish the project!