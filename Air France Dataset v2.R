#################################################
######
###### Air France Internet Marketing
###### Data Science: R - DAT-5302
######
###### Professor: Thomas Kurnicki
######
###### Submitted by Team 4, MSBA 4
######       Pierre Vilijn
######       Alban de Reamy
######       Benon Osinde
######       Lazza Berhe
######       Parisha Uppal
######
###### December 2020
######
#################################################


###################Import Data, packages and load libraries##################

library(readxl)
AF_dataset <- read_excel("Hult/Master in Business Analytics/Data Science - R/Air France Case Spreadsheet Supplement.xlsx",
                         sheet = "DoubleClick")

kayak <- read_excel("Hult/Master in Business Analytics/Data Science - R/Air France Case Spreadsheet Supplement.xlsx",
                    sheet = "Kayak")

#install.packages("tidyverse")
#install.packages("tm")
#install.packages("wordcloud2")
#install.packages("RColorBrewer")
#install.packages("ggcorrplot")
#install.packages("plotly")
#install.packages("gridExtra")
library(magrittr)
library(stringr)
library(tidyr)
library(ggplot2)
library(dplyr)
library(wordcloud2)
library(tm)
library(RColorBrewer)
library(plotly)
library(ggcorrplot)
library(gridExtra)

#####################DATA CLEANING###########################################

#######################AF_dataset

#Look through the data set for data cleaning
AF_dataframe <- as.data.frame(AF_dataset)
str(AF_dataframe)

#replace column names with lower capital and remove spaces
colnames(AF_dataframe) %<>% str_replace_all("\\s", "_") %<>% tolower()

#replace NA and correct typos in bid strategy
AF_dataframe$bid_strategy <- replace_na(AF_dataframe$bid_strategy,"Not defined")
AF_dataframe$bid_strategy <- gsub('Postiion', 'Position', AF_dataframe$bid_strategy)
AF_dataframe$bid_strategy <- gsub('1 -2', '1-2', AF_dataframe$bid_strategy)
AF_dataframe$bid_strategy <- gsub('1- 3', '1-3', AF_dataframe$bid_strategy)

########################KAYAK

#remove rows
kayak <- kayak[-c(1, 2, 4, 5),]

#Rename Columns
kayak <-  kayak %>%
  rename(
    search_engine = "Sponsored Links - Air France",
    clicks = "...2",
    media_cost = "...3",
    total_bookings = "...4",
    avg_ticket = "...5",
    total_revenue = "...6",
    net_revenue = "...7"
  )

#Convert chr to numerical
kayak$clicks <- as.numeric(kayak$clicks)
kayak$media_cost <- as.numeric(kayak$media_cost)
kayak$total_bookings <- as.numeric(kayak$total_bookings)
kayak$avg_ticket <- as.numeric(kayak$avg_ticket)
kayak$total_revenue <- as.numeric(kayak$total_revenue)
kayak$net_revenue <- as.numeric(kayak$net_revenue)

#Creating new column for Air France data
AF_dataframe$net_revenue <- AF_dataframe$amount - AF_dataframe$total_cost
AF_dataframe$ROA <- AF_dataframe$net_revenue / AF_dataframe$total_cost
AF_dataframe$avg_revenue_per_booking <- AF_dataframe$amount / AF_dataframe$total_volume_of_bookings
AF_dataframe$probability_of_booking <- AF_dataframe$`engine_click_thru_%` * AF_dataframe$`trans._conv._%` / 10000 
AF_dataframe$advertising_cost_per_booking <- AF_dataframe$total_cost / AF_dataframe$total_volume_of_bookings

#creating new colums for kayak data
kayak$probability_of_booking <- (kayak$total_bookings / kayak$clicks)*100
kayak$avg._cost_per_click <- (kayak$media_cost / kayak$clicks)

AF_dataframe <- AF_dataframe[-which(AF_dataframe$total_cost == 0),]

#####################ANALYSIS#################################################

#Creating publisher pivot table
publisher_pivot <- AF_dataframe %>%
  select(publisher_name, net_revenue, probability_of_booking, avg._cost_per_click)%>%
  group_by(publisher_name)%>%
  summarise(net_revenue = sum(net_revenue), probability_of_booking = mean(probability_of_booking), avg._cost_per_click = mean(avg._cost_per_click))

#adding new row on the table averages and binding it to publisher_pivot
avg_pubpivot <- c("average", mean(publisher_pivot$net_revenue), mean(publisher_pivot$probability_of_booking), mean(publisher_pivot$avg._cost_per_click) )
publisher_pivot <- rbind(publisher_pivot, avg_pubpivot)

#converting character to numerical in publisher_pivot
publisher_pivot$net_revenue <- as.numeric(publisher_pivot$net_revenue)
publisher_pivot$probability_of_booking <- as.numeric(publisher_pivot$probability_of_booking)
publisher_pivot$avg._cost_per_click <- as.numeric(publisher_pivot$avg._cost_per_click)

#Creating publisher pivot table summary
publisher_pivot_summary <- AF_dataframe %>%
  select(publisher_name, total_volume_of_bookings, net_revenue, total_cost, ROA )%>%
  group_by(publisher_name)%>%
  summarise(total_volume_of_bookings = sum(total_volume_of_bookings), net_revenue = sum(net_revenue),total_cost = sum(total_cost), ROA = mean(ROA)
            )

#converting character to numerical in publisher_pivot_summary
publisher_pivot_summary$total_volume_of_bookings <- as.numeric(publisher_pivot_summary$total_volume_of_bookings)
publisher_pivot_summary$net_revenue <- as.numeric(publisher_pivot_summary$net_revenue)
publisher_pivot_summary$total_cost <- as.numeric(publisher_pivot_summary$total_cost)
publisher_pivot_summary$ROA <- as.numeric(publisher_pivot_summary$ROA)

publisher_pivot_summary$ROA <- round(publisher_pivot_summary$ROA, digits=2)
publisher_pivot_summary$net_revenue <- round(publisher_pivot_summary$net_revenue, digits=0)
publisher_pivot_summary$total_cost <- round(publisher_pivot_summary$total_cost, digits=0)


#preparing kayak_summary pivot for rbind
kayak_pivot_summary <- data.frame(publisher_name = "Kayak", total_volume_of_bookings = round(kayak$total_bookings, digits=0),
                                  net_revenue = round(kayak$net_revenue, digits=0), total_cost = round(kayak$media_cost, digits=0),
                                  ROA = round(kayak$net_revenue / kayak$media_cost, digits = 2 )
                                  
                          )

#Adding Kayak to new_publisher_pivot_summary
new_publisher_final <- rbind(publisher_pivot_summary, kayak_pivot_summary)


#adding new row on the table averages and binding it to publisher_pivot_summary
avg_pubpivot_summary <- c("average",
                          mean(round(publisher_pivot_summary$total_volume_of_bookings, digits = 0)),
                          mean(round(publisher_pivot_summary$net_revenue, digits = 0)),
                          mean(round(publisher_pivot_summary$total_cost, digits = 0)),
                          mean(round(publisher_pivot_summary$ROA, digits = 0)) 
                               )


publisher_pivot_final <- rbind(new_publisher_final, avg_pubpivot_summary)

publisher_pivot_final$total_volume_of_bookings <- as.numeric(publisher_pivot_final$total_volume_of_bookings)
publisher_pivot_final$net_revenue <- as.numeric(publisher_pivot_final$net_revenue)
publisher_pivot_final$total_cost <- as.numeric(publisher_pivot_final$total_cost)
publisher_pivot_final$ROA <- as.numeric(publisher_pivot_final$ROA)

publisher_pivot_final$ROA <- round(publisher_pivot_final$ROA, digits=2)
publisher_pivot_final$net_revenue <- round(publisher_pivot_final$net_revenue, digits=0)
publisher_pivot_final$total_volume_of_bookings <- round(publisher_pivot_final$total_volume_of_bookings, digits=0)
publisher_pivot_final$total_cost <- round(publisher_pivot_final$total_cost, digits=0)

#Bar plot for ROA
ggplot(data=publisher_pivot_final) + 
  geom_col(aes(x= publisher_name, y = ROA)) +
  labs(x="Publisher Name", y="Return on Ads spend", title="ROA")

#Change name on pivot table to make it easier to read 
names(publisher_pivot_final)[1] <- "Publisher Name"
names(publisher_pivot_final)[2] <- "Total Volume of Booking"
names(publisher_pivot_final)[3] <- "Net Revenue ($)"
names(publisher_pivot_final)[4] <- "Total Cost ($)"
names(publisher_pivot_final)[5] <- "ROA (%)"

#creating scatterplot for publisher segment without kayak
ggplot(data = publisher_pivot)+
  geom_vline(xintercept = publisher_pivot$probability_of_booking[8])+
  geom_hline(yintercept = publisher_pivot$avg._cost_per_click[8])+
  geom_point (aes( x = probability_of_booking,
                   y = avg._cost_per_click,
                   size = net_revenue,
                   color = publisher_name,
                   shape = 2), shape = 16)+
  scale_size_continuous(range = c(5, 20))+
  geom_text(aes(x= probability_of_booking, y = avg._cost_per_click), label = publisher_pivot$publisher_name, size = 3)+
  theme_minimal()
  


#preparing kayak pivot for rbind
kayak_pivot <- data.frame(publisher_name = "Kayak", net_revenue = kayak$net_revenue,
                          probability_of_booking = kayak$probability_of_booking,
                          avg._cost_per_click = kayak$avg._cost_per_click)

#Adding Kayak to new_publisher_pivot
new_publisher_pivot <- rbind(publisher_pivot, kayak_pivot)

#adding new row on the table averages and binding it to new_publisher_pivot
new_avg_pubpivot <- c("average", mean(new_publisher_pivot$net_revenue), mean(new_publisher_pivot$probability_of_booking), mean(new_publisher_pivot$avg._cost_per_click) )
new_publisher_pivot <- rbind(new_publisher_pivot, avg_pubpivot)

#converting character to numerical in new_publisher_pivot
new_publisher_pivot$net_revenue <- as.numeric(new_publisher_pivot$net_revenue)
new_publisher_pivot$probability_of_booking <- as.numeric(new_publisher_pivot$probability_of_booking)
new_publisher_pivot$avg._cost_per_click <- as.numeric(new_publisher_pivot$avg._cost_per_click)

#new publisher graph version with kayak for comparison
ggplot(data = new_publisher_pivot)+
  geom_vline(xintercept = new_publisher_pivot$probability_of_booking[8])+
  geom_hline(yintercept = new_publisher_pivot$avg._cost_per_click[8])+
  geom_point (aes( x = probability_of_booking,
                   y = avg._cost_per_click,
                   size = net_revenue,
                   color = publisher_name,
                   shape = 2), shape = 16)+
  scale_size_continuous(range = c(5, 20))+
  geom_text(aes(x= probability_of_booking, y = avg._cost_per_click), label = new_publisher_pivot$publisher_name, size = 3)+
  theme_minimal()

#prepping wordcloud2
word_list <- dplyr::pull(AF_dataset, Keyword)# pulling data from dataset
word_list <- Corpus(VectorSource(word_list))#creating a corpus
#cleaning corpus
word_list <- word_list %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
word_list <- tm_map(word_list, content_transformer(tolower))
word_list <- tm_map(word_list, removeWords, stopwords("english"))
dtm <- TermDocumentMatrix(word_list) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
#creating wordcloud
wordcloud2(data=df, size = 0.45, shape = 'square')

###############funnel chart

#subseting AF dataframe to create filter
mainpubdata <- AF_dataframe[-which(AF_dataframe$publisher_id == "K1122" | AF_dataframe$publisher_id == "K1123"),]

brandandgeodata <- AF_dataframe[-which(AF_dataframe$publisher_id == "K1122" | AF_dataframe$publisher_id == "K1123" | AF_dataframe$campaign == "Business Class" | AF_dataframe$campaign == "French Destinations" | 
                                         AF_dataframe$campaign == "General Terms" | AF_dataframe$campaign == "Google_Yearlong 2006" |
                                         AF_dataframe$campaign == "Outside Western Europe" | AF_dataframe$campaign == "Paris & France Terms" | 
                                         AF_dataframe$campaign == "Western Europe Destinations" |AF_dataframe$campaign == "Unassigned"),]

brandeddata <- AF_dataframe[which(AF_dataframe$campaign == "Air France Brand & French Destinations" | AF_dataframe$campaign == "Air France Branded" | AF_dataframe$campaign == "Air France Global Campaign"),]

#using filter to create four vector for each funnel level
alldatavect <- c("All Keywords", length(AF_dataframe[,4]), sum(AF_dataframe$amount),sum(AF_dataframe$total_cost))
Mainpub <- c("Main Publishers", length(mainpubdata[,4]), sum(mainpubdata$amount),sum(mainpubdata$total_cost))
brandandgeo <- c("Branded and Geotargeted", length(brandandgeodata[,4]), sum(brandandgeodata$amount),sum(brandandgeodata$total_cost))
branded <- c("Branded", length(brandeddata[,4]), sum(brandeddata$amount),sum(brandeddata$total_cost))

#creating funnel table
funnel_table <- rbind(alldatavect, Mainpub,brandandgeo,branded)#using rbind to put the 4 vector together
funnel_table <- as.data.frame(funnel_table)#transforming the matrix in a dataframe
funnel_table$V1 <- factor(funnel_table$V1, levels = c("All Keywords", "Main Publishers","Branded and Geotargeted","Branded"))#setting levels for the first variable
funnel_table$V2 <- as.numeric(funnel_table$V2)#transforming V2, V3 and V4 to numerical
funnel_table$V3 <- as.numeric(funnel_table$V3)
funnel_table$V4 <- as.numeric(funnel_table$V4)

#creating a funnel chart using plotly
fig <- plot_ly() 
fig <- fig %>%
  add_trace(
    type = "funnel",
    y = funnel_table[,1], 
    x = funnel_table[,2])

#showing the funnel graph
fig

#creating revenue graph for comparison with the funnel
revenue<- ggplot(data = funnel_table, aes (x = reorder(V1, V3) , y = V3))+
  geom_col(fill = "dark green")+
  geom_text(aes(x= V1, y = V3), label = round(funnel_table$V3), size = 5, hjust = 1, color = "white")
revenue + coord_flip() + theme_void()+ labs(title = "Revenues")

#creating cost graph for comparison with the funnel
cost<- ggplot(data = funnel_table)+
  geom_col(data = funnel_table, aes (x = reorder(V1, V4) , y = -V4), fill = "red")+
  geom_text(aes(x= V1, y = -V4), label = round(funnel_table$V4), size = 5, hjust =-0.05, color = "white")
  
cost + coord_flip()+ theme_void()+ labs(title = "Cost")

#Cost Graph

ggplot(data = new_publisher_final)+
  geom_col(aes(x = publisher_name, y = net_revenue), fill = "dark green")+
  geom_col(aes(x = publisher_name, y = total_cost), fill = "red")+
  geom_line(aes(x = publisher_name, y = 20000*ROA), color = "yellow", group = 1, size = 2)+
  theme_minimal()+
  geom_text(aes(x= publisher_name, y = net_revenue, label = round(net_revenue), vjust = -0.5), size = 3)+
  geom_text(aes(x= publisher_name, y = total_cost, label = round(total_cost), vjust = -0.5), size = 3)+
  scale_y_continuous(sec.axis = sec_axis(~./20000, name = "ROA"))

