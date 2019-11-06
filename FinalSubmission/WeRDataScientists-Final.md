#########################################################################################################
#
# WE R DATA SCIENTISTS
#
#    Scientist 1: Ram Yellepeddi
#    Scientist 2: Danny Kramer
#    Scientist 3: Rebecca Johnson
#    Scientist 4: Mahanthi Bukkapatnam
#
# Question we are trying to answer:
#    Who is the MALE Tennis G.O.A.T of the open era?
#
#########################################################################################################
```{r echo = FALSE, warning = FALSE, message = FALSE}
message("in WeRDataScientists.R")

# Clear workspace
rm(list = ls())

# Load the libraries
library(rvest)
library(readr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(eeptools)
library(data.table)

# Set the following flag to TRUE to download the Jeff Sackmann's files

download_from_internet <- FALSE

#########################################################################################################
# PART 1   Get rankings data from wikipedia
#########################################################################################################
#read in Wikipedia df
# library (rvest)
url <- "https://en.wikipedia.org/wiki/List_of_ATP_number_1_ranked_singles_tennis_players"
pg <- read_html(url)
tb <- html_table(pg, fill = TRUE)
df_numOfWeeksRanked1 <- tb [[4]]


#########################################################################################################
# PART 2    We explored the following dataset ;but was not used
#########################################################################################################

# df_atpWinLossPct <- read.csv("ATPWinLossPercentage.csv", stringsAsFactors = FALSE)


#########################################################################################################
# PART 3 : Download the Jeff Sackmann's files to current directory
#########################################################################################################

message( getwd())

if (download_from_internet == TRUE) {     # If the flag is TRUE, Lets download the files from Internet
  message("Downloading Jeff Sackmann files...START") 
  urlFirstPart <- "https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_"
  urlLastPart <- ".csv"
  
  for( year in 1973:2019 ) {              # Starting from the Open Era 1973 till the current year 2019
    url <- paste0(urlFirstPart,year,urlLastPart)
    downloadFileName <- paste0("jeffsackmann_",year,".csv")  #Form the full download URL
    
    message(paste0("downloading file: ", url))    
    
    df <- read.csv(url)                   # DOWNLOAD the file and store into a Data Frame 
    write.csv(df, file=downloadFileName)  # Save it to the local directory as a CSV file
  }
  message("Downloading Jeff Sackmann files...DONE!!!") 
} 

message("Reading Jeff Sackmann files...start") 

list_of_dataframes <- vector(mode = "list")

colNames <- c(      # Specifying the column names of the CSV columns
  "tourney_name",
  "surface",
  "tourney_level",
  "tourney_date",
  "winner_name",
  "winner_ioc",
  "winner_age",
  "loser_name",
  "loser_ioc",
  "loser_age",
  "score",
  "best_of",
  "round",
  "winner_rank",
  "loser_rank"
)

i = 1
for( year in 1973:2019) {
  message(paste0("    Reading file for year: ", year))
  
  df <- read.csv(paste0("jeffsackmann_",year,".csv")
                 ,stringsAsFactors = FALSE
  )
  
  df$tourney_name  <- as.character(df$tourney_name)
  df$surface       <- as.character(df$surface)
  df$tourney_level <- as.character(df$tourney_level)
  df$tourney_date  <- as.character(df$tourney_date)
  df$winner_name   <- as.character(df$winner_name)
  df$winner_ioc    <- as.character(df$winner_ioc)
  df$winner_age    <- as.numeric  (df$winner_age)
  df$loser_name    <- as.character(df$loser_name)
  df$loser_ioc     <- as.character(df$loser_ioc)
  df$loser_age     <- as.numeric  (df$loser_age)
  df$score         <- as.character(df$score)
  df$best_of       <- as.numeric  (df$best_of)
  df$round         <- as.character(df$round)
  df$winner_rank   <- as.integer  (df$winner_rank)
  df$loser_rank    <- as.integer  (df$loser_rank)
  
  df <- df[, colNames]
  
  list_of_dataframes[[i]] <- df    # Let us store the df into the list_of_dataframes array
  df <- NULL
  i <- i + 1
}


df_full <- bind_rows(list_of_dataframes)    # Merge all the dataframes for each year into one full dataframe
list_of_dataframes <- NULL

colnames(df_full)[colnames(df_full)=="winner_ioc"] <- "winner_country"  #rename this column to winner_country
colnames(df_full)[colnames(df_full)=="loser_ioc"]  <- "loser_country"   #rename this column to loser_country

#Change the following columns to factors/dates
df_full$surface           <- as.factor(df_full$surface)
df_full$tourney_level     <- as.factor(df_full$tourney_level)
df_full$round             <- as.factor(df_full$round)
df_full$tourney_date      <- as.Date(df_full$tourney_date,"%Y%m%d")
df_full$winner_country    <- as.factor(df_full$winner_country)
df_full$loser_country     <- as.factor(df_full$loser_country)

#Creating columns for key variables
df_full$won_grandslam_title <- ifelse( 
 (df_full$tourney_level == "G" & df_full$round=="F"),
 as.character(df_full$winner_name), 
 NA)
df_full$runnerup_grandslam_title <- ifelse( 
  (df_full$tourney_level == "G" & df_full$round=="F"),
  as.character(df_full$loser_name), 
  NA)

df_full$lost_grandslam_semi <- ifelse( 
  (df_full$tourney_level == "G" & df_full$round=="SF"),
  as.character(df_full$loser_name), 
  NA)

df_full$won_other_tour_title <- ifelse( 
  ((df_full$tourney_level == "M" | df_full$tourney_level == "A") & df_full$round=="F"),
  as.character(df_full$winner_name), 
  NA)

df_full$runnerup_other_tour_title <- ifelse( 
  ( (df_full$tourney_level == "M" | df_full$tourney_level == "A")  & df_full$round=="F"),
  as.character(df_full$loser_name), 
  NA)

df_full$lost_other_tour_semi <- ifelse( 
  ((df_full$tourney_level == "M" | df_full$tourney_level == "A") & df_full$round=="SF"),
  as.character(df_full$loser_name), 
  NA)

#We will now filter out the NAs for the key variables.
df_stat <- filter(df_full,( is.na(won_grandslam_title) == FALSE |
                            is.na(runnerup_grandslam_title) == FALSE |
                            is.na(lost_grandslam_semi) == FALSE |
                            is.na(won_other_tour_title) == FALSE |
                            is.na(runnerup_other_tour_title) == FALSE |
                            is.na(lost_other_tour_semi) == FALSE
                            ) )
#str(df_stat) 



#########################################################################################################
#
# Create new dataframes 
#     * for key variables, 
#     * add a new 'weight' column for each dataframe
#     * and assign a value for 'weight' column
#
#########################################################################################################

#GRAND SLAM TITLE
df_won_grandslam_title <- filter(data.frame(df_stat$won_grandslam_title), 
                          (is.na(df_stat.won_grandslam_title) == FALSE) )
names(df_won_grandslam_title)[names(df_won_grandslam_title)=="df_stat.won_grandslam_title"] <- "PlayerName"
df_won_grandslam_title$PlayerName <- as.character(df_won_grandslam_title$PlayerName)
df_won_grandslam_title$weight <- 1.6304


#GRAND SLAM RUNNERUP
df_runnerup_grandslam_title <- filter(data.frame(df_stat$runnerup_grandslam_title), 
                          (is.na(df_stat.runnerup_grandslam_title) == FALSE) )
names(df_runnerup_grandslam_title)[names(df_runnerup_grandslam_title)=="df_stat.runnerup_grandslam_title"] <- "PlayerName"
df_runnerup_grandslam_title$PlayerName <- as.character(df_runnerup_grandslam_title$PlayerName)
df_runnerup_grandslam_title$weight <- 0.9783


#df_finalist_grandslam <- bind_rows(df_won_grandslam_title,df_runnerup_grandslam_title)

#GRAND SLAM SEMI FINAL LOSERS
df_lost_grandslam_semi <- filter(data.frame(df_stat$lost_grandslam_semi), 
                                      (is.na(df_stat.lost_grandslam_semi) == FALSE) )
names(df_lost_grandslam_semi)[names(df_lost_grandslam_semi)=="df_stat.lost_grandslam_semi"] <- "PlayerName"
df_lost_grandslam_semi$PlayerName <- as.character(df_lost_grandslam_semi$PlayerName)
df_lost_grandslam_semi$weight <- 0.2935

#df_semifinalist_grandslam <- bind_rows(df_finalist_grandslam,df_lost_grandslam_semi)

#WON OTHER TOUR TITLE
df_won_other_tour_title <- filter(data.frame(df_stat$won_other_tour_title), 
                           (is.na(df_stat.won_other_tour_title) == FALSE) )
names(df_won_other_tour_title)[names(df_won_other_tour_title)=="df_stat.won_other_tour_title"] <- "PlayerName"
df_won_other_tour_title$PlayerName <- as.character(df_won_other_tour_title$PlayerName)
df_won_other_tour_title$weight <- 0.0323


#RUNNERUP OTHER TOUR TITLE
df_runnerup_other_tour_title <- filter(data.frame(df_stat$runnerup_other_tour_title), 
                                (is.na(df_stat.runnerup_other_tour_title) == FALSE) )
names(df_runnerup_other_tour_title)[names(df_runnerup_other_tour_title)=="df_stat.runnerup_other_tour_title"] <- "PlayerName"
df_runnerup_other_tour_title$PlayerName <- as.character(df_runnerup_other_tour_title$PlayerName)
df_runnerup_other_tour_title$weight <- 0.0194


#LOSERS OTHER TOUR SEMI FINALS 
df_lost_other_tour_semi <- filter(data.frame(df_stat$lost_other_tour_semi), 
                            (is.na(df_stat.lost_other_tour_semi) == FALSE) )                                                                                                                                                                           
names(df_lost_other_tour_semi)[names(df_lost_other_tour_semi)=="df_stat.lost_other_tour_semi"] <- "PlayerName"
df_lost_other_tour_semi$PlayerName <- as.character(df_lost_other_tour_semi$PlayerName)
df_lost_other_tour_semi$weight <- 0.0060    


#NUMBER OF WEEKS RANKED NUMBER ONE
df_numOfWeeksRanked1_new <- select(df_numOfWeeksRanked1, Player, Total)
df_numOfWeeksRanked1_new$Total <- df_numOfWeeksRanked1_new$Total * 0.0199
names(df_numOfWeeksRanked1_new)[names(df_numOfWeeksRanked1_new)=="Player"] <- "PlayerName"
names(df_numOfWeeksRanked1_new)[names(df_numOfWeeksRanked1_new)=="Total"] <- "weight"



################################################################################ 
#Grouping and Summarizing by SURFACE
################################################################################  

df_surface <- group_by(df_full, winner_name, surface)
summ <- summarize(df_surface, num_matches = n())
summ <- reshape2::dcast(summ, winner_name ~ surface, value.var = "num_matches")
message(summ)


#########################################################################################################
#
# Create new dataframes for each surface
#     * for surface variables: Clay, Hard, Grass
#     * add a new 'weight' column for each dataframe
#     * and assign a value for 'weight' column
#
#########################################################################################################

df_summ_temp <- select(summ,winner_name, Clay)
df_summ_clay <- subset(df_summ_temp, !is.na(Clay))
names(df_summ_clay)[names(df_summ_clay)=="winner_name"] <- "PlayerName"
names(df_summ_clay)[names(df_summ_clay)=="Clay"] <- "weight"
df_summ_clay$weight <- df_summ_clay$weight * 0.0007

df_summ_temp <- select(summ,winner_name, Hard)
df_summ_hard <- subset(df_summ_temp, !is.na(Hard))
names(df_summ_hard)[names(df_summ_hard)=="winner_name"] <- "PlayerName"
names(df_summ_hard)[names(df_summ_hard)=="Hard"] <- "weight"
df_summ_hard$weight <- df_summ_hard$weight * 0.0008

df_summ_temp <- select(summ,winner_name, Grass)
df_summ_grass <- subset(df_summ_temp, !is.na(Grass))
names(df_summ_grass)[names(df_summ_grass)=="winner_name"] <- "PlayerName"
names(df_summ_grass)[names(df_summ_grass)=="Grass"] <- "weight"
df_summ_grass$weight <- df_summ_grass$weight * 0.0029


#########################################################################################################
#
#  At this point we have created a dataframe (2 columns: 'PlayerName' and 'weight') for following
#  key variables:
#      won_grandslam_title,
#      runnerup_grandslam_title,
#      lost_grandslam_semi,
#      won_other_tour_title,
#      runnerup_other_tour_title,
#      lost_other_tour_semi,
#      numOfWeeksRanked1_new,
#      summ_hard,
#      summ_grass,
#      summ_clay
#
#  We will merge all these dataframes into a single dataframe for final calculation.
#
#  This final data frame will act as an INDEX for finding the G.O.A.T tennis player.
#
#########################################################################################################

df_index <- bind_rows(df_won_grandslam_title,
                      df_runnerup_grandslam_title,
                      df_lost_grandslam_semi,
                      df_won_other_tour_title,
                      df_runnerup_other_tour_title,
                      df_lost_other_tour_semi,
                      df_numOfWeeksRanked1_new,
                      df_summ_hard,
                      df_summ_grass,
                      df_summ_clay
)

# Group by Player Name and compute the Total Weight for each Player to rank the players
df_index_g <- group_by(df_index, PlayerName )
summ_index_g <- summarize(df_index_g, total_weight = sum(weight))  

```
########################################################################################
```{r echo = FALSE, warning = FALSE, message = FALSE}
#library(ggplot2)


########################################################################################
# fp1: Plot of Players with most weeks ranked at number 1
########################################################################################

df_subset_numOfWeeksRanked1 <- subset(df_numOfWeeksRanked1, Rank <= 5)
fp1 <- qplot(reorder(Player, -Total), Total, data = df_subset_numOfWeeksRanked1, 
             geom = "point", main = "Players with most Weeks Ranked #1", 
             xlab = "Player", ylab = "# of Weeks", color = Player, size = I(6))
fp1 <- fp1+theme(axis.text.x=element_text(angle=90))

ggsave(filename = "fp1.png", plot = fp1, width = 6, height=4,dpi = 600)
```
 ```{r, echo=FALSE}
plot(fp1)
```

```{r echo = FALSE, warning = FALSE, message = FALSE}

########################################################################################
# fp2: Plot for number of matches by surface
########################################################################################

df_subset_surface <- subset(df_full, 
                            surface=="Grass" | 
                            surface=="Clay" | 
                            surface =="Hard")

df_subset_surface_g <- select(df_subset_surface, surface)

df_subset_surface_s <- group_by(df_subset_surface_g, surface) %>%
                      summarize(count=n())

fp2 <- ggplot(df_subset_surface_s, aes(x=reorder(surface,-count), y=count, fill=surface)) + 
  geom_bar(position = 'dodge', stat='identity') +
  geom_text(aes(label=count), position=position_dodge(width=0.9), vjust=-0.25) +
  ggtitle("Number of Matches by Surface") +
  labs(x="Surface",y="# of Matches")

ggsave(filename = "fp2.png", plot = fp2, width = 6, height=4,dpi = 600)
```


```{r, echo=FALSE}
plot(fp2)
```

```{r echo = FALSE, warning = FALSE, message = FALSE}
########################################################################################
# fp3: Plot for number of matches by tourney level
########################################################################################

df_subset_tourney_level <- subset(df_full, 
                                  tourney_level=="G" | 
                                  tourney_level=="M" | 
                                  tourney_level=="A")

df_subset_tourney_level_g <- select(df_subset_tourney_level, tourney_level)

df_subset_tourney_level_s <- group_by(df_subset_tourney_level_g, tourney_level) %>%
                          summarize(count=n())

fp3 <- ggplot(df_subset_tourney_level_s, aes(x=reorder(tourney_level,-count), y=count, fill=tourney_level)) + 
  geom_bar(position = 'dodge', stat='identity') +
  geom_text(aes(label=count), position=position_dodge(width=0.9), vjust=-0.25) +
  ggtitle("Number of Matches by Tourney Level") +
  labs(x="Tourney Level",y="# of Matches")

ggsave(filename = "fp3.png", plot = fp3, width = 6, height=4,dpi = 600)
```

```{r, echo=FALSE}
plot(fp3)
```

```{r echo = FALSE, warning = FALSE, message = FALSE}
########################################################################################
# fp4: Plot for number of match wins by player and surface
########################################################################################

df_subset_topplayers_surf <- subset(df_full, 
                               (winner_name=="Novak Djokovic" 
                              | winner_name=="Rafael Nadal" 
                              | winner_name=="Jimmy Connors" 
                              | winner_name=="Ivan Lendl" 
                              | winner_name=="Andre Agassi" 
                              | winner_name=="Roger Federer" 
                              | winner_name=="Pete Sampras") &
                               (surface=="Clay" 
                              | surface=="Grass" 
                              | surface=="Hard"))

df_subset_topplayers_surf_g <- select(df_subset_topplayers_surf, winner_name, surface)

df_subset_topplayers_surf_s <- group_by(df_subset_topplayers_surf_g, winner_name, surface) %>%
                               summarize(count=n())

fp4 <- ggplot(df_subset_topplayers_surf_s, aes(x=reorder(winner_name,-count), y=count, fill=surface)) + 
  geom_bar(aes(label=count), stat='identity') +
  geom_text(aes(label=count), position=position_stack(vjust=1, reverse = FALSE)) +
  ggtitle("# of Match Wins by Player and by Surface") +
  theme(axis.text.x=element_text(angle=90)) +
  labs(x="Player",y="# of Match wins")

ggsave(filename = "fp4.png", plot = fp4, width = 6, height=4,dpi = 600)
```

```{r, echo=FALSE}
plot(fp4)
```

```{r echo = FALSE, warning = FALSE, message = FALSE}
########################################################################################
# fp5: Plot for number of titles by top players
########################################################################################

df_subset_topplayers <- subset(df_full, 
                                ( winner_name=="Novak Djokovic" 
                                | winner_name=="Rafael Nadal" 
                                | winner_name=="Jimmy Connors" 
                                | winner_name=="Ivan Lendl" 
                                | winner_name=="Andre Agassi" 
                                | winner_name=="Roger Federer" 
                                | winner_name=="Pete Sampras") &
                                  (round=="F"))

df_subset_topplayers_g <- select(df_subset_topplayers,winner_name)
df_subset_topplayers_s <- group_by(df_subset_topplayers_g,winner_name) %>%
                        summarize(count=n())

fp5 <- ggplot(df_subset_topplayers_s, aes(x=reorder(winner_name,-count), y=count, fill=count)) + 
  geom_bar(position = 'dodge', stat='identity') +
  geom_text(aes(label=count), position=position_dodge(width=0.9), vjust=-0.25) +
  ggtitle("Number of Titles by Player") +
  theme(axis.text.x=element_text(angle=90)) +
  labs(x="Player",y="# of Titles")

ggsave(filename = "fp5.png", plot = fp5, width = 6, height=4,dpi = 600)
```

```{r, echo=FALSE}
plot(fp5)
```

```{r echo = FALSE, warning = FALSE, message = FALSE}
########################################################################################
# fp6: Plot for Number of titles by top players by tourney type
########################################################################################

df_subset_topplayersbytitlesandtourneytype <- 
                        subset(df_full, 
                              (winner_name =="Novak Djokovic" 
                              | winner_name=="Rafael Nadal" 
                              | winner_name=="Jimmy Connors" 
                              | winner_name=="Ivan Lendl" 
                              | winner_name=="Andre Agassi" 
                              | winner_name=="Roger Federer" 
                              | winner_name=="Pete Sampras")  &
                              (round == "F")  &
                              (tourney_level=="G" | 
                               tourney_level=="A" |
                               tourney_level=="M"))
df_temp_topplyr_tourneys <- select(df_subset_topplayersbytitlesandtourneytype,winner_name,tourney_level)
df_temp_topplyr_tourneys_s <- group_by(df_temp_topplyr_tourneys,winner_name,tourney_level) %>%
                        summarize(count=n())


fp6 <- ggplot(df_temp_topplyr_tourneys_s, aes(x=reorder(winner_name,-count), y=count, fill=tourney_level)) + 
  geom_bar(aes(label=count), stat='identity') +
  geom_text(aes(label=count), position=position_stack(vjust=1, reverse = FALSE)) +
  ggtitle("Number of Titles by Player and Tourney Type") +
  theme(axis.text.x=element_text(angle=90)) +
  labs(x="Player",y="# of Titles")

ggsave(filename = "fp6.png", plot = fp6, width = 6, height=4,dpi = 600)
```

```{r, echo=FALSE}
plot(fp6)
```

```{r echo = FALSE, warning = FALSE, message = FALSE}
########################################################################################
# fp7: Plot for number of match wins by country
########################################################################################
df_temp_country <- select(df_full,winner_country)
df_temp_country_s <- group_by(df_temp_country, winner_country) %>%
                     summarize(count=n())

df_subset_country_s <- subset(df_temp_country_s, 
                            winner_country == "USA" | 
                            winner_country == "AUS" | 
                            winner_country == "FRA" |
                            winner_country == "RUS" | 
                            winner_country == "SWE" |
                            winner_country == "ESP" |
                            winner_country == "ARG")

fp7 <- ggplot(df_subset_country_s, aes(x=reorder(winner_country,-count), y=count, fill=winner_country)) + 
         geom_bar(position = 'dodge', stat='identity') +
         geom_text(aes(label=count), position=position_dodge(width=0.9), vjust=-0.25) +
         ggtitle("Number of Match Wins by Country") +
         labs(x="Country",y="# of Matches")

ggsave(filename = "fp7.png", plot = fp7, width = 10, height=6,dpi = 600)
```

```{r, echo=FALSE}
plot(fp7)
```
            
            
```{r echo = FALSE, warning = FALSE, message = FALSE}
########################################################################################
# Plot for TOP 10 Players from our INDEX dataframe (df_index_g) 
#
########################################################################################

#library(data.table)
df_our_ranks <- df_index_g
setDT(df_our_ranks)
df_our_ranks = df_our_ranks[ , .(total = sum(weight)), by = .(PlayerName)]
df_our_ranks$Rank <- ave( -df_our_ranks$total, FUN=rank )
# df_our_ranks 

df_T10_our_ranks <- top_n(df_our_ranks, 10, total)
df_T10_our_ranks <- df_T10_our_ranks[order(df_T10_our_ranks$Rank), ]

theme_set(theme_bw())

fp8 <-ggplot(df_T10_our_ranks, aes(x=reorder(PlayerName, total),y=Rank, label=Rank)) + 
  geom_point(stat="identity", size=10)+ 
  scale_color_manual(name="Rank") + 
  scale_fill_brewer(palette = "Greens")+
  theme(panel.background = element_rect(fill = "#31a354",size = 2, linetype = "solid"))+
  theme(plot.background = element_rect(fill = "#31a354"))+
  geom_segment(aes(y = 0, x = PlayerName, yend = Rank, xend = PlayerName))+
  theme(panel.border = element_blank())+
  coord_flip()+
  geom_text(color="white", size=6)+
  theme(axis.title.y = element_blank())+
  theme(axis.text.y = element_text(size = 20))+
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_blank())+
  theme(axis.ticks.y = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(panel.grid = element_blank())

ggsave(filename = "fp8.png", plot = fp8, width = 10, height=6,dpi = 600)
```

```{r, echo=FALSE}
plot(fp8)
```

```{r echo = FALSE, warning = FALSE, message = FALSE}
#########################################################################################################
#
# Predictions for the next 5 years (at the end of 2024 who would have the highest number of Grand Slams)
#
########################################################################################################

#library(eeptools)

# read in csv file with recent Grand Slam results for top 3 current players
df_big3recentGrandSlamResults <- read.csv("big3recentGrandSlamResults.csv", stringsAsFactors = FALSE)

#change column name 
names(df_big3recentGrandSlamResults)[names(df_big3recentGrandSlamResults)=="ï..Player"]<- "Player"

#remove empty rows from csv
df_big3recentGrandSlamResults <- df_big3recentGrandSlamResults[complete.cases(df_big3recentGrandSlamResults),]

#change dob (date of birth) column to date class
df_big3recentGrandSlamResults$dob <- as.Date(df_big3recentGrandSlamResults$dob)


#calculate age in years for 3 players
DjokovicBirthday <- df_big3recentGrandSlamResults[1,"dob"]
DjokovicAge <- age_calc(DjokovicBirthday, units='years',precise = TRUE)
DjokovicAgeyears <- round(DjokovicAge)
  
NadalBirthday <- df_big3recentGrandSlamResults[2,"dob"]
NadalAge <- age_calc(NadalBirthday, units='years',precise = TRUE)
NadalAgeyears <- round(NadalAge)

FedererBirthday <- df_big3recentGrandSlamResults[3,"dob"]
FedererAge <- age_calc(FedererBirthday, units='years',precise = TRUE)
FedererAgeyears <- round(FedererAge)


#complete calculation to prepare for prediction formula
d <- DjokovicAgeyears-27
n <- NadalAgeyears-27
f <- FedererAgeyears-27

#initialize data vectors
Djokovicdata <- c()
Nadaldata <- c()
Federerdata <- c()

#enter data into data vectors for each player
Djokovicdata <- df_big3recentGrandSlamResults[1,3:8]
Nadaldata <- df_big3recentGrandSlamResults[2,3:8]
Federerdata <- df_big3recentGrandSlamResults[3,3:8]

Djokovicdata <- as.numeric(append(Djokovicdata,d))
Nadaldata <- as.numeric(append(Nadaldata,n))
Federerdata <- as.numeric(append(Federerdata,f))

#create points vector for use in calculation
points <- c(15, 30, 90, 6, 12, 36, -8)

#calculate points for each player
DjokovicPoints <- Djokovicdata*points
NadalPoints <- Nadaldata*points
FedererPoints <- Federerdata*points

#create a function to add the contents for each players' points vector
myfunc <- function(vector){
    total <- 0
  for(i in vector)
  total <- i + total
  total
}


#########################################################################
#
# Calculate the predicted number of future Grand Slams over 
# next 5 years for top 3 players and message results
#
#########################################################################

DjokovicPredSlams <- (myfunc(DjokovicPoints))/100
message(DjokovicPredSlams)
NadalPredSlams <- (myfunc(NadalPoints))/100
message(NadalPredSlams)
FedererPredSlams <- (myfunc(FedererPoints))/100
message(FedererPredSlams)
```

