library(tidyverse)
library(rvest)



########### test for scrapping ############
html1 <- read_html("https://www.espncricinfo.com/series/world-t20-2015-16-901359/england-vs-west-indies-15th-match-super-10-group-1-951331/full-scorecard")
a <- html1 %>% html_elements(".ds-text-tight-m.ds-font-regular.ds-text-ui-typo-mid") %>% html_text()
a[2]
b <- html1 %>% html_elements(".ds-text-compact-m.ds-text-typo-title.ds-text-right.ds-whitespace-nowrap") %>% html_text()
b[2]
v <- unlist (gregexpr (',', a[1]))
substring(a[1],v[3]+1,v[4]-1)
substring(a[1],v[2]+1,v[3]-1)
stats1 <- html1 %>% html_table()
stats1[[5]][[2]][[2]]
#######################################


html <- read_html("https://www.espncricinfo.com/series/world-t20-2015-16-901359/match-schedule-fixtures-and-results")
link <- html %>% html_nodes("[class='ds-no-tap-higlight']") %>% html_attr("href")
n <- length(link)
url <- numeric(length = 35)
for(i in 1:35)
{
  url[i] <- paste("https://www.espncricinfo.com",link[n-35+i],sep = "")
}

##################### test for scrapping #############
read_html(url[1]) %>% html_elements(".ds-text-tight-m.ds-font-regular.ds-text-ui-typo-mid") %>% html_text()
field <- read_html(url[1]) %>% html_table()  
field[[5]][[1]][[1]]  
###################################


stadium <- numeric(length = 35)
for(i in 1:35){
  field <- read_html(url[i]) %>% html_table()
  stadium[i] <- field[[5]][[1]][[1]]
} 
  
f7 <- read_html(url[7]) %>% html_table()
stadium[7] <- f7[[3]][[1]][[1]]
f8 <- read_html(url[8]) %>% html_table()
stadium[8] <- f8[[3]][[1]][[1]]
  
unique_stadium <- unique(stadium)

#### for the first match data scrapping ####
dat <- read_html(url[1]) %>% html_table()
toss <- dat[[5]][[2]][[2]]
scorecard <- read_html(url[1]) %>% html_elements(".ds-text-compact-m.ds-text-typo-title.ds-text-right.ds-whitespace-nowrap strong") %>% html_text()
team1_scorecard <- scorecard[1]
team2_scorecard <- scorecard[2]
team <- read_html(url[1]) %>% html_elements(".ds-text-tight-l.ds-font-bold.ds-text-ui-typo") %>% html_text()
team1 <- team[1]
team2 <- team[2]
Reasult <- read_html(url[1]) %>% html_elements(".ds-text-tight-m.ds-font-regular.ds-truncate.ds-text-typo-title span") %>% html_text()
POM <- dat[[5]][[2]][[5]]
a <- read_html(url[1]) %>% html_elements(".ds-text-tight-m.ds-font-regular.ds-text-ui-typo-mid") %>% html_text()
v <- unlist (gregexpr (',', a[1]))
Date <- paste(substring(a[1],v[3]+1,v[4]-1),substring(a[1],v[4]+1,v[5]-1),sep = "")

###############################################


###################### scrapping all the matches ##############

Team1 = Team2 = Date = Toss = Team1_scorecard = Team2_scorecard = Reasult = POM = array(0)

for (i in 1:35) {
  print(paste("**starting",i))
  team <- read_html(url[i]) %>% html_elements(".ds-text-tight-l.ds-font-bold.ds-text-ui-typo") %>% html_text()
  Team1[i] <- team[1]
  Team2[i] <- team[2]
  a <- read_html(url[i]) %>% html_elements(".ds-text-tight-m.ds-font-regular.ds-text-ui-typo-mid") %>% html_text()
  v <- unlist (gregexpr (',', a[1]))
  Date[i] <- substring(a[1],v[3]+1,v[5]-1)
  scorecard <- read_html(url[i]) %>% html_elements(".ds-text-compact-m.ds-text-typo-title.ds-text-right.ds-whitespace-nowrap strong") %>% html_text()
  Team1_scorecard[i] <- scorecard[1]
  Team2_scorecard[i] <- scorecard[2]
  Reasult[i] <- read_html(url[i]) %>% html_elements(".ds-text-tight-m.ds-font-regular.ds-truncate.ds-text-typo-title span") %>% html_text()
  dat <- read_html(url[i]) %>% html_table()
  Toss[i] <- dat[[5]][[2]][[2]]
  POM[i] <- dat[[5]][[2]][[5]]
}

for (i in 33:35) {
  a <- read_html(url[i]) %>% html_elements(".ds-text-tight-m.ds-font-regular.ds-text-ui-typo-mid") %>% html_text()
  v <- unlist (gregexpr (',', a[1]))
  Date[i] <- substring(a[1],v[2]+1,v[4]-1)
  
}
T20_worldcup_2016 <- data.frame(1:35 , Team1 , Team2 , Date , stadium , 
                                Toss , Team1_scorecard , Team2_scorecard , Reasult , POM )


view(T20_worldcup_2016)

save(T20_worldcup_2016,file = "T20_Worldcup_2016.Rdata")


##################################################
attach(T20_worldcup_2016)


################################################# data cleaning #############
T20_wc_2016 <- T20_worldcup_2016[-c(7,8),]



x <- substring(T20_wc_2016$Toss , 1 , 3)

library(dplyr)

Toss_Winner <- recode(x, Hon = "Hong Kong", Afg = "Afghanistan", Net = "Netherlands", Ire= "Ireland", Zim = "Zimbabwe", 
                      Oma = "Oman", New = "New Zealand", Pak = "Pakistan", Wes = "West Indies", Eng = "England", 
                      Ind = "India", Sou = "South Africa", Aus = "Australia", Ban = "Bangladesh", Sri = "Sri Lanka")


y <- str_sub(T20_wc_2016$Toss,-9,-7)

Toss_decision <- recode(y, eld = "Field first", bat = "Bat first")

Team1_runs <- numeric(length = 33)

for (i in 1:33) {
  if(nchar(T20_wc_2016$Team1_scorecard[i]) > 3){
    Team1_runs[i] <- substring(T20_wc_2016$Team1_scorecard[i],1,3)
  }else{
    Team1_runs[i] <- substring(T20_wc_2016$Team1_scorecard[i],1,)
  } 
}
Team1_runs[9] = "59"
Team1_Runs <- as.numeric(Team1_runs) 

Team1_wic <- numeric(length = 33)

for (i in 1:33) {
  if(nchar(T20_wc_2016$Team1_scorecard[i]) > 3){
    Team1_wic[i] <- str_sub(T20_wc_2016$Team1_scorecard[i],-1,-1)
  }else{
    Team1_wic[i] <- "10"
  } 
}

Team1_Wickets <- as.numeric(Team1_wic)

Team2_runs <- numeric(length = 33)

for (i in 1:33) {
  if(nchar(T20_wc_2016$Team2_scorecard[i]) > 3){
    Team2_runs[i] <- substring(T20_wc_2016$Team2_scorecard[i],1,3)
  }else{
    Team2_runs[i] <- substring(T20_wc_2016$Team2_scorecard[i],1,)
  } 
}

Team2_runs[8:10] <- c("78","47","65")

Team2_Runs <- as.numeric(Team2_runs) 

Team2_wic <- numeric(length = 33)

for (i in 1:33) {
  if(nchar(T20_wc_2016$Team2_scorecard[i]) > 3){
    Team2_wic[i] <- str_sub(T20_wc_2016$Team2_scorecard[i],-1,-1)
  }else{
    Team2_wic[i] <- "10"
  } 
}
Team2_Wickets <- as.numeric(Team2_wic)

Match_Winner <- recode(substring(T20_wc_2016$Reasult , 1 , 3),Hon = "Hong Kong", Afg = "Afghanistan", Net = "Netherlands", Ire= "Ireland", Zim = "Zimbabwe", 
                       Oma = "Oman", New = "New Zealand", Pak = "Pakistan", Wes = "West Indies", Eng = "England", 
                       Ind = "India", Sou = "South Africa", Aus = "Australia", Ban = "Bangladesh", Sri = "Sri Lanka",Sco = "Scotland")

df <- T20_wc_2016[,-c(6,7,8,9,10)]

POM <- T20_worldcup_2016$POM[-c(7,8)]

T20_WorldCup_2016 <- as.data.frame(cbind(df,Toss_Winner,Toss_decision,Team1_Runs,Team1_Wickets,Team2_Runs,
                                         Team2_Wickets,POM))

T20_WorldCup_2016 <- T20_WorldCup_2016 %>% mutate(Match_Winner, .before = POM)

Matches <- c(T20_WorldCup_2016$Team1,T20_WorldCup_2016$Team2)
table(Matches)

view(T20_WorldCup_2016)

save(T20_WorldCup_2016,file = "T20_WorldCup_2016_1.Rdata")

attach(T20_WorldCup_2016)

view(T20_WorldCup_2016)

###################### India Matches ################

India_Matches <- T20_WorldCup_2016 [ T20_WorldCup_2016$Team1 == "India" | T20_WorldCup_2016$Team2== "India",]
view(India_Matches)
save(India_Matches,file = "INDIA.Rdata")

######################## IND vs NZ match ######################

sc <- read_html(url[13]) %>% html_table()
df13_I <- data.frame(sc[[3]][[1]],sc[[3]][[3]])[-c(seq(2,20,2),23,24),]
colnames(df13_I) <- c("Players", "Runs")

df13_I$Runs <- as.numeric(df13_I$Runs)

view(df13_I)



pie(ind_nz_sc$runs,labels = ind_nz_sc$runs,main = "India vs NZ India's Innings"
    ,col = rainbow(length(ind_nz_sc$runs)))

legend("topright",ind_nz_sc$players,cex = 0.8,fill = rainbow(length(ind_nz_sc$runs)))


#########################IND vs PAK match ###########


sc <- read_html(url[19]) %>% html_table()
df19_I <- data.frame(sc[[3]][[1]],sc[[3]][[3]])[-c(2,4,7,9,12,13,14),]
colnames(df19_I) <- c("Players", "Runs")
  
df19_I$Runs <- as.numeric(df19_I$Runs)

Ind_Mat[[3]]$Runs
  
pie(Ind_Mat[[3]]$Runs,labels = Ind_Mat[[3]]$Runs,main = "India vs PAk India's Innings"
    ,col = rainbow(length(Ind_Mat[[3]]$Runs)))

legend("topright",Ind_Mat[[3]]$Players,cex = 0.8,fill = rainbow(length(Ind_Mat[[3]]$Runs)))





#########################IND vs Bang match ###########
  
  
  sc <- read_html(url[25]) %>% html_table()
  df25_I <- data.frame(sc[[1]][[1]],sc[[1]][[3]])[-c(2,4,6,8,10,13,15,18,19,20),]
  colnames(df25_I) <- c("Players", "Runs")
  
  df25_I$Runs <- as.numeric(df25_I$Runs)


#########################IND vs Aus match ###########
  
  
sc <- read_html(url[31]) %>% html_table()
df31_I <- data.frame(sc[[3]][[1]],sc[[3]][[3]])[-c(2,4,7,9,12,13,14),]
colnames(df31_I) <- c("Players", "Runs")
  
df31_I$Runs <- as.numeric(df31_I$Runs)

view(df31_I) 

save(df31,file = paste0("ind_aus",".Rdata"))

#########################IND vs Aus match ###########


sc <- read_html(url[31]) %>% html_table()
df31 <- data.frame(sc[[3]][[1]],sc[[3]][[3]])[-c(2,4,7,9,12,13,14),]
colnames(df31) <- c("Players", "Runs")

df31$Runs <- as.numeric(df31$Runs)

view(df31) 

save(df31,file = paste0("ind_aus",".Rdata"))

#########################IND vs WI match ###########


sc <- read_html(url[34]) %>% html_table()
df34_I <- data.frame(sc[[1]][[1]],sc[[1]][[3]])[-c(2,4,8,9,10),]
colnames(df34_I) <- c("Players", "Runs")

df34_I$Runs <- as.numeric(df34_I$Runs)

view(df34_I) 

save(df34,file = paste0("ind_wi",".Rdata"))



Ind_Mat <- list(ind_nz = df13_I , ind_pak = df19_I , ind_bang = df25_I , ind_aus = df31_I , ind_wi = df34_I)
Ind_Mat
save(Ind_Mat , file = "India_match.Rdata")



###################### WI Matches ################

Wi_Matches <- T20_WorldCup_2016 [T20_WorldCup_2016$Team1 == "West Indies" | T20_WorldCup_2016$Team2== "West Indies",]
view(Wi_Matches)
save(Wi_Matches,file = "WI.Rdata")


#15

sc <- read_html(url[15]) %>% html_table() 
df15_W <- data.frame(sc[[3]][[1]],sc[[3]][[3]])[-c(2,5,7,9,12,13,14),]
colnames(df15_W) <- c("Players", "Runs")
df15_W$Runs <- as.numeric(df15_W$Runs)
view(df15_W)


#21

sc <- read_html(url[21]) %>% html_table() 
df21_W <- data.frame(sc[[3]][[1]],sc[[3]][[3]])[-c(3,5,7,10,11,12),]
colnames(df21_W) <- c("Players", "Runs")
df21_W$Runs <- as.numeric(df21_W$Runs)
view(df21_W)


#27

sc <- read_html(url[27]) %>% html_table() 
df27_W <- data.frame(sc[[3]][[1]],sc[[3]][[3]])[-c(seq(2,14,2),18,19,20),]
colnames(df27_W) <- c("Players", "Runs")
df27_W$Runs <- as.numeric(df27_W$Runs)
view(df27_W)

#30

sc <- read_html(url[30]) %>% html_table() 
df30_W <- data.frame(sc[[3]][[1]],sc[[3]][[3]])[-c(2,4,7,9,11,13,15,17,20,21,22),]
colnames(df30_W) <- c("Players", "Runs")
df30_W$Runs <- as.numeric(df30_W$Runs)
view(df30_W)

#34

sc <- read_html(url[34]) %>% html_table() 
df34_W <- data.frame(sc[[3]][[1]],sc[[3]][[3]])[-c(2,4,6,10,11,12),]
colnames(df34_W) <- c("Players", "Runs")
df34_W$Runs <- as.numeric(df34_W$Runs)
view(df34_W)

#35

sc <- read_html(url[35]) %>% html_table() 
df35_W <- data.frame(sc[[3]][[1]],sc[[3]][[3]])[-c(2,4,7,9,11,13,16,17,18),]
colnames(df35_W) <- c("Players", "Runs")
df35_W$Runs <- as.numeric(df35_W$Runs)
view(df35_W)


Wi_Mat <- list(eng_wi = df15_W , sl_wi = df21_W , sa_wi = df27_W , afg_wi = df30_W , wi_ind_semifinal = df34_W , wi_eng_final = df35_W)
Wi_Mat
save(Wi_Mat , file = "WestIndies_match.Rdata")


###################### NZ Matches ################

nz_Matches <- T20_WorldCup_2016 [T20_WorldCup_2016$Team1 == "New Zealand" | T20_WorldCup_2016$Team2== "New Zealand",]
view(nz_Matches)
save(nz_Matches,file = "NZ.Rdata")

#13

sc <- read_html(url[13]) %>% html_table() 
df13_NZ <- data.frame(sc[[1]][[1]],sc[[1]][[3]])[-c(seq(2,14,2),18,19,20),]
colnames(df13_NZ) <- c("Players", "Runs")
df13_NZ$Runs <- as.numeric(df13_NZ$Runs)
view(df13_NZ)

#17

sc <- read_html(url[17]) %>% html_table() 
df17_NZ <- data.frame(sc[[1]][[1]],sc[[1]][[3]])[-c(seq(2,16,2),19,20,21),]
colnames(df17_NZ) <- c("Players", "Runs")
df17_NZ$Runs <- as.numeric(df17_NZ$Runs)
view(df17_NZ)

#23

sc <- read_html(url[23]) %>% html_table() 
df23_NZ <- data.frame(sc[[1]][[1]],sc[[1]][[3]])[-c(2,4,6,8,11,14,15,16),]
colnames(df23_NZ) <- c("Players", "Runs")
df23_NZ$Runs <- as.numeric(df23_NZ$Runs)
view(df23_NZ)

#28

sc <- read_html(url[28]) %>% html_table() 
df28_NZ <- data.frame(sc[[1]][[1]],sc[[1]][[3]])[-c(seq(2,12,2),15,17,20,21,22),]
colnames(df28_NZ) <- c("Players", "Runs")
df28_NZ$Runs <- as.numeric(df28_NZ$Runs)
view(df28_NZ)

#33

sc <- read_html(url[33]) %>% html_table() 
df33_NZ <- data.frame(sc[[1]][[1]],sc[[1]][[3]])[-c(seq(2,12,2),15,17,19,20,21),]
colnames(df33_NZ) <- c("Players", "Runs")
df33_NZ$Runs <- as.numeric(df33_NZ$Runs)
view(df33_NZ)


nz_Mat <- list(nz_ind = df13_NZ , nz_aus = df17_NZ , nz_pak = df23_NZ , nz_bang = df28_NZ , nz_eng_semifinal = df33_NZ)
nz_Mat
save(nz_Mat , file = "NewZealand_match.Rdata")

###################### Eng Matches ################

eng_Matches <- T20_WorldCup_2016 [T20_WorldCup_2016$Team1 == "England" | T20_WorldCup_2016$Team2== "England",]
view(eng_Matches)
save(eng_Matches,file = "ENG.Rdata")

#15

sc <- read_html(url[15]) %>% html_table() 
df15_ENG <- data.frame(sc[[1]][[1]],sc[[1]][[3]])[-c(seq(2,8,2),11,13,15,16,17),]
colnames(df15_ENG) <- c("Players", "Runs")
df15_ENG$Runs <- as.numeric(df15_ENG$Runs)
view(df15_ENG)

#18

sc <- read_html(url[18]) %>% html_table() 
df18_ENG <- data.frame(sc[[3]][[1]],sc[[3]][[3]])[-c(seq(2,12,2),15,17,20,21,22),]
colnames(df18_ENG) <- c("Players", "Runs")
df18_ENG$Runs <- as.numeric(df18_ENG$Runs)
view(df18_ENG)

#24

sc <- read_html(url[24]) %>% html_table() 
df24_ENG <- data.frame(sc[[1]][[1]],sc[[1]][[3]])[-c(seq(2,12,2),15,18,19,20),]
colnames(df24_ENG) <- c("Players", "Runs")
df24_ENG$Runs <- as.numeric(df24_ENG$Runs)
view(df24_ENG)

#29

sc <- read_html(url[29]) %>% html_table() 
df29_ENG <- data.frame(sc[[1]][[1]],sc[[1]][[3]])[-c(seq(2,6,2),9,12,13,14),]
colnames(df29_ENG) <- c("Players", "Runs")
df29_ENG$Runs <- as.numeric(df29_ENG$Runs)
view(df29_ENG)

#33

sc <- read_html(url[33]) %>% html_table() 
df33_ENG <- data.frame(sc[[3]][[1]],sc[[3]][[3]])[-c(2,4,7,10,11,12),]
colnames(df33_ENG) <- c("Players", "Runs")
df33_ENG$Runs <- as.numeric(df33_ENG$Runs)
view(df33_ENG)

#35

sc <- read_html(url[35]) %>% html_table() 
df35_ENG <- data.frame(sc[[1]][[1]],sc[[1]][[3]])[-c(seq(2,14,2),17,19,22,23),]
colnames(df35_ENG) <- c("Players", "Runs")
df35_ENG$Runs <- as.numeric(df35_ENG$Runs)
view(df35_ENG)


eng_Mat <- list(wi_eng = df15_ENG , eng_sa = df18_ENG , eng_afg = df24_ENG , eng_sl = df29_ENG , eng_nz_semifinal = df33_ENG , eng_wi_final = df35_ENG)
eng_Mat
save(eng_Mat,file = "England_Matches.Rdata")


############################### top batsmen & Bowlers ##########################

html <- read_html("https://stats.espncricinfo.com/icc-world-twenty20-2016/engine/records/batting/most_runs_career.html?id=10291;type=tournament")

Most_runs <- html %>% html_table()

most_runs_table <- as.data.frame(Most_runs[[1]])
save(most_runs_table,file = "Top_batsmen.Rdata")
view(most_runs_table)
top5_batsman <- most_runs_table[1:5,]

top10_batsman <- most_runs_table[1:10,]

html1 <- read_html("https://stats.espncricinfo.com/icc-world-twenty20-2016/engine/records/bowling/most_wickets_career.html?id=10291;type=tournament")

Most_wickets <- html1 %>% html_table() 

most_wickets_table <- as.data.frame(Most_wickets[[1]])

view(most_wickets_table)

save(most_wickets_table,file = "Top_bowlers.Rdata")

