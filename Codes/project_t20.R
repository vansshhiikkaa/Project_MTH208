################### wi matches all data ################
#15

sc <- read_html(url[15]) %>% html_table()
df15_w_bow <- data.frame(sc[[2]][[1]],sc[[2]][[5]],sc[[2]][[6]],sc[[2]][[7]],
                         sc[[2]][[8]],sc[[2]][[9]])[-c(4,6,8),]

colnames(df15_w_bow) <- c("players","wickets","ECON","0's","4's","6's")

df15_E_bow <- data.frame(sc[[4]][[1]],sc[[4]][[5]],sc[[4]][[6]],sc[[4]][[7]],
                         sc[[4]][[8]],sc[[4]][[9]])[-c(2,4,8,10),]

colnames(df15_E_bow) <- c("players","wickets","ECON","0's","4's","6's")
colnames(df15_w_bow) <- c("players","wickets","ECON","0's","4's","6's")


for (i in 2:6) {
  df15_E_bow[,i] <- as.numeric(df15_E_bow[,i])
  df15_w_bow[,i] <- as.numeric(df15_w_bow[,i])
}
view(df15_E_bow)

#21

##sl batting ##

sc <- read_html(url[21]) %>% html_table() 
df21_sl <- data.frame(sc[[1]][[1]],sc[[1]][[3]])[-c(seq(2,18,2),22,23),]
colnames(df21_sl) <- c("Players", "Runs")
df21_sl$Runs <- as.numeric(df21_sl$Runs)
view(df21_sl)

## wi & sl bowling ######

df21_w_bow <- data.frame(sc[[2]][[1]],sc[[2]][[5]],sc[[2]][[6]],sc[[2]][[7]],
                         sc[[2]][[8]],sc[[2]][[9]])[-c(2,4,7,9),]

df21_sl_bow <- data.frame(sc[[4]][[1]],sc[[4]][[5]],sc[[4]][[6]],sc[[4]][[7]],
                         sc[[4]][[8]],sc[[4]][[9]])[-c(5,7),]

colnames(df21_w_bow) <- c("players","wickets","ECON","0's","4's","6's")
colnames(df21_sl_bow) <- c("players","wickets","ECON","0's","4's","6's")


for (i in 2:6) {
  df21_w_bow[,i] <- as.numeric(df21_w_bow[,i])
  df21_sl_bow[,i] <- as.numeric(df21_sl_bow[,i])
}

view(df15_E_bow)

#27

##sa batting ##

sc <- read_html(url[27]) %>% html_table() 
df27_sa <- data.frame(sc[[1]][[1]],sc[[1]][[3]])[-c(seq(2,14,2),17,19,20,21),]
colnames(df27_sa) <- c("Players", "Runs")
df27_sa$Runs <- as.numeric(df27_sa$Runs)

## wi & sa bowling ######

df27_w_bow <- data.frame(sc[[2]][[1]],sc[[2]][[5]],sc[[2]][[6]],sc[[2]][[7]],
                         sc[[2]][[8]],sc[[2]][[9]])[-c(3,5,9),]

df27_sa_bow <- data.frame(sc[[4]][[1]],sc[[4]][[5]],sc[[4]][[6]],sc[[4]][[7]],
                          sc[[4]][[8]],sc[[4]][[9]])[-c(seq(2,10,2)),]

colnames(df27_w_bow) <- c("players","wickets","ECON","0's","4's","6's")
colnames(df27_sa_bow) <- c("players","wickets","ECON","0's","4's","6's")


for (i in 2:6) {
  df27_w_bow[,i] <- as.numeric(df27_w_bow[,i])
  df27_sa_bow[,i] <- as.numeric(df27_sa_bow[,i])
}

view(df27_sa_bow)

#30

########## afg batting ###########

sc <- read_html(url[30]) %>% html_table() 
df30_afg <- data.frame(sc[[1]][[1]],sc[[1]][[3]])[-c(seq(2,10,2),13,15,18,19,20),]
colnames(df30_afg) <- c("Players", "Runs")
df30_afg$Runs <- as.numeric(df30_afg$Runs)

view(df30_afg)

## wi & afg bowling ######

df30_w_bow <- data.frame(sc[[2]][[1]],sc[[2]][[5]],sc[[2]][[6]],sc[[2]][[7]],
                         sc[[2]][[8]],sc[[2]][[9]])[-c(2,4,7,10),]

df30_afg_bow <- data.frame(sc[[4]][[1]],sc[[4]][[5]],sc[[4]][[6]],sc[[4]][[7]],
                          sc[[4]][[8]],sc[[4]][[9]])[-c(2,4,6,8,11),]

colnames(df30_w_bow) <- c("players","wickets","ECON","0's","4's","6's")
colnames(df30_afg_bow) <- c("players","wickets","ECON","0's","4's","6's")


for (i in 2:6) {
  df30_w_bow[,i] <- as.numeric(df30_w_bow[,i])
  df30_afg_bow[,i] <- as.numeric(df30_afg_bow[,i])
}

view(df30_w_bow)


#34
sc <- read_html(url[34]) %>% html_table()

## wi & Ind bowling ######

df34_w_bow <- data.frame(sc[[2]][[1]],sc[[2]][[5]],sc[[2]][[6]],sc[[2]][[7]],
                         sc[[2]][[8]],sc[[2]][[9]])[-c(2,4),]

df34_ind_bow <- data.frame(sc[[4]][[1]],sc[[4]][[5]],sc[[4]][[6]],sc[[4]][[7]],
                           sc[[4]][[8]],sc[[4]][[9]])[-c(2,4,9),]

colnames(df34_w_bow) <- c("players","wickets","ECON","0's","4's","6's")
colnames(df34_ind_bow) <- c("players","wickets","ECON","0's","4's","6's")


for (i in 2:6) {
  df34_w_bow[,i] <- as.numeric(df34_w_bow[,i])
  df34_ind_bow[,i] <- as.numeric(df34_ind_bow[,i])
}

view(df34_ind_bow)

#35

sc <- read_html(url[35]) %>% html_table()

## wi & Eng bowling ######

df35_w_bow <- data.frame(sc[[2]][[1]],sc[[2]][[5]],sc[[2]][[6]],sc[[2]][[7]],
                         sc[[2]][[8]],sc[[2]][[9]])[-c(2,4,7,9),]

df35_eng_bow <- data.frame(sc[[4]][[1]],sc[[4]][[5]],sc[[4]][[6]],sc[[4]][[7]],
                           sc[[4]][[8]],sc[[4]][[9]])[-c(2,4,8),]

colnames(df35_w_bow) <- c("players","wickets","ECON","0's","4's","6's")
colnames(df35_eng_bow) <- c("players","wickets","ECON","0's","4's","6's")


for (i in 2:6) {
  df35_w_bow[,i] <- as.numeric(df35_w_bow[,i])
  df35_eng_bow[,i] <- as.numeric(df35_eng_bow[,i])
}

view(df35_eng_bow)

Wi_Mat <- list(wi_bat_15 = Wi_Mat[[1]] , wi_bat_21 = Wi_Mat[[2]] , wi_bat_27 = Wi_Mat[[3]] , wi_bat_30 = Wi_Mat[[4]] , wi_bat_34 = Wi_Mat[[5]] , wi_bat_35 = Wi_Mat[[6]],
               eng_bat_15 = eng_Mat[[1]] , sl_bat_21 = df21_sl , sa_bat_27 = df27_sa,
               afg_bat_30 = df30_afg , ind_bat_34 = Ind_Mat[[5]] , eng_bat_35 = eng_Mat[[6]] , 
               wi_bowl_15 = df15_w_bow , wi_bowl_21 = df21_w_bow , wi_bowl_27 = df27_w_bow , wi_bowl_30 = df30_w_bow , wi_bowl_34 = df34_w_bow , 
               wi_bowl_35 = df35_w_bow , eng_bowl_15 = df15_E_bow , sl_bowl_21 = df21_sl_bow , sa_bowl_27 = df27_sa_bow , afg_bowl_30 = df30_afg_bow ,
               ind_bowl_34 = df34_ind_bow , eng_bowl_35 = df35_eng_bow)
Wi_Mat
save(Wi_Mat , file = "WestIndies_Match.Rdata")

#################### NZ matches all data #############

#13

sc <- read_html(url[13]) %>% html_table()

## nz & Ind bowling ######

df13_nz_bow <- data.frame(sc[[4]][[1]],sc[[4]][[5]],sc[[4]][[6]],sc[[4]][[7]],
                         sc[[4]][[8]],sc[[4]][[9]])[-c(2,5,8,10),]

df13_ind_bow <- data.frame(sc[[2]][[1]],sc[[2]][[5]],sc[[2]][[6]],sc[[2]][[7]],
                           sc[[2]][[8]],sc[[2]][[9]])[-c(2,4,6,8,10),]

colnames(df13_nz_bow) <- c("players","wickets","ECON","0's","4's","6's")
colnames(df13_ind_bow) <- c("players","wickets","ECON","0's","4's","6's")


for (i in 2:6) {
  df13_nz_bow[,i] <- as.numeric(df13_nz_bow[,i])
  df13_ind_bow[,i] <- as.numeric(df13_ind_bow[,i])
}

view(df13_nz_bow)


#17

###### aus batting ###########
sc <- read_html(url[17]) %>% html_table()
df17_aus <- data.frame(sc[[3]][[1]],sc[[3]][[3]])[-c(seq(2,18,2),22,23),]
colnames(df17_aus) <- c("Players", "Runs")
df17_aus$Runs <- as.numeric(df17_aus$Runs)

view(df17_aus)

## nz & Ind bowling ######

df17_nz_bow <- data.frame(sc[[4]][[1]],sc[[4]][[5]],sc[[4]][[6]],sc[[4]][[7]],
                          sc[[4]][[8]],sc[[4]][[9]])[-c(2,6,8,11),]

df17_aus_bow <- data.frame(sc[[2]][[1]],sc[[2]][[5]],sc[[2]][[6]],sc[[2]][[7]],
                           sc[[2]][[8]],sc[[2]][[9]])[-c(3,6,9,11),]

colnames(df17_nz_bow) <- c("players","wickets","ECON","0's","4's","6's")
colnames(df17_aus_bow) <- c("players","wickets","ECON","0's","4's","6's")


for (i in 2:6) {
  df17_nz_bow[,i] <- as.numeric(df17_nz_bow[,i])
  df17_aus_bow[,i] <- as.numeric(df17_aus_bow[,i])
}

view(df17_nz_bow)

#23

###### aus batting ###########

sc <- read_html(url[23]) %>% html_table()
df23_pak <- data.frame(sc[[3]][[1]],sc[[3]][[3]])[-c(seq(2,10,2),14,15,16),]
colnames(df23_pak) <- c("Players", "Runs")
df23_pak$Runs <- as.numeric(df23_pak$Runs)

view(data.frame(sc[[2]][[1]],sc[[2]][[5]],sc[[2]][[6]],sc[[2]][[7]],
                sc[[2]][[8]],sc[[2]][[9]]))

## nz & pak bowling ######

df23_nz_bow <- data.frame(sc[[4]][[1]],sc[[4]][[5]],sc[[4]][[6]],sc[[4]][[7]],
                          sc[[4]][[8]],sc[[4]][[9]])[-c(2,5,9),]

df23_pak_bow <- data.frame(sc[[2]][[1]],sc[[2]][[5]],sc[[2]][[6]],sc[[2]][[7]],
                           sc[[2]][[8]],sc[[2]][[9]])[-c(3,5,8),]

colnames(df23_nz_bow) <- c("players","wickets","ECON","0's","4's","6's")
colnames(df23_pak_bow) <- c("players","wickets","ECON","0's","4's","6's")


for (i in 2:6) {
  df23_nz_bow[,i] <- as.numeric(df23_nz_bow[,i])
  df23_pak_bow[,i] <- as.numeric(df23_pak_bow[,i])
}

view(df23_pak_bow)

#28

###### bang batting ###########

sc <- read_html(url[28]) %>% html_table()
df28_bang <- data.frame(sc[[3]][[1]],sc[[3]][[3]])[-c(seq(2,14,2),seq(17,23,2),24),]
colnames(df28_bang) <- c("Players", "Runs")
df28_bang$Runs <- as.numeric(df28_bang$Runs)

view(data.frame(sc[[2]][[1]],sc[[2]][[5]],sc[[2]][[6]],sc[[2]][[7]],
                sc[[2]][[8]],sc[[2]][[9]]))

## nz & bang bowling ######

df28_nz_bow <- data.frame(sc[[4]][[1]],sc[[4]][[5]],sc[[4]][[6]],sc[[4]][[7]],
                          sc[[4]][[8]],sc[[4]][[9]])[-c(2,5,7,9,11),]

df28_bang_bow <- data.frame(sc[[2]][[1]],sc[[2]][[5]],sc[[2]][[6]],sc[[2]][[7]],
                           sc[[2]][[8]],sc[[2]][[9]])[-c(2,6,8),]

colnames(df28_nz_bow) <- c("players","wickets","ECON","0's","4's","6's")
colnames(df28_bang_bow) <- c("players","wickets","ECON","0's","4's","6's")


for (i in 2:6) {
  df28_nz_bow[,i] <- as.numeric(df28_nz_bow[,i])
  df28_bang_bow[,i] <- as.numeric(df28_bang_bow[,i])
}

view(df28_bang_bow)

#33

sc <- read_html(url[33]) %>% html_table()

## nz & eng bowling ######

df33_nz_bow <- data.frame(sc[[4]][[1]],sc[[4]][[5]],sc[[4]][[6]],sc[[4]][[7]],
                          sc[[4]][[8]],sc[[4]][[9]])[-c(5,7),]

df33_eng_bow <- data.frame(sc[[2]][[1]],sc[[2]][[5]],sc[[2]][[6]],sc[[2]][[7]],
                            sc[[2]][[8]],sc[[2]][[9]])[-c(2,4,6,9,11),]

colnames(df33_nz_bow) <- c("players","wickets","ECON","0's","4's","6's")
colnames(df33_eng_bow) <- c("players","wickets","ECON","0's","4's","6's")


for (i in 2:6) {
  df33_nz_bow[,i] <- as.numeric(df33_nz_bow[,i])
  df33_eng_bow[,i] <- as.numeric(df33_eng_bow[,i])
}

view(df33_eng_bow)

nz_Mat <- list(nz_bat_13 = nz_Mat[[1]] , nz_bat_17 = nz_Mat[[2]] , nz_bat_23 = nz_Mat[[3]] , nz_bat_28 = nz_Mat[[4]] , nz_bat_33 = nz_Mat[[5]] ,
               ind_bat_13 = Ind_Mat[[1]] , aus_bat_17 = df17_aus , pak_bat_23 = df23_pak,
               bang_bat_28 = df28_bang , eng_bat_33 = eng_Mat[[5]] , 
               nz_bowl_13 = df13_nz_bow , nz_bowl_17 = df17_nz_bow , nz_bowl_23 = df23_nz_bow , nz_bowl_28 = df28_nz_bow , nz_bowl_33 = df33_nz_bow , 
               ind_bowl_13 = df13_ind_bow , aus_bowl_17 = df17_aus_bow , pak_bowl_23 = df23_pak_bow , bang_bowl_28 = df28_bang_bow ,
               eng_bowl_33 = df33_eng_bow )



save(nz_Mat , file = "NewZealand_match.Rdata")

html <- read_html("https://www.cricwaves.com/cricket/tour/299/t20-world-cup-2016/2/247/virat-kohli/PlayerStats.html")

stats <- html %>% html_elements(".pdetl.pdet_colr") %>% html_text()

runs_vk <- as.numeric(stats[seq(2,30,7)])


link <- html %>% html_node("[data-num-posts='4']") %>% html_attr("href")


html2 <- read_html("https://www.cricwaves.com/cricket/tour/299/t20-world-cup-2016/2/Stats.html")
links <- html2 %>% html_nodes("[title='View.Player.Stats.in.T20.World.Cup.2016']") %>% html_attr("href")

boundarys <- most_runs_table$`4s` + most_runs_table$`6s`
head(most_runs_table)

library(ggplot2)
library(plotly)

p1 <-most_runs_table %>%  ggplot(aes(SR , Runs, size = boundarys, color = Mat)) +
  geom_point() +
  theme_bw()

ggplotly(p1)

cor(most_runs_table$SR , most_runs_table$Runs)


head(most_wickets_table)

p2 <-most_wickets_table %>%  ggplot(aes(Econ , Wkts, size = Runs, color = Mat)) +
  geom_point() +
  theme_bw()

ggplotly(p2)

cor(most_wickets_table$Econ , most_wickets_table$Wkts)

library(tidyverse)
library(rvest)
stats_vk <- read_html("https://www.cricwaves.com/cricket/tour/299/t20-world-cup-2016/2/247/virat-kohli/PlayerStats.html")%>%
  html_elements(".pdetl.pdet_colr") %>% html_text() 
runs_vk <- as.numeric(stats_vk[seq(2,30,7)])

stats_tamim <- read_html("https://www.cricwaves.com/cricket/tour/299/t20-world-cup-2016/2/120/tamim-iqbal/PlayerStats.html")%>%
  html_elements(".pdetl.pdet_colr") %>% html_text() 
runs_tamim <- as.numeric(stats_tamim[seq(2,30,7)])

stats_jroot <- read_html("https://www.cricwaves.com/cricket/tour/299/t20-world-cup-2016/2/4348/joe-root/PlayerStats.html")%>%
  html_elements(".pdetl.pdet_colr") %>% html_text() 
runs_jroot <- as.numeric(stats_jroot[seq(2,30,7)])

stats_mshahzad <- read_html("https://www.cricwaves.com/cricket/tour/299/t20-world-cup-2016/2/3894/mohammad-shahzad/PlayerStats.html")%>%
  html_elements(".pdetl.pdet_colr") %>% html_text()

runs_mshahzad <- as.numeric(stats_mshahzad[seq(2,30,7)])

stas_jbuttler <- stats_jroot <- read_html("https://www.cricwaves.com/cricket/tour/299/t20-world-cup-2016/2/2113/jos-buttler/PlayerStats.html")%>%
  html_elements(".pdetl.pdet_colr") %>% html_text() 

runs_jbuttler <-  as.numeric(stas_jbuttler[seq(2,30,7)])

stas_jjroy <- stats_jroot <- read_html("https://www.cricwaves.com/cricket/tour/299/t20-world-cup-2016/2/4241/jason-roy/PlayerStats.html")%>%
  html_elements(".pdetl.pdet_colr") %>% html_text() 

runs_jjroy <-  as.numeric(stas_jjroy[seq(2,30,7)])

top6_batsman <- data.frame(slno = 1:5 , player = c(rep("Tamim Ikbal",5), rep("Virat Kohli",5),rep("Joe Root",5),rep("Mohammad Shahzad",5) , rep("JC Buttler",5), rep("JJ Roy",5)) , 
           Runs = c(runs_tamim , runs_vk , runs_jroot , runs_mshahzad , runs_jbuttler , runs_jjroy))


var(runs_tamim)

var(runs_vk)

var(runs_jroot)

var(runs_mshahzad)

var(runs_jbuttler)

var(runs_jjroy)

ggplot(top6_batsman, aes(x = slno , y = Runs, color = player)) +
  geom_line(lwd = 1.1) +
  guides(color = guide_legend(title = "Bowlers")) +
  scale_color_discrete(labels = c("Ravichandran Ashwin", "Ashish Nehra", "Jasprit Bumrah", "Ravindra Jadeja", "Hardik Pandya")) +
  labs(x = "Matches", y = "Economy", title = "Consistency of India Bowlers")


nz_Mat[[3]]
nz_Mat[[4]]












