sc <- read_html(url[13]) %>% html_table()
df13_I_bow <- data.frame(sc[[2]][[1]],sc[[2]][[5]],sc[[2]][[6]],sc[[2]][[7]],
                         sc[[2]][[8]],sc[[2]][[9]])[-c(2,4,6,8,10),]

colnames(df13_I_bow) <- c("players","wickets","ECON","0's","4's","6's")

df13_N_bow <- data.frame(sc[[4]][[1]],sc[[4]][[5]],sc[[4]][[6]],sc[[4]][[7]],
                         sc[[4]][[8]],sc[[4]][[9]])[-c(2,5,8,10),]

colnames(df13_N_bow) <- c("players","wickets","ECON","0's","4's","6's")


for (i in 2:6) {
  df13_I_bow[,i] <- as.numeric(df13_I_bow[,i])
  df13_N_bow[,i] <- as.numeric(df13_N_bow[,i])
}

#view(df13_N_bat)

# ------------------------------------------------------------------------------

sc <- read_html(url[19]) %>% html_table()
df19_I_bow <- data.frame(sc[[2]][[1]],sc[[2]][[5]],sc[[2]][[6]],sc[[2]][[7]],
                         sc[[2]][[8]],sc[[2]][[9]])[-c(2,5,7,9,11),]

colnames(df19_I_bow) <- c("players","wickets","ECON","0's","4's","6's")

df19_P_bow <- data.frame(sc[[4]][[1]],sc[[4]][[5]],sc[[4]][[6]],sc[[4]][[7]],
                         sc[[4]][[8]],sc[[4]][[9]])[-c(2,5,9),]

colnames(df19_P_bow) <- c("players","wickets","ECON","0's","4's","6's")

df19_P_bat <- data.frame(sc[[1]][[1]],sc[[1]][[3]])[-c(seq(2,10,2),14,15,16),]
colnames(df19_P_bat) <- c("Players", "Runs")
df19_P_bat$Runs <- as.numeric(df19_P_bat$Runs)

for (i in 2:6) {
  df19_I_bow[,i] <- as.numeric(df19_I_bow[,i])
  df19_P_bow[,i] <- as.numeric(df19_P_bow[,i])
}

view(df19_P_bat)

# ------------------------------------------------------------------------------

sc <- read_html(url[25]) %>% html_table()
df25_B_bow <- data.frame(sc[[2]][[1]],sc[[2]][[5]],sc[[2]][[6]],sc[[2]][[7]],
                         sc[[2]][[8]],sc[[2]][[9]])[-c(3,5,7,9,11),]

colnames(df25_B_bow) <- c("players","wickets","ECON","0's","4's","6's")

df25_I_bow <- data.frame(sc[[4]][[1]],sc[[4]][[5]],sc[[4]][[6]],sc[[4]][[7]],
                         sc[[4]][[8]],sc[[4]][[9]])[-c(2,5,7,9,11),]

colnames(df25_I_bow) <- c("players","wickets","ECON","0's","4's","6's")

df25_B_bat <- data.frame(sc[[6]][[1]],sc[[6]][[3]])[-c(11,12),]
colnames(df25_B_bat) <- c("Players", "Runs")
df25_B_bat$Runs <- as.numeric(df25_B_bat$Runs)

for (i in 2:6) {
  df25_B_bow[,i] <- as.numeric(df25_B_bow[,i])
  df25_I_bow[,i] <- as.numeric(df25_I_bow[,i])
}

view(df25_B_bat)

# ------------------------------------------------------------------------------
     
sc <- read_html(url[31]) %>% html_table()
df31_I_bow <- data.frame(sc[[2]][[1]],sc[[2]][[5]],sc[[2]][[6]],sc[[2]][[7]],
                         sc[[2]][[8]],sc[[2]][[9]])[-c(2,4,6,9,11),]

colnames(df31_I_bow) <- c("players","wickets","ECON","0's","4's","6's")

df31_A_bow <- data.frame(sc[[4]][[1]],sc[[4]][[5]],sc[[4]][[6]],sc[[4]][[7]],
                         sc[[4]][[8]],sc[[4]][[9]])[-c(3,5,7),]

colnames(df31_A_bow) <- c("players","wickets","ECON","0's","4's","6's")

df31_A_bat <- data.frame(sc[[1]][[1]],sc[[1]][[3]])[-c(seq(2,10,2),13,16,17,18),]
colnames(df31_A_bat) <- c("Players", "Runs")
df31_A_bat$Runs <- as.numeric(df31_A_bat$Runs)

for (i in 2:6) {
  df31_I_bow[,i] <- as.numeric(df31_I_bow[,i])
  df31_A_bow[,i] <- as.numeric(df31_A_bow[,i])
}

view(df31_A_bow)

# ------------------------------------------------------------------------------

sc <- read_html(url[34]) %>% html_table()
df34_W_bow <- data.frame(sc[[2]][[1]],sc[[2]][[5]],sc[[2]][[6]],sc[[2]][[7]],
                         sc[[2]][[8]],sc[[2]][[9]])[-c(2,4),]

colnames(df34_W_bow) <- c("players","wickets","ECON","0's","4's","6's")

df34_I_bow <- data.frame(sc[[4]][[1]],sc[[4]][[5]],sc[[4]][[6]],sc[[4]][[7]],
                         sc[[4]][[8]],sc[[4]][[9]])[-c(2,4,9),]

colnames(df34_I_bow) <- c("players","wickets","ECON","0's","4's","6's")


for (i in 2:6) {
  df34_W_bow[,i] <- as.numeric(df34_W_bow[,i])
  df34_I_bow[,i] <- as.numeric(df34_I_bow[,i])
}

view(df34_W_bow)

# ------------------------------------------------------------------------------

Ind_Mat <- list(Ind_bat_13 = Ind_Mat[[1]] , Ind_bat_19 = Ind_Mat[[2]] , Ind_bat_25 = Ind_Mat[[3]] , Ind_bat_31 = Ind_Mat[[4]] , Ind_bat_34 = Ind_Mat[[5]] ,
                NZ_bat_13 = nz_Mat[[1]] , Pak_bat_19 = df19_P_bat , Ban_bat_25 = df25_B_bat, Aus_bat_31 = df31_A_bat , WI_bat_34 = Wi_Mat[[5]] , 
                Ind_bowl_13 = df13_I_bow , Ind_bowl_19 = df19_I_bow , Ind_bowl_25 = df25_I_bow , Ind_bowl_31 = df31_I_bow , Ind_bowl_34 = df34_I_bow , 
                NZ_bowl_13 = df13_N_bow , Pak_bowl_19 = df19_P_bow , Ban_bowl_25 = df25_B_bow , Aus_bowl_31 = df31_A_bow , WI_bowl_34 = df34_W_bow)

save(Ind_Mat, file = "India_Matches.Rdata")
# ------------------------------------------------------------------------------

sc <- read_html(url[18]) %>% html_table()
df18_E_bow <- data.frame(sc[[2]][[1]],sc[[2]][[5]],sc[[2]][[6]],sc[[2]][[7]],
                         sc[[2]][[8]],sc[[2]][[9]])[-c(2,5,9),]

colnames(df18_E_bow) <- c("players","wickets","ECON","0's","4's","6's")

df18_S_bow <- data.frame(sc[[4]][[1]],sc[[4]][[5]],sc[[4]][[6]],sc[[4]][[7]],
                         sc[[4]][[8]],sc[[4]][[9]])[-c(2,5,7,9),]

colnames(df18_S_bow) <- c("players","wickets","ECON","0's","4's","6's")

df18_S_bat <- data.frame(sc[[1]][[1]],sc[[1]][[3]])[-c(seq(2,8,2),12,13,14),]
colnames(df18_S_bat) <- c("Players", "Runs")
df18_S_bat$Runs <- as.numeric(df18_S_bat$Runs)

for (i in 2:6) {
  df18_E_bow[,i] <- as.numeric(df18_E_bow[,i])
  df18_S_bow[,i] <- as.numeric(df18_S_bow[,i])
}

view(df18_S_bat)

# ------------------------------------------------------------------------------

sc <- read_html(url[24]) %>% html_table()
df24_A_bow <- data.frame(sc[[2]][[1]],sc[[2]][[5]],sc[[2]][[6]],sc[[2]][[7]],
                         sc[[2]][[8]],sc[[2]][[9]])[-c(2,5,7,9),]

colnames(df24_A_bow) <- c("players","wickets","ECON","0's","4's","6's")

df24_E_bow <- data.frame(sc[[4]][[1]],sc[[4]][[5]],sc[[4]][[6]],sc[[4]][[7]],
                         sc[[4]][[8]],sc[[4]][[9]])[-c(2,4,7,9,11),]

colnames(df24_E_bow) <- c("players","wickets","ECON","0's","4's","6's")

df24_A_bat <- data.frame(sc[[3]][[1]],sc[[3]][[3]])[-c(seq(2,16,2),19,22,23),]
colnames(df24_A_bat) <- c("Players", "Runs")
df24_A_bat$Runs <- as.numeric(df24_A_bat$Runs)

for (i in 2:6) {
  df24_A_bow[,i] <- as.numeric(df24_A_bow[,i])
  df24_E_bow[,i] <- as.numeric(df24_E_bow[,i])
}

view(df24_A_bat)

# ------------------------------------------------------------------------------

sc <- read_html(url[29]) %>% html_table()
df29_SL_bow <- data.frame(sc[[2]][[1]],sc[[2]][[5]],sc[[2]][[6]],sc[[2]][[7]],
                         sc[[2]][[8]],sc[[2]][[9]])[-c(3,5),]

colnames(df29_SL_bow) <- c("players","wickets","ECON","0's","4's","6's")

df29_E_bow <- data.frame(sc[[4]][[1]],sc[[4]][[5]],sc[[4]][[6]],sc[[4]][[7]],
                         sc[[4]][[8]],sc[[4]][[9]])[-c(2,4,6),]

colnames(df29_E_bow) <- c("players","wickets","ECON","0's","4's","6's")

df29_SL_bat <- data.frame(sc[[3]][[1]],sc[[3]][[3]])[-c(seq(2,8,2),seq(11,17,2),20,21,22),]
colnames(df29_SL_bat) <- c("Players", "Runs")
df29_SL_bat$Runs <- as.numeric(df29_SL_bat$Runs)

for (i in 2:6) {
  df29_SL_bow[,i] <- as.numeric(df29_SL_bow[,i])
  df29_E_bow[,i] <- as.numeric(df29_E_bow[,i])
}

view(df29_SL_bow)

# ------------------------------------------------------------------------------  

eng_Mat <- list(Eng_bat_15 = eng_Mat[[1]] , Eng_bat_18 = eng_Mat[[2]] , Eng_bat_24 = eng_Mat[[3]] , Eng_bat_29 = eng_Mat[[4]] , Eng_bat_33 = eng_Mat[[5]] , Eng_bat_35 = eng_Mat[[6]] ,
                WI_bat_15 = Wi_Mat[[1]] , SA_bat_18 = df18_S_bat , Afg_bat_24 = df24_A_bat, SL_bat_29 = df29_SL_bat , NZ_bat_33 = nz_Mat[[5]] , WI_bat_35 = Wi_Mat[[6]] ,
                Eng_bowl_15 = Wi_Mat[[19]] , Eng_bowl_18 = df18_E_bow , Eng_bowl_24 = df24_E_bow , Eng_bowl_29 = df29_E_bow , Eng_bowl_33 = nz_Mat[[20]] , Eng_bowl_35 = Wi_Mat[[24]] ,
                WI_bowl_15 = Wi_Mat[[13]] , SA_bowl_18 = df18_S_bow , Afg_bowl_24 = df24_A_bow, SL_bowl_29 = df29_SL_bow , NZ_bowl_33 = nz_Mat[[15]] , WI_bowl_35 = Wi_Mat[[18]])

save(eng_Mat, file = "England_Matches.Rdata")



