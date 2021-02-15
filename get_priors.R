# get_priors.R
# John Kearns
# 11/07/2020

library(tidyverse)
library(caret)
library(nnetpredint)
library(ncaahoopR)
library(lubridate)
'%!in%' <- function(x,y)!('%in%'(x,y))
library(imputeTS)


setwd("~/2020/NBN/Model/Files/")

################################# Create 2020-21 Priors
########## 247_Scraper.R
conferences <- c("ACC", "Big-12", "Big-Ten", "Pac-12", "SEC", "AAC",
                 "C-USA", "MAC", "M-West", "A-10", "A-East", "A-Sun",
                 "Big-East", "Big-west", "Big-Sky", "B-South", "CAA",
                 "Horizon", "Ivy", "MAAC", "MEAC", "MVC", "NEC", "OVC",
                 "Patriot", "SBC", "SLC", "Southern", "Summit", "SW-AC",
                 "WAC", "WCC")

year <- 2020

for(conf in conferences) {
  print(paste("Scraping", conf))
  url <- paste0("https://247sports.com/Season/", year, "-Basketball/CompositeTeamRankings?Conference=", conf)
  x <- scan(url, sep = "\n", what = "")
  x <- x[73]
  nums = gregexpr(pattern ='/Season/2020-Basketball/',x)[[1]]
  y = c()
  for(i in 1:length(nums)){
    y = append(y,substr(x,nums[i],nums[i]+473))
  }
  z = y[which(grepl("college",y))]
  if(length(z)%%2 != 0){
    stop("Teams are not matched")
  }
  #y <- strsplit(x, '<div class=\"rank-column\">')[[1]][-1]
  
  if(length(z) > 0) {
    recruits <- data.frame("team" = rep(NA, length(z)/2),
                           "conf" = rep(NA, length(z)/2),
                           "conf_rank" = rep(NA, length(z)/2),
                           "stars_5" = rep(NA, length(z)/2),
                           "stars_4" = rep(NA, length(z)/2),
                           "stars_3" = rep(NA, length(z)/2),
                           "num_star_players" = rep(NA, length(z)/2),
                           "avg_recruit_score" = rep(NA, length(z)/2),
                           "composite_score" = rep(NA, length(z)/2))
    
    n=1
    for(i in 1:nrow(recruits)) {
      recruits$team[i] <- str_sub(strsplit(z[n],">|<")[[1]][2], end=-2)
      recruits$conf[i] <- conf
      recruits$conf_rank[i] <- i
      a <- strsplit(z[i], "/Commits/\">")[[1]][3]  
      recruits$num_star_players[i] <- as.numeric(str_sub(strsplit(z[n+1],">|<")[[1]][2], end=-10))
      star5 <- substring(gsub("^\\s*", "", gsub("[^0-9 ]", "", gsub(".*<h2>5-Star</h2>", "", z[n+1]))), 1, 1)
      star4 <- substring(gsub("^\\s*", "", gsub("[^0-9 ]", "", gsub(".*<h2>4-Star</h2>", "", z[n+1]))), 1, 1)
      star3 <- substring(gsub("^\\s*", "", gsub("[^0-9 ]", "", gsub(".*<h2>3-Star</h2>", "", z[n+1]))), 1, 1)
      recruits[i,c("stars_5", "stars_4", "stars_3")] <- as.numeric(c(star5, star4, star3))
      recruits$avg_recruit_score[i] <- as.numeric(gsub(" </div>.*", "", gsub(".*<div class=\"avg\"> ", "", z[n+1])))
      recruits$composite_score[i] <- as.numeric(tail(strsplit(z[n+1], paste0('/Season/', year, '-Basketball/Commits/\">| <'))[[1]],2)[1])
      n = n+2
    }
    
    if(conf == "ACC") {
      master <- recruits 
    }
    else{
      master <- rbind(master, recruits)
    }
  }
}

### Fix Errors by hard code (check for composite score NA)
if(year == 2018) {
  master$composite_score[master$team == "Santa Clara"] <- 37.44
}else if(year == 2017) {
  master$composite_score[master$team == "Santa Clara"] <- 27.40
  master$composite_score[master$team == "Loyola Marymount"] <- 13.33
  master$composite_score[master$team == "San Francisco"] <- 13.33
}else if(year == 2016) {
  master <- filter(master, team != "Abilene Christian")
  master$composite_score[master$team == "Loyola Marymount"] <- 13.33
  master$composite_score[master$team == "San Francisco"] <- 6.67
}else if(year==2020){
  master <- rbind(master,c("Loyola Marymount","WCC","7",0,0,1,1,87.28,17.28))
  master <- rbind(master,c("Santa Clara","WCC","6",0,0,1,1,91.57,21.57))
}


### Save
master <- master[order(master$composite_score, decreasing = T),]
write.csv(master, paste0("247_rankings_", year, ".csv"), row.names = F)
master[3:9] <- sapply(master[3:9],as.numeric)


# Getting Bart Torvik Returning Minutes
# Copy and paste from BartTorvik.com
BT = read.csv("BT.csv") %>% 
  filter(TEAM != "" & TEAM != "TEAM") %>%
  rename(RANK=`ï..RK`,
         CONF_REC=X) %>% 
  mutate(RANK = as.numeric(RANK),
         ADJOE = as.numeric(ADJOE),
         ADJDE=as.numeric(ADJDE),
         BARTHAG = as.numeric(BARTHAG),
         `RET.MINS` = as.numeric(gsub("%","",`RET.MINS`))/100,
         RPMS = as.numeric(gsub("%","",RPMS))/100) %>% 
  select(-c(RANK,REC))
BT$CONF_REC = str_replace_all(BT$CONF_REC,setNames(as.character(1:12),month.abb[1:12]))

mins_2021 = BT %>% 
  select(c(TEAM,`RET.MINS`)) %>% 
  rename(team=TEAM,
         mins = `RET.MINS`)


trank_colnames = read_csv("trank_colnames.csv",col_names=FALSE)

# Thank you to Bart Torvik
# getadvstats.php?year=2019&csv=1
trank_2017 = read_csv("trank_2017.csv",col_names=FALSE)
trank_2017a = trank_2017
colnames(trank_2017a) = c("player_name","team","conf","porpag")

trank_2018 = read_csv("trank_2018.csv",col_names=FALSE)
colnames(trank_2018)[1:length(trank_colnames$X1)] = trank_colnames$X1
trank_2018a = trank_2018 %>% 
  select(c(player_name,team,conf,porpag))

trank_2019 = read_csv("trank_2019.csv",col_names=FALSE)
colnames(trank_2019)[1:length(trank_colnames$X1)] = trank_colnames$X1
trank_2019a = trank_2019 %>% 
  select(c(player_name,team,conf,porpag))

trank_2020 = read_csv("trank_2020.csv",col_names=FALSE)
colnames(trank_2020)[1:length(trank_colnames$X1)] = trank_colnames$X1
trank_2020a = trank_2020 %>% 
  select(c(player_name,team,conf,porpag))

trank_2021 = read_csv("trank_2021.csv",col_names=FALSE)
colnames(trank_2021)[1:length(trank_colnames$X1)] = trank_colnames$X1
trank_2021a = trank_2021 %>% 
  select(c(player_name,team,conf,porpag))


ppg_df_2019 = data.frame()
for(tm in intersect(unique(trank_2018a$team),unique(trank_2019a$team))){
  departing = trank_2018a %>% 
    filter(team==tm) %>% 
    filter(player_name %!in% trank_2019a$player_name[trank_2019a$team==tm]) %>% 
    summarise(departing_ppg = sum(porpag,na.rm=TRUE))
  
  returning = trank_2018a %>% 
    filter(team==tm) %>% 
    filter(player_name %in% trank_2019a$player_name[trank_2019a$team==tm]) %>% 
    summarise(returning_ppg = sum(porpag,na.rm=TRUE))
  
  recruiteda = trank_2018a %>%
    filter(player_name %in% trank_2019a$player_name[trank_2019a$team==tm] & player_name %!in% trank_2018a$player_name[trank_2018a$team==tm])
  
  recruitedb = trank_2017a %>%
    filter(player_name %in% trank_2019a$player_name[trank_2019a$team==tm] & player_name %!in% trank_2017a$player_name[trank_2017a$team==tm] & player_name %!in% recruiteda$player_name)
  
  recruited_ppg = sum(recruiteda$porpag,recruitedb$porpag,na.rm=TRUE)
  
  ppg_df_2019 = rbind(ppg_df_2019,c(tm,trank_2019a$conf[trank_2019a$team==tm][1],departing$departing_ppg,returning$returning_ppg,recruited_ppg))
}
colnames(ppg_df_2019) = c("team","conf","departing_ppg","returning_ppg","recruited_ppg")
ppg_df_2019 = ppg_df_2019 %>% 
  mutate_at(vars(departing_ppg:recruited_ppg),as.numeric)

print(setdiff(unique(trank_2019a$team),unique(trank_2018a$team)))
ppg_df_2019 = rbind(ppg_df_2019,c("Cal Baptist",trank_2019a$conf[trank_2019a$team=="Cal Baptist"][1],mean(ppg_df_2019$departing_ppg[ppg_df_2019$conf==trank_2019a$conf[trank_2019a$team=="Cal Baptist"][1]],na.rm=TRUE),mean(ppg_df_2019$returning_ppg[ppg_df_2019$conf==trank_2019a$conf[trank_2019a$team=="Cal Baptist"][1]],na.rm=TRUE),mean(ppg_df_2019$recruited_ppg[ppg_df_2019$conf==trank_2019a$conf[trank_2019a$team=="Cal Baptist"][1]],na.rm=TRUE)))
ppg_df_2019 = rbind(ppg_df_2019,c("North Alabama",trank_2019a$conf[trank_2019a$team=="North Alabama"][1],mean(ppg_df_2019$departing_ppg[ppg_df_2019$conf==trank_2019a$conf[trank_2019a$team=="North Alabama"][1]],na.rm=TRUE),mean(ppg_df_2019$returning_ppg[ppg_df_2019$conf==trank_2019a$conf[trank_2019a$team=="North Alabama"][1]],na.rm=TRUE),mean(ppg_df_2019$recruited_ppg[ppg_df_2019$conf==trank_2019a$conf[trank_2019a$team=="North Alabama"][1]],na.rm=TRUE)))


####

ppg_df_2020 = data.frame()
for(tm in intersect(unique(trank_2019a$team),unique(trank_2020a$team))){
  departing = trank_2019a %>% 
    filter(team==tm) %>% 
    filter(player_name %!in% trank_2020a$player_name[trank_2020a$team==tm]) %>% 
    summarise(departing_ppg = sum(porpag,na.rm=TRUE))
  
  returning = trank_2019a %>% 
    filter(team==tm) %>% 
    filter(player_name %in% trank_2020a$player_name[trank_2020a$team==tm]) %>% 
    summarise(returning_ppg = sum(porpag,na.rm=TRUE))
  
  recruiteda = trank_2019a %>%
    filter(player_name %in% trank_2020a$player_name[trank_2020a$team==tm] & player_name %!in% trank_2019a$player_name[trank_2019a$team==tm])
  
  recruitedb = trank_2018a %>%
    filter(player_name %in% trank_2020a$player_name[trank_2020a$team==tm] & player_name %!in% trank_2018a$player_name[trank_2018a$team==tm] & player_name %!in% recruiteda$player_name)
  
  recruited_ppg = sum(recruiteda$porpag,recruitedb$porpag,na.rm=TRUE)
  
  ppg_df_2020 = rbind(ppg_df_2020,c(tm,trank_2020a$conf[trank_2020a$team==tm][1],departing$departing_ppg,returning$returning_ppg,recruited_ppg))
}
colnames(ppg_df_2020) = c("team","conf","departing_ppg","returning_ppg","recruited_ppg")
ppg_df_2020 = ppg_df_2020 %>% 
  mutate_at(vars(departing_ppg:recruited_ppg),as.numeric)

print(setdiff(unique(trank_2020a$team),unique(trank_2019a$team)))
ppg_df_2020 = rbind(ppg_df_2020,c("Merrimack",trank_2020a$conf[trank_2020a$team=="Merrimack"][1],mean(ppg_df_2020$departing_ppg[ppg_df_2020$conf==trank_2020a$conf[trank_2020a$team=="Merrimack"][1]],na.rm=TRUE),mean(ppg_df_2020$returning_ppg[ppg_df_2020$conf==trank_2020a$conf[trank_2020a$team=="Merrimack"][1]],na.rm=TRUE),mean(ppg_df_2020$recruited_ppg[ppg_df_2020$conf==trank_2020a$conf[trank_2020a$team=="Merrimack"][1]],na.rm=TRUE)))

ppg_df_2021 = data.frame()
for(tm in intersect(unique(trank_2020a$team),unique(trank_2021a$team))){
  departing = trank_2020a %>% 
    filter(team==tm) %>% 
    filter(player_name %!in% trank_2021a$player_name[trank_2021a$team==tm]) %>% 
    summarise(departing_ppg = sum(porpag,na.rm=TRUE))
  
  returning = trank_2020a %>% 
    filter(team==tm) %>% 
    filter(player_name %in% trank_2021a$player_name[trank_2021a$team==tm]) %>% 
    summarise(returning_ppg = sum(porpag,na.rm=TRUE))
  
  recruiteda = trank_2020a %>%
    filter(player_name %in% trank_2021a$player_name[trank_2021a$team==tm] & player_name %!in% trank_2020a$player_name[trank_2020a$team==tm])
  
  recruitedb = trank_2019a %>%
    filter(player_name %in% trank_2021a$player_name[trank_2021a$team==tm] & player_name %!in% trank_2019a$player_name[trank_2019a$team==tm] & player_name %!in% recruiteda$player_name)
  
  recruited_ppg = sum(recruiteda$porpag,recruitedb$porpag,na.rm=TRUE)
  
  ppg_df_2021 = rbind(ppg_df_2021,c(tm,trank_2021a$conf[trank_2021a$team==tm][1],departing$departing_ppg,returning$returning_ppg,recruited_ppg))
}
colnames(ppg_df_2021) = c("team","conf","departing_ppg","returning_ppg","recruited_ppg")
ppg_df_2021 = ppg_df_2021 %>% 
  mutate_at(vars(departing_ppg:recruited_ppg),as.numeric)

print(setdiff(unique(trank_2021a$team),unique(trank_2020a$team)))
ppg_df_2021 = rbind(ppg_df_2021,c("UC San Diego",trank_2021a$conf[trank_2021a$team=="UC San Diego"][1],mean(ppg_df_2021$departing_ppg[ppg_df_2021$conf==trank_2021a$conf[trank_2021a$team=="UC San Diego"][1]],na.rm=TRUE),mean(ppg_df_2021$returning_ppg[ppg_df_2021$conf==trank_2021a$conf[trank_2021a$team=="UC San Diego"][1]],na.rm=TRUE),mean(ppg_df_2021$recruited_ppg[ppg_df_2021$conf==trank_2021a$conf[trank_2021a$team=="UC San Diego"][1]],na.rm=TRUE)))
ppg_df_2021 = rbind(ppg_df_2021,c("Bellarmine",trank_2021a$conf[trank_2021a$team=="Bellarmine"][1],mean(as.numeric(ppg_df_2021$departing_ppg[ppg_df_2021$conf==trank_2021a$conf[trank_2021a$team=="Bellarmine"][1]]),na.rm=TRUE),mean(as.numeric(ppg_df_2021$returning_ppg[ppg_df_2021$conf==trank_2021a$conf[trank_2021a$team=="Bellarmine"][1]]),na.rm=TRUE),mean(as.numeric(ppg_df_2021$recruited_ppg[ppg_df_2021$conf==trank_2021a$conf[trank_2021a$team=="Bellarmine"][1]]),na.rm=TRUE)))


mins_2021 = full_join(mins_2021,ppg_df_2021,by="team")

yusag_coef_2020 = read_csv("https://raw.githubusercontent.com/lbenz730/NCAA_Hoops/master/3.0_Files/History/history_2019_20.csv") %>% 
  filter(date==max(date))

name_dict = read_csv("name_dict.csv")
name_dict$ESPN_PBP[255] = "San José State"

mins_2021$team = str_replace_all(mins_2021$team,setNames(name_dict$NCAA,name_dict$Trank))
mins_2021$team = gsub("U. ","",mins_2021$team)
mins_2021$team[mins_2021$team=="Ga. Southern U."] = "Ga. Southern"
mins_2021$team[mins_2021$team=="Texas Southern U."] = "Texas Southern"

new = setdiff(unique(mins_2021$team),unique(yusag_coef_2020$team))
new = c(new[1:6],"USC Upstate",new[10])
old = c("Ole Miss","UC Irvine","UC Santa Barbara","UC Riverside","UT Arlington","UC Davis","Southern California Upstate","UT Martin")
yusag_coef_2020$team = str_replace_all(yusag_coef_2020$team,setNames(new,old))
priors_2020 = left_join(yusag_coef_2020,mins_2021,by="team")

master$team = str_replace_all(master$team,setNames(name_dict$NCAA,name_dict$name_247))
master$team[which(master$team=="Miami (FL) (OH)")] = "Miami (FL)"
priors_2020$team = str_replace_all(priors_2020$team,setNames(setdiff(unique(master$team),unique(priors_2020$team)),c("Irvine","SIUE","Loyola Chicago","Riverside","Illinois-Chicago","Martin")))

priors_2020 = left_join(priors_2020,master,by="team") %>% 
  select(-c(conference,rank,off_rank,def_rank,date,conf.x,conf.y,conf_rank)) 
priors_2020$returning_ppg = as.numeric(priors_2020$returning_ppg)
priors_2020 = na_replace(priors_2020,0)

priors_2019 = read_csv("https://raw.githubusercontent.com/lbenz730/NCAA_Hoops/master/3.0_Files/Info/stats_2019.csv")

ppg_df_2020$team = str_replace_all(ppg_df_2020$team,setNames(name_dict$NCAA,name_dict$Trank))
ppg_df_2020$team = gsub("U. ","",ppg_df_2020$team)
ppg_df_2020$team[ppg_df_2020$team=="Ga. Southern U."] = "Ga. Southern"
ppg_df_2020$team[ppg_df_2020$team=="Texas Southern U."] = "Texas Southern"
priors_2019$team[which(priors_2019$team=="Ole Miss")] = "Mississippi"
ppg_df_2020$team = str_replace_all(ppg_df_2020$team,setNames(setdiff(unique(priors_2019$team),unique(ppg_df_2020$team))[2:8],c("Davis","Irvine","Riverside","Santa Barbara","Southern California Upstate","Arlington","Martin")))

priors_2019 = left_join(priors_2019,ppg_df_2020,by="team")

priors_2019 = inner_join(priors_2019,priors_2020[,c(1:4)],by="team") %>% 
  select(-conf) %>% 
  mutate(recruited_ppg = as.numeric(recruited_ppg))
priors_2019 = na_replace(priors_2019,0)

# begin priors regression and fitting with 10-fold cross validation
set.seed(123)
train.control <- trainControl(method = "cv", number = 10,savePredictions = TRUE)
# Train the model
model_off <- caret::train(off_coeff.y ~returning_ppg.x+mins+off_coeff.x+composite_score+recruited_ppg+(stars_5>0)+((stars_3+stars_4+stars_5)>2), data = priors_2019, method = "lm",
                          trControl = train.control,na.action=na.exclude)
model_def = caret::train(def_coeff.y ~returning_ppg.x+mins+def_coeff.x+composite_score+recruited_ppg+(stars_5>2), data = priors_2019, method = "lm",
                         trControl = train.control,na.action=na.exclude)
model_yusag = caret::train(yusag_coeff.y ~returning_ppg.x+mins+def_coeff.x+composite_score+recruited_ppg+(stars_5>2), data = priors_2019, method = "lm",
                           trControl = train.control,na.action=na.exclude)
# Summarize the results
print(model_off)
print(model_def)
print(model_yusag)

fit_2019_off = lm(model_off,data=priors_2019)
off_2019_prior = predict(fit_2019_off,se=TRUE)

fit_2019_def = lm(model_def,data=priors_2019)
def_2019_prior = predict(fit_2019_def,se=TRUE)

fit_2019_yusag = lm(model_yusag,data=priors_2019)
yusag_2019_prior = predict(fit_2019_yusag,se=TRUE)

preds_off = cbind(unlist(off_2019_prior$fit),unlist(off_2019_prior$se.fit))
colnames(preds_off) = c("prior_off","se_off")

preds_def = cbind(unlist(def_2019_prior$fit),unlist(def_2019_prior$se.fit))
colnames(preds_def) = c("prior_def","se_def")

preds_yusag = cbind(unlist(yusag_2019_prior$fit),unlist(yusag_2019_prior$se.fit))
colnames(preds_yusag) = c("prior_yusag","se_yusag")

preds_full = as.data.frame(cbind(preds_off,preds_def,preds_yusag))

priors_2019_full = merge(priors_2019,preds_full,by=0,all=TRUE) %>% 
  select(-`Row.names`) %>% 
  mutate(preds_yusag_2019 = prior_off+prior_def,
         se_yusag_2019 = se_yusag)

# now run regression for 2020 priors data frame
priors_2020 = priors_2020 %>% 
  rename(`off_coeff.x`=off_coeff,
         `def_coeff.x`=def_coeff,
         `yusag_coeff.x`=yusag_coeff,
         `departing_ppg.x`=departing_ppg,
         `returning_ppg.x`=returning_ppg) %>% 
  mutate(`returning_ppg.x`=as.numeric(`returning_ppg.x`),
         recruited_ppg = as.numeric(recruited_ppg),
         `departing_ppg.x`=as.numeric(`departing_ppg.x`))
priors_2020$team = str_replace_all(priors_2020$team,setNames(c("Loyola Chicago","UC Davis","UC Santa Barbara","UT Arlington"),c("Loyola (Chi)","Davis","Santa Barbara","Arlington")))

off_2020_prior = predict(fit_2019_off,newdata = priors_2020 ,se=TRUE)
def_2020_prior = predict(fit_2019_def,newdata = priors_2020,se=TRUE)
yusag_2020_prior = predict(fit_2019_yusag,newdata = priors_2020,se=TRUE)


preds_off = cbind(unlist(off_2020_prior$fit),unlist(off_2020_prior$se.fit))
colnames(preds_off) = c("prior_off","se_off")

preds_def = cbind(unlist(def_2020_prior$fit),unlist(def_2020_prior$se.fit))
colnames(preds_def) = c("prior_def","se_def")

preds_full_2020 = cbind(preds_off,preds_def)

priors_2020_full = merge(priors_2020,preds_full_2020,by=0,all=TRUE) %>% 
  select(-`Row.names`) %>% 
  mutate(preds_yusag = prior_off+prior_def,
         se_yusag = sqrt((se_off^2)+(se_def^2)+(2*cov(prior_off,prior_def))))
write.csv(priors_2020_full,"priors_input.csv")

priors_2020_full = priors_2020_full %>% 
  mutate(se_yusag = sqrt((se_off^2)+(se_def^2)+(2*cov(priors_2020_full$prior_off,priors_2020_full$prior_def,use="complete.obs"))))
priors_2020_full$team[which(priors_2020_full$team=="Loyola (Chi)")] = "Loyola Chicago"

priors = priors_2020_full %>% 
  select(c(team,preds_yusag,prior_off,prior_def)) %>% 
  rename(yusag_coeff=preds_yusag,
         off_coeff=prior_off,
         def_coeff=prior_def) %>% 
  filter(team %in% x$team)

trank = read.csv("BT.csv") %>% 
  filter(TEAM != "TEAM") %>% 
  select(TEAM,CONF,ADJOE,ADJDE) %>% 
  mutate(ADJOE=as.numeric(ADJOE),
         ADJDE = as.numeric(ADJDE))
trank = trank[seq(1,nrow(trank),by=2),]
corr_df = left_join(priors,trank,by=c("team"="TEAM"))
summary(lm(off_coeff~as.numeric(ADJOE),data=corr_df))
summary(lm(def_coeff~as.numeric(ADJDE),data=corr_df))

missing1 = c("Dixie State","Tarleton","UC San Diego","UM Kansas City","USC Upstate") # put these into the df
missing2 = c("Dixie St.","Tarleton","UC San Diego","UMKC","USC Upstate")
for(i in 1:length(missing1)){
priors = add_row(priors,team=missing1[i],yusag_coeff=(-53.94464+(.52492*trank$ADJOE[grep(missing2[i],trank$TEAM)]))+(44.51966+(-.44234*trank$ADJDE[grep(missing2[i],trank$TEAM)])),off_coeff=(-53.94464+(.52492*trank$ADJOE[grep(missing2[i],trank$TEAM)])),def_coeff=(44.51966+(-.44234*trank$ADJDE[grep(missing2[i],trank$TEAM)])))
}
# get rid of the extra San Diego
priors = priors[-which(priors$team=="San Diego")[1],]
priors = priors[-which(priors$team=="USC Upstate")[1],]

priors = priors %>% 
  arrange(team)

write.csv(priors,"priors_final.csv")

rank_priors_conf = function(conf){
  if(conf=="all"){
    team_ratings = x %>% 
      arrange(-preds_yusag) %>% 
      select(team,preds_yusag,se_yusag)
  }
  else{
  priors_conf = priors_2020_full %>% 
    filter(team %in% unique(mins_2021$team[mins_2021$conf==conf])) %>% 
    arrange(-preds_yusag) %>% 
    select(team,preds_yusag,se_yusag)
  }
  return(priors_conf)
}

rank_ratings_conf = function(conf){
  if(conf=="all"){
    team_ratings = x %>% 
      arrange(-yusag_coeff) %>% 
      group_by(team) %>% 
      mutate(predrec = paste(round(sum(wins,na.rm=TRUE),digits=0),"-",round(length(wins)-sum(wins,na.rm=TRUE),digits=0),sep="")) %>% 
      select(team,yusag_coeff,predrec) %>% 
      distinct()
  }
  else{
    team_ratings = x %>% 
      filter(team_conf==conf) %>% 
      arrange(-yusag_coeff) %>% 
      group_by(team) %>% 
      mutate(predrec = paste(round(sum(wins,na.rm=TRUE),digits=0),"-",round(length(wins)-sum(wins,na.rm=TRUE),digits=0),sep="")) %>% 
      select(team,yusag_coeff,predrec) %>% 
      distinct()
  }
  return(team_ratings)
}

conf_ranks = list()
for(conf in unique(mins_2021$conf)[1:32]){
  conf_ranks[[conf]] = rank_priors_conf(conf)
}

plot_conf_prior = function(conf,type){
  df = rank_priors_conf(conf) %>% 
    group_by(team) %>% 
    summarise(density = rnorm(1000,preds_yusag,se_yusag)) %>% 
    mutate(run = row_number()) %>% 
    group_by(run) %>% 
    mutate(rank = rank(desc(density),ties.method="average"))
  
  if(type=="density"){
  priors_teams_plot = ggplotly(ggplot(df) + 
                                 geom_density(aes(x = density, colour = team)) + 
                                 scale_colour_manual(values=ncaa_colors$primary_color[ncaa_colors$ncaa_name %in% mins_2021$team[mins_2021$conf==conf]]) +
                                 labs(title=paste0("Conference ranking density plot for ",conf),x = 'Points Above Avg. Team', y = 'Density'),caption="twitter: @databyKearns")
    
  }
  if(type=="box"){
    priors_teams_plot = ggplot(df) + 
                                   geom_boxplot(aes(x = reorder(team,-rank),y=rank, colour = team)) + 
                                   scale_colour_manual(values=ncaa_colors$primary_color[ncaa_colors$ncaa_name %in% conferences$team[conferences$conference==conf]]) +
                                   labs(title=paste0("Conference ranking box plot for ",conf),y = 'Conference Ranking',x="team",caption="twitter: @databyKearns") +
                                   theme(axis.text.x = element_text(angle=45,size=12),axis.text.y=element_text(size=12),plot.title=element_text(size=15),legend.position="none",plot.caption = element_text(size=15))+
                                   scale_y_continuous(trans="reverse")
  }
  
  return(priors_teams_plot)
}