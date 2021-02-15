# NBN_Model.R
# Goal: begin recreating models from Luke Benz geared towards CAA, similar 
#       to how he focused on the Ivy League
# John Kearns
# Date Created: 10/12/2020
# Last Updated: 10/12/2020

setwd("~/2020/NBN/Model/Files")

# install and load packages
library(tidyverse)
library(readr)
library(lubridate)
library(ncaahoopR)
library(reshape2)
library(bigballR)
library(glmnet)

setwd("~/2020/NBN/Model/")

# Read in season data and results
#x = read.csv(url("https://raw.githubusercontent.com/lbenz730/NCAA_Hoops/master/3.0_Files/Results/2020-21/NCAA_Hoops_Results_12_2_2020.csv"))
source("get_schedule.R")

# Read in Train from FixingFilesandFuncs.R
#train = read.csv(url("https://raw.githubusercontent.com/lbenz730/NCAA_Hoops/master/3.0_Files/Results/2017-18/training.csv"))
train = read.csv(url("https://raw.githubusercontent.com/lbenz730/NCAA_Hoops/master/3.0_Files/Results/2017-18/training.csv"))%>% 
  mutate(weights=1)

# Read in conferences from FixingFilesandFuncs.R
#conferences = read.csv(url("https://raw.githubusercontent.com/lbenz730/NCAA_Hoops/master/3.0_Files/Info/conferences.csv"))
conferences = read.csv("conferences.csv")
ineligible = c("Alabama A&M","Alabama State","Auburn","Delaware State","SFA","Arizona")
conferences$eligible = ifelse(conferences$team %in% ineligible,FALSE,TRUE)
conferences$eliminated = FALSE
conferences$eliminated = ifelse(conferences$eligible==FALSE,TRUE,FALSE)

# Read in deadlines
#deadlines = read.csv(url("https://raw.githubusercontent.com/lbenz730/NCAA_Hoops/master/3.0_Files/Info/deadlines.csv")) %>% 
#  mutate(deadline = as.Date(deadline, "%m/%d/%y"))
deadlines = read.csv("deadlines.csv") %>% 
  mutate(deadline = as.Date(deadline,format="%Y-%m-%d")) %>% 
  select(-X)
# need to eventually amend this

# Write code to get priors
# priors = read.csv(url("https://raw.githubusercontent.com/lbenz730/NCAA_Hoops/master/3.0_Files/Info/prior.csv"))
# source(get_priors.R)
priors = read.csv("priors_final.csv")


# Data cleaning
x = z %>% 
  filter(canceled==FALSE)
x$teamscore = as.numeric(x$teamscore)
x$oppscore = as.numeric(x$oppscore)
x <- x %>%
  rename(team_score = teamscore, opp_score = oppscore) %>%
  mutate(date = as.Date(paste(year, month, day, sep = "-")),
         score_diff = team_score - opp_score,
         total_score = team_score + opp_score,
         season_id = "2020-21", game_id = NA, opp_game_id = NA, 
         team_conf = NA, opp_conf = NA, conf_game = NA) %>%
  select(date, team, opponent, location, team_score, opp_score,
         score_diff, total_score, game_id, opp_game_id, team_conf, opp_conf,
         year, month, day, season_id, D1, OT) %>%
  filter(`D1` == 2)

teams <- sort(unique(x$team))

### Game IDs
for(i in 1:length(teams)) {
  x[x$team == teams[i],] <- x %>%
    arrange(date) %>% 
    filter(team == teams[i]) %>%
    mutate(game_id = seq(1, sum(team == teams[i]), 1))
}

### Opp Game IDs
x <- 
  inner_join(x, select(x, date, team, opponent, game_id), 
             by = c("team" = "opponent",
                    "opponent" = "team", 
                    "date" = "date")) %>%
  filter(!duplicated(paste(team, game_id.x))) %>%
  mutate(opp_game_id = game_id.y) %>%
  rename(game_id = game_id.x) %>%
  select(-game_id.y)

### Append conference information
x <- mutate(x, team_conf = sapply(x$team, get_conf),
            opp_conf = sapply(x$opponent, get_conf),
            conf_game = identical(team_conf,opp_conf))
x = x %>% 
  mutate(team_conf = as.character(team_conf),
         opp_conf = as.character(opp_conf),
         conf_game = team_conf==opp_conf,
         D1 = ifelse(team_conf=="character(0)"|opp_conf=="character(0)",1,2)) %>% 
  filter(D1==2)

### Regular season indicator
x <- inner_join(x, deadlines, by = c("team_conf" = "conf")) %>%
  mutate(reg_season = ifelse(date < deadline,TRUE,FALSE)) %>%
  select(-deadline)

### Eliminate Teams from Auto Bid contention
conferences = eliminate(filter(x, score_diff < 0, !reg_season)[,2], conferences)

################################# Set Weights ##################################
x$weights <- 0
for(i in 1:nrow(x)) {
  w_team <- 1 - (max(c(0, x$game_id[x$team == x$team[i] & !is.na(x$score_diff)])) - x$game_id[i])/
    max(c(0, x$game_id[x$team == x$team[i] & !is.na(x$score_diff)]))
  w_opponent <- 1 - (max(c(0, x$game_id[x$team == x$opponent[i] & !is.na(x$score_diff)])) - x$opp_game_id[i])/
    max(c(1, x$game_id[x$team == x$opponent[i] & !is.na(x$score_diff)]))
  rr <- mean(c(w_team, w_opponent))
  x$weights[i] <- 1/(1 + (0.5^(5 * rr)) * exp(-rr))
}   

length(unique(x$team))
############################### Create Models ##################################
### Current Season
lm.hoops <- lm(score_diff ~ team + opponent + location, weights = weights, data = x) 
lm.off <- lm(team_score ~ team + opponent + location, weights = weights, data = x) 
lm.def <- lm(opp_score ~ team + opponent + location, weights = weights, data = x)


### Update With Pre-Season Priors
priors = priors %>% 
  filter(team %in% teams)

priors <- mutate(priors, 
                 rel_yusag_coeff = yusag_coeff - yusag_coeff[1],
                 rel_off_coeff = off_coeff - off_coeff[1],
                 rel_def_coeff = def_coeff - def_coeff[1]) %>%
  arrange(team) 
  

w <- sapply(unique(priors$team), prior_weight)
w = ifelse(w>mean(w),mean(w),w)

#### for pre-season, weight is equal to 1: ####
selectedteams = cbind(c(lm.hoops$coefficients[2:339]),c(lm.hoops$coefficients[(339+1):(339*2-1)]))
selectedteams = rownames_to_column(as.data.frame(selectedteams))
selectedteams = as.data.frame(selectedteams) %>%  rowwise() %>% mutate(test = V1) %>% select(c(rowname,test))
lm.hoops = data.frame(c(lm.hoops$coefficients[1]+lm.hoops$coefficients[339*2],rep(0,(nrow(priors)-1)*2),lm.hoops$coefficients[1]+lm.hoops$coefficients[339*2+1],lm.hoops$coefficients[1]+lm.hoops$coefficients[339*2+2]))
colnames(lm.hoops) = "coefficients"
rownames(lm.hoops)[1] = "(Intercept)"
rownames(lm.hoops)[2:(nrow(lm.hoops))] = c(paste(c("team"),priors$team[2:nrow(priors)],sep=""),paste(c("opponent"),priors$team[2:nrow(priors)],sep=""),"locationN","locationA")
lm.hoops = rownames_to_column(lm.hoops)
lm.hoops = full_join(lm.hoops,selectedteams,by="rowname")
for(i in 1:nrow(lm.hoops)){
  if(is.na(lm.hoops$test[i])){
    lm.hoops$test[i] = lm.hoops$coefficients[i]
  }
}
lm.hoops = lm.hoops[,-2]
colnames(lm.hoops) = c("team","coefficients")

selectedteams = cbind(c(lm.off$coefficients[2:339]),c(lm.off$coefficients[(339+1):(339*2-1)]))
selectedteams = rownames_to_column(as.data.frame(selectedteams))
selectedteams = as.data.frame(selectedteams) %>%  rowwise() %>% mutate(test = V1) %>% select(c(rowname,test))
lm.off = data.frame(c(lm.off$coefficients[1]+lm.off$coefficients[339*2],rep(0,(nrow(priors)-1)*2),lm.off$coefficients[1]+lm.off$coefficients[339*2+1],lm.off$coefficients[1]+lm.off$coefficients[339*2+2]))
colnames(lm.off) = "coefficients"
rownames(lm.off)[1] = "(Intercept)"
rownames(lm.off)[2:(nrow(lm.off))] = c(paste(c("team"),priors$team[2:nrow(priors)],sep=""),paste(c("opponent"),priors$team[2:nrow(priors)],sep=""),"locationN","locationA")
lm.off = rownames_to_column(lm.off)
lm.off = full_join(lm.off,selectedteams,by="rowname")
for(i in 1:nrow(lm.off)){
  if(is.na(lm.off$test[i])){
    lm.off$test[i] = lm.off$coefficients[i]
  }
}
lm.off = lm.off[,-2]
colnames(lm.off) = c("team","coefficients")

selectedteams = cbind(c(lm.def$coefficients[2:339]),c(lm.def$coefficients[(339+1):(339*2-1)]))
selectedteams = rownames_to_column(as.data.frame(selectedteams))
selectedteams = as.data.frame(selectedteams) %>%  rowwise() %>% mutate(test = V1) %>% select(c(rowname,test))
lm.def = data.frame(c(lm.def$coefficients[1]+lm.def$coefficients[339*2],rep(0,(nrow(priors)-1)*2),lm.def$coefficients[1]+lm.def$coefficients[339*2+1],lm.def$coefficients[1]+lm.def$coefficients[339*2+2]))
colnames(lm.def) = "coefficients"
rownames(lm.def)[1] = "(Intercept)"
rownames(lm.def)[2:(nrow(lm.def))] = c(paste(c("team"),priors$team[2:nrow(priors)],sep=""),paste(c("opponent"),priors$team[2:nrow(priors)],sep=""),"locationN","locationA")
lm.def = rownames_to_column(lm.def)
lm.def = full_join(lm.def,selectedteams,by="rowname")
for(i in 1:nrow(lm.def)){
  if(is.na(lm.def$test[i])){
    lm.def$test[i] = lm.def$coefficients[i]
  }
}
lm.def = lm.def[,-2]
colnames(lm.def) = c("team","coefficients")
####

# -1 because you do not want to include the home intercept term
lm.hoops$coefficients[2:nrow(priors)] <- 
  lm.hoops$coefficients[2:nrow(priors)] * w[-1] + priors$rel_yusag_coeff[-1] * (1-w[-1])
lm.hoops$coefficients[(nrow(priors)+1):(nrow(priors)*2-1)] <- 
  lm.hoops$coefficients[(nrow(priors)+1):(nrow(priors)*2-1)] * w[-1] - priors$rel_yusag_coeff[-1] * (1-w[-1])
lm.off$coefficients[2:nrow(priors)] <- 
  lm.off$coefficients[2:nrow(priors)] * w[-1] + priors$rel_off_coeff[-1] * (1-w[-1])
lm.off$coefficients[(nrow(priors)+1):(nrow(priors)*2-1)] <- 
  lm.off$coefficients[(nrow(priors)+1):(nrow(priors)*2-1)] * w[-1] - priors$rel_off_coeff[-1] * (1-w[-1])
lm.def$coefficients[2:nrow(priors)] <- 
  lm.def$coefficients[2:nrow(priors)] * w[-1] - priors$rel_def_coeff[-1] * (1-w[-1])
lm.def$coefficients[(nrow(priors)+1):(nrow(priors)*2-1)] <- 
  lm.def$coefficients[(nrow(priors)+1):(nrow(priors)*2-1)] * w[-1] + priors$rel_def_coeff[-1] * (1-w[-1])

# set the Home, Away, and Neutral coefficients for this season
lm.hoops$coefficients[c(1, (nrow(priors)*2):(nrow(priors)*2+1))] <- 
  w[1] * lm.hoops$coefficients[c(1, (nrow(priors)*2):(nrow(priors)*2+1))] + 
  (1-w[1]) * c(3.325, -3.325, -6.65)
lm.off$coefficients[c(1, (nrow(priors)*2):(nrow(priors)*2+1))] <- 
  w[1] * lm.off$coefficients[c(1, (nrow(priors)*2):(nrow(priors)*2+1))] + 
  (1-w[1]) * c(61.6856, -2.8523,  -3.3325)
lm.def$coefficients[c(1, (nrow(priors)*2):(nrow(priors)*2+1))] <- 
  w[1] * lm.def$coefficients[c(1, (nrow(priors)*2):(nrow(priors)*2+1))] + 
  (1-w[1]) * c(58.3531, 0.4803,  3.3325)


x = x %>% 
  filter(!(date<"2021-02-14"&is.na(team_score)))

################################ Power Rankings ################################
power_rankings <- pr_compute(by_conf = FALSE)
history <- read.csv("history/history.csv")
x <- x %>% 
  mutate(x, pr_date = as.character(Sys.Date())) %>%
  inner_join(select(history, team, yusag_coeff, rank, date, off_coeff, def_coeff), 
             by = c("team" = "team","pr_date" = "date")) %>%
  inner_join(select(history, team, yusag_coeff, rank, date, off_coeff, def_coeff), 
             by = c("opponent" = "team","pr_date" = "date")) %>% 
  rename(rank = rank.x, opp_rank = rank.y, 
         yusag_coeff = yusag_coeff.x, opp_yusag_coeff = yusag_coeff.y,
         off_coeff = off_coeff.x, opp_off_coeff = off_coeff.y,
         def_coeff = def_coeff.x, opp_def_coeff = def_coeff.y) %>%
  select(-pr_date)

# for pre-season:
#history = power_rankings
#x = x %>% 
#  inner_join(select(history, team, yusag_coeff, rank, off_coeff, def_coeff), 
#             by = c("team" = "team")) %>% 
#  inner_join(select(history, team, yusag_coeff, rank, off_coeff, def_coeff), 
#             by = c("opponent" = "team")) %>% 
#  rename(rank = rank.x, opp_rank = rank.y, 
#         yusag_coeff = yusag_coeff.x, opp_yusag_coeff = yusag_coeff.y,
#         off_coeff = off_coeff.x, opp_off_coeff = off_coeff.y,
#         def_coeff = def_coeff.x, opp_def_coeff = def_coeff.y)
  
############################### Predictions ####################################
x <- 
  mutate(x, "hca" = case_when(location == "H" ~ lm.hoops$coefficients[1],
                              location == "V" ~ -lm.hoops$coefficients[1],
                              location == "N" ~ 0),
         "hca_off" = case_when(location == "H" ~ abs(lm.off$coefficients[1]),
                               location == "V" ~ lm.off$coefficients[nrow(priors)*2+1] - lm.off$coefficients[1],
                               location == "N" ~ 0),
         "hca_def" = case_when(location == "H" ~ -lm.def$coefficients[1],
                               location == "V" ~ lm.def$coefficients[nrow(priors)*2+1] - lm.def$coefficients[1],
                               location == "N" ~ 0)) %>%
  mutate("pred_score_diff" = round(yusag_coeff - opp_yusag_coeff + hca, 1),
         "pred_team_score" = round(70 + off_coeff - opp_def_coeff + hca_off, 1),
         "pred_opp_score" = round(70 -def_coeff + opp_off_coeff + hca_def, 1),
         "pred_total_score" = pred_team_score + pred_opp_score) %>%
  select(-hca, -hca_off, -hca_def)



######################## Point Spread to Win Percentage Model #################
# for pre-season:
#preds_save = read_csv(url("https://raw.githubusercontent.com/lbenz730/NCAA_Hoops/master/3.0_Files/Results/2019-20/NCAA_Hoops_Results_3_9_2020.csv"))
#preds = preds_save
#ranks_save = read_csv(url("https://raw.githubusercontent.com/lbenz730/NCAA_Hoops/master/3.0_Files/History/history_2019_20.csv")) %>% 
#  filter(date=="2020-03-12")
#ranks=ranks_save
#preds =  %>% 
#  inner_join(select(ranks, team, yusag_coeff, rank, off_coeff, def_coeff), 
#             by = c("team" = "team")) %>% 
#  inner_join(select(ranks, team, yusag_coeff, rank, off_coeff, def_coeff), 
#             by = c("opponent" = "team")) %>% 
#  rename(rank = rank.x, opp_rank = rank.y, 
#         yusag_coeff = yusag_coeff.x, opp_yusag_coeff = yusag_coeff.y,
#         off_coeff = off_coeff.x, opp_off_coeff = off_coeff.y,
#         def_coeff = def_coeff.x, opp_def_coeff = def_coeff.y) %>% 
#  mutate(score_diff = teamscore-oppscore,
#         location = ifelse(location=="V","V",location))

#x <- 
#  mutate(x, "hca" = case_when(location == "H" ~ lm.hoops$coefficients[1],
#                              location == "V" ~ -lm.hoops$coefficients[1],
#                              location == "N" ~ 0),
#         "hca_off" = case_when(location == "H" ~ -1*(lm.off$coefficients[nrow(priors)*2+1] - lm.off$coefficients[nrow(priors)*2]),
#                               location == "V" ~ lm.off$coefficients[nrow(priors)*2+1] - lm.off$coefficients[nrow(priors)*2],
#                               location == "N" ~ 0),
#         "hca_def" = case_when(location == "H" ~ -(lm.def$coefficients[nrow(priors)*2+1] - lm.def$coefficients[nrow(priors)*2]),
#                               location == "V" ~ lm.def$coefficients[nrow(priors)*2+1] - lm.def$coefficients[nrow(priors)*2],
#                               location == "N" ~ 0)) %>%
#  mutate("pred_score_diff" = round(yusag_coeff - opp_yusag_coeff + hca, 1),
#         "pred_team_score" = round(70 + off_coeff - opp_def_coeff + hca_off, 1),
#         "pred_opp_score" = round(70 -def_coeff + opp_off_coeff + hca_def, 1),
#         "pred_total_score" = pred_team_score + pred_opp_score) %>%
#  select(-hca, -hca_off, -hca_def)

x$wins = ifelse(x$score_diff>0,1,ifelse(x$score_diff<0,0,NA))
# regular code starts here
x$x[x$score_diff > 0] <- 1
x$wins[x$score_diff < 0] <- 0

glm.pointspread <- glm(wins ~ pred_score_diff, 
                       data = bind_rows(select(train, wins, pred_score_diff),
                                        select(x, wins, pred_score_diff)), 
                       family=binomial) 
saveRDS(glm.pointspread, file = "glm_pointspread.rds")
x$wins[is.na(x$wins)] <- 
  round(predict(glm.pointspread, newdata = x[is.na(x$wins),], type = "response"), 3)
by_conf <- pr_compute(by_conf = T)
write.csv(x, "Predictions/predictions.csv")

########################### Bracketology #######################################
#rpi <- rpi_compute(new = T)
#resumes <- get_resumes(new = T)
#bracket <- make_bracket(tourney = T)
#bracket_math <- make_bracket(tourney = F)

############################## NCAA Sims #######################################
#ncaa_sims <- ncaa_sim(10000)
#write.csv(ncaa_sims, "3.0_Files/Predictions/ncaa_sims.csv", row.names = F)

################################ CAA Sims ######################################
playoffs <- caa.sim(nsims = 5000)
playoff_graphic()
psf_results <- psf(nsims = 500, min_date = "2021-01-30", max_date = "2021-04-01")
psf_graphic()
############################# Conference Sims (No Tie-Breaks) ##################
for(conf in unique(conferences$conference)) {
  print(conf)
  sims <- conf_sim(conf, 5000)
  write.csv(sims, paste0("Predictions/conf_sims/", str_to_lower(conf), ".csv"))
 }
############################ System Evaluation #################################
min_date <- as.Date("2020-11-25")
max_date <- Sys.Date()
y <- filter(x, date >= min_date, date <= max_date)
cat(paste("System Evaluation:", min_date, "Through", max_date),
    "\n-------------------------------------------------------------\n",
    "Predictive Accuracy: ",
    round(100 * mean(sign(y$pred_score_diff) == sign(y$score_diff), na.rm = T), 1), "%\n", 
    "Mean Absolute Error in Predicted Score Differential: ",
    round(mean(abs(y$pred_score_diff - y$score_diff), na.rm = T), 2), "\n",
    "Games w/in 2 Points of Observed Score Differential: ",
    round(100 * mean(abs(y$pred_score_diff - y$score_diff) <= 2, na.rm = T), 2), "%\n",
    "Games w/in 5 Points of Observed Score Differential: ",
    round(100 * mean(abs(y$pred_score_diff - y$score_diff) <= 5, na.rm = T), 2), "%\n",
    "Predicted Totals Score <= Total Score: ",
    round(100 * mean(y$pred_total_score <= y$total_score, na.rm = T), 1), "%\n",
    "Predicted Totals Score > Total Score: ",
    round(100 * mean(y$pred_total_score > y$total_score, na.rm = T), 1), "%\n",
    "Mean Absolute Error in Total Score Predictions: ",
    round(mean(abs(y$pred_total_score - y$total_score), na.rm = T),  2), "\n",
    "Games w/in 2 Points of Observed Total Score: ",
    round(100 * mean(abs(y$pred_total_score - y$total_score) <= 2, na.rm = T), 2), "%\n",
    "Games w/in 5 Points of Observed Total Score: ",
    round(100 * mean(abs(y$pred_total_score - y$total_score) <= 5, na.rm = T), 2), "%\n",
    sep = "")
nrow(filter(x, round(pred_team_score) == team_score))
filter(x, round(pred_team_score) == team_score, round(pred_opp_score) == opp_score)