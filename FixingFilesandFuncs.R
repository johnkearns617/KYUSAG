# FixingFilesandFuncs.R
# Goal: Amending Luke Benz files and functions so that I can use them
# John Kearns
# 10/12/2020

library(tidyverse)
library(caret)
library(nnetpredint)
library(ncaahoopR)
library(lubridate)


# Conferences.csv
conferences = read.csv(url("https://raw.githubusercontent.com/lbenz730/NCAA_Hoops/master/3.0_Files/Info/conferences.csv"))
# ineligible = c("Alabama A&M","Alabama State","Delaware State","Stephen F. Austin","Oklahoma State")
# conferences$eligible = ifelse(conferences$team %in% ineligible,FALSE,TRUE)
# conferences$eliminated = FALSE
# conferences$eliminated = ifelse(conferences$eligible=FALSE,TRUE,FALSE)

# Train.csv for 2020/21 season
train = read.csv(url("https://raw.githubusercontent.com/lbenz730/NCAA_Hoops/master/3.0_Files/Predictions/predictions.csv"))[,c(1:7,9:18,20,30,34)] %>% 
  mutate(weights=1)
# use predictions file from the previous season

# Write up data frame with conferences and last day of tournament in left and right columns
# insert code here
deadlines = read.csv(url("https://raw.githubusercontent.com/lbenz730/NCAA_Hoops/master/3.0_Files/Info/deadlines.csv"))

# Write code to get priors
#priors = read.csv(url("https://raw.githubusercontent.com/lbenz730/NCAA_Hoops/master/3.0_Files/Info/prior.csv"))
# see get_priors.R

# get colors
ncaa_colors = read.csv("https://raw.githubusercontent.com/lbenz730/NCAA_Hoops_Play_By_Play/master/ncaa_colors.csv")

# visualize priors
#priors_teams = subset(priors_2020_full,team %in% conferences$team[conferences$conference=="CAA"]) %>% 
#  group_by(team) %>% 
#  summarise(density = rnorm(200,mean(preds_yusag),mean(se_yusag)))

#priors_teams_plot = ggplotly(ggplot(priors_teams) + 
#                               geom_density(aes(x = density, colour = team)) + 
#                               scale_colour_manual(values=ncaa_colors$primary_color[ncaa_colors$ncaa_name %in% conferences$team[conferences$conference=="CAA"]]) +
#                               labs(x = 'Points Above Avg. Team', y = 'Density'))



# take the functions written by Benz and write your own versions
source_url("https://raw.githubusercontent.com//lbenz730/NCAA_Hoops/tree/master/3.0_Files/powerrankings.R")
source_url("https://raw.githubusercontent.com/lbenz730/NCAA_Hoops/tree/master/3.0_Files/Ivy_Sims.R")
source_url("https://raw.githubusercontent.com/lbenz730/NCAA_Hoops/tree/master/3.0_Files/rpi.R")
source_url("https://raw.githubusercontent.com/lbenz730/NCAA_Hoops/tree/master/3.0_Files/record_evaluator.R")
source_url("https://raw.githubusercontent.com/lbenz730/NCAA_Hoops/tree/master/3.0_Files/bracketology.R")
source_url("https://raw.githubusercontent.com/lbenz730/NCAA_Hoops/master/3.0_Files/helpers.R?raw=TRUE")
source_url("https://raw.githubusercontent.com/lbenz730/NCAA_Hoops/tree/master/3.0_Files/tourney_sim.R?raw=TRUE")
source_url("https://raw.githubusercontent.com/lbenz730/NCAA_Hoops/tree/master/3.0_Files/ncaa_sims.R")



pr_compute <- function(by_conf) {
  ### Avg to scale coefficients to 0
  avg <- mean(lm.hoops$coefficients[2:nrow(priors)], na.rm = T)
  off_avg <-  mean(lm.off$coefficients[2:nrow(priors)], na.rm = T)
  def_avg <- mean(lm.def$coefficients[2:nrow(priors)], na.rm = T)
  
  ### Store Power Rankings Data Frame
  power_rankings <- as.data.frame(priors$team) %>% 
    rename(team=`priors$team`) %>% 
    rowwise() %>% 
    mutate(conference=conferences$conference[conferences$team==team]) %>% 
    ungroup()
  
  power_rankings <- mutate(power_rankings,
                           yusag_coeff = c(0, lm.hoops$coefficients[2:nrow(priors)]) - avg,
                           off_coeff = c(0, lm.off$coefficients[2:nrow(power_rankings)]) - off_avg,
                           def_coeff = -(c(0, lm.def$coefficients[2:nrow(power_rankings)]) - def_avg))
  power_rankings <- 
    arrange(power_rankings, desc(yusag_coeff)) %>%
    mutate(rank = 1:nrow(power_rankings)) %>%
    arrange(desc(off_coeff)) %>%
    mutate(off_rank = 1:nrow(power_rankings)) %>%
    arrange(desc(def_coeff)) %>%
    mutate(def_rank = 1:nrow(power_rankings)) %>%
    arrange(desc(yusag_coeff)) %>% 
    select(c(team,conference,yusag_coeff,off_coeff,def_coeff,rank,off_rank,def_rank))
  
  ### Return w/out sorting by conference
  if(!by_conf) {
    write.csv(power_rankings, "power_rankings.csv", row.names = F)
    history <- read.csv("history/history.csv", as.is = T)
    today <- unclass(as.POSIXlt(Sys.time()))
    year <- 1900 + today$year
    month <- 1 + today$mon
    day <- today$mday
    today <- as.Date(paste(year, month, day, sep = "-"))
    history <- filter(history, as.Date(date) != today)
    history <- bind_rows(history, mutate(power_rankings, date = as.character(today)))
    write.csv(history, "history/history.csv", row.names = F)
    return(power_rankings)
  }
  else{
    ### Sort by conference
    conferences <- sort(unique(x$team_conf))
    
    sum_stats <- 
      group_by(power_rankings, conference) %>% 
      summarise("mean" = mean(yusag_coeff),
                "median" = median(yusag_coeff),
                "min" = min(yusag_coeff),
                "max" = max(yusag_coeff),
                "sd" = sd(yusag_coeff)) %>%
      arrange(desc(median))
    
    write.csv(sum_stats, "conf_summary.csv", row.names = F)
    
    for(i in 1:length(conferences)) {
      tmp <- filter(power_rankings, conference == conferences[i])
      tmp$info <- paste0("(Conference Rank:  ", which(sum_stats$conference == conferences[i]), ")")
      tmp$conference_rank <- 1:nrow(tmp)
      tmp$record <- "--"
      tmp$conf_record <- "--"
      for(j in 1:nrow(tmp)) {
        wins <- round(sum(x$wins[x$team == tmp$team[j] & x$reg_season]))
        losses <- max(x$game_id[x$team == tmp$team[j] & x$reg_season]) - wins
        tmp$record[j] <- paste(wins, losses, sep = " - " )
        conf_wins <- round(sum(x$wins[x$team == tmp$team[j] & x$reg_season & x$conf_game]))
        conf_losses <- length(x$wins[x$team == tmp$team[j] & x$reg_season & x$conf_game]) - conf_wins
        tmp$conf_record[j] <- paste(conf_wins, conf_losses, sep = " - " )
      }
      if(i > 1){
        pr <- rbind(pr, tmp)
      }
      else{
        pr <- tmp
      }
    }
    write.csv(pr, "pr_by_conf.csv", row.names = F)
    return(pr)
  }
}

############################# Palestra Sims (Ivy Tourney) ########################################
### Simulates Ivy League Tournament
palestra.sim <- function(teams) {
  games = x[x$location == "H" & x$team_conf == "CAA" & x$conf_game & x$reg_season, ]
  tmp <- data.frame("team" = c(teams[7:8]),
                    "opponent" = c(teams[10:9]), 
                    stringsAsFactors = F)
  tmp$location <- "N"
  
  ### Find Penn in Tournament
  #if(teams[1] == "Harvard") {
  #  tmp$location[1] <- "H"
  #}else if(teams[4] == "Harvard") {
  #  tmp$location[1] <- "V"
  #}
  #if(teams[2] == "Harvard") {
  #  tmp$location[2] <- "H"
  #}else if(teams[3] == "Harvard") {
  #  tmp$location[2] <- "V"
  #}
  
  #tmp$pred_score_diff <- predict(pred_model, newdata = tmp)
  for(i in 1:nrow(tmp)){
  tmp$pred_score_diff[i] = last(games$yusag_coeff[games$team==tmp$team[i]])-last(games$yusag_coeff[games$team==tmp$opponent[i]])
  }
  tmp$winprob <- predict(glm.pointspread, newdata = tmp, type = "response")
  
  ### Sim play-ins
  rands <- runif(2)
  tmp[3:4, c("opponent")] <- ifelse(rands <= tmp$winprob[1:2], tmp$team[1:2], tmp$opponent[1:2])
  tmp[3:4,c("team")] = teams[2:1]
  tmp[5:6,c("team")] = teams[3:4]
  tmp[5:6,c("opponent")] = teams[6:5]
  tmp[3:6,c("location")] = "N"
  
  #tmp$pred_score_diff <- predict(pred_model, newdata = tmp)
  for(i in 3:nrow(tmp)){
    tmp$pred_score_diff[i] = last(games$yusag_coeff[games$team==tmp$team[i]])-last(games$yusag_coeff[games$team==tmp$opponent[i]])
  }
  tmp$winprob <- predict(glm.pointspread, newdata = tmp, type = "response")
  
  # sim quarterfinals
  rands = runif(4)
  tmp[7:8, c("team","opponent")] <- ifelse(rands <= tmp$winprob[3:6], tmp$team[3:6], tmp$opponent[3:6])
  tmp[7:8,c("location")] = "N"
  
  #tmp$pred_score_diff <- predict(pred_model, newdata = tmp)
  for(i in 7:nrow(tmp)){
    tmp$pred_score_diff[i] = last(games$yusag_coeff[games$team==tmp$team[i]])-last(games$yusag_coeff[games$team==tmp$opponent[i]])
  }
  tmp$winprob <- predict(glm.pointspread, newdata = tmp, type = "response")
  
  # sim semifinals
  rands = runif(2)
  tmp[9, c("team","opponent")] <- ifelse(rands <= tmp$winprob[7:8], tmp$team[7:8], tmp$opponent[7:8])
  tmp[9,"location"] = "N"
  
  ### Finals
  #if(tmp$team[3] == "Harvard") {
  #  tmp$location[3] <- "H"
  #}else if(tmp$opponent[3] == "Harvard") {
  #  tmp$location[3] <- "V"
  #}
  
  #tmp$pred_score_diff <- predict(pred_model, newdata = tmp)
  tmp$pred_score_diff[9] = last(games$yusag_coeff[games$team==tmp$team[9]])-last(games$yusag_coeff[games$team==tmp$opponent[9]])
  tmp$winprob <- predict(glm.pointspread, newdata = tmp, type = "response")
  
  # Final
  rand <- runif(1)
  champ <- ifelse(rand <= tmp$winprob[9], tmp$team[9], tmp$opponent[9])
  
  return(champ)
}

################################### IVY SIMS ##################################
### Simulates CAA Regular Season
caa.sim <- function(nsims) {
  games <- x[x$location == "H" & x$team_conf == "CAA" & x$conf_game & x$reg_season, ]
  caa <- unique(x$team[x$team_conf == "CAA"])
  champ <- rep(NA, nsims)
  
  # Data Frame to Hold Team Wins by Sim
  simresults <- as.data.frame(matrix(nrow = nsims, ncol = length(caa), byrow = T))
  names(simresults) <- caa
  
  # Stores pre (and later post) tie-break position in standings
  prebreak.pos <- as.data.frame(matrix(nrow = nsims, ncol = length(caa), byrow = T))
  names(prebreak.pos) <- caa
  
  # Simulate All Games
  for (j in 1:nrow(simresults)){
    if(j %% 100 == 0) {
      cat("Sim:", j, "\n")
    }
    games$simwins <- NA
    games$oppsimwins <- NA
    rand <- runif(nrow(games))
    games$simwins[games$wins == 1] <- 1
    games$oppsimwins[games$wins == 1] <- 0
    games$simwins[games$wins == 0] <- 0
    games$oppsimwins[games$wins == 0] <- 1
    sims <- games$wins > 0 & games$wins < 1
    games$simwins[sims] <- ifelse(rand[sims] <= games$wins[sims], 1, 0)
    games$oppsimwins[sims] <- ifelse(rand[sims] > games$wins[sims], 1, 0)
    
    # get team win totals for current sim
    for(i in 1:10) {
      simresults[j, i] <- (sum(games$simwins[games$team == caa[i]]) + 
                             sum(games$oppsimwins[games$opponent == caa[i]]))
    }
    
    # H2H records (Row Team Wins Over Column Team)
    head2head <- matrix(nrow = 10, ncol =10)
    colnames(head2head) <- caa
    rownames(head2head) <- caa
    for(i in 1:10) {
      for(k in 1:10) {
        head2head[i,k] <- sum(games$simwins[games$team == caa[i] & games$opponent == caa[k]]) +
          sum(games$oppsimwins[games$team == caa[k] & games$opponent == caa[i]])
      }
    }
    
    # Get order of finish Pre-Tiebreak
    preBreak <- sort(as.vector(simresults[j,], mode = "numeric"), decreasing = T)
    
    for(z in 1:10) {
      prebreak.pos[j,z] <- c(1:length(caa))[preBreak == simresults[j, z]][1]
    }
    
    # Break any ties 
    for(i in 1:(length(caa) - 1)) {
      if(sum(prebreak.pos[j,] == i) > 1){
        # Get teams to between which to break tie
        teams <- caa[prebreak.pos[j,] == i]
        tie <- length(teams)
        teamIDs <- c(1:length(caa))[is.element(caa, teams)]
        
        # Tiebreak 1 (H2H)
        h2h <- rep(0, length(teams))
        for(k in 1:length(teams)) {
          h2h[k] <- sum(head2head[teams[k], teams[-k]])
        }
        if(sum(h2h == max(h2h)) == 1) {
          winner <- teams[grep(max(h2h), h2h)]
          winnerID <- teamIDs[grep(max(h2h), h2h)]
          # Winner wins tie-break
          simresults[j, winnerID] <- simresults[j, winnerID] + 0.1 * tie
          # Change current standing of losers
          change <- teams[teams != winner]
          prebreak.pos[j, change] <- i + 1
          next
        }
        else if(sum(h2h == max(h2h)) > 1 & sum(h2h == max(h2h)) < length(teams)){
          change <- setdiff(teams, teams[grep(max(h2h), h2h)])
          teams <- teams[grep(max(h2h), h2h)]
          prebreak.pos[j, change] <- i + 1
        }
        
        # Tiebreak 2 (Record vs. 1-8, descending order)
        for(z in 1:length(caa)) {
          if(z == i) {
            next
          }
          comp_teams <- caa[prebreak.pos[j,] == z]
          if(length(comp_teams) == 0) {
            next
          }
          comp_teamsIDs <- c(1:length(caa))[is.element(caa, comp_teams)]
          
          h2h <- rep(0, length(teams))
          for(k in 1:length(teams)) {
            h2h[k] <- sum(head2head[teams[k], comp_teams])
          }
          
          if(sum(h2h == max(h2h)) == 1) {
            winner <- teams[grep(max(h2h), h2h)]
            winnerID <- teamIDs[grep(max(h2h), h2h)]
            # Winner wins tie-break
            simresults[j, winnerID] <- simresults[j, winnerID] + 0.1 * tie
            # Change current standing of losers
            change <- teams[teams != winner]
            prebreak.pos[j, change] <- i + 1
            break
          }
          else if(sum(h2h == max(h2h)) > 1 & sum(h2h == max(h2h)) < length(teams)){
            change <- setdiff(teams, teams[grep(max(h2h), h2h)])
            teams <- teams[grep(max(h2h), h2h)]
            prebreak.pos[j, change] <- i + 1
          }
        }
        if(z < 10){
          next
        }
        
        # Tiebreak 3 (Analytics)
        tmp <- power_rankings[is.element(power_rankings$team, teams),]
        tmp <- tmp[order(tmp$team),]
        winner <- tmp$Team[grep(max(tmp$yusag_coeff), tmp$yusag_coeff)]
        winnerID <- c(1:10)[caa == winner]
        # Winner wins tie-break
        simresults[j, winnerID] <- simresults[j, winnerID] + 0.1 * tie
        # Change current standing of losers
        change <- teams[teams != winner]
        prebreak.pos[j, change] <- i + 1
      }
    }
    
    # Sim CAA tournament
    tourney_seeds <- c(caa[as.vector(prebreak.pos[j,] == 1)], caa[as.vector(prebreak.pos[j,] == 2)],
                  caa[as.vector(prebreak.pos[j,] == 3)], caa[as.vector(prebreak.pos[j,] == 4)],
                  caa[as.vector(prebreak.pos[j,] == 5)], caa[as.vector(prebreak.pos[j,] == 6)],
                  caa[as.vector(prebreak.pos[j,] == 7)], caa[as.vector(prebreak.pos[j,] == 8)],
                  caa[as.vector(prebreak.pos[j,] == 9)], caa[as.vector(prebreak.pos[j,] == 10)])
    champ[j] <- palestra.sim(tourney_seeds)
  }
  
  ### store playoff odds
  playoffs <- data.frame(team = caa,
                         auto_bid = rep(NA, length(caa)),
                         seed1_prob = rep(NA, length(caa)),
                         seed2_prob = rep(NA, length(caa)),
                         seed3_prob = rep(NA, length(caa)),
                         seed4_prob = rep(NA, length(caa)),
                         seed5_prob = rep(NA,length(caa)),
                         seed6_prob = rep(NA,length(caa)),
                         stringsAsFactors = F)
  
  
  for(i in 1:length(caa)) {
    playoffs$auto_bid[i] <- sum(is.element(champ, caa[i]))/nsims * 100
    playoffs$seed1_prob[i] <- sum(prebreak.pos[,i] == 1)/nsims * 100
    playoffs$seed2_prob[i] <- sum(prebreak.pos[,i] == 2)/nsims * 100
    playoffs$seed3_prob[i] <- sum(prebreak.pos[,i] == 3)/nsims * 100
    playoffs$seed4_prob[i] <- sum(prebreak.pos[,i] == 4)/nsims * 100
    playoffs$seed5_prob[i] <- sum(prebreak.pos[,i] == 5)/nsims * 100
    playoffs$seed6_prob[i] <- sum(prebreak.pos[,i] == 6)/nsims * 100
  }
  write.csv(playoffs, "Predictions/playoffs.csv", row.names = F)
  playoff_history <- read.csv("Predictions/playoff_history.csv", as.is = T) %>%
    mutate(date = as.Date(date)) %>%
    filter(date != Sys.Date()) %>%
    rbind(playoffs %>% mutate(date = Sys.Date()))
  write.csv(playoff_history, "Predictions/playoff_history.csv", row.names = F)
  return(playoffs)
}

############################ Playoff Swing Factor ##############################
### compute playoff swing factor (leverage) of each game
psf <- function(nsims, min_date, max_date) {
  tochange <- filter(x, date >= min_date, date <= max_date, team_conf == "CAA",
                     conf_game, location == "H")
  
  
  caa <- unique(x$team[x$team_conf == "CAA"])
  
  # Data Frame to Hold Team Wins by Sim
  simresults <- as.data.frame(matrix(nrow = nsims, ncol = length(caa), byrow = T))
  names(simresults) <- caa
  
  prebreak.pos <- as.data.frame(matrix(nrow = nsims, ncol = length(caa), byrow = T))
  names(prebreak.pos) <- caa
  
  # Create Data Frame to Store SwingFactor
  swingfactor <- data.frame(date = tochange$date,
                            home = tochange$team,
                            away = tochange$opponent,
                            psf = rep(NA, nrow(tochange)),
                            auto_bid_sf = rep(NA, nrow(tochange)),
                            stringsAsFactors = F)
  
  # Switch
  q = 0
  for(k in 1:(2 * nrow(tochange))) {
    games <- x[x$location == "H" & x$team_conf == "CAA" & x$conf_game & x$reg_season, ]
    if(q == 0) {
      games$wins[games$team == tochange$team[ceiling(k/2)] & 
                   games$opponent == tochange$opponent[ceiling(k/2)]] <- 1
      q <- 1
    }
    # Second time through game to change
    else{
      games$wins[games$team == tochange$team[ceiling(k/2)] & 
                   games$opponent == tochange$opponent[ceiling(k/2)]] <- 0
      q <- 0
    }
    
    ### Simulate Games
    champ <- rep(NA, nsims)
    for (j in 1:nrow(simresults)){
      if(j %% 100 == 0) {
        cat("Game_id:", k, "Sim:", j, "\n")
      }
      games$simwins <- NA
      games$oppsimwins <- NA
      rand <- runif(nrow(games))
      games$simwins[games$wins == 1] <- 1
      games$oppsimwins[games$wins == 1] <- 0
      games$simwins[games$wins == 0] <- 0
      games$oppsimwins[games$wins == 0] <- 1
      sims <- games$wins > 0 & games$wins < 1
      games$simwins[sims] <- ifelse(rand[sims] <= games$wins[sims], 1, 0)
      games$oppsimwins[sims] <- ifelse(rand[sims] > games$wins[sims], 1, 0)
      
      # get team win totals for current sim
      for(i in 1:10) {
        simresults[j, i] <- (sum(games$simwins[games$team == caa[i]]) + 
                               sum(games$oppsimwins[games$opponent == caa[i]]))
      }
      
      # H2H records (Row Team Wins Over Column Team)
      head2head <- matrix(nrow = 10, ncol = 10)
      colnames(head2head) <- caa
      rownames(head2head) <- caa
      for(i in 1:10) {
        for(m in 1:10) {
          head2head[i,m] <- sum(games$simwins[games$team == caa[i] & games$opponent == caa[m]]) +
            sum(games$oppsimwins[games$team == caa[m] & games$opponent == caa[i]])
        }
      }
      
      # Get order of finish Pre-Tiebreak
      preBreak <- sort(as.vector(simresults[j,], mode = "numeric"), decreasing = T)
      
      for(z in 1:10) {
        prebreak.pos[j,z] <- c(1:length(caa))[preBreak == simresults[j, z]][1]
      }
      
      # Break any ties 
      for(i in 1:(length(caa) - 1)) {
        if(sum(prebreak.pos[j,] == i) > 1){
          # Get teams to between which to break tie
          teams <- caa[prebreak.pos[j,] == i]
          tie <- length(teams)
          teamIDs <- c(1:length(caa))[is.element(caa, teams)]
          
          # Tiebreak 1 (H2H)
          h2h <- rep(0, length(teams))
          for(m in 1:length(teams)) {
            h2h[m] <- sum(head2head[teams[m], teams[-m]])
          }
          if(sum(h2h == max(h2h)) == 1) {
            winner <- teams[grep(max(h2h), h2h)]
            winnerID <- teamIDs[grep(max(h2h), h2h)]
            # Winner wins tie-break
            simresults[j, winnerID] <- simresults[j, winnerID] + 0.1 * tie
            # Change current standing of losers
            change <- teams[teams != winner]
            prebreak.pos[j, change] <- i + 1
            next
          }
          else if(sum(h2h == max(h2h)) > 1 & sum(h2h == max(h2h)) < length(teams)){
            change <- setdiff(teams, teams[grep(max(h2h), h2h)])
            teams <- teams[grep(max(h2h), h2h)]
            prebreak.pos[j, change] <- i + 1
          }
          
          # Tiebreak 2 (Record vs. 1-8, descending order)
          for(z in 1:length(caa)) {
            if(z == i) {
              next
            }
            comp_teams <- caa[prebreak.pos[j,] == z]
            if(length(comp_teams) == 0) {
              next
            }
            comp_teamsIDs <- c(1:length(caa))[is.element(caa, comp_teams)]
            
            h2h <- rep(0, length(teams))
            for(m in 1:length(teams)) {
              h2h[m] <- sum(head2head[teams[m], comp_teams])
            }
            
            if(sum(h2h == max(h2h)) == 1) {
              winner <- teams[grep(max(h2h), h2h)]
              winnerID <- teamIDs[grep(max(h2h), h2h)]
              # Winner wins tie-break
              simresults[j, winnerID] <- simresults[j, winnerID] + 0.1 * tie
              # Change current standing of losers
              change <- teams[teams != winner]
              prebreak.pos[j, change] <- i + 1
              break
            }
            else if(sum(h2h == max(h2h)) > 1 & sum(h2h == max(h2h)) < length(teams)){
              change <- setdiff(teams, teams[grep(max(h2h), h2h)])
              teams <- teams[grep(max(h2h), h2h)]
              prebreak.pos[j, change] <- i + 1
            }
          }
          if(z < 10){
            next
          }
          
          # Tiebreak 3 (Analytics)
          tmp <- power_rankings[is.element(power_rankings$team, teams),]
          tmp <- tmp[order(tmp$team),]
          winner <- tmp$Team[grep(max(tmp$yusag_coeff), tmp$yusag_coeff)]
          winnerID <- c(1:10)[caa == winner]
          # Winner wins tie-break
          simresults[j, winnerID] <- simresults[j, winnerID] + 0.1 * tie
          # Change current standing of losers
          change <- teams[teams != winner]
          prebreak.pos[j, change] <- i + 1
        }
      }
      # Sim Ivy tournament
      palestra <- c(caa[as.vector(prebreak.pos[j,] == 1)], caa[as.vector(prebreak.pos[j,] == 2)],
                    caa[as.vector(prebreak.pos[j,] == 3)], caa[as.vector(prebreak.pos[j,] == 4)],
                    caa[as.vector(prebreak.pos[j,] == 5)], caa[as.vector(prebreak.pos[j,] == 6)],
                    caa[as.vector(prebreak.pos[j,] == 7)], caa[as.vector(prebreak.pos[j,] == 8)],
                    caa[as.vector(prebreak.pos[j,] == 9)], caa[as.vector(prebreak.pos[j,] == 10)])
      champ[j] <- palestra.sim(palestra)
    }
    
    
    playoffs <- data.frame(Team = caa,
                           seed1_prob = rep(NA, length(caa)),
                           seed2_prob = rep(NA, length(caa)),
                           seed3_prob = rep(NA, length(caa)),
                           seed4_prob = rep(NA, length(caa)),
                           seed5_prob = rep(NA, length(caa)),
                           seed6_prob = rep(NA, length(caa)),
                           auto_bid = rep(NA, length(caa)))
    
    
    for(i in 1:length(caa)) {
      playoffs$seed1_prob[i] <- round(sum(prebreak.pos[,i] == 1)/nsims * 100, 1)
      playoffs$seed2_prob[i] <- round(sum(prebreak.pos[,i] == 2)/nsims * 100, 1)
      playoffs$seed3_prob[i] <- round(sum(prebreak.pos[,i] == 3)/nsims * 100, 1)
      playoffs$seed4_prob[i] <- round(sum(prebreak.pos[,i] == 4)/nsims * 100, 1)
      playoffs$seed5_prob[i] <- round(sum(prebreak.pos[,i] == 5)/nsims * 100, 1)
      playoffs$seed6_prob[i] <- round(sum(prebreak.pos[,i] == 6)/nsims * 100, 1)
      playoffs$auto_bid[i] <- round(sum(champ == caa[i])/nsims * 100, 1)
    }
    
    if(q == 1) {
      ### store playoff odds
      simplayoffs <- data.frame(Team = caa,
                                seed1_prob1 = rep(NA, length(caa)),
                                seed1_prob2 = rep(NA, length(caa)),
                                seed2_prob1 = rep(NA, length(caa)),
                                seed2_prob2 = rep(NA, length(caa)),
                                seed3_prob1 = rep(NA, length(caa)),
                                seed3_prob2 = rep(NA, length(caa)),
                                seed4_prob1 = rep(NA, length(caa)),
                                seed4_prob2 = rep(NA, length(caa)),
                                auto_bid_1 = rep(NA, length(caa)),
                                auto_bid_2 = rep(NA, length(caa)))
      simplayoffs$auto_bid_1 <- playoffs$auto_bid
    }
    else{
      simplayoffs$seed1_prob2 <- playoffs$seed1_prob
      simplayoffs$auto_bid_2 <- playoffs$auto_bid
      swingfactor$psf[k/2] <- sum(abs(simplayoffs$seed1_prob2 - simplayoffs$seed1_prob1))
      swingfactor$auto_bid_sf[k/2] <- sum(abs(simplayoffs$auto_bid_2 - simplayoffs$auto_bid_1))
    }
    
  }
  
  psf_history <- read.csv("Predictions/psf_history.csv", as.is = T) %>%
    mutate(date = as.Date(date)) %>%
    filter(date < Sys.Date())
  psf_history <- rbind(psf_history, swingfactor) 
  psf_history <- filter(psf_history, date >= "2019-11-25")
  write.csv(psf_history, "Predictions/psf_history.csv", row.names = F) 
  write.csv(swingfactor, "Predictions/psf.csv", row.names = F)
  return(swingfactor)
}


fast.sim <- function(nsims) {
  games <- x[x$location == "H" & x$team_conf == "Ivy" & x$conf_game & x$reg_season, ]
  ivy <- unique(x$team[x$team_conf == "Ivy"])
  
  # Data Frame to Hold Team Wins by Sim
  simresults <- as.data.frame(matrix(nrow = nsims, ncol = length(ivy), byrow = T))
  names(simresults) <- ivy
  
  # Stores pre (and later post) tie-break position in standings
  prebreak.pos <- as.data.frame(matrix(nrow = nsims, ncol = length(ivy), byrow = T))
  names(prebreak.pos) <- ivy
  
  # Simulate All Games
  for (j in 1:nrow(simresults)){
    if(j %% 100 == 0) {
      cat("Sim:", j, "\n")
    }
    games$simwins <- NA
    games$oppsimwins <- NA
    rand <- runif(nrow(games))
    games$simwins[games$wins == 1] <- 1
    games$oppsimwins[games$wins == 1] <- 0
    games$simwins[games$wins == 0] <- 0
    games$oppsimwins[games$wins == 0] <- 1
    sims <- games$wins > 0 & games$wins < 1
    games$simwins[sims] <- ifelse(rand[sims] <= games$wins[sims], 1, 0)
    games$oppsimwins[sims] <- ifelse(rand[sims] > games$wins[sims], 1, 0)
    
    # get team win totals for current sim
    for(i in 1:8) {
      simresults[j, i] <- (sum(games$simwins[games$team == ivy[i]]) + 
                             sum(games$oppsimwins[games$opponent == ivy[i]]))
    }
  }
  return(simresults)
}

### Compute team's winning percentage
wp_compute <- function(team, team_iq) {
  games <- x[x$team == team & x$opponent != team_iq,]
  wwins <- sum(1.3 * games$wins[games$location == "A"],na.rm=TRUE) + 
    sum(0.7 * games$wins[games$location == "H"],na.rm=TRUE) +
    sum(games$wins[games$location == "N"],na.rm=TRUE)
  wloses <- sum(1.3 * (1 - games$wins[games$location == "H"]),na.rm=TRUE) + 
    sum(0.7 * (1 - games$wins[games$location == "A"]),na.rm=TRUE) +
    sum((1 - games$wins[games$location == "N"]),na.rm=TRUE)
  wp <- wwins/(wwins + wloses)
  return(wp)
}

### Compute team's opponent winning percentage
owp_compute <- function(team) {
  opponents <- x$opponent[x$team == team]
  owp <- sapply(opponents, wp_compute, team_iq = team)
  return(mean(owp))
}

### Compute team's opponent's opponent winning percentage
oowp_compute <- function(team) {
  opponents <- x$opponent[x$team == team]
  oowp <- sapply(opponents, owp_compute)
  return(mean(oowp))
}

### Compute RPI
rpi_calc <- function(team) {
  rpi <- 0.25 * wp_compute(team, team) + 0.5 * owp_compute(team) + 0.25 * oowp_compute(team)
  return(round(rpi, 4))
}

### Get and return team's RPI
rpi_compute <- function(new) {
  stats = as.data.frame(cbind(priors$team,rep(NA,length(length(priors$team)))))
  colnames(stats) = c("team","rpi")
  
  if(new) {
    stats$rpi <- sapply(priors$team, rpi_calc)
    stats <- stats[order(stats$rpi, decreasing = T),]
    stats$rpi_rank <- 1:length(priors$team)
    write.table(stats, "Bracketology/rpi.csv", row.names = F, col.names = T, sep = ",")
  }
  else{
    stats <- read.csv("Bracketology/rpi.csv", as.is = T)
  }
  return(stats)
}

record_eval <- function(team) {
  ### Get Team's games
  games <- x[x$team == team,]
  games <- inner_join(select(games, -opp_rank), 
                      select(power_rankings, team, rank), 
                      by = c("opponent" = "team")) %>%
    rename(team_rank = rank.x , opp_rank = rank.y)
  
  ### Classify wins into NCAA's 4 tiers
  tierAw <- 
    sum(games$wins[games$opp_rank <= 50 & games$location == "N"]) + 
    sum(games$wins[games$opp_rank <= 30 & games$location == "H"]) + 
    sum(games$wins[games$opp_rank <= 75 & games$location == "A"])
  tierBw <- 
    sum(games$wins[games$opp_rank >= 51 & games$opp_rank <= 100 & games$location == "N"]) + 
    sum(games$wins[games$opp_rank >= 31 & games$opp_rank <= 75 & games$location == "H"]) + 
    sum(games$wins[games$opp_rank >= 76 & games$opp_rank <= 135 & games$location == "A"]) 
  tierCw <- 
    sum(games$wins[games$opp_rank >= 101 & games$opp_rank <= 200 & games$location == "N"]) + 
    sum(games$wins[games$opp_rank >= 76 & games$opp_rank <= 160 & games$location == "H"]) + 
    sum(games$wins[games$opp_rank >= 136 & games$opp_rank <= 240 & games$location == "A"])
  tierDw <- 
    sum(games$wins[games$opp_rank >= 201 & games$opp_rank <= 353 & games$location == "N"]) + 
    sum(games$wins[games$opp_rank >= 161 & games$opp_rank <= 353 & games$location == "H"]) + 
    sum(games$wins[games$opp_rank >= 241 & games$opp_rank <= 353 & games$location == "A"])
  tierAl <- 
    sum(1 - games$wins[games$opp_rank <= 50 & games$location == "N"]) + 
    sum(1 - games$wins[games$opp_rank <= 30 & games$location == "H"]) + 
    sum(1 - games$wins[games$opp_rank <= 75 & games$location == "A"])
  tierBl <- 
    sum(1 - games$wins[games$opp_rank >= 51 & games$opp_rank <= 100 & games$location == "N"]) + 
    sum(1 - games$wins[games$opp_rank >= 31 & games$opp_rank <= 75 & games$location == "H"]) + 
    sum(1 - games$wins[games$opp_rank >= 76 & games$opp_rank <= 135 & games$location == "A"]) 
  tierCl <- 
    sum(1 - games$wins[games$opp_rank >= 101 & games$opp_rank <= 200 & games$location == "N"]) + 
    sum(1 - games$wins[games$opp_rank >= 76 & games$opp_rank <= 160 & games$location == "H"]) + 
    sum(1 - games$wins[games$opp_rank >= 136 & games$opp_rank <= 240 & games$location == "V"])
  tierDl <- 
    sum(1 - games$wins[games$opp_rank >= 201 & games$opp_rank <= 353 & games$location == "N"]) + 
    sum(1 - games$wins[games$opp_rank >= 161 & games$opp_rank <= 353 & games$location == "H"]) + 
    sum(1 - games$wins[games$opp_rank >= 241 & games$opp_rank <= 353 & games$location == "A"])
  
  ### Resume Bonus
  qual_bonus <- 16 * tierAw + 8 * tierBw + 2 * tierCw + tierDw - 
    4 * tierAl - 8 * tierBl - 16 * tierCl - 32 * tierDl - 
    25 * (tierAl >= 8) - 25 * (tierAl >= 10) - 50 * (tierAl >= 12) 
  
  ### Compute Strength of Record
  test <- power_rankings$team[1:25]
  sor <- rep(0, 25)
  for(j in 1:length(sor)) {
    data <- games
    data$team <- test[j]
    data$pred_score_diff <- round(predict(lm.hoops, newdata = data), 1)
    data$wins <- 
      round(predict.glm(glm.pointspread, newdata = data, type = "response"), 3)
    sor[j] <- sum(games$wins) - sum(data$wins)
  }
  
  ### Compute Wins Above Bubble (Ignore eligibity)
  autobids <- by_conf$team[by_conf$conference_rank == 1]
  atlarge <- power_rankings[!is.element(power_rankings$team, autobids),]
  
  bubble <- arrange(power_rankings, desc(yusag_coeff))$team[32:40]
  wab <- rep(0, length(bubble))
  for(j in 1:length(bubble)) {
    data <- games
    data$team <- bubble[j]
    data$pred_score_diff <- round(predict(lm.hoops, newdata = data), 1)
    data$wins <- 
      round(predict.glm(glm.pointspread, newdata = data, type = "response"), 3)
    wab[j] <- sum(games$wins) - sum(data$wins)
  }
  wins <- sum(games$wins) 
  losses = sum(1 - games$wins) 
  
  return(list("qual_bonus" = qual_bonus, "sor" = mean(sor), "wab" = mean(wab),
              "wins" = wins, "losses" = losses))
}


### Get and Return Team's resumes
get_resumes <- function(new){
  if(new){
    tmp <- data.frame("team" = priors$team,
                      "sor" = rep(0, length(priors$team)),
                      "qual_bonus" = rep(0, length(priors$team)),
                      "wab" = rep(0, length(priors$team)),
                      "wins" = rep(0, length(priors$team)),
                      "losses" = rep(0, length(priors$team)))
    for(i in 1:nrow(tmp)) {
      print(paste("Evaluating Team #: ", i, sep = ""))
      rec_eval <- record_eval(teams[i])
      tmp$sor[i] <- rec_eval$sor
      tmp$wab[i] <- rec_eval$wab
      tmp$qual_bonus[i] <- rec_eval$qual_bonus
      tmp$wins[i] <- rec_eval$wins
      tmp$losses[i] <- rec_eval$losses
    }
    write.csv(tmp, "3.0_Files/Bracketology/resumes.csv", row.names = F)
  }
  else{
    tmp <- read.csv("3.0_Files/Bracketology/resumes.csv", as.is = T)
  }
  return(tmp)
}

make_bracket <- function(tourney) {
  bracket <- data.frame("team" = teams,
                        "conf" = rep(NA, nrow(priors)),
                        "yusag_coeff" = rep(NA, nrow(priors)),
                        "rpi" = rep(NA, nrow(priors)),
                        "sor" = rep(NA, nrow(priors)),
                        "wab" = rep(NA, nrow(priors)),
                        "qual_bonus" = rep(NA, nrow(priors)),
                        "yusag_rank" = rep(NA, nrow(priors)),
                        "rpi_rank" = rep(NA, nrow(priors)),
                        "sor_rank" = rep(NA, nrow(priors)),
                        "resume_rank" = rep(NA, nrow(priors)),
                        "wab_rank" = rep(NA, nrow(priors)),
                        "mid_major" = rep(NA, nrow(priors)),
                        "loss_bonus" = rep(NA, nrow(priors)),
                        stringsAsFactors = F)
  
  ### Get Advanced Metric Ranks
  rpi <- arrange(rpi, desc(rpi)) %>% 
    mutate(rank = 1:nrow(priors))
  
  resumes <- 
    arrange(resumes, desc(sor)) %>%
    mutate(sor_rank = 1:nrow(priors)) %>%
    arrange(desc(wab)) %>%
    mutate(wab_rank = 1:nrow(priors)) %>%
    arrange(desc(qual_bonus)) %>%
    mutate(resume_rank = 1:nrow(priors))
  
  
  for(i in 1:length(teams)) {
    bracket$yusag_coeff[i] <- power_rankings$yusag_coeff[power_rankings$team == teams[i]]
    bracket$conf[i] <- get_conf(teams[i])
    bracket$rpi[i] <- rpi$rpi[rpi$team == teams[i]]
    bracket$sor[i] <- resumes$sor[resumes$team == teams[i]]
    bracket$wab[i] <- resumes$wab[resumes$team == teams[i]]
    bracket$qual_bonus[i] <- resumes$qual_bonus[resumes$team == teams[i]]
    bracket$yusag_rank[i] <- power_rankings$rank[power_rankings$team == teams[i]]
    bracket$rpi_rank[i] <- rpi$rank[rpi$team == teams[i]]
    bracket$sor_rank[i] <- resumes$sor_rank[resumes$team == teams[i]]
    bracket$wab_rank[i] <- resumes$wab_rank[resumes$team == teams[i]]  
    bracket$resume_rank[i] <- resumes$resume_rank[resumes$team == teams[i]]
    bracket$mid_major[i] <- confs$mid_major[confs$team == teams[i]]
    bracket$wins[i] <- resumes$wins[resumes$team == teams[i]]
    bracket$losses[i] <- resumes$losses[resumes$team == teams[i]]
    bracket$loss_bonus[i] <- resumes$losses[resumes$team == teams[i]] <= 4
    bracket$conf[i] %in% c("Big 10", "Big 12", "Big East", "ACC", "Pac 12", "Big 12")
  }
  
  bracket$blend <- 0.15 * bracket$rpi_rank + 0.125 * bracket$wab_rank + 
    0.1 * bracket$sor_rank + 0.2 * bracket$yusag_rank + 0.425 * bracket$resume_rank
  
  bracket$avg <- 0.2 * bracket$rpi_rank + 0.2 * bracket$wab_rank + 
    0.2 * bracket$sor_rank + 0.2 * bracket$yusag_rank + 0.2 * bracket$resume_rank 
  
  bracket <- arrange(bracket, desc(yusag_coeff))
  
  autobid_calc <- function(conf) {
    tmp <- bracket$team[bracket$conf == conf]
    for(i in 1:length(tmp)) {
      if(confs$eligible[confs$team == tmp[i]] & !confs$eliminated[confs$team == tmp[i]]) {
        return(tmp[i])
      }
    }
  }
  
  bracket_math <- 
    read.csv("https://raw.githubusercontent.com/lbenz730/NCAA_Hoops/master/3.0_Files/Bracketology/historical/bracket_math_2016.csv", as.is = T) %>%
    bind_rows(read.csv("https://raw.githubusercontent.com/lbenz730/NCAA_Hoops/master/3.0_Files/Bracketology/historical/bracket_math_2017.csv", as.is = T)) %>%
    bind_rows(read.csv("https://raw.githubusercontent.com/lbenz730/NCAA_Hoops/master/3.0_Files/Bracketology/historical/bracket_math_2018.csv", as.is = T)) 
  
  bracket_math$avg <- 0.2 * bracket_math$rpi_rank + 
    0.2 * bracket_math$wab_rank +  0.2 * bracket_math$sor_rank + 
    0.2 * bracket_math$yusag_rank + 0.2 * bracket_math$resume_rank 
  
  bracket_math$bid <- bracket_math$seed <= 10
  glm.madness <- suppressWarnings(glm(bid ~ blend +
                                        + (mid_major & yusag_rank > 50) 
                                      + (mid_major & yusag_rank > 25) 
                                      + (loss_bonus & yusag_rank <= 10), 
                                      data = bracket_math, family = "binomial"))
  lm.seed <- lm(seed ~ blend 
                + (mid_major & yusag_rank > 50) 
                + (mid_major & yusag_rank > 25) 
                + (loss_bonus & yusag_rank <= 10) 
                + (losses <= 3), 
                data = bracket_math)
  bracket$odds <- 
    round(predict(glm.madness, newdata = bracket, type = "response"), 4) * 100
  bracket$odds <- ifelse(bracket$odds > 99.9, 99.99, bracket$odds)
  bracket$odds[bracket$wins - bracket$losses <= 2] <- 
    bracket$odds[bracket$wins - bracket$losses <= 2]/4
  bracket$seed <- 
    predict(lm.seed, newdata = bracket, type = "response")
  
  correct <- c("Texas Tech", "Saint Mary's (CA)", "Duke")
  correct2 <- c("Purdue", "UCLA")
  correct3 <- c("Gonzaga")
  correct4 <- c("San Diego St.")
  bracket$odds[bracket$team %in% correct] <- bracket$odds[bracket$team %in% correct] + 0.5 * (100 - bracket$odds[bracket$team %in% correct])
  bracket$odds[bracket$team %in% correct2] <- bracket$odds[bracket$team %in% correct2] + 0.25 * (100 - bracket$odds[bracket$team %in% correct2])
  bracket$avg[bracket$team %in% correct3] <- 0.8 * bracket$avg[bracket$team %in% correct3]
  bracket$avg[bracket$team %in% correct4] <- 2 * bracket$avg[bracket$team %in% correct4]
  bracket <- arrange(bracket, desc(round(odds, 1)), avg)
  
  if(tourney == T) {
    ### Get Autobids
    autobids <- vector()
    for(j in 1:length(unique(confs$conference))){
      autobids[j] <- autobid_calc(unique(confs$conference)[j])
    }
    bracket$autobid <- is.element(bracket$team, autobids)
    tmp <- bracket[!bracket$autobid, ]
    j <- 1
    z <- 1
    
    ### Get At-Large bids
    atlarge <- vector()
    while(j <= 36) {
      for(k in z:length(teams)){
        z <- z + 1
        if(confs$eligible[confs$team == tmp$team[k]]) {
          atlarge[j] <- as.character(tmp$team)[k]
          j <- j + 1
          break
        }
      }
    }
    
    ### Write Bracket    
    bracket$atlarge <- is.element(bracket$team, atlarge)
    bracket <- rbind(bracket[bracket$autobid,], bracket[bracket$atlarge,])
    bracket <- arrange(bracket, desc(round(odds, 1)), avg)
    bracket <- select(bracket, -mid_major, -wins, -losses, -loss_bonus, -seed)
    bracket$seed_overall <- 1:68
    bracket$seed_line <- c(rep(1,4), rep(2,4), rep(3,4), rep(4,4), rep(5,4),
                           rep(6,4), rep(7,4), rep(8,4), rep(9,4), rep(10,4),
                           rep(11,6), rep(12,4), rep(13,4), rep(14,4), rep(15,4),
                           rep(16,6))
    f4 <- bracket$seed_overall[!bracket$autobid][33:36]
    bracket$first4 <- is.element(bracket$seed_overall, f4) | is.element(bracket$seed_overall, c(65:68))
    ### Not 3 1 seeds from 1 conference
    if(sum(bracket$conf[4] == bracket$conf[1:3]) == 2) {
      bracket[4:5,] <- rbind(bracket[5,], bracket[4,])
    }
    write.csv(bracket, "3.0_Files/Bracketology/bracket.csv", row.names = F)
    
    ### First teams out
    j <- 37
    z <- 37
    bubble <- vector()
    while(j <= 52) {
      for(k in z:length(teams)){
        z <- z + 1
        if(confs$eligible[confs$team == tmp$team[k]]) {
          bubble[j - 36] <- as.character(tmp$team)[k]
          j <- j + 1
          break
        }
      }
    }
    bubble <- tmp[is.element(tmp$team, bubble),]
    write.csv(bubble, "3.0_Files/Bracketology/bubble.csv", row.names = F)
    
    ### Bid Summary by Conference
    bids <- group_by(bracket, conf) %>%
      summarise("n_bid" = n()) %>%
      arrange(desc(n_bid))
    write.csv(bids, "3.0_Files/Bracketology/bids.csv", row.names = F)
    return(bracket)
  }
  else{
    bracket <- select(bracket, -mid_major, -wins, -losses, -loss_bonus, -seed)
    write.csv(bracket, "3.0_Files/Bracketology/bracket_math.csv", row.names = F)
    return(bracket)
  }
}

library(ggplot2)
library(ggridges)
library(viridis)
library(knitr)
library(kableExtra)
library(ncaahoopR)


### get team's conference
get_conf <- function(team) {
  return(conferences$conference[conferences$team == team])
}

### Visualizations
yusag_plot <- function(data){
  data %>% mutate(group = reorder(team_conf, yusag_coeff, median)) %>%
    ggplot(aes(x = yusag_coeff, y = group, fill = ..x..)) + 
    geom_density_ridges_gradient(scale = 1.5) + theme_ridges() +
    scale_y_discrete(expand = c(0.01, 0)) + 
    scale_x_continuous(expand = c(0.01, 0)) +
    scale_fill_viridis(name = "group", option = "C") +
    labs(y = "Conference", 
         x = "Points Above Average Team",
         title = "NCAA Men's Basketball Power Rankings") + 
    theme(plot.title = element_text(size = 16, hjust = 0.5),
          axis.title.y = element_text(size = 14, hjust = 0.5),
          axis.title.x = element_text(size = 14, hjust = 0.5),
          legend.position="none")
  
}

box_plot <- function(data) {
  data %>% mutate(group = reorder(team_conf, yusag_coeff, median)) %>%
    ggplot(aes(y = yusag_coeff, x = group)) + 
    geom_point(alpha = 0.2) + 
    geom_boxplot(fill  = "orange", alpha = 0.2) +
    geom_hline(yintercept = 0, lty = 2, size = 1.2, col = "orange") + 
    labs(x = "Conference", 
         y = "Points Above Average Team",
         title = "NCAA Men's Basketball Power Rankings") + 
    theme_bw() + 
    theme(plot.title = element_text(size = 16, hjust = 0.5),
          axis.title.y = element_text(size = 14, hjust = 0.5),
          axis.title.x = element_text(size = 14, hjust = 0.5),
          axis.text.x = element_text(angle = 90),
          legend.position="none")
}

#### Conference Sims
conf_sim <- function(conf, nsims) {
  conf_teams <- filter(conferences, conference == conf) %>% 
    pull(team) %>% 
    unique()
  results <- data.frame("team" = conf_teams,
                        "shared_title" = rep(0, length(conf_teams)),
                        "sole_title" = rep(0, length(conf_teams)),
                        "avg_wins" = rep(0, length(conf_teams)),
                        "avg_losses" = rep(0, length(conf_teams)),
                        stringsAsFactors = F)
  
  ### Sim Schedule
  schedule <- filter(x, conf_game, team_conf == conf, location != "V") %>%
    mutate(simwins = 0, opp_simwins = 0)
  schedule$tmp <- case_when(
    schedule$team < schedule$opponent ~ paste(schedule$team, schedule$opponent, schedule$date),
    T ~ paste(schedule$opponent, schedule$team, schedule$date)
  )
  schedule <- filter(schedule, !duplicated(tmp))
  
  sim_season <- rep(0, nrow(results))
  max_wins <- nrow(schedule) * 2 / nrow(results)
  
  for(i in 1:nsims) {
    if(i %% 100 == 0) {
      cat("Sim:", i, "\n")
    }
    rands <- runif(nrow(schedule))
    schedule$simwins <- ifelse(rands <= schedule$wins, 1, 0)
    schedule$opp_simwins <- abs(1 - schedule$simwins)
    for(j in 1:nrow(results)) {
      sim_season[j] <- sum(schedule$simwins[schedule$team == conf_teams[j]]) +
        sum(schedule$opp_simwins[schedule$opponent == conf_teams[j]])
    }
    
    results$avg_wins <- results$avg_wins + sim_season/nsims
    results$avg_losses <- results$avg_losses + (max_wins - sim_season)/nsims
    winners <- grep(max(sim_season), sim_season)
    results$shared_title[winners] <- results$shared_title[winners] + 1/nsims
    if(length(winners) == 1) {
      results$sole_title[winners] <- results$sole_title[winners] + 1/nsims
    }
  }
  return(results)
}

#### Conference Sims
conf_fast_sim <- function(conf, nsims) {
  conf_teams <- filter(confs, conference == conf) %>% 
    pull(team) %>% 
    unique()
  results <- data.frame("team" = rep(conf_teams, nsims),
                        "sim" = rep(1:nsims, each = length(conf_teams)),
                        "n_wins" = NA,
                        "place" = NA,
                        stringsAsFactors = F)
  
  
  ### Sim Schedule
  schedule <- filter(x, conf_game, team_conf == conf, location != "V") %>%
    mutate(simwins = 0, opp_simwins = 0)
  schedule$tmp <- case_when(
    schedule$team < schedule$opponent ~ paste(schedule$team, schedule$opponent, schedule$date),
    T ~ paste(schedule$opponent, schedule$team, schedule$date)
  )
  schedule <- filter(schedule, !duplicated(tmp))
  
  sim_season <- rep(0, length(conf_teams))
  
  for(i in 1:nsims) {
    if(i %% 100 == 0) {
      cat("Sim:", i, "\n")
    }
    rands <- runif(nrow(schedule))
    schedule$simwins <- ifelse(rands <= schedule$wins, 1, 0)
    schedule$opp_simwins <- abs(1 - schedule$simwins)
    for(j in 1:length(conf_teams)) {
      sim_season[j] <- sum(schedule$simwins[schedule$team == conf_teams[j]]) +
        sum(schedule$opp_simwins[schedule$opponent == conf_teams[j]])
    }
    
    results$n_wins[results$sim == i] <- sim_season
    results$place[results$sim == i] <- rank(-sim_season, ties = "min")
  }
  return(results)
}

### Jerome EP Calcultor
jerome <- function(t_sim) {
  v <- 3 * t_sim$finals + 2 * (1 + c(0, 0, rep(1, nrow(t_sim) - 2))) * t_sim$champ/t_sim$finals
  names(v) <- t_sim$team
  return(v)
}

### eliminate teams from AutoBid Contention
eliminate <- function(teams, conferences) {
  for(team in teams) {
    conferences$eliminated[conferences$team == team] <- T
  }
  write.csv(conferences, "conferences.csv", row.names = F)
  return(conferences)
}

### find regular season
reg_season <- function(date, conf) {
  
}

### Compute Weights for Pre-season prior
prior_weight <- function(school) {
  w <- 1.95 * max(c(0, 
                    ifelse(length(filter(x, team == school, !is.na(score_diff))[,9])>0,max(filter(x, team == school, !is.na(score_diff))[,9]),0)
  ), 
  na.rm = T)/(
    max(c(1, 
          max(filter(x, team == school)[,9])
    ), na.rm = T)
  )
  
  w <- min(c(w, 1))
  return(w)
}



rank_plot <- function(conf) {
  ggplot(subset(history,conference%in%conf), aes(x = as.Date(date), y = rank, group = team, col = team)) + 
    facet_wrap(~conference, ncol = 8) +
    geom_line() + 
    theme_bw() + 
    theme(plot.title = element_text(size = 16, hjust = 0.5),
          axis.title.y = element_text(size = 14, hjust = 0.5),
          axis.title.x = element_text(size = 14, hjust = 0.5),
          axis.text.x = element_text(angle = 90, size = 6),
          legend.position = "none") + 
    labs(x = "Date", 
         y = "D1 Rank",
         title = "Evolution of the NCAA Basketball Universe") + 
    scale_y_reverse()
}


evo_plot <- function() {
  ggplot(history, aes(x = as.Date(date), y = yusag_coeff)) + 
    facet_wrap(~conference, ncol = 8) +
    geom_line(aes(group = team, col = team)) + 
    theme_bw() + 
    theme(plot.title = element_text(size = 16, hjust = 0.5),
          axis.title.y = element_text(size = 14, hjust = 0.5),
          axis.title.x = element_text(size = 14, hjust = 0.5),
          axis.text.x = element_text(angle = 90, size = 6),
          legend.position = "none") + 
    labs(x = "Date", 
         y = "Points Above Average Team",
         title = "Evolution of the NCAA Basketball Universe")
}


max_date <- function(date, hist_dates) {
  return(max(hist_dates[hist_dates <= date]))
}


caa_preds <- function() {
  filter(x, conf_game, 
         team_conf == "CAA", 
         date > Sys.Date(), date <= Sys.Date() + 2, 
         location == "H") %>% 
    select(date, team, opponent, location, pred_team_score, 
           pred_opp_score, pred_score_diff, pred_total_score) %>%
    #rbind(read.csv("Predictions/caa_predictions.csv", as.is = T) %>%
    #        mutate("date" = as.Date(date)) %>%
    #        filter(date <= Sys.Date())) %>%
    filter(!duplicated(paste(date, team, opponent))) %>%
    write.csv("Predictions/caa_predictions.csv", row.names = F)
}

####### Ivy League Graphics
playoff_graphic <- function() {
  background_colors <- arrange(playoffs, desc(auto_bid), desc(seed1_prob)) %>%
    pull(team) %>%
    sapply(., function(x) { ncaa_colors$primary_color[ncaa_colors$ncaa_name == x] })
  text_colors <- arrange(playoffs, desc(auto_bid), desc(seed1_prob)) %>%
    pull(team) %>%
    sapply(., function(x) { ncaa_colors$secondary_color[ncaa_colors$ncaa_name == x] })
  
  mutate(playoffs, auto_bid = case_when(
    auto_bid > 0.05 ~ round(auto_bid, 1),
    T ~ 0),
    seed1_prob = 
      case_when(
        seed1_prob > 0.05 ~ round(seed1_prob, 1),
        seed1_prob > 0 ~ 0.1,
        T ~ 0),
    seed2_prob = 
      case_when(
        seed2_prob > 0.05 ~ round(seed2_prob, 1),
        seed2_prob > 0 ~ 0.1,
        T ~ 0),
    seed3_prob = 
      case_when(
        seed3_prob > 0.05 ~ round(seed3_prob, 1),
        seed3_prob > 0 ~ 0.1,
        T ~ 0),
    seed4_prob = 
      case_when(
        seed4_prob > 0.05 ~ round(seed4_prob, 1),
        seed4_prob > 0 ~ 0.1,
        T ~ 0)
  ) %>%
    arrange(desc(auto_bid), desc(seed1_prob)) %>%
    rename("Team" = team,
           "Auto Bid" = auto_bid,
           "1st Seed" = seed1_prob,
           "2nd Seed" = seed2_prob,
           "3rd Seed" = seed3_prob,
           "4th Seed" = seed4_prob,
           "5th Seed" = seed5_prob,
           "6th Seed" = seed6_prob) %>%
    kable(., align = "cccccccc") %>%
    kable_styling("striped", full_width = F, position = "center") %>%
    row_spec(1, color = "white", background = background_colors[1], bold = T) %>%
    row_spec(2, color = "white", background = background_colors[2], bold = T) %>%
    row_spec(3, color = "white", background = background_colors[3], bold = T) %>%
    row_spec(4, color = "white", background = background_colors[4], bold = T) %>%
    row_spec(5, color = "white", background = background_colors[5], bold = T) %>%
    row_spec(6, color = "white", background = background_colors[6], bold = T) %>%
    row_spec(7, color = "white", background = background_colors[7], bold = T) %>%
    row_spec(8, color = "white", background = background_colors[8], bold = T) %>%
    row_spec(9, color = "white", background = background_colors[9], bold = T) %>%
    row_spec(10, color = "white", background = background_colors[10], bold = T) %>%
    row_spec(0, bold = T, font_size = 16)
}

psf_graphic <- function() {
  background_colors <- arrange(playoffs, desc(auto_bid), desc(seed1_prob)) %>%
    pull(team) %>%
    sapply(., function(x) { ncaa_colors$primary_color[ncaa_colors$ncaa_name == x] })
  text_colors <- arrange(playoffs, desc(auto_bid), desc(seed1_prob)) %>%
    pull(team) %>%
    sapply(., function(x) { ncaa_colors$secondary_color[ncaa_colors$ncaa_name == x] })
  text_colors[c()] <- "#FFFFFF"

  psf_results <- arrange(psf_results, desc(psf))
  
  inner_join(psf_results, select(x, team, opponent, pred_team_score, date,
                                 pred_opp_score, wins), by = c("home" = "team",
                                                               "away" = "opponent",
                                                               "date" = "date")) %>%
    mutate("winner" = ifelse(pred_team_score > pred_opp_score, home, away),
           "result" = case_when(
             pred_team_score > pred_opp_score ~ paste0(home, ": ", sprintf("%.1f", pred_team_score), " - ",
                                                       sprintf("%.1f", pred_opp_score), " (", sprintf("%.1f", 100 * wins), "%)"),
             pred_opp_score > pred_team_score ~ paste0(away, ": ", sprintf("%.1f", pred_opp_score), " - ",
                                                       sprintf("%.1f", pred_team_score), " (", sprintf("%.1f", 100 * (1 - wins)), "%)")
           )
    ) %>%
    select(home, away, result, psf, auto_bid_sf, winner) %>%
    mutate(home = cell_spec(home, 
                            color = text_colors[home], 
                            background = background_colors[home],
                            bold = T),
           away = cell_spec(away, 
                            color = text_colors[away], 
                            background = background_colors[away],
                            bold = T),
           result = 
             cell_spec(result, 
                       color = text_colors[winner], 
                       background = background_colors[winner],
                       bold = T)
    ) %>%
    mutate(psf = cell_spec(
      psf, color = "white", bold = T,
      background = spec_color(as.numeric(psf), end = 0.9, option = "C", direction = 1)
    )) %>%
    mutate(auto_bid_sf = cell_spec(
      auto_bid_sf, color = "white", bold = T,
      background = spec_color(as.numeric(auto_bid_sf), end = 0.9, option = "C", direction = 1)
    )) %>%
    select(-winner) %>%
    rename("Home" = home,
           "Away" = away,
           "Predicted Result" = result,
           "Playoff Swing Factor" = psf,
           "Auto Bid Swing Factor" = auto_bid_sf) %>%
    kable(., escape = F, align = "cccccc") %>%
    kable_styling("striped", full_width = F, position = "center") %>%
    row_spec(0, bold = T, font_size = 16)# %>%
    #add_header_above(c("Ivy League Men's Basketball Predictions" = 10), bold = T)
}

tourney_sim  <- function(teams, seeds, byes, double_byes, hca, nsims) {
  ### Champ Storage
  n <- length(teams)
  simresults <- data.frame("team" = teams,
                           "seed" = seeds,
                           "champ" = rep(0, n),
                           "finals" = rep(0, n))
  for(j in 1:nsims) {
    if(j %% 100 == 0) {
      cat("Sim #:", j, "of", nsims, "\n")
    }
    ### Fill in First Round
    games <- data.frame("team" = rep(NA, n-1),
                        "opponent" = rep(NA, n-1),
                        "location" = rep(NA, n-1),
                        "team_seed" = rep(NA, n-1),
                        "opp_seed" = rep(NA, n-1),
                        "pred_score_diff" = rep(NA, n-1),
                        "win_prob" = rep(NA, n-1),
                        "winner" = rep(NA, n-1))
    game_count <- 0
    non_bye <- n - byes - double_byes
    
    for(i in 1:((non_bye)/2)) {
      games$team[i] <- teams[double_byes + byes + i]
      games$team_seed[i] <- seeds[double_byes + byes + i]
      games$opponent[i] <- teams[n + 1 - i]
      games$opp_seed[i] <- seeds[n + 1 - i]
      
      if(is.na(hca)) {
        games$location[i] <- "N" 
      }
      else if(hca == "seed") {
        games$location[i] <- "H"
      }
      else if(games$team[i] == hca) {
        games$location[i] <- "H"
      }
      else if(games$opponent[i]  == hca) {
        games$location[i] <- "V"
      }
      else{
        games$location[i] <- "N" 
      }
      game_count <- game_count + 1
    }
    cur_round <- 1:(non_bye/2)
    next_round <- ((non_bye)/2 + 1):((non_bye)/2 + (byes + (non_bye/2))/2)
    
    ### Fill in teams w/ first round byes
    if(byes > 0) {
      one_bye <- (double_byes + 1):(double_byes + byes)
      games$team[next_round[1:length(cur_round)]] <- teams[one_bye[1:length(cur_round)]]
      games$team_seed[next_round[1:length(cur_round)]] <- seeds[one_bye[1:length(cur_round)]]
      left <- one_bye[-c(1:length(cur_round))]
      games$team[next_round[-(1:length(cur_round))]] <- teams[left[1:(length(left)/2)]]
      games$team_seed[next_round[-(1:length(cur_round))]] <- seeds[left[1:(length(left)/2)]]
      games$opponent[next_round[-(1:length(cur_round))]] <- teams[sort(left[((length(left)/2) + 1):length(left)], decreasing = T)]
      games$opp_seed[next_round[-(1:length(cur_round))]] <- seeds[sort(left[((length(left)/2) + 1):length(left)], decreasing = T)]
      
      ### Sim Any Non-Bye Games
      games$pred_score_diff[cur_round] <- predict(lm.hoops, newdata = games[cur_round,])
      games$win_prob[cur_round] <- predict(glm.pointspread, newdata = games[cur_round,] , type = "response")
      games$winner[cur_round] <- ifelse(games$win_prob[cur_round] >= runif(length(cur_round)), games$team[cur_round],
                                        games$opponent[cur_round])
      games$opponent[next_round[length(cur_round):1]] <- games$winner[cur_round] 
      games$opp_seed[next_round[length(cur_round):1]] <- 
        seeds[unlist(apply(as.data.frame(gsub("[()]", "", paste("^", games$winner[cur_round], "$", sep = ""))), 
                           1, grep, gsub("[()]", "", teams)))]
      
      cur_round <- next_round
      next_round <- (max(cur_round) + 1):(max(cur_round) + (double_byes + length(cur_round))/2)
    }
    
    if(double_byes > 0) {
      ### Set Double Bye Teams
      these <- (max(cur_round) + 1):(max(cur_round) + double_byes)
      games$team[these] <- teams[1:double_byes]
      games$team_seed[these] <- 1:double_byes
      
      ### Sim Games with Single Bye Teams
      for(i in cur_round) {
        if(is.na(hca)) {
          games$location[i] <- "N" 
        }
        else if(hca == "seed") {
          games$location[i] <- "H"
        }
        else if(games$team[i] == hca) {
          games$location[i] <- "H"
        }
        else if(games$opponent[i]  == hca) {
          games$location[i] <- "V"
        }
        else{
          games$location[i] <- "N" 
        }
      }
      
      games$pred_score_diff[cur_round] <- predict(lm.hoops, newdata = games[cur_round,])
      games$win_prob[cur_round] <- predict(glm.pointspread, newdata = games[cur_round,] , type = "response")
      games$winner[cur_round] <- ifelse(games$win_prob[cur_round] >= runif(length(cur_round)), games$team[cur_round],
                                        games$opponent[cur_round])
      games$opponent[next_round[length(cur_round):1]] <- games$winner[cur_round] 
      games$opp_seed[next_round] <- 
        seeds[apply(as.data.frame(gsub("[()]", "", paste("^", games$opponent[next_round], "$", sep = ""))), 
                    1, grep, gsub("[()]", "", teams))]
      
      cur_round <- next_round
      next_round <- (max(cur_round) + 1):(max(cur_round) + length(cur_round)/2)
      
    }
    
    
    ### Sim Remainder of Tournament
    while(is.na(games$winner[n-1])) {
      for(i in cur_round) {
        if(is.na(hca)) {
          games$location[i] <- "N" 
        }
        else if(hca == "seed") {
          games$location[i] <- "H"
        }
        else if(games$team[i] == hca) {
          games$location[i] <- "H"
        }
        else if(games$opponent[i]  == hca) {
          games$location[i] <- "V"
        }
        else{
          games$location[i] <- "N" 
        }
      }
      
      
      games$pred_score_diff[cur_round] <- predict(lm.hoops, newdata = games[cur_round,])
      games$win_prob[cur_round] <- predict(glm.pointspread, newdata = games[cur_round,] , type = "response")
      games$winner[cur_round] <- ifelse(games$win_prob[cur_round] >= runif(length(cur_round)), games$team[cur_round],
                                        games$opponent[cur_round])
      
      if(next_round[1] < n) {
        for(i in 1:(length(cur_round)/2)) {
          team1 <- games$winner[cur_round[i]]
          team2 <- games$winner[cur_round[length(cur_round) + 1 - i]]
          team_seed1 <- seeds[grep(gsub("[()]", "", paste("^", team1, "$", sep = "")), 
                                   gsub("[()]", "", teams))]
          team_seed2 <- seeds[grep(gsub("[()]", "", paste("^", team2, "$", sep = "")), 
                                   gsub("[()]", "", teams))]
          if(team_seed1 < team_seed2) {
            games$team[next_round[i]] <- team1
            games$opponent[next_round[i]] <- team2
            games$team_seed[next_round[i]] <- team_seed1
            games$opp_seed[next_round[i]] <- team_seed2
          }
          else{
            games$team[next_round[i]] <- team2
            games$opponent[next_round[i]] <- team1
            games$team_seed[next_round[i]] <- team_seed2
            games$opp_seed[next_round[i]] <- team_seed1
          }
        }
        
        ### Update Indicies of Rounds
        cur_round <- next_round
        next_round <- (next_round[length(next_round)] + 1):(next_round[length(next_round)] + length(next_round)/2)
      }
    }
    simresults$champ[simresults$team == games$winner[n-1]] <- 
      simresults$champ[simresults$team == games$winner[n-1]] + 1/nsims
    simresults$finals[simresults$team == games$winner[n-1]] <- 
      simresults$finals[simresults$team == games$winner[n-1]] + 1/nsims
    loser <- setdiff(c(games$team[n-1], games$opponent[n-1]), games$winner[n-1])
    simresults$finals[simresults$team == loser] <- 
      simresults$finals[simresults$team == loser] + 1/nsims
  }
  return(simresults)
}

ncaa_sim <- function(nsims) {
  
  ### Hard Code Teams, Regions, and Seeds
  # change when the tournament is announced
  east <- c("Duke", "Michigan St.", "LSU", "Virginia Tech", 
            "Mississippi St.", "Maryland", "Louisville",
            "VCU", "UCF", "Minnesota", "Belmont", "Temple",
            "Liberty", "Saint Louis", "Yale", "Bradley", 
            "North Dakota St.", "N.C. Central")
  south <- c("Virginia", "Tennessee", "Purdue", "Kansas St.",
             "Wisconsin", "Villanova", "Cincinnati", "Ole Miss",
             "Oklahoma", "Iowa", "Saint Mary's (CA)", "Oregon",
             "UC Irvine", "Old Dominion", "Colgate", "Gardner-Webb")
  west <- c("Gonzaga", "Michigan", "Texas Tech", "Florida St.", 
            "Marquette", "Buffalo", "Nevada", "Syracuse", 
            "Baylor", "Florida", "Arizona St.", "St. John's (NY)",
            "Murray St.", "Vermont", "Northern Ky.", "Montana",
            "Fairleigh Dickinson", "Prairie View")
  midwest <- c("North Carolina", "Kentucky", "Houston", "Kansas",
               "Auburn", "Iowa St.", "Wofford", "Utah St.", 
               "Washington", "Seton Hall", "Ohio St.", "New Mexico St.", 
               "Northeastern", "Georgia St.", "Abilene Christian", "Iona")
  east_seeds <- c(1:10, 11, 11, 12:15, 16, 16)
  west_seeds <- c(1:10, 11, 11, 12:15, 16, 16)
  south_seeds <- 1:16
  mw_seeds <- 1:16
  
  ### Keep Track of teams that have won
  winners <- list()
  winners[[1]]<- c("Belmont", "Arizona St.", "Fairleigh Dickinson", "North Dakota St.")
  winners[[2]] <- c("Minnesota", "LSU", "Auburn", "Florida St.", "Maryland",
                    "Murray St.", "Kansas", "Michigan St.", "Gonzaga", 
                    "Kentucky", "Baylor", "Wofford", "Purdue", "Michigan",
                    "Florida", "Villanova", "Tennessee", "Iowa", "Washington",
                    "North Carolina", "UCF", "Duke", "Buffalo", "Texas Tech",
                    "Liberty", "Virginia Tech", "Oklahoma", "Virginia", 
                    "Ohio St.", "Houston", "UC Irvine",
                    "Oregon")
  winners[[3]] <- c("Kentucky", "LSU", "Gonzaga", "Florida St.", "Michigan",
                    "Purdue", "Michigan St.", "Auburn", "Duke", "North Carolina",
                    "Virginia", "Texas Tech", "Tennessee", "Oregon", "Houston",
                    "Virginia Tech")
  winners[[4]] <- c("Gonzaga", "Texas Tech", "Virginia", "Purdue",
                    "Duke", "Kentucky", "Auburn", "Michigan St.")
  winners[[5]] <- vector()
  winners[[6]] <- vector()
  
  first_four <- data.frame("team" = c(east[c(11, 17)], west[c(11, 17)]),
                           "opponent" = c(east[c(12, 18)], west[c(12, 18)]),
                           "location" = "N",
                           stringsAsFactors = F)
  first_four$pred_score_diff <- predict(lm.hoops, newdata = first_four)
  first_four$win_prob <- predict(glm.pointspread, newdata = first_four, type = "response")
  first_four <- mutate(first_four, win_prob = 
                         case_when(team %in% winners[[1]] ~ 1,
                                   opponent %in% winners[[1]] ~ 0,
                                   T ~ win_prob))
  
  ### Storage of Sim Results
  ncaa_sims <- data.frame("team" = c(east, west, south, midwest),
                          "seed" = c(east_seeds, west_seeds, south_seeds, mw_seeds),
                          "region" = c(rep("East", length(east)), 
                                       rep("West", length(west)),
                                       rep("South", length(south)), 
                                       rep("Midwest", length(midwest))),
                          "r64" = 0,
                          "r32" = 0,
                          "s16" = 0,
                          "e8" = 0,
                          "f4" = 0,
                          "ncg" = 0,
                          "champ" = 0,
                          stringsAsFactors = F)
  for(j in 1:nsims) {
    if(j %% 100 == 0) {
      cat("Sim:", j, "\n")
    }
    
    ### Sim First 4
    first_four_losers <- ifelse(runif(4) <= first_four$win_prob, 
                                first_four$opponent, 
                                first_four$team)
    
    ### Sim Up to Final 4
    final4 <- rep(NA, 4)
    for(k in 1:4) {
      if(k == 1) {
        vec <- setdiff(east, first_four_losers)
      }
      else if(k == 2) {
        vec <- setdiff(west, first_four_losers)
      }
      else if(k == 3) {
        vec <- south
      }
      else{
        vec <- midwest
      }
      ncaa_sims$r64[ncaa_sims$team %in% vec] <- 
        ncaa_sims$r64[ncaa_sims$team %in% vec] + 100/nsims
      round <- 2
      ### Sim Remainder of Tournament
      while(round < 6) {
        ngame <- 16/(2^(round-1))
        games <- data.frame("team" = rep(NA, ngame),
                            "opponent" = rep(NA, ngame),
                            "location" = rep("N", ngame),
                            "pred_score_diff" = rep(NA, ngame),
                            "win_prob" = rep(NA, ngame),
                            stringsAsFactors = F)
        games$team <- vec[1:ngame]
        games$opponent <- vec[(2*ngame):(ngame + 1)]
        games$pred_score_diff <- predict(lm.hoops, newdata = games)
        games$win_prob <- predict(glm.pointspread, newdata = games, type = "response")
        games <- mutate(games, win_prob = 
                          case_when(team %in% winners[[round]] ~ 1,
                                    opponent %in% winners[[round]] ~ 0,
                                    T ~ win_prob))
        vec <- ifelse(runif(ngame) <= games$win_prob, games$team, games$opponent)
        ncaa_sims[ncaa_sims$team %in% vec, round+3] <- 
          ncaa_sims[ncaa_sims$team %in% vec, round+3] + 100/nsims
        round <- round + 1
      }
      final4[k] <- vec
    }
    final4 <- data.frame("team" = final4[1:2],
                         "opponent" = final4[4:3],
                         "location" = rep("N", 2),
                         stringsAsFactors = F)
    final4$pred_score_diff <- predict(lm.hoops, newdata = final4)
    final4$win_prob <- predict(glm.pointspread, newdata = final4, type = "response")
    final4 <- mutate(final4, win_prob = 
                       case_when(team %in% winners[[6]] ~ 1,
                                 opponent %in% winners[[6]] ~ 0,
                                 T ~ win_prob))
    ncg <- ifelse(runif(2) <= final4$win_prob, final4$team, final4$opponent)
    ncaa_sims$ncg[ncaa_sims$team %in% ncg] <- 
      ncaa_sims$ncg[ncaa_sims$team %in% ncg] + 100/nsims
    ncg <- data.frame("team" = ncg[1], 
                      "opponent" = ncg[2],
                      "location" = "N",
                      stringsAsFactors = F)
    ncg$pred_score_diff <- predict(lm.hoops, newdata = ncg)
    ncg$win_prob <- predict(glm.pointspread, newdata = ncg, type = "response")
    champ <- ifelse(runif(1) <= ncg$win_prob, ncg$team, ncg$opponent)
    ncaa_sims$champ[ncaa_sims$team == champ] <- 
      ncaa_sims$champ[ncaa_sims$team == champ] + 100/nsims
  }
  return(ncaa_sims)
}


######## NCAA_Hoops_Scraper.R
# Get date materials
today <- unclass(as.POSIXlt(Sys.time()))
year <- 1900 + today$year
month <- 1 + today$mon
day <- today$mday

# Stripwhite function 
stripwhite <- function(x) gsub("\\s*$", "", gsub("^\\s*", "", x))

### Fix 
fix_team <- function(x) {
  possible_fix <- teamid$team[sapply(teamid$team, grepl, x)]
  return(possible_fix[nchar(possible_fix) == max(nchar(possible_fix))])
}

### Read in ID's table
teamid <- 
  read.csv("3.0_Files/Info/conferences.csv", as.is = T) %>%
  select(team, ncaa_id, conference) %>%
  arrange(team)

################################################################################
baseurl <- 'http://stats.ncaa.org/teams/'
tournaments <- c('Empire Classic (Riverside)', 'Myrtle Beach Invitational', 
                 'Maui Invitational (Mainland)', 'Cayman Islands (Mainland)',
                 'Hawaiian Airlines Diamond Head Classic') 
conf_tournaments <- paste0(c("Big South", "Patriot League", "Northeast Conference",
                             "Horizon League", "Mountain West", "ASUN", "MAC", "WCC", 
                             "America East", "Sun Belt", "SWAC", "MEAC", "Big East", 
                             "Big 10", "Big 12", "ACC", "SEC", "Ivy League"), ".*MBB.*")


z <- NULL
bad <- NULL
for (i in 1:nrow(teamid)) {
  cat("Getting", i, teamid$team[i], "\n")
  
  # Elegantly scan and handle hiccups:
  ct <- 0
  while (ct >= 0 && ct <= 5) {
    x <- try(scan(paste(baseurl, teamid$ncaa_id[i], sep=""),
                  what="", sep="\n"))
    if (class(x) != "try-error") {
      ct <- -1
    } else {
      warning(paste(ct, "try(scan) failed, retrying team ID",
                    teamid$id[i], "\n"))
      Sys.sleep(0.5)
      ct <- ct + 1
      if (ct > 5) {
        warning("Big internet problem")
        bad <- c(bad, teamid$ncaa_id[i])
      }
    }
  }
  if (ct <= 5) {
    x <- x[-grep("^\\s*$", x)]  # Drop lines with only whitespace
    
    # Which lines contain dates surrounded immediately by > ... < ?
    datelines <- grep("\\d\\d/\\d\\d/\\d\\d\\d\\d", x)
    
    dates <- stripwhite(gsub("<[^<>]*>", "", x[datelines]))
    dates <- gsub(" .*", "", dates)
    ix <- !dates %in% c("Start", "Team", "End")
    datelines <- datelines[ix]
    dates <- dates[ix]
    dates <- matrix(as.numeric(unlist(strsplit(dates, "/"))),
                    ncol=3, byrow=TRUE)
    
    opploc <- stripwhite(gsub("<[^<>]*>", "", x[datelines+2]))
    loc <- rep("H", length(opploc))
    loc[grep("@", opploc, fixed=TRUE)] <- "N"
    loc[grep("^@", opploc)] <- "V"
    opp <- opploc
    opp[loc == "V"] <- gsub("@", "", gsub("@ ", "", opp[loc == "V"]))
    opp[loc == "N"] <- substring(opp, 1, regexpr("@", opp)-2)[loc == "N"]
    
    
    
    result <- stripwhite(gsub("<[^<>]*>", "", x[datelines+5]))
    OT <- suppressWarnings(as.numeric(gsub("^.*\\((\\d)OT\\)",
                                           "\\1", result))) # warnings okay
    result <- gsub(" \\(.*\\)", "", result)
    result <- strsplit(substring(result, 3, 20), "-")
    if (any(sapply(result, length) == 0)) {
      result[sapply(result, length) == 0] <- list(c(NA, NA))
    }
    result <- matrix(as.numeric(unlist(result)), ncol=2, byrow=TRUE)
    
    res <- data.frame(year=dates[,3],
                      month=dates[,1],
                      day=dates[,2],
                      team=teamid$team[i],
                      opponent=opp,
                      location=loc,
                      teamscore=result[,1],
                      oppscore=result[,2],
                      OT=OT, stringsAsFactors=FALSE)
    res$date <- paste(res$month, res$day, sep = "_") 
    res$opponent <- stripwhite(gsub("&amp;", "&", gsub("&x;", "'", gsub("[0-9]", "", gsub("#", "", res$opponent)))))
    #fix <- sapply(res$opponent, function(x) { any(sapply(teamid$team, grepl, x)) }) &
    #  !res$opponent %in% teamid$team
    #res$opponent[fix] <- sapply(res$opponent[fix], fix_team)
    
    for(tourney in tournaments) {
      res$opponent <- stripwhite(gsub(tourney, "", res$opponent))
    }
    
    for(tourney in conf_tournaments) {
      res$opponent <- stripwhite(gsub(tourney, "", res$opponent))
    }
    
    # Fix non-unique dates problem
    uni_dates <- unique(res$date)
    z <- rbind(z, res[uni_dates %in% res$date, -ncol(res)])
    
  }
}

# Extract D1 Games
# rows <- strsplit(z$opponent[grep("@", z$opponent)], "@")
# if(length(rows) > 1) {
#   for (i in 1:length(z$opponent[grep("@", z$opponent)])) {
#     z$opponent[grep("@", z$opponent)][1] <- rows[[i]][1]
#   }
# }
z$opponent <- stripwhite(z$opponent)

z$D1 <- z$team %in% teamid$team + z$opponent %in% teamid$team

### Save Results
write.csv(z, paste("3.0_Files/Results/2019-20/NCAA_Hoops_Results_", month, "_", 
                   day, "_", year, ".csv", sep=""), row.names = F)


# make pretty table for conference rankings
powerrank_table = function(conf){
  if(conf %!in% c("Top 25","leaders")){
  df = pr_compute(TRUE) %>% 
    filter(conference==conf) %>% 
    select(team,yusag_coeff,conference_rank,rank,record,conf_record) %>% 
    arrange(-yusag_coeff)
  df = df[!duplicated(df$team),]
  df$actrec = NA
  df$actconfrec = NA
  for(tm in df$team){
    games = x %>% filter(tm==team)
    df$actrec[df$team==tm] = paste0(sum(games$score_diff>0,na.rm=TRUE),"-",sum(games$score_diff<0,na.rm=TRUE))
    df$actconfrec[df$team==tm] = paste0(sum(games$score_diff[games$conf_game==TRUE]>0,na.rm=TRUE),"-",sum(games$score_diff[games$conf_game==TRUE]<0,na.rm=TRUE))
  }
  background_colors = c()
  text_colors = c()
  for(i in 1:length(df$team)){
  background_colors = c(background_colors,ncaa_colors$primary_color[ncaa_colors$ncaa_name==df$team[i]])
  text_colors = c(text_colors,ncaa_colors$secondary_color[ncaa_colors$ncaa_name==df$team[i]])
  }
  df = df %>% 
    relocate(c(actrec,actconfrec),.after=rank)

  table = kable(df, align = "cccccccc",col.names=c("Team","YUSAG Coefficient","Conference Rank","NCAA Rank","Overall Record","Conference Record","Predicted Overall Record","Predicted Conference Record"),caption=paste0(conf," Rankings"),format="html") %>%
    kable_styling("striped", full_width = F, position = "center") %>%
    column_spec(1,color="white",background=background_colors)
  
  return(table)
  }
  if(conf=="Top 25"){
    df = pr_compute(TRUE) %>% 
      filter(rank<=25) %>% 
      select(rank,team,conference,yusag_coeff,record,conf_record) %>% 
      arrange(-yusag_coeff)
    df$actrec = NA
    df$actconfrec = NA
    for(tm in df$team){
      games = x %>% filter(tm==team)
      df$actrec[df$team==tm] = paste0(sum(games$score_diff>0,na.rm=TRUE),"-",sum(games$score_diff<0,na.rm=TRUE))
      df$actconfrec[df$team==tm] = paste0(sum(games$score_diff[games$conf_game==TRUE]>0,na.rm=TRUE),"-",sum(games$score_diff[games$conf_game==TRUE]<0,na.rm=TRUE))
    }
    background_colors = c()
    text_colors = c()
    for(i in 1:length(df$team)){
      background_colors = c(background_colors,ncaa_colors$primary_color[ncaa_colors$ncaa_name==df$team[i]])
      text_colors = c(text_colors,ncaa_colors$secondary_color[ncaa_colors$ncaa_name==df$team[i]])
    }
    df = df %>% 
      relocate(c(actrec,actconfrec),.after=yusag_coeff)

    table = kable(df, align = "cccccccc",col.names=c("NCAA Rank","Team","Conference","YUSAG Coefficient","Overall Record","Conference Record","Predicted Overall Record","Predicted Conference Record"),caption=paste0(conf," Rankings"),format="html") %>%
      kable_styling(c("striped","hover"), full_width = F, position = "center") %>%
      column_spec(2,color="white",background=background_colors)
    
    return(table)
  }
    if(conf=="leaders"){
      df = pr_compute(TRUE) %>% 
        filter(conference_rank==1) %>% 
        select(team,conference,rank,yusag_coeff,record,conf_record) %>% 
        arrange(-yusag_coeff)
      background_colors = c()
      text_colors = c()
      for(i in 1:length(df$team)){
        background_colors = c(background_colors,ncaa_colors$primary_color[ncaa_colors$ncaa_name==df$team[i]])
        text_colors = c(text_colors,ncaa_colors$secondary_color[ncaa_colors$ncaa_name==df$team[i]])
      }
      df$actrec = NA
      df$actconfrec = NA
      for(tm in df$team){
        games = x %>% filter(tm==team)
        df$actrec[df$team==tm] = paste0(sum(games$score_diff>0,na.rm=TRUE),"-",sum(games$score_diff<0,na.rm=TRUE))
        df$actconfrec[df$team==tm] = paste0(sum(games$score_diff[games$conf_game==TRUE]>0,na.rm=TRUE),"-",sum(games$score_diff[games$conf_game==TRUE]<0,na.rm=TRUE))
      }
      df = df %>% 
        relocate(c(actrec,actconfrec),.after=yusag_coeff)
      
      
      table = kable(df, align = "cccccc",col.names=c("Team","Conference",'NCAA Rank',"YUSAG Coefficient","Overall Record","Conference Record","Predicted Overall Record","Predicted Conference Record"),caption=paste0(conf," Rankings"),format="html") %>%
        kable_styling(c("striped","hover"), full_width = F, position = "center") %>%
        column_spec(1,color="white",background=background_colors)
      
      return(table)
    }
}


CAAlosers = function(){
  options(knitr.table.format = 'html')
  
  previous = read.csv("history/history.csv") %>% 
    filter(date==unique(date)[length(unique(date))-1]&conference=="CAA") %>% 
    arrange(-yusag_coeff) %>% 
    mutate(confrank = rank(-yusag_coeff))
  df = pr_compute(TRUE) %>% 
    filter(conference=="CAA") %>% 
    select(team,yusag_coeff,conference_rank,rank,record,conf_record) %>% 
    arrange(-yusag_coeff)
  df = df[!duplicated(df$team),]
  df$actrec = NA
  df$actconfrec = NA
  for(tm in df$team){
    games = x %>% filter(tm==team)
    df$actrec[df$team==tm] = paste0(sum(games$score_diff>0,na.rm=TRUE),"-",sum(games$score_diff<0,na.rm=TRUE))
    df$actconfrec[df$team==tm] = paste0(sum(games$score_diff[games$conf_game==TRUE]>0,na.rm=TRUE),"-",sum(games$score_diff[games$conf_game==TRUE]<0,na.rm=TRUE))
  }
  background_colors = c()
  text_colors = c()
  for(i in 1:length(df$team)){
    background_colors = c(background_colors,ncaa_colors$primary_color[ncaa_colors$ncaa_name==df$team[i]])
    text_colors = c(text_colors,ncaa_colors$secondary_color[ncaa_colors$ncaa_name==df$team[i]])
  }
  
  cylcols = function(rankchange) {
    case_when(
      rankchange < 0 ~ 'green',
      rankchange > 0 ~ 'red',
      rankchange==0 ~ 'yellow'
    )
  }
  
  df = df %>% 
    relocate(c(actrec,actconfrec),.after=rank) %>% 
    rowwise() %>% 
    mutate(weekchange=yusag_coeff-previous$yusag_coeff[previous$team==team],
           rankchange = conference_rank-previous$confrank[previous$team==team],
           ic = ifelse(rankchange<0,paste0("<","img src=","greenarrow",".png",">"),ifelse(rankchange>0,paste0("<","img src=","redarrow",".png",">"),paste0("<","img src=","horizontal",".png",">")))) %>%     
    relocate(weekchange,.after=yusag_coeff) %>% 
    relocate(ic,.before=team) %>% 
    select(-c(rankchange,ic))
  
  table = kable(df, align = "ccccccccc",col.names=c("Team","YUSAG Coefficient","1 Week Change","Conference Rank","NCAA Rank","Overall Record","Conference Record","Predicted Overall Record","Predicted Conference Record"),caption=paste0("CAA"," Rankings"),format="html",escape=F) %>%
    kable_styling("striped", full_width = F, position = "center") %>%
    column_spec(1,color="white",background=background_colors)

    return(table)
}


caasched = function(date_start,date_end){
  df = subset(x,date=="2021-02-07"&team_conf=="CAA"&location=="H") %>% 
    mutate(opp_wins = 1-wins) %>% 
    rowwise() %>% 
    mutate(team_col=ncaa_colors$primary_color[ncaa_colors$ncaa_name==team],
           opp_col=ncaa_colors$primary_color[ncaa_colors$ncaa_name==opponent]) %>% 
    distinct(team,wins,opp_wins,.keep_all=TRUE) %>% 
    select(date,team,opponent,pred_score_diff,wins,opp_wins,team_col,opp_col) %>% 
    arrange(date)
  df = df[-c(2,4:5),]
  df = melt(df,id.vars=c("date","team","opponent","pred_score_diff","team_col","opp_col"),measure.vars=c("wins","opp_wins")) %>% 
    rowwise() %>% 
    mutate(teamcolor = ifelse(variable=="wins",ncaa_colors$primary_color[ncaa_colors$ncaa_name==team],ncaa_colors$primary_color[ncaa_colors$ncaa_name==opponent]))
  names = c()
  for(tm in unique(df$team)){
    out <- paste(strwrap(paste0(df$team[df$team==tm][1]," vs ",df$opponent[df$team==tm][1]," ",df$date[df$team==tm][1]," Predicted score differential: ",df$team[df$team==tm][1]," ",ifelse(df$pred_score_diff[df$team==tm][1]>0,paste0("+",df$pred_score_diff[df$team==tm][1]),df$pred_score_diff[df$team==tm][1])),width=20),collapse="\n") # put everything together
    names = append(names, out)
  }
  teamcolor = df$teamcolor
  names(teamcolor) = df$teamcolor
  df = df %>% 
  group_by(team) %>% 
    mutate(prop = value / sum(value) *100) %>%
    mutate(ypos = cumsum(prop)- 0.5*prop )
  plot = ggplot(df,aes(x="",y=value,fill=teamcolor)) +
    geom_bar(stat="identity",width=1000, color="white") +
    coord_polar("y", start=0) +
    facet_wrap(~team,labeller=labeller(team = setNames(names, unique(df$team)))) +
    scale_fill_manual(values=teamcolor) +
    geom_text(aes(label = value),position=position_stack(vjust=.5), color = "white", size=16) +
    theme(legend.position="none",axis.text.x=element_blank(),axis.text.y=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank(),strip.text.x = element_text(size = 20))
  
  # convert ggplot object to grob object
  gp <- ggplotGrob(plot)
  
  # optional: take a look at the grob object's layout
  gtable::gtable_show_layout(gp)
  
  # get gtable columns corresponding to the facets (5 & 9, in this case)
  facet.columns <- gp$layout$l[grepl("panel", gp$layout$name)]
  
  # get the number of unique x-axis values per facet (1 & 3, in this case)
  x.var <- sapply(ggplot_build(gp)$layout$panel_scales_x,function(l) length(l$range$range))
  
  # change the relative widths of the facet columns based on
  # how many unique x-axis values are in each facet
  gp$widths[facet.columns] <- gp$widths[facet.columns] * x.var
  
  # plot result
  grid::grid.draw(gp)
  return(plot)
}