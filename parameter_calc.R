library(readr)
library(dplyr)
library(tidyverse)

my_reaction <- function(p, n){
  if(p == TRUE && n == TRUE){
    return(999)}
  if(p == TRUE && n == FALSE){
    return(1)}
  if(p == FALSE && n == TRUE){
    return(0)}
  if(p == FALSE && n == FALSE){
    return(999)}
}
#filtering data
filter_data <- function(file_path) {
  print(sprintf("...filtering data %s", file_path))
  output <- read_csv(file_path)
   clean_output <- output %>%
    filter(gridCond == 3 & inTrial == TRUE)
  clean_output_reaction <- mutate(clean_output, SystemTime = as.integer(as.POSIXct(format(clean_output$SystemTime, '2019-01-01 %H:%M:%S'))))
  clean_output_reaction <- clean_output_reaction %>% rowwise %>% mutate(Reaction = my_reaction(PressingPosEval, PressingNegEval))
    # do({
    # result = as_data_frame(.)
    #result$Reaction <- my_reaction(result$PressingPosEval, result$PressingNegEval)
    #result
  #})
  ML_dataset <- clean_output_reaction %>% select(VP, SystemTime, NPCCondition, gridCond, inTrial, numTrials, numSteps, inPause,`velocity_m/s`,`rightGazeDirection.z`, `rightPupilRad`, Fixation, Fixation_duration_ms, Saccade, Saccade_duration_ms, Reaction)%>%
    add_row()
  print("Done filtering!")
  return(ML_dataset)
}

#cleaning data with fixation duration/saccade duration column transformation.
clean_data <- function(dataset){ 
  print("...cleaning data %s")
  
  currentFixDur = as.integer(0)
  currentSaccDur = as.integer(0)
  currentFixNum = as.integer(0)
  currentSaccNum = as.integer(0)
  indexF = as.integer(1)
  indexS = as.integer(1)
  
  Clean_ML_dataset <- dataset %>% rowwise %>% do({
    result = as_tibble(.)
    if(!is.na(result$Fixation)) {
      if(result$Fixation > currentFixNum) {
        currentFixDur <- result$Fixation_duration_ms
        currentFixNum <- result$Fixation
      }
      if(result$Fixation == currentFixNum) {
        result$Fixation_duration_ms <- currentFixDur
      }
    }
    
    if(!is.na(result$Saccade)) {
      if(result$Saccade > currentSaccNum) {
        currentSaccDur <- result$Saccade_duration_ms
        currentSaccNum <- result$Saccade
      }
      if(result$Saccade == currentSaccNum) {
        result$Saccade_duration_ms <- currentSaccDur
      }
    }
    
    result
  })
  print("Done cleaning")
  return(Clean_ML_dataset)
}
#replacing NA 
replacement_na <- function(dataset){
  
  indexF = as.integer(1)
  indexS = as.integer(1)
  
  Clean_ML_dataset2 <- dataset%>% rowwise %>% do({
    result = as_tibble(.)
    if(!is.na(result$Fixation) & !is.na(dataset[indexF + 1,]$Fixation)) {
      result$Fixation_duration_ms <- NA
    }
    else result$Fixation_duration_ms <- result$Fixation_duration_ms
    indexF <- indexF + 1
    
    if(!is.na(result$Saccade) & !is.na(dataset[indexS + 1,]$Saccade)) {
      result$Saccade_duration_ms <- NA
    }
    else result$Saccade_duration_ms <- result$Saccade_duration_ms
    indexS <- indexS + 1
    
    result
    
  })
  
  Clean_ML_dataset2 <- Clean_ML_dataset2 %>% filter((!is.na(Fixation) && !is.na(Fixation_duration_ms)) || (!is.na(Saccade) && !is.na(Saccade_duration_ms)) || !is.na(Reaction))
  
#  i = as.integer(0)
  
#  currentReaction = as.integer(0)
  
#  Clean_ML_dataset2 <- Clean_ML_dataset2 %>% rowwise %>% do({
#    result = as_tibble(.)
#    if(is.na(result$Reaction)) {
#      result$Reaction <- 999
#    }
#    result
#  }) %>% rowwise %>% do({
#    result = as_tibble(.)
 #   if(result$Reaction == 999 && currentReaction < 999) {
#      i <- i + 1
#    }
#    currentReaction <- result$Reaction
    
#    result$ReactionNumber <- i
#    result
#  })
  return(Clean_ML_dataset2)
}

#defining reaction type 0/1/999 (positive/negative/non-reaction)
reaction_type_define <- function(df){
  already_changed <- FALSE
  
  k <- tail(df, n=1)$Reaction
  
  if(k != 999) {
    return(k)
  }
  
  for(i in nrow(df):1) {
    row <- df[i,]
    if(row$Reaction != k & !already_changed) {
      k = row$Reaction
      already_changed = TRUE
    }
  }
  
  return(k)
}

#parameter calculation
calculation <- function(dataset){
  Clean_ML_dataset3 <- dataset%>%
    group_by(numTrials,numSteps)%>%
    group_map({function(x, y) {
      saccs <- na.omit(as.vector(x$Saccade_duration_ms))
      sac_mean <- round(mean(saccs), 2)
      sac_median <- median(saccs)
      sac_velocity_data <- x %>% filter(is.na(Fixation) & !is.na(Saccade))
      sac_velocity <- na.omit(as.vector(sac_velocity_data$`velocity_m/s`))
      sac_velocity_mean <- round(mean(sac_velocity), 6)
      reaction_type = reaction_type_define(x)
      sac_total <- length(saccs)
      sac_rate <- round(sac_total / (tail(x, n=1)$SystemTime - head(x, n=1)$SystemTime), 2)
      in_range_sac <- nrow(x %>% filter(Saccade_duration_ms >20 & Saccade_duration_ms <200))
      in_range_sac_total <-x %>% filter(Saccade_duration_ms >20 & Saccade_duration_ms <200)
      in_range_sac_mean <- round(mean(in_range_sac_total$Saccade_duration_ms),2)
      sac_proportion <- round((in_range_sac / sac_total),2)
      sac_amplitude <- round(((sac_mean - 21) / 2.2), 2)
      radius_velocity <- na.omit(as.vector(x$`rightGazeDirection.z`))
      radius <- round(mean(radius_velocity),6)
      sac_velocity_angular<- round(((sac_velocity_mean/radius)*180) / 3.14, 2)
      sac_peak_velocity <- round(75*(sac_amplitude^0.6))
      fixs <- na.omit(as.vector(x$Fixation_duration_ms))
      fix_mean <- round(mean(fixs), 2)
      fix_median <- median(fixs)
      fix_total <- length(fixs)
      fix_rate <- round(fix_total / (tail(x, n=1)$SystemTime - head(x, n=1)$SystemTime), 2)
      in_range_fix <- nrow(x %>% filter(Fixation_duration_ms >150 & Fixation_duration_ms <900))
      in_range_fix_total <- x %>% filter(Fixation_duration_ms >150 & Fixation_duration_ms <900)
      in_range_fix_mean <- round(mean(in_range_fix_total$Fixation_duration_ms),2) 
      fix_proportion <- round((in_range_fix / fix_total),2)
      pupil_rad <- na.omit(as.vector(x$`rightPupilRad`))
      pupil_radius <- round(mean(pupil_rad), 6)
      pupil_radius_max <- max(pupil_rad)
      pupil_radius_min <- min(pupil_rad)
      pupil_radius_dif <- pupil_radius_max -pupil_radius_min
      
      df <- data.frame(c(y), fix_mean, fix_median, fix_total, in_range_fix, in_range_fix_mean, fix_rate, fix_proportion, sac_mean, sac_median, sac_total, in_range_sac, in_range_sac_mean, sac_rate, sac_proportion, sac_velocity_mean, sac_velocity_angular, sac_peak_velocity, sac_amplitude, pupil_radius, pupil_radius_dif, reaction_type)
      return(df)
    }})
  #head(Clean_ML_dataset3,10)
  Clean_ML_dataset <- bind_rows(Clean_ML_dataset3)
  #str(Clean_ML_dataset3)
  return(Clean_ML_dataset)
}
#view(Clean_ML_dataset3)

path <- "./ML_dataset_full/3"
lapply(
  list.files(path = path),
  { function(filename){
    print(sprintf("%s/%s", path, filename))
    data <- 
      calculation(
      replacement_na(
        clean_data(
          filter_data(sprintf("%s/%s", path, filename))
          )
        )
      )
     file_path <- sprintf('%s/../condition3.csv', path)
    
     write.table(data, file_path, sep = ",", col.names = !file.exists(file_path),row.names = FALSE, append = TRUE)
  }}
)

