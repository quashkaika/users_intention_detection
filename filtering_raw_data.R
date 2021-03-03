library(readr)
library(dplyr)
library(tidyverse)
#filtering data
filter_data <- function(file_path) {
  output <- read_csv(file_path)
  clean_output <- output %>%
    select(VP, SystemTime, NPCCondition, gridCond, inTrial, inPause, numTrials, numSteps, Fixation, Fixation_duration_ms, Saccade, Saccade_duration_ms)%>%
    filter(inTrial == TRUE & gridCond == 3)
  clean_output_fixation <- clean_output %>% select(VP, SystemTime, NPCCondition, gridCond, inTrial, inPause, numTrials, numSteps, Fixation, Fixation_duration_ms)
  clean_output_fixation <- na.omit(clean_output_fixation)
  clean_output_saccade <- clean_output %>% select(VP, SystemTime, NPCCondition, gridCond, inTrial, inPause, numTrials, numSteps, Saccade, Saccade_duration_ms)
  clean_output_saccade <- na.omit(clean_output_saccade)
  
  return(list(clean_output_fixation, clean_output_saccade))
}

filter_by_trials <- function(fix_and_sacc_tuple, trial_num) {
  fix <- fix_and_sacc_tuple[[1]]
  sacc <- fix_and_sacc_tuple[[2]]
  results <- list(fix %>% filter(numTrials == trial_num), sacc %>% filter(numTrials == trial_num))
  return(results)
}

filter_by_steps <- function(fix_and_sacc_steps, steps) {
  fix_steps <- fix_and_sacc_steps[[1]]
  sacc_steps <- fix_and_sacc_steps[[2]]
  results2 <- list(fix_steps %>% filter(numSteps == steps), sacc_steps %>% filter(numSteps == steps))
  return(results2)
}

t_test <- function(ds1, ds2, col_name) {
  tryCatch(
    t.test(ds1[[col_name]], ds2[[col_name]], paired = FALSE, alternative="two.sided"),
    error = function(cond){
      print(cond)
    }
  )
}
#t-test results into file
append_test_resutl_to_file <- function(vp, trial_num, step, test_result, f_path) {
  out <- list(vp, trial_num, step, test_result$estimate[['mean of x']], test_result$estimate[['mean of y']], test_result$statistic[['t']], test_result$parameter[['df']], test_result$p.value, test_result$conf.int[[1]], test_result$conf.int[[2]])
  write(paste(out, collapse = ','), file=f_path, append=TRUE)
}

lapply(
  list.dirs(path = "./VP_Logs", full.names = TRUE, recursive = FALSE), 
  { function(path) {
    tuple1 <- filter_data(sprintf("%s/1.csv", path))
    tuple2 <- filter_data(sprintf("%s/2.csv", path))
    tuple3 <- filter_data(sprintf("%s/3.csv", path))
    for(trial_num in seq(0, 9)) {
      tuple11 <- filter_by_trials(tuple1, trial_num)
      tuple22 <- filter_by_trials(tuple2, trial_num)
      tuple33 <- filter_by_trials(tuple3, trial_num)
      for(step in seq(0, 8)) {
        tuple111 <- filter_by_steps(tuple11,step)
        tuple222 <- filter_by_steps(tuple22,step)
        tuple333 <- filter_by_steps(tuple33,step)
        
        # append test result to common file:
        append_test_resutl_to_file(
          path,
          trial_num,
          step,
          t_test(tuple111[[1]], tuple222[[1]], "Fixation_duration_ms"), 
          sprintf("%s/../fix_steps_1_2.csv", path))
        append_test_resutl_to_file(
          path,
          trial_num,
          step,
          t_test(tuple222[[1]], tuple333[[1]], "Fixation_duration_ms"), 
          sprintf("%s/../fix_steps_2_3.csv", path))
        append_test_resutl_to_file(
          path,
          trial_num,
          step,
          t_test(tuple111[[1]], tuple333[[1]], "Fixation_duration_ms"), 
          sprintf("%s/../fix_steps_1_3.csv", path))
        
        append_test_resutl_to_file(
          path,
          trial_num,
          step,
          t_test(tuple111[[2]], tuple222[[2]], "Saccade_duration_ms"), 
          sprintf("%s/../sac_steps_1_2.csv", path))
        append_test_resutl_to_file(
          path,
          trial_num,
          step,
          t_test(tuple222[[2]], tuple333[[2]], "Saccade_duration_ms"), 
          sprintf("%s/../sac_steps_2_3.csv", path))
        append_test_resutl_to_file(
          path,
          trial_num,
          step,
          t_test(tuple111[[2]], tuple333[[2]], "Saccade_duration_ms"), 
          sprintf("%s/../sac_steps_1_3.csv", path))
        
        
      }}
  }
  }
)