#Cronin M. 
#Testing the effectiveness of interactive training on sexual harassment and assault in field science
### Moderation analyses for testing whether prevention behavior or personal norm impacted responses for behavioral intention
#RQ3 H3c
#please use the correlated data produced in the file Correlation_analyses.R to run this code. 

rm(list=ls())
# Load required libraries
library(stringr)
library(dplyr)
library(sf)
library(sp)
library (lme4)
library(ggplot2)
library(reshape2)
library(readxl)
library(ordinal)
library(broom.mixed)
library(broom)
library(MuMIn)
library(rmeta)
library(gridExtra)
library(ggfortify)
library(forestplot)
library(tidyr)
library(sjPlot)
library(effects)
library(psych)
library(lme4)
library(sjPlot)
library(psych)
library(gridExtra)
library(grid)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(viridis)
library(lmerTest)
library(patchwork)

##FIRST, upload all data #####
setwd("/Users/melissacronin/Desktop/Harassment work/cdfw_data_new/")


norms<-c("Q3_a","Q4_a")
behavioral_intention<- c("Q11_Q12_a", "Q11_Q12_b", "Q11_Q12_c", "Q11_Q12_d")

data<- read.csv( "data_correlations_grouped.csv", sep="," ) %>% 
  dplyr::select(RecordedDate, response_id, wave,
                Q3_a, Q4_a,Q3_b,Q4_b,
                all_of(behavioral_intention)) %>% 
  filter(complete.cases(Q3_a, Q4_a, Q11_Q12_a, Q11_Q12_b, Q11_Q12_c, Q11_Q12_d)) 

data_clean<-data %>% 
  filter(wave %in% c("1", "2")) %>% #find only paired responses
  group_by(response_id) %>%
  filter(sum(wave == 1) == 1 & sum(wave == 2) == 1) %>%
  ungroup() %>% 
  group_by(response_id) %>% 
  dplyr::mutate(composite_variable=mean(c(Q3_a, Q4_a, Q3_b, Q4_b ) )) %>%  #create composite correlated value for norms (correlation tested below)
  pivot_wider(id_cols=response_id, names_from = wave, values_from = c(composite_variable, behavioral_intention)) %>% 
  rename(composite_variable= composite_variable_1) #pre data only

# # Test correlation
# norms <- data[, c("Q3_a_1",  "Q4_a_1")] #just pre data
# correlation <- cor(norms) #Check that Q3a and Q4a are correlated; that Q3b and Q4b are correlated 
# #both are >.9
# alpha_value <- psych::alpha(correlation)
# 
# data_wide$Q3_a<-as.numeric(data_wide$Q3_a_1)
# data_wide$Q4_a<-as.numeric(data_wide$Q4_a_1)
# # Create the composite variable by taking the average


######  Create the composite variable by taking the average ####
hist(data_clean$composite_variable)
mean(data_clean$composite_variable)


# Determine the cutoff point(s)
cutoff <- mean(data_clean$composite_variable, na.rm=T)

#norms
data_clean$norm_group <- ifelse(data_clean$composite_variable>= cutoff, "High", "Low")

# Calculate the range for the high norm group
high_norm_range <- range(data_clean$composite_variable[data_clean$norm_group == "High"])
# Calculate the range for the low norm group
low_norm_range <- range(data_clean$composite_variable[data_clean$norm_group == "Low"])


# Check the distribution of the norm groups
table(data_clean$norm_group)

########
norms<-c("Q3_a","Q4_a")
behavioral_intention<- c("Q11_Q12_a", "Q11_Q12_b", "Q11_Q12_c", "Q11_Q12_d")

# Calculate score change for each behavioral intention variable
for (var in behavioral_intention) {
  data_clean[[paste0(var, "_change")]] <- data_clean[[paste0(var, "_2")]] - data_clean[[paste0(var, "_1")]]
}


# Create an empty dataframe to store the regression results
regression_results_norm <- data.frame(variable = character(),
                                 term = character(),
                                 estimate = numeric(),
                                 std.error = numeric(),
                                 p.value = numeric(),
                                 ci.lower = numeric(),
                                 ci.upper = numeric(),
                                 stringsAsFactors = FALSE)

# Loop through each variable and generate the plot
for (var in behavioral_intention) {
  # Fit a linear regression model
  model <- lm(paste0(var, "_change ~ norm_group"), data = data_clean)
  
  # Check the summary of the model
  summary_table <- summary(model)
  
  # Extract the p-value for the coefficient
  p_value <- summary_table$coefficients["norm_groupLow", "Pr(>|t|)"]
  
  # Filter the data for low and high norm groups separately. in case you want to plot it.
  data_low_norm <- data_clean[data_clean$norm_group == "Low", ]
  data_high_norm <- data_clean[data_clean$norm_group == "High", ]
  
  # Extract the confidence intervals
  ci <- confint(model)["norm_groupLow", ]
  
  # Store the regression results in the list
  regression_result <- broom::tidy(model) %>%
    dplyr::select(term, estimate, std.error) %>%
    mutate(variable = var, p.value = p_value, ci.lower = ci[1], ci.upper = ci[2])
  
  regression_results_norm <- rbind(regression_results_norm, regression_result)
  
}

print(regression_results_norm)



#PREVENTION BEHAVIOR


self_reported_behavior<-c("Q7_Q8_a", "Q7_Q8_b", "Q7_Q8_c", "Q7_Q8_d", "Q7_Q8_e")
behavioral_intention<- c("Q11_Q12_a", "Q11_Q12_b", "Q11_Q12_c", "Q11_Q12_d")


data<- read.csv( "data_correlations_grouped.csv", sep="," ) %>% 
  dplyr::select(RecordedDate, response_id, wave, 
                all_of(self_reported_behavior),all_of(behavioral_intention))   %>% 
  tidyr::fill(Q7_Q8_a, Q7_Q8_b, Q7_Q8_c, Q7_Q8_d, Q7_Q8_e, .direction = "down") %>%
  drop_na(Q11_Q12_a, Q11_Q12_b, Q11_Q12_c, Q11_Q12_d) %>% 
    drop_na("Q7_Q8_a", "Q7_Q8_b", "Q7_Q8_c", "Q7_Q8_d", "Q7_Q8_e") %>%
   dplyr::group_by(response_id) %>% 
   dplyr::mutate(behavior_composite=mean(c(Q7_Q8_a, Q7_Q8_b, Q7_Q8_c, Q7_Q8_d, Q7_Q8_e ) )) %>%  #create composite correlated value for self-reported behavior
  dplyr:: select(-c(Q7_Q8_a, Q7_Q8_b, Q7_Q8_c, Q7_Q8_d, Q7_Q8_e))
  

#test correlation between behavior columns 
# behavior_norms<-data_clean[c("Q7_Q8_a", "Q7_Q8_b", "Q7_Q8_c", "Q7_Q8_d", "Q7_Q8_e")]
# # Convert data to a data frame
# self_reported_behavior <- as.data.frame(self_reported_behavior)
# correlation <- cor(behavior_norms)
# alpha(correlation)


data_clean<- data %>%
  filter(wave %in% c("1", "2")) %>% #find only paired responses
  group_by(response_id) %>%
  filter(sum(wave == 1) == 1 & sum(wave == 2) == 1) %>%
  group_by(response_id) %>%
  filter(n() == 2) %>%
  ungroup() %>% 
  pivot_wider(id_cols=response_id, names_from = wave, values_from = c(behavior_composite, behavioral_intention)) %>% 
  rename(behavior_composite=behavior_composite_1) # only pre data

hist(data_clean$behavior_composite)
mean(data_clean$behavior_composite)

# Determine the cutoff point(s)
cutoff <- mean(data_clean$behavior_composite, na.rm=T)

# Create a new variable indicating group membership
data_clean$behavior_group <- ifelse(data_clean$behavior_composite >= cutoff, "High", "Low")

# Calculate the range for the high behavior group
high_behavior_range <- range(data_clean$behavior_composite[data_clean$behavior_group == "High"])

# Calculate the range for the low behavior group
low_behavior_range <- range(data_clean$behavior_composite[data_clean$behavior_group == "Low"])

# Check the distribution of the behavior groups
table(data_clean$behavior_group)


########
behavioral_intention<- c("Q11_Q12_a", "Q11_Q12_b", "Q11_Q12_c", "Q11_Q12_d")

# Calculate score change for each behavioral intention variable
for (var in behavioral_intention) {
  data_clean[[paste0(var, "_change")]] <- data_clean[[paste0(var, "_2")]] - data_clean[[paste0(var, "_1")]]
}


# Create an empty dataframe to store the regression results
regression_results_behavior <- data.frame(variable = character(),
                                 term = character(),
                                 estimate = numeric(),
                                 std.error = numeric(),
                                 p.value = numeric(),
                                 ci.lower = numeric(),
                                 ci.upper = numeric(),
                                 stringsAsFactors = FALSE)

# Loop through each variable and generate the plot
for (var in behavioral_intention) {
  # Fit a linear regression model
  model <- lm(paste0(var, "_change ~ behavior_group"), data = data_clean)
  
  # Check the summary of the model
  summary_table <- summary(model)
  
  # Extract the p-value for the coefficient
  p_value <- summary_table$coefficients["behavior_groupLow", "Pr(>|t|)"]
  
  # Filter the data for low and high norm groups separately. in case you want to plot it.
  data_low_behavior <- data_clean[data_clean$behavior_group == "Low", ]
  data_high_behavior <- data_clean[data_clean$behavior_group == "High", ]
  
  # Extract the confidence intervals
  ci <- confint(model)["behavior_groupLow", ]
  
  # Store the regression results in the list
  regression_result <- broom::tidy(model) %>%
    dplyr::select(term, estimate, std.error) %>%
    mutate(variable = var, p.value = p_value, ci.lower = ci[1], ci.upper = ci[2])
  
  regression_results_behavior <- rbind(regression_results_behavior, regression_result)
  
}

print(regression_results_behavior)

df<- rbind(regression_results_behavior, regression_results_norm)


write.csv(df, "norm_prevention_behavior_moderation_results.csv")



