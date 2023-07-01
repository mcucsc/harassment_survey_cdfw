#Cronin M. 
#Testing the effectiveness of interactive training on sexual harassment and assault in field science
### Linear regressions using within-subjects pre-post analyses (n=196). 
#please use the correlated data produced in the file Correlation_analyses.R to run this code. 

rm(list=ls())
# Load required libraries
library(stringr)
require(dplyr)
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
library(gridExtra)
library(grid)
library(ggplot2)
library(dplyr)
library(lmerTest)


##FIRST, upload all data #####
setwd("/Users/melissacronin/Desktop/Harassment work/cdfw_data_new/")

variables <- c("Q1_Q2_a", "Q1_Q2_b",  "Q1_Q2_c", "Q1_Q2_d", "Q1_Q2_e", #self-efficacy , grouped
               "Q11_Q12_a", "Q11_Q12_b", "Q11_Q12_c", "Q11_Q12_d" # behavioral intention
)

data<- read.csv( "data_correlations_grouped.csv", sep="," ) %>% 
  filter(wave %in% c("1", "2")) %>% #find only paired responses
  group_by(response_id) %>%
  filter(all(c("1", "2") %in% wave)) %>%
  ungroup() %>% 
  dplyr::select(RecordedDate, response_id, wave, t_c,
                all_of(variables)) 

data$wave<-factor(data$wave)
data$response_id<-factor(data$response_id)


###A PLOT -  #####
#Create an empty data frame to store the data
plot_data <- data.frame(variable = character(0), wave = integer(0),
                        mean = numeric(0), se = numeric(0), p_value = numeric(0),
                        ci_lower = numeric(0), ci_upper = numeric(0))

general_regression_table <- data.frame(term = character(),
                                       variable = character(),
                                       coefficient = numeric(),
                                       std_error = numeric(),
                                       p_value = numeric(),
                                       ci_lower = numeric(),
                                       ci_upper = numeric(),
                                       stringsAsFactors = FALSE)

# Loop through the variables
for (var in variables) {
  # Convert the variable to numeric
  data <- data %>%
    mutate(!!var := as.numeric(!!sym(var)))
  
  # Compute summary statistics
  sum_stat <- data %>%
    group_by(wave) %>%
    dplyr::summarize(mean = mean(!!sym(var), na.rm = TRUE),
                     sd = replace_na(sd(!!sym(var), na.rm = TRUE), 0),
                     n = sum(!is.na(!!sym(var)))) %>%
    mutate(se = sd / sqrt(n))
  
  # Fit linear regression model
  model <- lm(paste(var, "~ wave"), data = data)
  
  # Extract coefficients, standard errors, and p-values
  coef <- coef(summary(model))
  p_value <- coef["wave2", "Pr(>|t|)"]
  
  # Calculate confidence intervals for coefficients
  ci <- confint(model, level = 0.95)
  
  # Create a data frame for the current variable
  var_data <- data.frame(variable = var,
                         mean = sum_stat$mean,
                         se = sum_stat$se,
                         wave = sum_stat$wave,
                         p_value = p_value,
                         ci_lower = ci[2, 1],
                         ci_upper = ci[2, 2])
  
  # Add the data frame to the plot_data data frame
  plot_data <- rbind(plot_data, var_data)
  
  # Create a data frame for the regression results
  regression_results <- data.frame(term = rownames(coef),
                                   variable = var,
                                   coefficient = coef[, "Estimate"],
                                   std_error = coef[, "Std. Error"],
                                   p_value = coef[, "Pr(>|t|)"],
                                   ci_lower = ci[, 1],
                                   ci_upper = ci[, 2],
                                   stringsAsFactors = FALSE)
  
  # Add the regression results to the general_regression_table data frame
  general_regression_table <- rbind(general_regression_table, regression_results)
}

# Print the general regression table
print(general_regression_table)


# Write the regression table to a CSV file. This is Table S5
output_file <- "general_regression_table_efficacy_intention.csv"
write.csv(general_regression_table, file = output_file, row.names = FALSE)



#Check model assumptions #####


#group by concept
variables <- c("Q1_Q2_a", #knowledge grouped
               "Q1_Q2_b", "Q1_Q2_c", "Q1_Q2_d", "Q1_Q2_e", #self-efficacy , grouped
               "Q11_Q12_a", "Q11_Q12_b", "Q11_Q12_c", "Q11_Q12_d" # behavioral intention
)

model_data<- drop_na(data, all_of(variables))


# Loop through the variables
for (var in variables) {
  # Subset the data for the current variable
  # Fit linear regression model
  model <- lm(paste(var, "~ wave"), data = data)
  
  # Create a new plot window for each variable
  plot.new()
  
  # Create a layout grid for the four assumption plots
  par(mfrow = c(2, 2))
  
  # Residuals vs Fitted plot
  plot(fitted(model), residuals(model),
       xlab = "Fitted values", ylab = "Residuals",
       main = paste0("Residuals vs Fitted - ", var))
  abline(h = 0, lty = "dashed")
  
  # Normal Q-Q plot
  qqnorm(residuals(model),
         main = paste0("Normal Q-Q Plot - ", var))
  qqline(residuals(model))
  
  # Scale-Location plot
  plot(sqrt(abs(residuals(model))) ~ fitted(model),
       xlab = "Fitted values",
       ylab = "Square root of absolute residuals",
       main = paste0("Scale-Location Plot - ", var))
  abline(h = 0, lty = "dashed")
  
  # Residuals vs Leverage plot
  plot(hatvalues(model), residuals(model),
       xlab = "Leverage", ylab = "Residuals",
       main = paste0("Residuals vs Leverage - ", var))
  abline(h = 0, lty = "dashed")
  
  # Attempt to plot smooth curve, handle the error if it occurs
  tryCatch(
    {
      abline(smooth.spline(fitted(model), sqrt(abs(residuals(model)))), col = "red")
    },
    error = function(e) {
      # Do nothing or add error handling code here
    }
  )
}



#Demographic data ####
#re-read fresh dataset
data<- read.csv( "data_correlations_grouped.csv", sep="," ) %>% 
  filter(wave %in% c("1", "2")) %>% #find only paired responses
  group_by(response_id) %>%
  filter(all(c("1", "2") %in% wave)) %>%
  ungroup() %>% 
  dplyr::select(RecordedDate, response_id,gender, wave, t_c,
                all_of(variables)) 

data$wave<-factor(data$wave)
data$response_id<-factor(data$response_id)

# Sort the dataset by response_id and wave
data_sorted <- data %>%
  arrange(response_id, wave)

# Duplicate demographic data from wave 1 to wave 2 rows within respondent
data_s<- data_sorted %>% 
  mutate(gender = ifelse(gender == "", NA, gender))

data_f<- data_s %>%
  group_by(response_id) %>% 
  do(fill(., gender)) %>%
  ungroup() %>% 
  filter(gender=="Man"| gender=="Woman") #%>% 

#check gender data
data_f %>%
  group_by(wave, gender) %>%
  summarize(count=n(),
            mean = mean(Q1_Q2_a, na.rm = TRUE),
            n=n())



#first, only test whether the training intervention worked for each gender.
###A PLOT - GENDER #####
data_f$gender<-factor(data_f$gender)

data_f$wave<-factor(data_f$wave)
# Create an empty data frame to store the data
plot_data <- data.frame(variable = character(0), wave = integer(0), gender = character(0),
                        mean = numeric(0), se = numeric(0), p_value = numeric(0))

# Loop through the variables
for (var in variables) {
  # Convert the variable to numeric
  data_f <- data_f %>%
    mutate(!!var := as.numeric(!!sym(var)))
  
  # Compute summary statistics
  sum_stat <- data_f %>%
    group_by(wave, gender) %>%
    dplyr::summarize(mean = mean(!!sym(var), na.rm = TRUE),
                     sd = replace_na(sd(!!sym(var), na.rm = TRUE), 0),
                     n = sum(!is.na(!!sym(var)))) %>%
    mutate(se = sd / sqrt(n))
  
  # Fit linear regression models for each gender group
  model_man <- lm(paste(var, "~ wave"), data = data_f %>% filter(gender == "Man"))
  model_woman <- lm(paste(var, "~ wave"), data = data_f %>% filter(gender == "Woman"))
  
  # Extract p-values for Time 1 vs Time 2 comparisons within each gender group
  p_value_man <- summary(model_man)$coefficients[2, "Pr(>|t|)"]
  p_value_woman <- summary(model_woman)$coefficients[2, "Pr(>|t|)"]
  
  # Create a data frame for the current variable
  var_data <- data.frame(variable = var,
                         gender = sum_stat$gender,
                         mean = sum_stat$mean,
                         se = sum_stat$se,
                         wave = sum_stat$wave,
                         p_value = ifelse(sum_stat$gender == "Man", p_value_man, p_value_woman))
  
  # Add the data frame to the plot_data data frame
  plot_data <- rbind(plot_data, var_data)
}



#now, test whether gender impacted change in scores between pre- and post-training. 
# Create an empty data frame to store the regression results
gender_regression_table <- data.frame(term = character(0),
                                      variable = character(0),
                                      coefficient = numeric(0),
                                      std_error = numeric(0),
                                      p_value = numeric(0),
                                      ci_lower = numeric(0),
                                      ci_upper = numeric(0),
                                      stringsAsFactors = FALSE)

# Loop through the variables
for (var in variables) {
  # Fit linear regression model including interaction term
  model <- lm(paste(var, "~ wave + gender + wave*gender", sep = ""), data = data_f)
  
  # Extract model coefficients, standard errors, and p-values
  model_coefs <- coef(summary(model))
  coefficients <- model_coefs[, "Estimate"]
  std_errors <- model_coefs[, "Std. Error"]
  p_values <- model_coefs[, "Pr(>|t|)"]
  
  # Calculate confidence intervals for each coefficient
  cis <- confint(model, level = 0.95)
  ci_lower <- cis[, 1]
  ci_upper <- cis[, 2]
  
  # Create a data frame with the regression results for the current variable
  var_results <- data.frame(term = rownames(model_coefs),
                            variable = var,
                            coefficient = coefficients,
                            std_error = std_errors,
                            p_value = p_values,
                            ci_lower = ci_lower,
                            ci_upper = ci_upper)
  
  # Append the results to the regression table
  gender_regression_table <- rbind(gender_regression_table, var_results)
}

# Write the regression table to a CSV file. This will go into Table 4. 
output_file <- "gender_regression_table_efficacy_intention.csv"
write.csv(gender_regression_table, file = output_file, row.names = FALSE)



#plot the gender data for Fig 3 ######

# Create the plot using ggplot2 - this is looking at genders separtately 
ggplot(plot_data, aes(x = as.factor(wave), y = mean,  group =gender)) +
    geom_point(aes(color=gender),size = 2, alpha = 0.7) +
   geom_line(aes(color=gender),linewidth=0.7, alpha = 0.7) +
  geom_ribbon(aes(ymin = mean - se, ymax = mean + se, fill=gender),
              alpha = 0.2) +  # Add error shading
  labs(x = "Wave", y = "Mean", fill = "Gender", color="Gender") +
  theme_classic() +
  facet_wrap(~ variable, ncol = 2, scales="free_x") +
  scale_y_continuous(limits=c(4, 7))+
  scale_x_discrete(labels = c("Pre", "Post"))+
  geom_text(data = plot_data[plot_data$wave == 2, ],
            aes(color=gender,label = ifelse(p_value < 0.001, "***",
                                            ifelse(p_value < 0.01, "**",
                                                   ifelse(p_value < 0.05, "*", "")))),
            hjust = -0.2, vjust = -1.8, size = 5, show.legend=FALSE) +
  theme(legend.position="top",
        text = element_text(size = 16),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16)
  )+
  # Modify the theme to remove facet labels
  theme(strip.text = element_blank())+
  scale_color_manual(values = c("Man" = "#404387", "Woman" = "#9E2F8C"),
                     labels = c("Man", "Woman"),
                     guide = guide_legend(override.aes = list(shape = c(16, 16))))+
  scale_fill_manual(values = c("Man" = "#404387", "Woman" = "#9E2F8C"),
                    labels = c("Man", "Woman"))


#ggsave("RQ3_gender_jun22.tiff", dpi=300, height=9, width=8)





### now do the same process for race ####

data<- read.csv( "data_correlations_grouped.csv", sep="," ) %>% 
  filter(wave %in% c("1", "2")) %>% #find only paired responses
  group_by(response_id) %>%
  filter(all(c("1", "2") %in% wave)) %>%
  ungroup() %>% 
  dplyr::select(RecordedDate, response_id,new_race, wave, t_c, #new_race refers to the binned URM group 
                all_of(variables)) 

data$response_id<-factor(data$response_id)

# Sort the dataset by response_id and wave
data_sorted <- data %>%
  arrange(response_id, wave)

# Duplicate demographic data from wave 1 to wave 2 rows within subjects
data_s<- data_sorted %>% 
  mutate(new_race = ifelse(new_race == "", NA,new_race))

data_f<- data_s %>%
  group_by(response_id) %>% 
  do(fill(., new_race)) %>%
  ungroup() %>% 
  filter(new_race=="U"| new_race=="W") %>% 
  drop_na(Q1_Q2_a,Q1_Q2_b,Q1_Q2_c, Q1_Q2_d, Q1_Q2_e)
  #drop_na(Q11_Q12_a,Q11_Q12_b,Q11_Q12_c, Q11_Q12_d)

#check race data
data_f %>%
  group_by(wave, new_race) %>%
  summarize(mean = mean(Q1_Q2_a, na.rm = TRUE),
            n=n())




###PLOT - race #####
data_f$new_race<-factor(data_f$new_race)

data_f$wave<-factor(data_f$wave)
# Create an empty data frame to store the data
plot_data <- data.frame(variable = character(0), wave = integer(0), new_race = character(0),
                        mean = numeric(0), se = numeric(0), p_value = numeric(0))

# Loop through the variables
for (var in variables) {
  # Convert the variable to numeric
  data_f <- data_f %>%
    mutate(!!var := as.numeric(!!sym(var)))
  
  # Compute summary statistics
  sum_stat <- data_f %>%
    group_by(wave, new_race) %>%
    dplyr::summarize(mean = mean(!!sym(var), na.rm = TRUE),
                     sd = replace_na(sd(!!sym(var), na.rm = TRUE), 0),
                     n = sum(!is.na(!!sym(var)))) %>%
    mutate(se = sd / sqrt(n))
  
  #IF TESTING DIFFS BETWEEN TIME 1 AND TIME 2, USE BELOW 
  # Fit linear regression models for each gender group
  model_U <- lm(paste(var, "~ wave"), data = data_f %>% filter(new_race == "U"))
  model_W <- lm(paste(var, "~ wave"), data = data_f %>% filter(new_race == "W"))
  # 
  # # Extract p-values for Time 1 vs Time 2 comparisons within each gender group
  p_value_U <- summary(model_U)$coefficients[2, "Pr(>|t|)"]
  p_value_W <- summary(model_W)$coefficients[2, "Pr(>|t|)"]
  
  # Create a data frame for the current variable
  var_data <- data.frame(variable = var,
                         new_race= sum_stat$new_race,
                         mean = sum_stat$mean,
                         se = sum_stat$se,
                         wave = sum_stat$wave,
                         p_value = ifelse(sum_stat$new_race == "U", p_value_U, p_value_W))
  
  # Add the data frame to the plot_data data frame
  plot_data <- rbind(plot_data, var_data)
}

# Relevel the new_race variable
data_f$new_race <- relevel(data_f$new_race, ref = "W")

# Create an empty data frame to store the regression results
race_regression_table <- data.frame(term=character(0),
                                    variable = character(0),
                                    coefficient = numeric(0),
                                    std_error = numeric(0),
                                    p_value = numeric(0),
                                    ci_lower = numeric(0),
                                    ci_upper = numeric(0),
                                    stringsAsFactors = FALSE)

# Loop through the variables
for (var in variables) {
  # Fit linear regression model including interaction term
  model <- lm(paste(var, "~ wave + new_race + wave*new_race", sep = ""), data = data_f)
  
  # Extract model coefficients, standard errors, and p-values
  model_coefs <- coef(summary(model))
  coefficients <- model_coefs[, "Estimate"]
  std_errors <- model_coefs[, "Std. Error"]
  p_values <- model_coefs[, "Pr(>|t|)"]
  
  # Calculate confidence intervals for each coefficient
  cis <- confint(model, level = 0.95)
  ci_lower <- cis[, 1]
  ci_upper <- cis[, 2]
  
  # Create a data frame with the regression results for the current variable
  var_results <- data.frame(term = rownames(model_coefs),
                            variable = var,
                            coefficient = coefficients,
                            std_error = std_errors,
                            p_value = p_values,
                            ci_lower = ci_lower,
                            ci_upper = ci_upper)
  
  # Append the results to the regression table
  race_regression_table <- rbind(race_regression_table, var_results)
}


all_regressions<- rbind(gender_regression_table, race_regression_table)
# Write the regression table to a CSV file
#This creates Table 4
output_file <- "all_regression_table_results.csv"
write.csv(all_regressions, file = output_file, row.names = FALSE)

# Print a message indicating the file was saved
cat("Regression table saved as", output_file, "\n")


#### test all demographic variables for effect while controlling for gender #####
#H3b


demographic_variables<-c( "gender", "new_race", "education",
                          "Position", "Experience", "age" , "agecat" )

data<- read.csv( "data_correlations_grouped.csv", sep="," ) %>% 
  filter(wave %in% c("1", "2")) %>% #find only paired responses
  group_by(response_id) %>%
  filter(all(c("1", "2") %in% wave)) %>%
  ungroup() %>% 
  dplyr::select(response_id, wave, 
                all_of(demographic_variables),
                all_of(variables)) %>% 
  drop_na(all_of(variables))

demo_data<- data %>%  dplyr::select(response_id, all_of(demographic_variables)) %>% 
  group_by(response_id) %>%
  dplyr::slice(-2) %>%
  ungroup()

data$response_id<-factor(data$response_id)


data_pivot <- data %>%
  pivot_wider(
    id_cols = c(response_id),
    names_from = wave,
    values_from = all_of(variables))

data_scores <-  data_pivot %>%
  mutate(across(ends_with("_2"), ~ . - get(sub("_2$", "_1", cur_column())), .names = "{.col}_change")) %>% 
  select(response_id, ends_with("_change"))
demo_data$response_id<-factor(demo_data$response_id)
data_joined<- demo_data %>% 
  inner_join(data_scores, by="response_id") 

unique(data_joined$new_race)

data_clean <- data_joined %>%
  filter(gender!="") %>% 
  filter(new_race!="") %>% 
  group_by(gender, new_race, education, Position, Experience) %>% 
  filter(n() >2) %>%
  ungroup() %>% 
  drop_na(Q1_Q2_a_2_change) 


#recode demo data to be in smaller bins
data_clean <- data_clean %>%
  mutate(education_group = recode(education, #binary between bach and grad
                                  "Some college or associate‚Äôs degree" = "Some college or Associate's",
                                  "Bachelor‚Äôs degree" ="Bachelor"),
         position_group = recode(Position, 
                                 "Environmental Scientist" = "Scientist",#2, #
                                 "Senior Environmental Scientist" ="Senior scientist",#3, # 
                                 "Scientific Aide" = "Scientific Aide"),#1), #
         # "Other (please specify):" ="Other"),
         tenure_group = recode(Experience, # this is tenure ! eg compare seasonal/new vs people whove bee there a long time 
                               ">10 years" = "Senior",
                               "2-5 years" = "Mid",
                               "1-2 years" = "Junior",
                               "6 months to a year" = "Junior",
                               "Less than 6 months" =  "Junior")) %>% 
  filter(position_group!="Other") 

cor(data_clean$tenure_group, data_clean$position_group)
# Initialize a list to store the model results



#run models




data_clean$position_group<-factor(data_clean$position_group)
data_clean$tenure_group
# Create an empty list to store model results
model_results <- list()


# Create an empty list to store plot objects
plots <- list()
custom_labels<- c("Tenure", "Postition[Senior scientist]", "Position[Senior scientist]", "Position[Scientist]", "Race[white]", "Gender[woman]", "Age", "Intercept")
# Loop through each change score column
for (col in grep("_change$", colnames(data_clean), value = TRUE)) {
  # Create the formula for the linear regression model
  formula <- paste(col, "~ gender + new_race+position_group+ agecat", sep = "")
  
  # Fit the linear regression model
  model <- lm(formula, data = data_clean)
  
  # Store the model results in the list
  model_results[[col]] <- summary(model)
  
  # Create the plot using ggplot2
  plot_data <- broom::tidy(model)
  
  C <- ggplot(plot_data, aes(x = estimate, y = term, color = term)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    geom_point(size = 3, alpha = 0.8, position = position_dodge(width = 0.6)) +
    geom_errorbarh(aes(xmin = estimate - std.error, xmax = estimate + std.error),
                   position = position_dodge(width = 0.6), height = 0.01) +
    geom_text(aes(label = ifelse(p.value < 0.001, "***",
                                 ifelse(p.value < 0.01, "**",
                                        ifelse(p.value < 0.05, "*", "")))),
              position = position_dodge(width = 0.6), vjust = -0.3, fontface = "bold",
              size = 4, show.legend = FALSE) +
    labs(x = "Estimate", y = "Variable", color = "", title = paste( col)) +
    #scale_y_discrete(labels = custom_labels) +
    theme_classic() +
    theme(legend.position = "none",
          text = element_text(size = 15),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 14),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14))
  
  # Store the plot object in the list
  plots[[col]] <- C
}

# Combine all plots into one big plot
big_plot <- patchwork::wrap_plots(plots, ncol=5)

# Display the big plot
print(big_plot)

ggsave("grouped_model_coefficients_demographics.jpg", dpi=300, height=10, width=28)


# Create an empty dataframe to store the regression results
regression_results <- data.frame(
  Variable = character(),
  Estimate = numeric(),
  Std.Error = numeric(),
  p_value = numeric(),
  CI_Lower = numeric(),
  CI_Upper = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each change score column
for (col in grep("_change$", colnames(data_clean), value = TRUE)) {
  # Create the formula for the linear regression model
  formula <- paste(col, "~ gender + new_race + position_group + agecat", sep = "")
  
  # Fit the linear regression model
  model <- lm(formula, data = data_clean)
  
  # Extract the regression results
  results <- broom::tidy(model)
  
  # Extract the confidence intervals
  conf_int <- confint(model)
  
  # Combine the results and confidence intervals into one dataframe
  results <- cbind(results, conf_int)
  
  # Add the variable name to the dataframe
  results$Variable <- col
  
  # Append the results to the regression_results dataframe
  regression_results <- rbind(regression_results, results)
}

# Print the regression results
print(regression_results)

write.csv(regression_results, "RQ3_Hb_regression_results_demographics.csv")

