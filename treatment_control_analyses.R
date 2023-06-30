#Cronin M. 
#Testing the effectiveness of interactive training on sexual harassment and assault in field science
### Treatment-control analyses and figures
#please use the correlated data produced in the file Correlation_analyses.R to run this code. 

rm(list=ls())

library(dplyr)
library(likert)
library(ggplot2)
library(glmm)
library(patchwork)
library(tidyverse)
library(tidyr)
library(viridis)
library(table1) 
library(scico)
library(forcats)
library(ggpubr)
library(devtools)
library(PairedData)
library(broom)
library(cowplot)
require(dplyr)
library(gridExtra)


#call variables we want to test. These were created by the Correlation_analyses.R file.
variables <- c("Q1_Q2_a", #knowledge grouped
               "Q1_Q2_b",  "Q1_Q2_c", "Q1_Q2_d", "Q1_Q2_e", #self-efficacy , grouped
               "Q7_Q8_a", "Q7_Q8_b", "Q7_Q8_c", "Q7_Q8_d", "Q7_Q8_e", # self reported behavior
               "Q11_Q12_a", "Q11_Q12_b", "Q11_Q12_c", "Q11_Q12_d" # behavioral intention
)

## upload data #####

data<- read.csv( "data_correlations_grouped.csv", sep="," ) %>% 
  filter(wave=="1" |
           wave=="3") %>% 
  dplyr::select(RecordedDate, response_id, wave, t_c,all_of(variables))

#### data cleaning #####
# Convert date column to POSIXct format
data$RecordedDate <- as.POSIXct(data$RecordedDate, format = "%m/%d/%y %H:%M")

# filter only those who took Time 1 survey before a particular date
time1 <- data %>%
  filter(wave=="1") %>% 
  filter(as.POSIXct(RecordedDate, format="%m/%d/%y %H:%M") < as.POSIXct("2022-07-05", format="%Y-%m-%d")) %>%
  dplyr::select(response_id, t_c,  all_of(variables)) %>% 
  rename_with(~paste0(., "_t1"), all_of(variables))

#filter for Time 3 data 
time3 <- data %>%
  filter(wave=="3") %>% 
  dplyr::select(response_id, t_c,  all_of(variables)) %>% 
  dplyr::rename_with(~paste0(., "_t3"), all_of(variables))

# join the two data frames by response_id
joined_data <- inner_join(time1, time3, by = "response_id") %>% 
  dplyr::select(-t_c.x) %>% 
  dplyr::rename(treatment_group=t_c.y )
table(joined_data$treatment_group)

# filter out response_ids that are in Time 3 but not in Time 1
joined_data <- joined_data %>%
  semi_join(time1, by = "response_id")

#table of treatment (1) and control (0)
table<- joined_data %>% 
  group_by(treatment_group) %>% 
  dplyr::summarise(count=n())

#try first model without looping.
model <- lm(Q1_Q2_a_t3 ~ treatment_group + Q1_Q2_a_t1, data = joined_data)
summary(model)
table<-tidy(model)

#Check model assumptions
# Set up the plotting layout
par(mfrow = c(2, 2))

# Residual plot
plot(model, which = 1, main = "Residuals vs Fitted")

# Normal Q-Q plot
plot(model, which = 2, main = "Normal Q-Q")

# Scale-location plot
plot(model, which = 3, main = "Scale-Location")

# Residuals vs Leverage plot
plot(model, which = 5, main = "Residuals vs Leverage")

# Reset the plotting layout
par(mfrow = c(1, 1))

#Now, recreate that model but loop through all the variables

# Create an empty table to store results
results_table <- data.frame(variable = character(),
                            estimate = numeric(),
                            std_error = numeric(),
                            t_value = numeric(),
                            p_value = numeric(),
                            ci_lower = numeric(),
                            ci_upper = numeric(),
                            stringsAsFactors = FALSE)

# Loop over variables, fit the model, and store the results
for (var in variables) {
  formula <- paste0(var, "_t3 ~ treatment_group + ", var, "_t1")
  model <- lm(formula, data = joined_data)
  table <- broom::tidy(model)
  # Calculate confidence intervals
  ci <- confint(model, level = 0.95)
  
  results_table <- rbind(results_table, data.frame(variable = var,
                                                   estimate = table$estimate[2],
                                                   std_error = table$std.error[2],
                                                   t_value = table$statistic[2],
                                                   p_value = table$p.value[2],
                                                   ci_lower = ci[2, 1],
                                                   ci_upper = ci[2, 2],
                                                   stringsAsFactors = FALSE))
}


# Print the results table
print(results_table)


# Define the categories
knowledge <- c("Q1_Q2_a")
self_efficacy <- c("Q1_Q2_b", "Q1_Q2_c", "Q1_Q2_d", "Q1_Q2_e")
self_reported_behavior <- c( "Q7_Q8_a", "Q7_Q8_b", "Q7_Q8_c", "Q7_Q8_d","Q7_Q8_e")
behavioral_intention <- c("Q11_Q12_a", "Q11_Q12_b", "Q11_Q12_c", "Q11_Q12_d")

# Create a list of categories and their corresponding variables
categories <- list("Knowledge" = knowledge, "Self-efficacy" = self_efficacy, "Behavioral intention" = behavioral_intention, "Self-reported behavior" = self_reported_behavior)

# Create a list to store the plots
plot_list <- list()

# Loop through the categories
for (cat_name in names(categories)) {
  
  # Create a directory to store the plots for this category
  dir.create(cat_name, showWarnings = FALSE)
  
  # Subset the variables by category
  cat_vars <- categories[[cat_name]]
  
  # Create a list to store the plots for this category
  cat_plot_list <- list()
  
  # Loop through the variables in the category
  for (var in cat_vars) {
    # calculate p-value
    pval <- round(results_df$p_value[results_df$variable == var], 3)
    
    # check the significance level
    if (pval < 0.001) {
      significance <- "***"
    } else if (pval < 0.01) {
      significance <- "**"
    } else if (pval < 0.05) {
      significance <- "*"
    } else {
      significance <- ""
    }
    
    plot_data <- joined_data %>% 
      pivot_longer(cols = starts_with(var), 
                   names_to = "wave", 
                   values_to = "response") %>% 
      na.omit() # remove missing values
    
    # Calculate sample size (N) for each treatment group
    N_treatment <- sum(plot_data$treatment_group == "1")
    N_control <- sum(plot_data$treatment_group == "0")
    
    # Fit the linear regression model
    model <- lm(paste0("response ~ wave + treatment_group"), data = plot_data)
    
    # Extract the R-squared value
    r_squared <- round(summary(model)$r.squared, 3)
    
    # Calculate confidence intervals for coefficients
    ci <- confint(model, level = 0.95)
    
    # Extract coefficient estimates and their confidence intervals
    coef_estimates <- summary(model)$coef[, "Estimate"]
    coef_ci_lower <- ci[, 1]
    coef_ci_upper <- ci[, 2]
    
    plot <- ggplot(plot_data, aes(x = factor(wave), y = response, color = factor(treatment_group))) +
      
      geom_point(position = position_jitter(width = 0.2), alpha = 0.2) +
      geom_smooth(aes(group = treatment_group), method = "lm", se = TRUE) +
      labs(x = "", y = "") +
      theme_classic() +
      scale_x_discrete(labels = c("Time 1", "Time 3"))+
      theme(legend.position = "none") +
      scale_color_manual(values = c("#440154FF", "#21908CFF"), labels = c("Control", "Treatment")) +
      scale_y_continuous(limits = c(1, 7),  expand = c(NA, 0))+
      theme(text = element_text(size=20))+
      annotate("text", x = 1.4, y = 1.8, size = 10, 
               label = significance )
    
    # Append the plot to the list for this category
    cat_plot_list[[var]] <- plot
  }
  # Add the plots for this category to the overall plot list
  plot_list[[cat_name]] <- cat_plot_list
}

# Create an empty list to store the extracted plots
extracted_plots <- list()

# Extract all the plots from the plot_list
for (category_plots in plot_list) {
  extracted_plots <- c(extracted_plots, category_plots)
}

# Arrange the extracted plots using patchwork
combined_plot <- wrap_plots(extracted_plots, ncol = 5)

# Display the combined plot
print(combined_plot)

#ggsave("Fig_2.tiff", dpi=300, height=10, width=12)




# examine model assumptions ######

#first create long dataset 
long_data <- joined_data %>%
  pivot_longer(cols = starts_with("Q"), 
               names_to = c("variable", "wave"), 
               names_sep = "_t",
               values_to = "response") %>% 
  mutate(wave = ifelse(wave == "1", "Time 1", "Time 3"),
         question = str_sub(variable, end = -3))         

category <- c("Knowledge", # Q1_Q2_a
              "Self-efficacy", "Self-efficacy", "Self-efficacy", "Self-efficacy", # Q1_Q2_b to Q1_Q2_e
              "Self-reported behavior", "Self-reported behavior", "Self-reported behavior", "Self-reported behavior", "Self-reported behavior", # Q8_Q7a to Q8_Q7_e
              "Behavioral intention", "Behavioral intention", "Behavioral intention", "Behavioral intention") # Q11_Q12_a to Q11_Q12_d

# create a data frame with the variable names and categories
category_df <- data.frame(variable = variables, category = category)

# merge the category data with the long_data
long_data_final <- merge(long_data, category_df, by = "variable")


# Loop through the categories
for (var in variables) {
  # Subset the data for the current variable
  plot_data <- long_data_final[long_data_final$variable == var, ]
  
  # Fit the linear regression model
  model <- lm(response ~ wave + treatment_group, data = plot_data)
  
  # Create a new plot window for each variable
  plot.new()
  
  # Create a layout grid for the four assumption plots
  par(mfrow = c(2, 2))
  
  # Save the assumption plots as an image
  filename <- make.names(paste0("assumption_plots/", var, ".png"))
  png(filename)
  
  # Display the assumption plots
  plot(fitted(model), residuals(model),
       xlab = "Fitted values", ylab = "Residuals",
       main = paste0("Residuals vs Fitted - ", var))
  abline(h = 0, lty = "dashed")  # Add a horizontal dashed line at y = 0
  
  qqnorm(residuals(model),
         main = paste0("Normal Q-Q Plot - ", var))
  qqline(residuals(model))
  
  plot(sqrt(abs(residuals(model))) ~ fitted(model),
       xlab = "Fitted values",
       ylab = "Square root of absolute residuals",
       main = paste0("Scale-Location Plot - ", var))
  abline(smooth.spline(fitted(model), sqrt(abs(residuals(model)))), col = "red")
  
  plot(hatvalues(model), residuals(model),
       xlab = "Leverage", ylab = "Residuals",
       main = paste0("Residuals vs Leverage - ", var))
  abline(h = 0, lty = "dashed")  # Add a horizontal dashed line at y = 0
  
  # Reset the layout grid
  par(mfrow = c(2, 2))
  dev.off()
}


# Create a directory to save the images
dir.create("assumption_plots", showWarnings = FALSE)




