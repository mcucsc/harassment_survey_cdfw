###Cronin M. 
#Testing the effectiveness of interactive training on sexual harassment and assault in field science
### multi-level models  
#this code uses data produced by the Correlation_analyses.R file. 




rm(list=ls())
# Load required libraries
library(stringr)
require(dplyr)
library(sf)
library(sp)
library(ggplot2)
library(reshape2)
library(readxl)
library(ordinal)
library(broom.mixed)
library(broom)
library(MuMIn)
library(rmeta)
library(gridExtra)
library(patchwork)
library(ggfortify)
library(forestplot)
library(tidyr)
library(sjPlot)
library(merTools)
library(effects)
library(psych)
library(lme4)
library(gridExtra)
library(grid)

#Load data
setwd("/Users/melissacronin/Desktop/Harassment work/cdfw_data/")

#please use the file below, which contains only participants in the treatement group with a survey at each
#time point. Answers for questions asked separately for harassment and assault have been correlated into a single variable.
model_data<-read.csv("treatment_group_data_correlated.csv", sep=",", header=T) %>%   
  drop_na(Q1_Q2_a, Q1_Q2_b, Q1_Q2_c, Q1_Q2_d, Q1_Q2_e) 

# Define the variables
efficacy_list <- c("Q1_Q2_a", "Q1_Q2_b", "Q1_Q2_c", "Q1_Q2_d", "Q1_Q2_e")
behavioral_intention <- c("Q11_Q12_a", "Q11_Q12_b", "Q11_Q12_c", "Q11_Q12_d")


###plot the models #####
# Create an empty list to store the plots
plot_list <- list()

# Loop through the variables. Do this separately for behavioral intention and self-efficacy. 
for (var in efficacy_list) {
  # Define the hierarchical model using lmer function for multi-level models
  formula <- paste(var, "~ as.factor(wave) + (1 | respondent_id)")
  model <- lmer(formula, data = model_data)
  
  # Calculate AIC and print model summary
  cat("Variable:", var, "\n")
  cat("AIC:", AIC(model), "\n")
  summary(model)
  
  # Get the fixed-effects coefficients
  fixed_effects <- fixef(model)
  
  # Generate new data with all wave levels
  new_data <- data.frame(wave = unique(model_data$wave))
  
  # Calculate predicted values
  new_data$predicted <- predict(model, newdata = new_data, re.form = NA)
  
  # Calculate standard errors of fixed effects
  se <- sqrt(diag(vcov(model, type = "conditional")))
  
  # Calculate confidence intervals
  z_value <- qnorm(0.975) # For a 95% confidence interval
  new_data$lower <- new_data$predicted - z_value * se
  new_data$upper <- new_data$predicted + z_value * se
  
  n <- length(unique(model_data$respondent_id))
  r_squared_value <- r.squaredGLMM(model)[1]
  
  # Plot the data
  plot <- ggplot() +
    geom_boxplot(data = model_data,alpha=0.3,  aes(x = as.factor(wave), y = .data[[var]],color = as.factor(wave))) +
    geom_point(data = model_data, aes(x = as.factor(wave), y = .data[[var]], color = as.factor(wave)), position = "jitter", alpha = 0.1) +
    geom_line(data = new_data, aes(x = wave, y = predicted), color = "darkgray", size = 1) +
    geom_ribbon(data = new_data, aes(x = wave, ymin = lower, ymax = upper), fill = "gray", alpha = 0.4) +
    #geom_point(data = model_data, aes(x = wave, y = Q1_a, color = as.factor(wave)), alpha = 0.2, size = 3) +
    scale_color_viridis_d(begin = 0.2, end = 0.7) +
    scale_fill_viridis_d(begin = 0.2, end = 0.7) +
    scale_x_discrete(labels=c("1", "2", "3"))+
    theme_classic() +
    theme(legend.position = "top") +
    labs(y = "", x= "", color = "") +
    # annotate("text", x = Inf, y = Inf, label = paste0("n = ", n),
    #                                                   #"\nR-squared = ", round(r_squared_value, 3)),
    #          hjust = 1.4, vjust = 1, size = 8) +
    # ggtitle(paste("Variable:", var))+
    theme(text=element_text(size=20))
  plot_list[[var]] <- plot
  # Save the plot as an image
  #file_name <- paste0("plot_", var, ".png")
  # ggsave(file_name, plot, width = 10, height = 8)
  
  #cat("Plot saved as", file_name, "\n\n")
}


# Combine all the plots into a single pane
efficacy <- cowplot::plot_grid(plotlist = plot_list, nrow = 1)
intention <- cowplot::plot_grid(plotlist = plot_list, nrow = 1)

combined_plot<-efficacy/intention 
# Display the combined plot
combined_plot

#ggsave("mlm_combined.tiff", dpi=300, height=6, width=15)


#check model assumptions #####

#group by concept
variables <- c("Q1_Q2_a", #knowledge grouped
               "Q1_Q2_b", "Q1_Q2_c", "Q1_Q2_d", "Q1_Q2_e", #self-efficacy , grouped
               "Q11_Q12_a", "Q11_Q12_b", "Q11_Q12_c", "Q11_Q12_d" # behavioral intention
)

model_data<- drop_na(model_data, all_of(variables))

# Loop through the variables
for (var in variables) {
  # Subset the data for the current variable
  formula <- paste(var, "~ as.factor(wave) + (1 | respondent_id)")
  model <- lmer(formula, data = model_data)
  
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
  abline(smooth.spline(fitted(model), sqrt(abs(residuals(model)))), col = "red")
  
  
  # Residuals vs Leverage plot
  plot(hatvalues(model), residuals(model),
       xlab = "Leverage", ylab = "Residuals",
       main = paste0("Residuals vs Leverage - ", var))
  abline(h = 0, lty = "dashed")
  
}


#create nice tables of the results. Do separately for behavioral intention and self-efficacy. #####
models_list <- list()

# loop through the variables and create a model for each one
for (var in behavioral_intention) {
  model <- lmer(paste0(var, " ~ as.factor(wave) + (1 | respondent_id)"), data = model_data)
  models_list[[var]] <- model
}

# create a table of results for each model
results_list <- lapply(models_list, function(model){
  sjPlot::tab_model(model, 
                    show.re.var= TRUE, 
                    pred.labels =c("(Intercept)", "Wave"),
                    dv.labels= "Effects of Wave on Score")
})

# combine the tables into one large table
combined_results <- sjPlot::tab_model(models_list = models_list, tab_list = results_list, show.model.names = TRUE)
combined_results


###explore race and gender ####
#as an exploratory analysis, check the effect of race and gender on responses. This will not be used because of low sample size.
# Loop through the variables
for (var in behavioral_intention) {
  # Define the hierarchical model using lmer function for multi-level models
  formula <- paste(var, "~ as.factor(wave) + gender + new_race + (1 | respondent_id)")
  model <- lmer(formula, data = model_data)
  
  # Create table of results using sjPlot::tab_model()
  table <- tab_model(model)
  
  # Create the plot of fixed effects using plot_model() function
  plot <- plot_model(model, type = "est", show.values = TRUE, value.offset = 0.1, title = "Effects of Demographics on Score", colors = "Set1")
  
  # Print the plot
  print(plot)
}

