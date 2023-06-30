### corraltiona analyses 





rm(list=ls())

library(dplyr)
library(likert)
library(ggplot2)
library(glmm)
library(tidyverse)
library(tidyr)
library(viridis)
library(table1) 
library(scico)
library(forcats)
library(ggpubr)
library(devtools)
library(PairedData)
library(pwr) 
library(cowplot)
require(dplyr)
library(corrplot)


##FIRST, upload all data #####

setwd("/Users/melissacronin/Desktop/cdfw_data_new/")
data<- read.csv( "fullsurvfinal_deidentified.csv", sep="," ) 
#%>%  filter(wave==3) %>%  filter(t_c=="0")


variables <- c("Q1_a", "Q1_b", "Q1_c", "Q1_d", "Q1_e", #knowledge and self-efficacy 
               "Q2_a", "Q2_b", "Q2_c", "Q2_d", "Q2_e",
              "Q5_a", "Q5_b", # self reported behavior
                "Q6_a", "Q6_b", # self reported behavior
               "Q7_a", "Q7_b", "Q7_c", "Q7_d", "Q7_e", # self reported behavior
               "Q8_a", "Q8_b", "Q8_c", "Q8_d", "Q8_e",
               "Q10", # self reported behavior
               "Q11_a", "Q11_b", "Q11_c", "Q11_d", # behavioral intention
               "Q12_a", "Q12_b", "Q12_c", "Q12_d")

df <- data[, variables]
df <- df[complete.cases(df), ] 
  
corr <- cor(df)
# set the size of the graphics device
pdf("correlation_plot_all.pdf", width = 10, height = 10)

corrplot(corr, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, 
         tl.offset = 1, number.cex = 0.8)
dev.off()

# calculate the correlation coefficients

library(psych)

# Create a matrix of data with the variables
data_matrix <- as.matrix(data[, c("Q1_a", "Q2_a")])

# Calculate Cronbach's alpha
cronbach_alpha <- alpha(data_matrix)$total$raw_alpha

# Print the result
print(cronbach_alpha)

# convert the correlation matrix to a data frame
corr_df <- as.data.frame(corr)

# print the correlation coefficients
print(corr_df)

#based on these, we will group harassment and assualt questions

data$Q1_Q2_a <- rowMeans(data[,c("Q1_a", "Q2_a")])
data$Q1_Q2_b <- rowMeans(data[,c("Q1_b", "Q2_b")])
data$Q1_Q2_c <- rowMeans(data[,c("Q1_c", "Q2_c")])
data$Q1_Q2_d <- rowMeans(data[,c("Q1_d", "Q2_d")])
data$Q1_Q2_e <- rowMeans(data[,c("Q1_e", "Q2_e")])

data$Q7_Q8_a <- rowMeans(data[,c("Q7_a", "Q8_a")])
data$Q7_Q8_b <- rowMeans(data[,c("Q7_b", "Q8_b")])
data$Q7_Q8_c <- rowMeans(data[,c("Q7_c", "Q8_c")])
data$Q7_Q8_d <- rowMeans(data[,c("Q7_d", "Q8_d")])
data$Q7_Q8_e <- rowMeans(data[,c("Q7_e", "Q8_e")])

data$Q11_Q12_a <- rowMeans(data[,c("Q11_a", "Q12_a")])
data$Q11_Q12_b <- rowMeans(data[,c("Q11_b", "Q12_b")])
data$Q11_Q12_c <- rowMeans(data[,c("Q11_c", "Q12_c")])
data$Q11_Q12_d <- rowMeans(data[,c("Q11_d", "Q12_d")])

write.csv(data, "data_correlations_grouped.csv")
