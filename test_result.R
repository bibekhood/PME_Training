setwd('D:/OneDrive/M&E/Field Programs/PME Training')

if(!require(tidyverse)) install.packages('tidyverse')
if(!require(rstatix)) install.packages('rstatix')
if(!require(readxl)) install.packages('readxl')
if(!require(ggplot2)) install.packages('ggplot2')
if(!require(reshape2)) install.packages('reshape2')
if(!require(dplyr)) install.packages('dplyr')

library(tidyverse)
library(rstatix)
library(readxl)
library(ggplot2)
library(reshape2)
library(dplyr)

data = read_excel('test_results.xlsx', sheet='bhairahawa', na = c("NA", "na", "-", ''))

data$post_test[is.na(data$post_test)] = mean(data$post_test, na.rm = TRUE)
data$pre_test[is.na(data$post_test)] = mean(data$pre_test, na.rm = TRUE)
pre_test_mean = mean(data$pre_test)
post_test_mean = mean(data$post_test)


###Reshape data for plotting

data_long = melt(data = data,
                           id.vars =c('sn', 'venue', 'participant', 'province'),
                           variable.name = 'test',
                           value.name = 'score')
data_long$score = as.numeric(as.character(data_long$score))

###ggplot theme
mytheme = theme_minimal()+
  theme(
  plot.title = element_text(color = 'navy', size = 24, hjust = 0.5, face ='bold'),
  legend.title = element_blank(),
  axis.title = element_text(face = 'bold', color = 'darkblue', size = 18),
  axis.text = element_text(face = 'bold', color ='black', size = 12),
  legend.text = element_text(face = 'bold', size = 18),
  legend.key.height = unit(1, 'cm'), legend.key.width = unit(1, 'cm'),
  legend.key.spacing.y = unit(0.5, 'cm'),
  panel.grid = element_line (color ='grey90'),
  strip.text = element_text(size = 18, color = 'navy', face ='bold')
)

###density plot
ggplot(data_long, aes(x = score, fill = test)) +
  geom_density(alpha = 0.5) +
  labs(title = "Comparision of Test Scores", x = "Score", y = "Density") +
  theme_minimal() +
  scale_fill_discrete(name = "Test Type", labels = c("Pre-test", "Post-test"))+
  geom_vline(aes(xintercept = pre_test_mean), color = "blue", linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = post_test_mean), color = "orangered", linetype = "dashed", linewidth = 1) +
  scale_x_continuous(breaks =seq(30, 100, 10))+
  #facet_wrap(~venue)+
  mytheme

# Calculate differences
data = data%>%
  mutate(diff = post_test - pre_test,
         color = ifelse(diff >0, 'positive', 'negative'))
# Difference plot 
ggplot(data, aes(x = sn, y = diff, fill = color)) +
  geom_bar(stat = "identity", color ='black', size =0.3)+
  #geom_text(aes(label = round(diff, 2)), vjust = -0.5, color = "black") +
  labs(title = "Individual Score Gain (Post-test - Pre-test)", x = "Participant", y = "Score Difference")+
  theme_minimal() +
  scale_fill_manual (values = c('positive' = 'lightgreen','negative'= 'red'))+
  #facet_wrap(~venue)+
  scale_x_continuous(breaks = seq(0, 13, 13))+
  mytheme+
  theme(legend.position ='none')

###Box plot
ggplot(data_long, aes(x =test, y =score, fill = test))+
  geom_boxplot(outlier.shape ='X', outlier.size = 4, alpha =0.6, outlier.color ='blue')+
  geom_jitter(width = 0.3, size = 8, alpha = 0.5, color = "red")+
  labs(title = 'Score Comparision', x= '', y= 'Score')+
  scale_y_continuous(breaks =seq(30, 100, 10))+
  scale_x_discrete(labels=c('pre_test'= 'Pre-test', 'post_test' ="Post-test"))+
  #facet_wrap(~province)+
  mytheme+
  theme(legend.position = 'none',
        axis.text.x = element_text(color = 'navy', size =18))

###Boxplot with basic r graphics
boxplot(score~test, data =data_long,
         main ='Box Plot', 
         xlab ='',
         ylab ='Score')

###Boxplot mannual style
ggplot(data_long, aes(x = test, y = score, fill = test)) +
  stat_summary(
    fun.data = function(y) {
      return(data.frame(
        y = median(y),
        ymin = min(y),
        lower = quantile(y, 0.25),
        middle = median(y),
        upper = quantile(y, 0.75),
        ymax = max(y) )) },
    geom = "boxplot",
    fill = c("pre_test" = "blue", "post_test" = "hotpink"),
    position = position_dodge(width = 0.8) ) +
  labs(title = 'Box Plot', x = '', y = 'Score') +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  scale_x_discrete(labels = c("pre_test" = "Pre-test", "post_test" = "Post-test")) +
  mytheme
        
# Group statistics by province
province_stats = data %>%
  group_by(venue) %>%
  summarize(
    n =n(),
    pre_test_mean = mean(pre_test),
    post_test_mean = mean(post_test))
print(province_stats)

#Paired t-test (One tail)
paired_t = t.test(data$post_test, data$pre_test, paired =TRUE, alternative ='greater')
print(paired_t)

wilcox_t = wilcox.test(data$post_test, data$pre_test, paired =TRUE, alternative ='greater') 
print(wilcox_t)
