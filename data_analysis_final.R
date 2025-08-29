# Created: 2020-07-07 15:00:00
# Last modified: 2020-07-07 15:00:00

setwd("/Users/suchetachakravarty/Documents/projects/geode/")
rm(list=ls())

# Load the required libraries
library(data.table)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(rstatix)
library(gridExtra)
library(Hmisc)
library(rstan)
library(zoo) 
library(latex2exp)
library(afex)
library(emmeans)
library(minpack.lm)
library(tidyverse)

library(glmnet)
library(lme4)
library(broom.mixed)

# load the data
load("data/preprocessed_data.RData")

# create a binary group variable
alldata[, binary_group := fifelse(grepl("ASD", group, ignore.case = TRUE), "ASD", "TD")]

# create a verbal group variable
alldata[, group_verbal := fcase(
  is.na(vinabcstd), "flag",
  binary_group == "ASD" & vinabcstd >= 75, "ASD",
  binary_group == "TD" & vinabcstd >= 75, "TD",
  binary_group == "ASD" & vinabcstd < 75, "ASD (low verbal)",
  binary_group == "TD" & vinabcstd < 75, "flag_low_td",
  default = "Uncategorized"
)]

# create a srs normal group variable
alldata[, srs_normal := fifelse(
  (binary_group == "ASD" & srs2total > 20) | (binary_group == "TD" & srs2total < 120),1,0)]
# alldata[, table(srs_normal)]

# create a above criteria group variable
alldata[, above_crit := fifelse(
  (nTrials == 200 & perf > .616),1,0)]

# data summary
summary <- unique(alldata[, .(username, subID, game_version, source, binary_group, group_verbal, srs_normal, gender, race, age, ethnicity, vinabcstd, srs2total, bistotal,aasp_low_reg_raw,
                               aasp_sen_seek_raw,aasp_sen_ses_raw,aasp_sen_avoid_raw, perf, trainperf, nTrials, above_crit)])


###### table for subject numbers ####

summary[srs_normal == 1 & game_version=="ft", table(binary_group)]
summary[srs_normal == 1 & game_version=="ft" & group_verbal!="flag", table(binary_group)]
summary[above_crit==1 & srs_normal == 1 & game_version=="ft" & group_verbal!="flag" & !is.na(bistotal) & !is.na(aasp_low_reg_raw) & !is.na(aasp_sen_seek_raw) & !is.na(aasp_sen_ses_raw) & !is.na(aasp_sen_avoid_raw), table(binary_group)]


############################################################################################################
# Data for Supplemenatry Figure 1
# total number of participants in each binary group for game_version = "ft"
summary[game_version == "ft", table(binary_group)] #+1 ASD subject whose game data we could not track

# total number of participants in each binary group for game_version = "ft" and srs_normal = 1
summary[game_version == "ft" & srs_normal == 1, table(binary_group)]

# total number of participants in each binary group for game_version = "ft" and srs_normal = 1 and above_crit = 1
summary[game_version == "ft" & srs_normal == 1 & above_crit == 1, table(binary_group)]

# number of participants in binary_group = "ASD" for game_version = "ft" and srs_normal = 1 and above_crit = 0
summary[game_version == "ft" & srs_normal == 1 & above_crit == 0, table(binary_group)]

# subIDs of the ASD participants who did not meet the criteria
id1 = summary[game_version == "ft" & above_crit == 0 & binary_group == "ASD", .(subID)]

# subIDs ofparticipants who played game again
id2 = summary[game_version != "ft", .(subID)]

# remove the "_2" and "_3" at the end of the subID for id2 only where it is present
id2[, subID := gsub("_2$|_3$", "", subID)]

# common subIDs between id1 and id2
common_ids = id1[subID %in% id2$subID, .(subID)]


# subIDs of the ASD participants who played game_version = "ft-2" and met the criteria
id3 = summary1[game_version == "ft-2" & srs_normal == 1 & above_crit == 1 & binary_group == "ASD", .(subID)]


# clear id1, id2, common_ids
rm(id1,id2,common_ids)

############################################################################################################

# Supplemenatry Figure 2
# only include subjects with game_version = "ft" and srs_normal = 1
df = summary[game_version == "ft" & srs_normal==1]

# gender distribution
# If gender = "M", Male, if "F", Female, otherwise call it Other/Unknown
df[,gender := fcase(gender == "M", "Male",gender == "F","Female", default = "Other/Unknown")]

p1<-
ggplot(df,aes(x=gender, fill=binary_group, color=binary_group)) + 
geom_bar(position="dodge") + 
theme_bw()+labs(fill="",color="",xlab="",
# ylab="Frequency"
)+
        theme(legend.position = c(.7,.8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 1, hjust=1),
        legend.background=element_blank())


# age distribution
p2 <-
  ggplot(df,aes(x=age,fill=binary_group,color=binary_group))+
  geom_bar(width=.5,position=position_dodge(width = .5),alpha=.7)+
  theme_bw()+
  scale_x_continuous(breaks = c(11, 12, 13,14,15,16,17),limits = c(10,18))+
  # ylab("Frequency")+
  xlab("Age (y)")+
  labs(fill="",color="")+  
  theme(legend.position = "None",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background=element_blank())
  
  # race distribution
  unique(df$race)
  df[,race := fcase(race=="LA6156-9","Asian", 
  race=="LA10610-6","Black or African American",
  race=="LA10611-4","Native Hawaiian or Other Pacific Islander",
  race=="LA4457-3","White",
  race=="LA4489-6","Native Hawaiian or Other Pacific Islander",default="Unknown")]

     df$race<-factor(df$race,levels = c("White","Black or African American", "Asian",
                                    "Native Hawaiian or Other Pacific Islander","Unknown"))   
 p3 <- 
    ggplot(df,aes(x=race,fill=binary_group,color=binary_group))+
    geom_bar(width=.5,position=position_dodge(width = .5),alpha=.7)+
    theme_bw()+
    # ylab("Frequency")+
    xlab("")+
    labs(fill="",color="")+
    scale_x_discrete(labels = c("White","Black/African\nAmerican","Asian",
                                "Native Hawaiian/\nPacific Islander","Unknown"))+
    theme(legend.position = "None",
          panel.grid.major = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 1, hjust=1),
          panel.grid.minor = element_blank())

# ethnicity distribution
unique(df$ethnicity)
df[,ethnicity := fcase(ethnicity=="2135-2","Hispanic or Latino",
ethnicity=="2186-5","Not Hispanic or Latino",
default="Unknown")]
df$ethnicity<-factor(df$ethnicity,levels = c("Hispanic or Latino","Not Hispanic or Latino","Unknown"))
p4 <-
    ggplot(df,aes(x=ethnicity,fill=binary_group,color=binary_group))+
    geom_bar(width=.5,position=position_dodge(width = .5),alpha=.7)+
    theme_bw()+
    # ylab("Frequency")+
    xlab("")+
    labs(fill="",color="")+
    scale_x_discrete(labels = c("Hispanic/Latino","Not Hispanic/Latino","Unknown"))+
    theme(legend.position = "None",
          panel.grid.major = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 1, hjust=1),
          panel.grid.minor = element_blank())


# srs2total distribution
p5 <-
    ggplot(df,
               aes(x=srs2total))+
    geom_histogram(aes(#y = after_stat(density),
      fill=binary_group,
      color=binary_group),
      position="identity", 
      alpha=.5)+
    theme_bw()+
    xlab("SRS-2")+
    geom_vline(xintercept=70,linetype="dashed")+
    # ylab("Frequency")+
    labs(fill="",color="")+
    theme(legend.position = "None",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

# bistotal distribution
p6 <-
    ggplot(df,
               aes(x=bistotal))+
    geom_histogram(aes(#y = after_stat(density),
      fill=binary_group,
      color=binary_group),
      position="identity", 
      alpha=.5)+
    theme_bw()+
    xlab("BIS")+
    # ylab("Frequency")+
    labs(fill="",color="")+
    theme(legend.position = "None",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

# reverse scale vinabcstd distribution
df[,vinabcstd := 140 - vinabcstd]
# vinabcstd distribution
p7 <-
    ggplot(df,
               aes(x=vinabcstd))+
    geom_histogram(aes(#y = after_stat(density),
      fill=binary_group,
      color=binary_group),
      position="identity", 
      alpha=.5)+
    theme_bw()+
    xlab("VABS-3")+
    # ylab("Frequency")+
    labs(fill="",color="")+
    theme(legend.position = "None",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

# aasp_low_reg_raw distribution
p8 <-
    ggplot(df,
               aes(x=aasp_low_reg_raw))+
    geom_histogram(aes(#y = after_stat(density),
      fill=binary_group,
      color=binary_group),
      position="identity", 
      alpha=.5)+
    theme_bw()+
    xlab("AASP\n(Low Registration)")+
    # ylab("Frequency")+
    labs(fill="",color="")+
    theme(legend.position = "None",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

# aasp_sen_avoid_raw distribution
p9 <-
    ggplot(df,
               aes(x=aasp_sen_avoid_raw))+
    geom_histogram(aes(#y = after_stat(density),
      fill=binary_group,
      color=binary_group),
      position="identity", 
      alpha=.5)+
    theme_bw()+
    xlab("AASP\n(Sensory Avoiding)")+
    # ylab("Frequency")+
    labs(fill="",color="")+
    theme(legend.position = "None",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

# reverse scale aasp_sen_seek_raw distribution
df[,aasp_sen_seek_raw := 75 - aasp_sen_seek_raw]
p10 <- 
    ggplot(df,
               aes(x=aasp_sen_seek_raw))+
    geom_histogram(aes(#y = after_stat(density),
      fill=binary_group,
      color=binary_group),
      position="identity", 
      alpha=.5)+
    theme_bw()+
    xlab("AASP\n(Sensory Seeking)")+
    # ylab("Frequency")+
    labs(fill="",color="")+
    theme(legend.position = "None",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())


# aasp_sen_ses_raw distribution
p11 <-
    ggplot(df,
               aes(x=aasp_sen_ses_raw))+
    geom_histogram(aes(#y = after_stat(density),
      fill=binary_group,
      color=binary_group),
      position="identity", 
      alpha=.5)+
    theme_bw()+
    xlab("AASP\n(Sensory Sensitivity)")+
    # ylab("Frequency")+
    labs(fill="",color="")+
    theme(legend.position = "None",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())


# combine all the plots into publication ready format, add labels and save the plot
ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,ncol=4,nrow=3,common.legend = TRUE,align = "v",
labels = c("A","B","C","D","E","F","G","H","I","J","K")) %>% ggexport(filename = "results/figures/geodems/demographics_v2.pdf")

# remove plot objects
rm(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11)

############################################################################################################
# Main Figure 2

# distribution of vinabcstc for group_verbal (remove flagged subjects) and for srs_normal=1 and game_version = "ft"
df = summary[game_version == "ft" & srs_normal==1 & group_verbal != "flag" & group_verbal != "flag_low_td"]

df$group_verbal <- factor(df$group_verbal, levels = c("TD","ASD","ASD (low verbal)"),ordered = TRUE)


# plot distribution of vinabcstd for group_verbal
 p1 <-
  ggplot(df,
         aes(x=vinabcstd))+
    geom_histogram(aes(
      fill=group_verbal,color=group_verbal),
      position="identity", 
      alpha=.5)+
    theme_bw()+
    scale_fill_manual(values = c("blue","orange","red"))+
    scale_color_manual(values = c("blue","orange","red"))+
    xlab("VABS-3")+
    ylab("Frequency")+
    labs(fill="",color="")+
    theme(legend.position = "None",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.key = element_rect(colour = "transparent"),
          legend.text=element_text(size=4))


# scatter plot perf by vinabcstd 
p2 <-
  ggplot(df,aes(x=vinabcstd,y=perf*100))+
    geom_point(aes(color=group_verbal))+
    ylab("%Correct")+
    xlab("VABS-3")+
    scale_color_manual(values = c("blue","orange","red"))+
    geom_hline(yintercept = 62,linetype="dashed",size=.5)+
    theme_bw()+
    labs(color="")+
    theme(legend.position = "None",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

# plot percentage of subjects above the criteria for each group_verbal, show the percentage on the plot
df = df[, .(above_crit = mean(above_crit)), by = .(group_verbal)]
df$group_verbal <- factor(df$group_verbal, levels = c("TD","ASD","ASD (low verbal)"),ordered = TRUE)
p3 <-
  ggplot(df,
         aes(x=group_verbal,y=above_crit))+
    geom_bar(stat="identity",aes(fill=group_verbal),position="dodge")+
    geom_text(aes(label = paste0(round(above_crit*100,1),"%")),position = position_stack(vjust = 0.5), col = "white")+
    theme_bw()+
    scale_fill_manual(values = c("blue","orange","red"))+
    xlab("")+
    ylab("%above criteria")+
    labs(fill="")+
    theme(legend.position = "None",
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.key = element_rect(colour = "transparent"),
          legend.text=element_text(size=4))


#  score for trials 1 through 10 for each group_verbal from alldata, for game_version = "ft" and srs_normal = 1 and above_crit = 1
df = alldata[game_version == "ft" & srs_normal == 1 & above_crit == 1 & group_verbal != "flag" & group_verbal != "flag_low_td"]
df = df[,c("subID","group_verbal","trial","score")]
df = df[trial %in% 1:10]
# plot the average scores and se for each group_verbal
df = df[, .(mean_score = mean(score), se = sd(score)/sqrt(.N)), by = .(group_verbal,trial)]
df$group_verbal <- factor(df$group_verbal, levels = c("TD","ASD","ASD (low verbal)"),ordered = TRUE)

pairwise_test <- df %>%
  pairwise_t_test(mean_score ~ group_verbal, p.adjust.method = "bonferroni")
pairwise_test <- pairwise_test %>%
  add_xy_position(x = "group_verbal")

fit_learning_curve <- function(data) {
  nls(mean_score ~ pinf - (pinf - p0) * exp(-alpha * trial),
      data = data,
      start = list(pinf = 1, p0 = 0.5, alpha = 0.1),
      control = nls.control(maxiter = 100))
}
td_fit <- df %>% 
  filter(group_verbal == "TD") %>% 
  fit_learning_curve()

asd_fit <- df %>% 
  filter(group_verbal == "ASD") %>% 
  fit_learning_curve()

asd_low_verbal_fit <- df %>% 
  filter(group_verbal == "ASD (low verbal)") %>% 
  fit_learning_curve()

new_data <- data.frame(trial = seq(1, 10, 0.1))

td_pred <- data.frame(
  trial = new_data$trial,
  mean_score = predict(td_fit, newdata = new_data),
  group_verbal = "TD"
)

asd_pred <- data.frame(
  trial = new_data$trial,
  mean_score = predict(asd_fit, newdata = new_data),
  group_verbal = "ASD"
)

asd_low_verbal_pred <- data.frame(
  trial = new_data$trial,
  mean_score = predict(asd_low_verbal_fit, newdata = new_data),
  group_verbal = "ASD (low verbal)"
)
pred_data <- rbind(td_pred, asd_pred)
pred_data <- rbind(pred_data, asd_low_verbal_pred)

p4 <- 
ggplot() +
  geom_point(data = df, aes(x = trial, y = 100*mean_score, color = group_verbal)) +
  geom_errorbar(data = df, aes(x = trial, ymin = 100*(mean_score - se), ymax = 100*(mean_score + se), color = group_verbal), width = 0.2) +
  geom_line(data = pred_data, aes(x = trial, y = 100*mean_score, color = group_verbal)) +
  # stat_pvalue_manual(pairwise_test, label = "p.adj.signif", 
  #                    y.position = max(df$mean_score*100) + 0.1) +
  scale_x_continuous(breaks = 1:10) +
  labs(title = "Level 1",
       x = "Trial",
       y = "%Correct",
       color = "Group") +
  theme_bw() +
  ylim(60,105)+
  scale_color_manual(values = c("TD" = "blue", "ASD" = "orange", "ASD (low verbal)" = "red"))+
  theme(legend.position = "None",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key = element_rect(colour = "transparent"),
        legend.text=element_text(size=4))


# rolling average (trials 11 through 200) for each group_verbal
df = alldata[game_version == "ft" & srs_normal == 1 & above_crit == 1 & group_verbal != "flag" & group_verbal != "flag_low_td"]
df = df[,c("subID","group_verbal","trial","score")]
df = df[trial %in% 11:200]
window_size <- 50

df_rolling <- df %>%
  group_by(subID, group_verbal) %>%
  arrange(trial) %>%
  mutate(rolling_avg = rollmean(score, k = window_size, fill = NA, align = "left")) %>% ungroup()

df_summary <- df_rolling %>%
  group_by(group_verbal, trial) %>%
  summarise(
    avg_score = mean(rolling_avg, na.rm = TRUE),
    se = sd(rolling_avg, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# fit_learning_curve <- function(data) {
#   nls(avg_score ~ pinf - (pinf - p0) * exp(-alpha * trial),
#       data = data,
#       start = list(pinf = 1, p0 = 0.5, alpha = 0.1),
#       control = nls.control(maxiter = 100))
# }
# td_fit <- df_summary %>% 
#   filter(group_verbal == "TD") %>% 
#   fit_learning_curve()

# asd_fit <- df_summary %>%
#   filter(group_verbal == "ASD") %>% 
#   fit_learning_curve()

# asd_low_verbal_fit <- df_summary %>%
#   filter(group_verbal == "ASD (low verbal)") %>% 
#   fit_learning_curve()


p5 <- 
ggplot(df_summary, aes(x = trial, y = 100*avg_score, color = group_verbal)) +
  # geom_point() +
  geom_smooth(method = "loess",span=1,se=F,size=.5) +
  geom_ribbon(aes(ymin = 100*(avg_score - se), ymax = 100*(avg_score + se), fill = group_verbal), alpha = .4) +
  labs(title = "Rolling Average Score by Group (Trials 11-200)",
       x = "Trial",
       y = "Rolling Average Score",
       color = "Group",
       fill = "Group") +
  theme_bw() +
  ylim(75,90)+
 labs(title = "Level 2-10",
       x = "Trial",
       y = "%Correct",
       color = "Group") +
  scale_x_continuous(breaks = seq(10, 150, 20),limits = c(10,150)) +
  scale_color_manual(values = c("ASD" = "orange", "ASD (low verbal)" = "red", "TD" = "blue")) +
  scale_fill_manual(values = c("ASD" = "orange", "ASD (low verbal)" = "red", "TD" = "blue"))+
    theme(legend.position = "None",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key = element_rect(colour = "transparent"),
        legend.text=element_text(size=4))

# psychometric curve (unsigned)
df = alldata[game_version == "ft" & srs_normal == 1 & above_crit == 1 & group_verbal != "flag" & group_verbal != "flag_low_td"]
df = df[,c("subID","group_verbal","trial","score","dflash")]
df = df[trial %in% 101:200 & dflash != 0]
df$dflash <- abs(df$dflash)


df_subject_avg <- df %>%
  group_by(subID, group_verbal, dflash) %>%
  summarise(avg_score = mean(score, na.rm = TRUE), .groups = "drop")

df_long <- df_subject_avg %>%
  select(subID, group_verbal, dflash, avg_score) %>%
  mutate(dflash = factor(dflash,levels = c(1:10)))  # Convert dflash to a factor

aov_model <- aov_ez(
  id = "subID",
  dv = "avg_score",
  data = df_long,
  between = "group_verbal",
  within = "dflash",
  type = 3
)
print(aov_model)
posthoc <- emmeans(aov_model, ~ group_verbal | dflash)
group_comparisons <- pairs(posthoc)
print(group_comparisons)
significant_dflash <- group_comparisons %>%
  as.data.frame() %>%
  filter(p.value < 0.05) %>%
  pull(dflash) %>%
  unique()

  df_group_avg <- df_subject_avg %>%
  group_by(group_verbal, dflash) %>%
  summarise(
    mean_score = mean(avg_score, na.rm = TRUE),
    se = sd(avg_score, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )


p6<-
ggplot(df_group_avg, aes(x = dflash, y = mean_score, color = group_verbal)) +
  geom_point(size=.5)+
  geom_smooth(se=F,method="glm",method.args = list(family = "binomial"),size=.5) +
  # geom_ribbon(aes(ymin = mean_score - se, ymax = mean_score + se, fill = group_verbal), alpha = 0.2) +
  geom_errorbar(aes(ymin = mean_score - se, ymax = mean_score + se), alpha = 0.8, size=.5,width=.5) +
  labs(title = "Trial 101-200",
       x = "Cue difference",
       y = "Accuracy",
       color = "Group",
       fill = "Group") +
  ylim(.65,1.05)+
  theme_bw() +
  scale_x_continuous(breaks = seq(1, 10),limits = c(.5,10.5)) +
  scale_color_manual(values = c("ASD" = "orange", "ASD (low verbal)" = "red", "TD" = "blue")) +
  scale_fill_manual(values = c("ASD" = "orange", "ASD (low verbal)" = "red", "TD" = "blue"))+
theme(legend.position = "None",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


# p <- p + geom_point(data = df_group_avg %>% 
#                       filter(dflash %in% significant_dflash) %>% 
#                       group_by(dflash) %>% 
#                       summarise(max_y = max(mean_score + se)),
#                     aes(x = dflash, y = max_y + 0.05),
#                     shape = 8, size = 3, color = "black")


ggarrange(p1,p2,p3,p4,p5,p6,ncol=3,nrow=3,common.legend = TRUE,align = "v",
labels = c("A","B","C","D","E","F")) %>% ggexport(filename = "results/figures/geodems/descriptives_v2.pdf")



# remove variables
rm(df,df_group_avg,df_long,df_rolling,df_subject_avg,group_comparisons,pairwise_test,posthoc,significant_dflash,td_fit,asd_fit,asd_low_verbal_fit)
rm(td_pred,asd_pred,asd_low_verbal_pred,df_summary,new_data,pred_data)
rm(aov_model,window_size,fit_learning_curve)
# remove plot objects
rm(p1,p2,p3,p4,p5,p6)


####### Supplementary Figure 3 ######################

# the full psychometric curve 
df = alldata[game_version == "ft" & srs_normal == 1 & above_crit == 1 & group_verbal != "flag" & group_verbal != "flag_low_td"]
df = df[,c("subID","group_verbal","trial","choice","dflash","score")]
df = df[trial %in% 11:200]


df_subject_avg <- df %>%
  group_by(subID, group_verbal, dflash) %>%
  summarise(avg_choice = mean(choice, na.rm = TRUE), .groups = "drop")

df_group_avg <- df_subject_avg %>% group_by(group_verbal, dflash) %>% summarise(
    mean_choice = mean(avg_choice, na.rm = TRUE),
    se = sd(avg_choice, na.rm = TRUE) / sqrt(n()),
    .groups = "drop")

df_group_avg$group_verbal <- factor(df_group_avg$group_verbal, levels = c("TD","ASD","ASD (low verbal)"),ordered = TRUE)
p1<-
ggplot(df_group_avg, aes(x = dflash, y = mean_choice, color = group_verbal)) +
  geom_point(size=.5)+
  geom_smooth(se=F,method="glm",method.args = list(family = "binomial"),size=.5) +
  # geom_ribbon(aes(ymin = mean_choice - se, ymax = mean_choice + se, fill = group_verbal), alpha = 0.2) +
  geom_errorbar(aes(ymin = mean_choice - se, ymax = mean_choice + se), alpha = 0.8, size=.5,width=.5) +
  labs(title = "Trial 11-200",
       x = "Cue difference",
       y = "%Went right",
       color = "",
       fill = ""
       ) +
  ylim(-.05,1.05)+
  theme_bw() +
  scale_x_continuous(breaks = seq(-10, 10,3),limits = c(-11,11)) +
  scale_color_manual(values = c("ASD" = "orange", "ASD (low verbal)" = "red", "TD" = "blue")) +
  scale_fill_manual(values = c("ASD" = "orange", "ASD (low verbal)" = "red", "TD" = "blue"))+
theme(legend.position = "None",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

df$dflash <- abs(df$dflash)
df = df[df$dflash != 0]

df_subject_avg <- df %>%
  group_by(subID, group_verbal, dflash) %>%
  summarise(avg_score = mean(score, na.rm = TRUE), .groups = "drop")

df_group_avg <- df_subject_avg %>% group_by(group_verbal, dflash) %>% summarise(
    mean_score = mean(avg_score, na.rm = TRUE),
    se = sd(avg_score, na.rm = TRUE) / sqrt(n()),
    .groups = "drop")
df_group_avg$group_verbal <- factor(df_group_avg$group_verbal, levels = c("TD","ASD","ASD (low verbal)"),ordered = TRUE)

p2<-
ggplot(df_group_avg, aes(x = dflash, y = mean_score, color = group_verbal)) +
  geom_point(size=.5)+
  geom_smooth(se=F,method="glm",method.args = list(family = "binomial"),size=.5) +
  # geom_ribbon(aes(ymin = mean_choice - se, ymax = mean_choice + se, fill = group_verbal), alpha = 0.2) +
  geom_errorbar(aes(ymin = mean_score - se, ymax = mean_score + se), alpha = 0.8, size=.5,width=.5) +
  labs(title = "Trial 11-200",
       x = "Cue difference (absolute)",
       y = "Accuracy",
       color = "",
       fill = ""
       ) +
  ylim(.65,1.05)+
  theme_bw() +
  scale_x_continuous(breaks = seq(1, 10),limits = c(.5,10.5)) +
  scale_color_manual(values = c("ASD" = "orange", "ASD (low verbal)" = "red", "TD" = "blue")) +
  scale_fill_manual(values = c("ASD" = "orange", "ASD (low verbal)" = "red", "TD" = "blue"))+
theme(legend.position = "None",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


df <- alldata[trial>10 & srs_normal == 1 & above_crit == 1 & game_version == "ft",c("username","subID","choice","dflash","winstay","loseswitch","binary_group","group_verbal")]
df <- df[group_verbal != "flag" & group_verbal != "flag_low_td"]
# create trial bins
df <- df %>%
  group_by(subID) %>%
  mutate(trial= row_number(),
         trial_bin = cut(trial, breaks = 19, labels = FALSE)) %>%
  ungroup()
# function to fit a glm for each trial bin
fit_bin_glm <- function(bin_data) {
  glm(choice ~ dflash + winstay + loseswitch, 
      data = bin_data, 
      family = binomial(link = "logit"))
}
# fit the models
model_fits <- df %>%
  group_by(group_verbal, trial_bin) %>%
  nest() %>%
  mutate(model = map(data, fit_bin_glm))

# get parameter estimates for each bin and group
param_estimates <- model_fits %>%
  mutate(params = map(model, broom::tidy)) %>%
  unnest(params) %>%
  select(group_verbal, trial_bin, term, estimate, std.error) 


param_estimates <- param_estimates %>%
  mutate(significant = abs(estimate / std.error) > 1.96)  # p < 0.05



# Define the learning curve function
learning_curve <- function(n, p0, pinf, alpha) {
  pinf - (pinf - p0) * exp(-alpha * n)
}


# Filter data for dflash and split by group
dflash_data <- param_estimates %>% 
  filter(term == "dflash") %>%
  split(.$group_verbal)

# Function to fit the learning curve for a group
fit_learning_curve <- function(data) {
  nlsLM(estimate ~ learning_curve(trial_bin, p0, pinf, alpha),
        data = data,
        start = list(p0 = min(data$estimate),
                     pinf = max(data$estimate),
                     alpha = 0.1),
        lower = c(0.5, 0.5, 0.1),
        upper = c(1, 1, 1))
}


# Fit the model for each group
fits <- map(dflash_data, safely(fit_learning_curve))



# Extract parameters and create predictions
results <- imap_dfr(fits, function(fit, group_verbal) {
  if (is.null(fit$error)) {
    params <- coef(fit$result)
    new_data <- data.frame(trial_bin = seq(1, 19, length.out = 100))
    new_data$predicted <- predict(fit$result, newdata = new_data)
    new_data$group_verbal <- group_verbal
    return(new_data)
  } else {
    return(NULL)
  }
})


# Print the fitted parameters
walk2(fits, names(fits), function(fit, group_verbal) {
  if (is.null(fit$error)) {
    params <- coef(fit$result)
    cat(sprintf("%s: p0 = %.4f, pinf = %.4f, alpha = %.4f\n", 
                group_verbal, params["p0"], params["pinf"], params["alpha"]))
  } else {
    cat(sprintf("%s: Fitting failed\n", binary_group))
  }
})

# Plot
p3 <-
ggplot() +
  geom_point(data = param_estimates %>% filter(term == "dflash"), 
             aes(x = trial_bin*10, y = estimate, color = group_verbal),size=.5) +
  geom_ribbon(data = param_estimates %>% filter(term == "dflash"), 
              aes(x = trial_bin*10, ymin = estimate - std.error, ymax = estimate + std.error, fill = group_verbal), 
              alpha = 0.2) +
  geom_errorbar(data = param_estimates %>% filter(term == "dflash"), 
                aes(x = trial_bin*10, ymin = estimate - std.error, ymax = estimate + std.error, color = group_verbal), 
                width = .5,size=.5) +
  geom_line(data = results, 
            aes(x = trial_bin*10, y = predicted, color = group_verbal)) +
  labs(title = "",
       x = "Trial", 
       y = "Psychometric Slope",
       color="") +
  theme_bw()+
  scale_x_continuous(breaks = seq(11, 200,30),limits = c(5,205)) +
  scale_color_manual(values = c("ASD" = "orange", "ASD (low verbal)" = "red", "TD" = "blue")) +
  scale_fill_manual(values = c("ASD" = "orange", "ASD (low verbal)" = "red", "TD" = "blue"))+
theme(legend.position = "None",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


p4 <- 
ggplot() +
  geom_point(data = param_estimates %>% filter(term == "(Intercept)"), aes(x = trial_bin*10, y = estimate, color = group_verbal),size=.5) +
  # geom_line(data = new_data, aes(x = trial_bin, y = predicted), color = "red") +
   geom_ribbon(data = param_estimates %>% filter(term == "(Intercept)"), 
              aes(x = trial_bin*10, ymin = estimate - std.error, ymax = estimate + std.error, fill = group_verbal), 
              alpha = 0.2) +
  geom_errorbar(data = param_estimates %>% filter(term == "(Intercept)"), 
                aes(x = trial_bin*10, ymin = estimate - std.error, ymax = estimate + std.error, color = group_verbal), 
                width = .5,size=.5) +
  geom_smooth(data = param_estimates %>% filter(term == "(Intercept)"),
              aes(x = trial_bin*10, y = estimate, color = group_verbal),
              method = "lm", 
              # formula = y ~ s(x, bs = "cs"), 
              se = F,size=.5) +
  # facet_wrap(~ term, scales = "free_y", ncol = 3, nrow = 3) +
  labs(title = "",
       x = "Trial", 
       y = "Side-bias",
       color="",
       fill="") +
       ylim(-.8,.8)+
  theme_bw()+
  scale_x_continuous(breaks = seq(11, 200,30),limits = c(5,205)) +
  scale_color_manual(values = c("ASD" = "orange", "ASD (low verbal)" = "red", "TD" = "blue")) +
  scale_fill_manual(values = c("ASD" = "orange", "ASD (low verbal)" = "red", "TD" = "blue"))+
theme(legend.position = "None",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p5 <- 
ggplot() +
  geom_point(data = param_estimates %>% filter(term == "winstay"), aes(x = trial_bin*10, y = estimate, color = group_verbal),size=.5) +
  # geom_line(data = new_data, aes(x = trial_bin, y = predicted), color = "red") +
   geom_ribbon(data = param_estimates %>% filter(term == "winstay"), 
              aes(x = trial_bin*10, ymin = estimate - std.error, ymax = estimate + std.error, fill = group_verbal), 
              alpha = 0.2) +
  geom_errorbar(data = param_estimates %>% filter(term == "winstay"), 
                aes(x = trial_bin*10, ymin = estimate - std.error, ymax = estimate + std.error, color = group_verbal), 
                width = .5,size=.5) +
  geom_smooth(data = param_estimates %>% filter(term == "winstay"),
              aes(x = trial_bin*10, y = estimate, color = group_verbal),
              method = "lm", 
              # formula = y ~ s(x, bs = "cs"), 
              se = F,size=.5) +
  # facet_wrap(~ term, scales = "free_y", ncol = 3, nrow = 3) +
  labs(title = "",
       x = "Trial", 
       y = "Win-stay",
       color="",
       fill="") +
       ylim(-.8,.8)+
  theme_bw()+
  scale_x_continuous(breaks = seq(11, 200,30),limits = c(5,205)) +
  scale_color_manual(values = c("ASD" = "orange", "ASD (low verbal)" = "red", "TD" = "blue")) +
  scale_fill_manual(values = c("ASD" = "orange", "ASD (low verbal)" = "red", "TD" = "blue"))+
theme(legend.position = "None",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


p6 <- 
ggplot() +
  geom_point(data = param_estimates %>% filter(term == "loseswitch"), aes(x = trial_bin*10, y = estimate, color = group_verbal),size=.5) +
  # geom_line(data = new_data, aes(x = trial_bin, y = predicted), color = "red") +
   geom_ribbon(data = param_estimates %>% filter(term == "loseswitch"), 
              aes(x = trial_bin*10, ymin = estimate - std.error, ymax = estimate + std.error, fill = group_verbal), 
              alpha = 0.2) +
  geom_errorbar(data = param_estimates %>% filter(term == "loseswitch"), 
                aes(x = trial_bin*10, ymin = estimate - std.error, ymax = estimate + std.error, color = group_verbal), 
                width = .5,size=.5) +
  geom_smooth(data = param_estimates %>% filter(term == "loseswitch"),
              aes(x = trial_bin*10, y = estimate, color = group_verbal),
              method = "lm", 
              # formula = y ~ s(x, bs = "cs"), 
              se = F,size=.5) +
  # facet_wrap(~ term, scales = "free_y", ncol = 3, nrow = 3) +
  labs(title = "",
       x = "Trial", 
       y = "Lose-switch",
       color="",
       fill="") +
       ylim(-.8,1.8)+
  theme_bw()+
  scale_x_continuous(breaks = seq(11, 200,30),limits = c(5,205)) +
  scale_color_manual(values = c("ASD" = "orange", "ASD (low verbal)" = "red", "TD" = "blue")) +
  scale_fill_manual(values = c("ASD" = "orange", "ASD (low verbal)" = "red", "TD" = "blue"))+
theme(legend.position = "None",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


ggarrange(p1,p2,p3,p4,p5,p6,labels = c("A","B","C","D","E","F"),ncol=3,nrow=3,common.legend = TRUE,align = "v") %>% ggexport(filename = "results/figures/geodems/suppl_figure3.pdf")

# remove variables, functions and objects
rm(df,df_group_avg,df_subject_avg,dflash_data,fits,results)
rm(param_estimates,fit_bin_glm,model_fits)
rm(p1,p2,p3,p4,p5,p6)
rm(learning_curve,fit_learning_curve)


########### Figure 3 (flash weights) ######################

df = alldata[game_version == "ft" & srs_normal == 1 & above_crit == 1 & group_verbal != "flag" & group_verbal != "flag_low_td"]
df = df[,c("subID","group_verbal","trial","choice","dflash","lbin1","lbin2","lbin3","lbin4","lbin5","lbin6","lbin7","lbin8","lbin9","lbin10","rbin1","rbin2","rbin3","rbin4","rbin5","rbin6","rbin7","rbin8","rbin9","rbin10")]
df = df[trial %in% 101:200]

# Fit logistic regression model for each group
fit_model <- function(data) {
  glm(choice ~ lbin1 + lbin2 + lbin3 + lbin4 + lbin5 + lbin6 + lbin7 + lbin8 + lbin9 + lbin10 +
                rbin1 + rbin2 + rbin3 + rbin4 + rbin5 + rbin6 + rbin7 + rbin8 + rbin9 + rbin10, 
      family = binomial, 
      data = data)
}

models <- df %>%
  group_by(group_verbal) %>%
  nest() %>%
  mutate(model = map(data, fit_model))

# Extract coefficients
coef_data <- models %>%
  mutate(coef = map(model, tidy)) %>%
  select(group_verbal, coef) %>%
  unnest(coef) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    side = ifelse(grepl("^l", term), "left", "right"),
    bin = as.numeric(gsub("^[lr]bin", "", term)),
    weight = estimate * ifelse(side == "left", -1, 1)
  ) %>%
  group_by(group_verbal, bin) %>%
  reframe(avg_weight = mean(weight),se_avg_weight = sqrt(sum(std.error^2)) / 2)  # Using propagation of error method)
  

model <- aov(avg_weight ~ group_verbal * bin, data = coef_data)
summary(model)

# For group comparisons
emmeans(model, pairwise ~ group_verbal)

# For bin comparisons
emmeans(model, pairwise ~ bin)

# For side comparisons
# emmeans(lm_model, pairwise ~ side)

# For group:bin interaction
emmeans(model, pairwise ~ group_verbal | bin)

# For group:side interaction
# emmeans(lm_model, pairwise ~ group | side)


# pairwise_test <- coef_data %>%
#   pairwise_t_test(avg_weight ~ group_verbal, p.adjust.method = "bonferroni")
# pairwise_test <- pairwise_test %>%
#   add_xy_position(x = "group_verbal")


# Plot model weights
# p <- 
# ggplot(coef_data, aes(x = bin, y = estimate, color = group_verbal,shape=side)) +
#   geom_line() +
#   geom_point() +
#   labs(x = "Bin", y = "Weight", color = "Group") +
#   theme_minimal()
coef_data$group_verbal <- factor(coef_data$group_verbal, levels = c("TD","ASD","ASD (low verbal)"),ordered = TRUE)
p1<-
ggplot(coef_data, aes(x = bin/4, y = avg_weight, color = group_verbal),size=.5) +
  # geom_ribbon(aes(ymin=weight-std.error,ymax=weight+std.error,fill=group_verbal),alpha=.5) +
  # geom_point() +
  geom_errorbar(aes(ymin=avg_weight-se_avg_weight,ymax=avg_weight+se_avg_weight,color=group_verbal),size=.5,width=.1) +
  geom_smooth(span=1,se=F,size=.5)+
  ylim(0.25,1)+
  labs(x = "Flash time (s)", y = TeX("$\\beta$"), color = "") +
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 2.5, .5),limits = c(0,2.8)) +
  scale_color_manual(values = c("ASD" = "orange", "ASD (low verbal)" = "red", "TD" = "blue")) +
  scale_fill_manual(values = c("ASD" = "orange", "ASD (low verbal)" = "red", "TD" = "blue"))+
theme(legend.position = "None",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Function to group bins
# group_bins <- function(data) {
#   data %>%
#     mutate(
#       l_early = as.integer(lbin1 + lbin2 + lbin3>0),
#       l_middle = as.integer(lbin4 + lbin5 + lbin6 + lbin7>0),
#       l_late = as.integer(lbin8 + lbin9 + lbin10>0),
#       r_early = as.integer(rbin1 + rbin2 + rbin3>0),
#       r_middle = as.integer(rbin4 + rbin5 + rbin6 + rbin7>0),
#       r_late = as.integer(rbin8 + rbin9 + rbin10>0)
#     ) %>%
#     select(-starts_with("lbin"), -starts_with("rbin"))
# }

group_bins <- function(data) {
  data %>%
    mutate(
      l_early = lbin1 + lbin2 + lbin3,
      l_middle = lbin4 + lbin5 + lbin6 + lbin7,
      l_late = lbin8 + lbin9 + lbin10,
      r_early = rbin1 + rbin2 + rbin3,
      r_middle = rbin4 + rbin5 + rbin6 + rbin7,
      r_late = rbin8 + rbin9 + rbin10
    ) %>%
    select(-starts_with("lbin"), -starts_with("rbin"))
}

# Fit logistic regression model for each group
fit_model <- function(data) {
  data <- group_bins(data)
  glm(choice ~ l_early + l_middle + l_late + r_early + r_middle + r_late, 
      family = binomial, 
      data = data)
}

models <- df %>%
  group_by(group_verbal) %>%
  nest() %>%
  mutate(model = map(data, fit_model))

# Extract coefficients
coef_data <- models %>%
  mutate(coef = map(model, tidy)) %>%
  select(group_verbal, coef) %>%
  unnest(coef) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    side = ifelse(grepl("^l", term), "left", "right"),
    bin = gsub("^[lr]_", "", term),
    weight = estimate * ifelse(side == "left", -1, 1)
  )  %>%
  group_by(group_verbal, bin) %>%
  reframe(avg_weight = mean(weight),se_avg_weight = sqrt(sum(std.error^2)) / 2)  # Using propagation of error method)
  
  coef_data$bin <- factor(coef_data$bin, levels = c("early", "middle", "late"),ordered = TRUE)
  coef_data$bin_num <- as.numeric(coef_data$bin)

p2<-
ggplot(coef_data, aes(x = bin_num, y = avg_weight, color = group_verbal),size=.5) +
  # geom_ribbon(aes(ymin=weight-std.error,ymax=weight+std.error,fill=group_verbal),alpha=.5) +
  geom_point(size=.5) +
  geom_errorbar(aes(ymin=avg_weight-se_avg_weight,ymax=avg_weight+se_avg_weight,color=group_verbal),size=.5,width=.2) +
  geom_smooth(span=1,se=F,size=.5)+
  ylim(0.25,1)+
  labs(x = "Flash count", y = TeX("$\\beta$"), color = "") +
  theme_bw()+
  scale_x_discrete(limits = c("early", "middle", "late")) +
  scale_color_manual(values = c("ASD" = "orange", "ASD (low verbal)" = "red", "TD" = "blue")) +
  scale_fill_manual(values = c("ASD" = "orange", "ASD (low verbal)" = "red", "TD" = "blue"))+
theme(legend.position = "None",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
  


ggarrange(p1,p2, nrow=3,ncol = 3,common.legend = TRUE,legend = "top",align = "v",
labels = c("B","C")) %>% ggexport(filename = "results/figures/geodems/flash_weights.pdf")

# remove variables, functions and objects
rm(df,coef_data,models,fit_model)
rm(p1,p2)
rm(group_bins)

########### Figure 3 (heatmaps) ############################
df = alldata[game_version == "ft" & srs_normal == 1 & above_crit == 1 & group_verbal != "flag" & group_verbal != "flag_low_td"]
df = df[,c("subID","binary_group","group_verbal","trial","score","dflash","tflash","choice","numflashright","numflashleft")]

# Function to create heatmap for a single group
create_heatmap <- function(data, group_name) {
  # Aggregate data
  agg_data <- data[, .(
    accuracy = mean(score),
    n = .N
  ), by = .(numflashright, numflashleft)]
  
  # Keep only upper triangular matrix (including diagonal)
  agg_data <- agg_data[numflashright > numflashleft]
  #  agg_data <- agg_data[numflashright >0]
  # agg_data <- agg_data[numflashleft >0]
  
  # Create heatmap
  ggplot(agg_data, aes(x = numflashleft, y = numflashright, fill = accuracy)) +
    geom_tile() +
    scale_fill_gradient(low = "white", #mid = "white", 
    high = "mediumblue", 
    # midpoint = 0.75,
                         limits = c(0.6, 1),
                          name = "Accuracy") +
    labs(title = paste(group_name),
         x = "Smaller Number",
         y = "Larger Number") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    # geom_text(aes(label = sprintf("%.2f\n(n=%d)", accuracy, n)), 
              # color = "black", size = 3) +
    scale_x_continuous(breaks = seq(0, max(data$numflashleft)-1, by = 1)) +
    scale_y_continuous(breaks = seq(1, max(data$numflashright), by = 1)) +
    coord_fixed(ratio = 1) +
    theme(legend.position = c(.8, .3),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
    # +  # Make the plot square
    # geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black")  # Add diagonal line
}
# Create heatmaps for each group
asd_heatmap <- create_heatmap(df[binary_group == "ASD"], "ASD")
td_heatmap <- create_heatmap(df[binary_group == "TD"], "TD")


# asd_low_heatmap <- create_heatmap(df[group_verbal == "ASD (low verbal)"], "ASD (low verbal)")

# Display heatmaps
print(asd_heatmap)
print(td_heatmap)
# print(asd_low_heatmap)


agg_data_td <- df[binary_group=="TD", .(
    accuracy = mean(score),
    n = .N
  ), by = .(numflashright, numflashleft)]
 
 agg_data_asd <- df[binary_group=="ASD", .(
    accuracy = mean(score),
    n = .N
  ), by = .(numflashright, numflashleft)]
  
agg_data <- merge(agg_data_td,agg_data_asd,by=c("numflashleft","numflashright"))
agg_data$accuracy = agg_data$accuracy.x - agg_data$accuracy.y
agg_data <- agg_data[numflashright > numflashleft]


td_asd_heatmap <- ggplot(agg_data, aes(x = numflashleft, y = numflashright, fill = accuracy)) +
    geom_tile() +
    scale_fill_gradient2(low = "red", mid = "white", 
    high = "mediumblue", 
    # midpoint = 0.75,
                         limits = c(-0.15, .15),
                          name = "Accuracy") +
    labs(title = paste("TD - ASD"),
         x = "Smaller Number",
         y = "Larger Number") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    # geom_text(aes(label = sprintf("%.2f\n(n=%d)", accuracy, n)), 
              # color = "black", size = 3) +
    scale_x_continuous(breaks = seq(0, max(agg_data$numflashleft)-1, by = 1)) +
    scale_y_continuous(breaks = seq(1, max(agg_data$numflashright), by = 1)) +
    coord_fixed(ratio = 1) +
    theme(legend.position = c(.8, .3),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
    # +  # Make the plot square
    # geom_abline(i

  
ggarrange(td_heatmap, asd_heatmap, nrow=2,ncol = 3,
common.legend = TRUE,legend = "top",
align = "h",
labels = c("D","E","F")) %>% ggexport(filename = "results/figures/geodems/heatmaps.pdf")

ggarrange(td_asd_heatmap, nrow=2,ncol = 3,
common.legend = TRUE,legend = "right",
align = "h",
labels = c("F")) %>% ggexport(filename = "results/figures/geodems/diff_heatmaps.pdf")


rm(df,asd_heatmap,td_heatmap,asd_low_heatmap,create_heatmap,agg_data,agg_data_asd,agg_data_asd_low,agg_data_td)

############### Figure 4 #################################
library(optimx)
library(boot)

df = alldata[game_version == "ft" & srs_normal == 1 & above_crit == 1 & group_verbal != "flag" & group_verbal != "flag_low_td"]
df = df[,c("subID","binary_group","group_verbal","trial","score","dflash","tflash","choice","numflashright","numflashleft","winstay","loseswitch")]
df = df[trial %in% 11:200]
df$r <- df$numflashright
df$l <- df$numflashleft
df <- as.data.frame(df[,c("subID","binary_group","choice","r","l","trial","winstay","loseswitch")])


sdt_mdl <- function(params,data){
  p_choose_l = rep(0, length(data$choice))
  p_choose_r = rep(0, length(data$choice))
  if (min(params)<0){
    aic<-1000000
  }
  else{
    for (i in 1:length(data$r)){
      k0=params[1]
      k1=params[2]
      bs=params[3]
      b1=params[4]
      b2=params[5]
      p=pnorm(0,mean=data$r[i]-data$l[i]+bs+b1*data$winstay[i]+b2*data$loseswitch[i],
              sd=sqrt(k1*(data$r[i]^2+data$l[i]^2)+k0))
      if (length(p)==0){
        p=0
      }
      p_choose_l[i]=p
      p_choose_r[i]<-1-p_choose_l[i]
    }
    lhd<-rep(NA,length(data$choice))
    lhd[data$choice==1]<-p_choose_r[data$choice==1]
    lhd[data$choice==0]<-p_choose_l[data$choice==0]
    lhd[lhd==0]<-.0001
    log.lhd<-log(lhd)
    aic<- -2*sum(log.lhd)+2*length(params) # since there are 5 params
  }
  return(aic)
}

fit_model <- function(data) {
  result <- optim(par = c(0.5, 0.5, 0, 0, 0), 
                  fn = sdt_mdl,
                  data = data,
                  method = "SANN",
                  hessian = TRUE
                  )
}

# Fit model for ASD group
asd_data <- df[df$binary_group == "ASD", ]
asd_model <- fit_model(asd_data)
asd_se <- sqrt(diag(solve(asd_model$hessian)))


# Fit model for TD group
td_data <- df[df$binary_group == "TD", ]
td_model <- fit_model(td_data)
td_se <- sqrt(diag(solve(td_model$hessian)))

params_df <- data.frame(
  group = rep(c("ASD", "TD"), each = 5),
  parameter = rep(c("k0", "k1","bs", "b1", "b2"), 2),
  value = c(asd_model$par, td_model$par),
  se = c(asd_se, td_se)
)


params_df$parameter <- factor(params_df$parameter, 
                              levels = c("k0","k1","bs", "b1", "b2"))

# p<-
# ggplot(params_df, aes(x = group, y = value)) +
# geom_point(aes(color = group), size = 2) +
# geom_errorbar(aes(ymin = value - se, ymax = value + se,color = group), width = 0.2) +
#   # geom_bar(stat = "identity", position = "dodge") +
#   facet_wrap(~ parameter, scales = "free_y", nrow = 3,ncol=3,shrink = T,dir="v") +
#   theme_bw() +
#   labs(title = "SDT Model Parameters by Group", x = "Group", y = "Value",color="") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#   theme(#legend.position = c(.8, .3),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())

# ggexport(p, filename = "results/figures/geodems/sdt_model_params.pdf")

# Generate predictions for ASD group

mdl_func <- function(params,data){
  p_choose_l = rep(0, length(data$choice))
  p_choose_r = rep(0, length(data$choice))
    for (i in 1:length(data$r)){
      k0=params[1]
      k1=params[2]
      bs=params[3]
      b1=params[4]
      b2=params[5]
      p=pnorm(0,mean=data$r[i]-data$l[i]+bs+b1*data$winstay[i]+b2*data$loseswitch[i],
              sd=sqrt(k1*(data$r[i]^2+data$l[i]^2)+k0))
      if (length(p)==0){
        p=0
      }
      p_choose_l[i]=p
      p_choose_r[i]<-1-p_choose_l[i]
  }
  return(p_choose_r)
}

asd_pred <- mdl_func(asd_model$par, asd_data)
td_pred <- mdl_func(td_model$par, td_data)

fit_data <- rbind(
  data.frame(subID = asd_data$subID,group = "ASD", observed = asd_data$choice, predicted = asd_pred, dflash=asd_data$r-asd_data$l),
  data.frame(subID = td_data$subID,group = "TD", observed = td_data$choice, predicted = td_pred, dflash=td_data$r-td_data$l)
)

df_sub_avg <- fit_data %>%
  group_by(subID,group, dflash) %>%
  summarise(
  avg_choice = mean(observed, na.rm = TRUE), 
  avg_pred = mean(predicted, na.rm=TRUE), 
  .groups = "drop")

  df_group_avg <- df_sub_avg %>%
  group_by(group, dflash) %>%
  summarise(
    mean_choice = mean(avg_choice, na.rm = TRUE),
    mean_pred = mean(avg_pred, na.rm = TRUE),
    choice_se = sd(avg_choice, na.rm = TRUE) / sqrt(n()),
    pred_se = sd(avg_pred, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )


# p_fit <- 
# ggplot(df_group_avg, aes(x=dflash,color = group)) +
#   geom_point(aes(y=mean_choice),alpha = 0.5) +
#   geom_errorbar(aes(y=mean_choice, ymin = mean_choice - choice_se, ymax = mean_choice + choice_se), width = 0.5) +
#   geom_line(aes(y=mean_pred),alpha = 1) +
#   # geom_smooth(method = "glm",method.args = list(family = "binomial") ,se = FALSE) +
#   # facet_wrap(~group,nrow = 3,ncol=3) +
#   theme_bw() +
#   labs(title = "Model Fit vs Original Data", x = "Cue difference", y = "%Went right",color="")+
#   theme(legend.position = "None",
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())

# ggarrange(p_fit, nrow=3, ncol=3) %>%
# ggexport(p_fit, filename = "results/figures/geodems/sdt_model_fit.pdf")

# rm(p,p_fit)
setDT(df_group_avg)
p1 <- 
ggplot(df_group_avg[group=="ASD"],aes(x=dflash))+
  geom_point(aes(y=mean_choice),alpha = 0.5,color="red") +
  geom_errorbar(aes(y=mean_choice, ymin = mean_choice - choice_se, ymax = mean_choice + choice_se), width = 1,color="red") +
  geom_line(aes(y=mean_pred),alpha = 1,color="black") +
  theme_bw() +
  labs(title = "Model Fit: ASD", x = "Cue difference", y = "%Went right",color="")+
  theme(legend.position = "None",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p2 <- 
ggplot(df_group_avg[group=="TD"],aes(x=dflash))+
  geom_point(aes(y=mean_choice),alpha = 0.5,color="blue") +
  geom_errorbar(aes(y=mean_choice, ymin = mean_choice - choice_se, ymax = mean_choice + choice_se), width = 1,color="blue") +
  geom_line(aes(y=mean_pred),alpha = 1,color="black") +
  theme_bw() +
  labs(title = "TD", x = "Cue difference", y = "%Went right",color="")+
  theme(legend.position = "None",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggarrange(p1,p2, nrow=3,ncol = 3,common.legend = TRUE,legend = "top",align = "h",
labels = c("E","F")) %>% ggexport(filename = "results/figures/geodems/sdt_model_fit.pdf")

rm(p1,p2)

setDT(params_df)

p1 <- 
ggplot(params_df[parameter=="k0"], aes(x = group, y = value)) +
geom_point(aes(color = group), size = 2) +
geom_errorbar(aes(ymin = value - se, ymax = value + se,color = group), width = 0.2) +
#   facet_wrap(~ parameter, scales = "free_y", nrow = 3,ncol=3,shrink = T,dir="v") +
  theme_bw() +
  ylim(0.5,2) +
  labs(title = "", x="", y = TeX("$k_0$"),color="") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(#legend.position = c(.8, .3),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p2 <-
ggplot(params_df[parameter=="k1"], aes(x = group, y = value)) +
geom_point(aes(color = group), size = 2) +
geom_errorbar(aes(ymin = value - se, ymax = value + se,color = group), width = 0.2) +
  theme_bw() +
  ylim(0.05,0.15) +
  labs(x = "", y = TeX("$k_1$"),color="") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p3 <-
ggplot(params_df[parameter=="bs"], aes(x = group, y = value)) +
geom_point(aes(color = group), size = 2) +
geom_errorbar(aes(ymin = value - se, ymax = value + se,color = group), width = 0.2) +
  theme_bw() +
  ylim(-0.1,0.1) +
  labs(x = "", y = TeX("$bs$"),color="") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p4 <-
ggplot(params_df[parameter=="b1"], aes(x = group, y = value)) +
geom_point(aes(color = group), size = 2) +
geom_errorbar(aes(ymin = value - se, ymax = value + se,color = group), width = 0.2) +
  theme_bw() +
  ylim(0,0.3) +
  labs(x = "", y = TeX("$b1$"),color="") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


p5 <-
ggplot(params_df[parameter=="b2"], aes(x = group, y = value)) +
geom_point(aes(color = group), size = 2) +
geom_errorbar(aes(ymin = value - se, ymax = value + se,color = group), width = 0.2) +
  theme_bw() +
  ylim(0.3,0.7) +
  labs(x = "", y = TeX("$b2$"),color="") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggarrange(p1,p2,p3,p4,p5, nrow=3,ncol = 3,common.legend = TRUE,legend = "top",align = "v",
labels = c("G","H","I","J","K")) %>% ggexport(filename = "results/figures/geodems/sdt_model_params.pdf")

################ Supplementary figure 4 #################################


sdt_mdl_linear <- function(params,data){
  p_choose_l = rep(0, length(data$choice))
  p_choose_r = rep(0, length(data$choice))
  if (min(params)<0){
    aic<-1000000
  }
  else{
    for (i in 1:length(data$r)){
      k0=params[1]
      k1=params[2]
      bs=params[3]
      b1=params[4]
      b2=params[5]
      p=pnorm(0,mean=data$r[i]-data$l[i]+bs+b1*data$winstay[i]+b2*data$loseswitch[i],
              sd=sqrt(k1*(data$r[i]+data$l[i])+k0))
      if (length(p)==0){
        p=0
      }
      p_choose_l[i]=p
      p_choose_r[i]<-1-p_choose_l[i]
    }
    lhd<-rep(NA,length(data$choice))
    lhd[data$choice==1]<-p_choose_r[data$choice==1]
    lhd[data$choice==0]<-p_choose_l[data$choice==0]
    lhd[lhd==0]<-.0001
    log.lhd<-log(lhd)
    aic<- -2*sum(log.lhd)+2*length(params) # since there are 5 params
  }
  return(aic)
}

fit_model_linear <- function(data) {
  result <- optim(par = c(0.5, 0.5, 0, 0, 0), 
                  fn = sdt_mdl_linear,
                  data = data,
                  method = "SANN",
                  hessian = TRUE
                  )
}

asd_model_linear <- fit_model_linear(asd_data)
asd_se_linear <- sqrt(diag(solve(asd_model_linear$hessian)))

td_model_linear <- fit_model_linear(td_data)
td_se_linear <- sqrt(diag(solve(td_model_linear$hessian)))

params_df_linear <- data.frame(
  group = rep(c("ASD", "TD"), each = 5),
  parameter = rep(c("k0", "k1","bs", "b1", "b2"), 2),
  value = c(asd_model_linear$par, td_model_linear$par),
  se = c(asd_se_linear, td_se_linear)
)

params_df_linear$parameter <- factor(params_df_linear$parameter, 
                              levels = c("k0","k1","bs", "b1", "b2"))

mdl_func_linear <- function(params,data){
  p_choose_l = rep(0, length(data$choice))
  p_choose_r = rep(0, length(data$choice))
    for (i in 1:length(data$r)){
      k0=params[1]
      k1=params[2]
      bs=params[3]
      b1=params[4]
      b2=params[5]
      p=pnorm(0,mean=data$r[i]-data$l[i]+bs+b1*data$winstay[i]+b2*data$loseswitch[i],
              sd=sqrt(k1*(data$r[i]+data$l[i])+k0))
      if (length(p)==0){
        p=0
      }
      p_choose_l[i]=p
      p_choose_r[i]<-1-p_choose_l[i]
  }
  return(p_choose_r)
}

asd_pred_linear <- mdl_func_linear(asd_model_linear$par, asd_data)
td_pred_linear <- mdl_func_linear(td_model_linear$par, td_data)

fit_data_linear <- rbind(
  data.frame(subID = asd_data$subID,group = "ASD", observed = asd_data$choice, predicted = asd_pred_linear, dflash=asd_data$r-asd_data$l),
  data.frame(subID = td_data$subID,group = "TD", observed = td_data$choice, predicted = td_pred_linear, dflash=td_data$r-td_data$l)
)

df_sub_avg_linear <- fit_data_linear %>%
  group_by(subID,group, dflash) %>%
  summarise(
  avg_choice = mean(observed, na.rm = TRUE), 
  avg_pred = mean(predicted, na.rm=TRUE), 
  .groups = "drop")

  df_group_avg_linear <- df_sub_avg_linear %>%
  group_by(group, dflash) %>%
  summarise(
    mean_choice = mean(avg_choice, na.rm = TRUE),
    mean_pred = mean(avg_pred, na.rm = TRUE),
    choice_se = sd(avg_choice, na.rm = TRUE) / sqrt(n()),
    pred_se = sd(avg_pred, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

tmp <- data.frame(
  group = rep(c("ASD", "TD"), each = 2),
  mdl = rep(c("scalar noise","linear noise"),  2),
  aic = c(asd_model$value,
  asd_model_linear$value, 
  td_model$value,
  td_model_linear$value)
)




p <- 
ggplot(tmp, aes(x = group, y = log(aic),color = mdl)) +
geom_point(position = position_dodge(width = 0.5),size=.5) +
  theme_bw() +
  ylim(9,11)+
  scale_color_manual(values = c("scalar noise" = "darkgreen", "linear noise" = "maroon")) +
  geom_text(position = position_dodge(width = 1),aes(label = round(log(aic), 3)), vjust = -0.5,size=2) +
  labs(title = "Model comparison: Scalar vs Linear Noise", x = "", y = "log(AIC)",color="") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position = "bottom",
        # panel.grid.major = element_blank(),
        # panel.grid.minor = element_blank()
        )
ggarrange(p, nrow=3, ncol=3,align="h") %>% ggexport(filename = "results/figures/geodems/sdt_model_fit_linear.pdf")


setDT(params_df_linear)

p1 <- 
ggplot(params_df_linear[parameter=="k0"], aes(x = group, y = value)) +
geom_point(aes(color = group), size = 2) +
geom_errorbar(aes(ymin = value - se, ymax = value + se,color = group), width = 0.2) +
#   facet_wrap(~ parameter, scales = "free_y", nrow = 3,ncol=3,shrink = T,dir="v") +
  theme_bw() +
  ylim(0,1.5) +
  labs(title = "", x="", y = TeX("$k_0$"),color="") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(#legend.position = c(.8, .3),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p2 <-
ggplot(params_df_linear[parameter=="k1"], aes(x = group, y = value)) +
geom_point(aes(color = group), size = 2) +
geom_errorbar(aes(ymin = value - se, ymax = value + se,color = group), width = 0.2) +
  theme_bw() +
  ylim(0.5,.9) +
  labs(x = "", y = TeX("$k_1$"),color="") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p3 <-
ggplot(params_df_linear[parameter=="bs"], aes(x = group, y = value)) +
geom_point(aes(color = group), size = 2) +
geom_errorbar(aes(ymin = value - se, ymax = value + se,color = group), width = 0.2) +
  theme_bw() +
  ylim(-0.1,0.1) +
  labs(x = "", y = TeX("$bs$"),color="") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p4 <-
ggplot(params_df_linear[parameter=="b1"], aes(x = group, y = value)) +
geom_point(aes(color = group), size = 2) +
geom_errorbar(aes(ymin = value - se, ymax = value + se,color = group), width = 0.2) +
  theme_bw() +
  ylim(0,0.3) +
  labs(x = "", y = TeX("$b1$"),color="") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


p5 <-
ggplot(params_df_linear[parameter=="b2"], aes(x = group, y = value)) +
geom_point(aes(color = group), size = 2) +
geom_errorbar(aes(ymin = value - se, ymax = value + se,color = group), width = 0.2) +
  theme_bw() +
  ylim(0.3,0.7) +
  labs(x = "", y = TeX("$b2$"),color="") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggarrange(p1,p2,p3,p4,p5, nrow=3,ncol = 3,common.legend = TRUE,legend = "top",align = "v") %>% ggexport(filename = "results/figures/geodems/sdt_model_params_linear.pdf")


# p <-
# ggplot(params_df_linear, aes(x = group, y = value)) +
# geom_point(aes(color = group), size = 2) +
# geom_errorbar(aes(ymin = value - se, ymax = value + se,color = group), width = 0.2) +
#   # geom_bar(stat = "identity", position = "dodge") +
#   facet_wrap(~ parameter, scales = "free_y", nrow = 3,ncol=3,shrink = T,dir="v") +
#   theme_bw() +
#   labs(title = "Linear Noise Model Parameters by Group", x = "Group", y = "Value",color="") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#   theme(#legend.position = c(.8, .3),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())

# ggexport(p, filename = "results/figures/geodems/sdt_model_params_linear.pdf")


# setDT(df_group_avg_linear)
# setDT(df_group_avg)
# df_group_avg$mdl <- "scalar noise"
# df_group_avg_linear$mdl <- "linear noise"

# tmp <- rbind(df_group_avg,df_group_avg_linear)
# setDT(tmp)

# # p_fit_linear <-
# ggplot(tmp[group=="ASD"],aes(x=dflash,color=mdl)) +
#   geom_point(aes(y=mean_choice),color="red",alpha = 0.5,size=.5) +
#   geom_errorbar(aes(y=mean_choice,ymin = mean_choice - choice_se, ymax = mean_choice + choice_se), color="red",width = 0.5) +
#   geom_line(aes(y=mean_pred),alpha = 1) +
#   theme_bw() +
#   scale_color_manual(values = c("scalar noise" = "darkgreen", "linear noise" = "maroon")) +
#   labs(title = "Scalar vs Linear Noise: ASD", x = "Cue difference", y = "%Went right",color="")+
#   theme(#legend.position = "None",
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())

# ggarrange(p, nrow=3, ncol=3,align="h") %>%
# ggexport(filename = "results/figures/geodems/sdt_model_fit_linear.pdf")



rm(p_fit_linear,p_fit,params_df_linear,asd_model_linear,asd_se_linear,td_model_linear,td_se_linear,fit_data_linear,df_sub_avg_linear,df_group_avg_linear)
rm(asd_data,td_data,asd_model,asd_se,td_model,td_se,fit_data,df_sub_avg,df_group_avg)
rm(tmp,p,fit_model_linear,sdt_mdl_linear,params_df_linear,mdl_func_linear,asd_pred_linear,td_pred_linear)
rm(fit_model,sdt_mdl,params_df,mdl_func,asd_pred,td_pred,fit_model_linear,sdt_mdl_linear,params_df_linear,mdl_func_linear,asd_pred_linear,td_pred_linear)
rm(df,df_avg)


### Figure 5 ########

library(rstan)
load("results/files/params_expdata_svnarrow.RData")

asdhbmfit <- rstan::extract(fitasd)
tdhbmfit <- rstan::extract(fittd)

params_df <- data.frame(group=rep(c("ASD","TD"),each=length(asdhbmfit$mu0)),
mu0=c(asdhbmfit$mu0,tdhbmfit$mu0),
mu1=c(asdhbmfit$mu1,tdhbmfit$mu1),
sig0=c(asdhbmfit$sig0,tdhbmfit$sig0),
sig1=c(asdhbmfit$sig1,tdhbmfit$sig1),
k0=c(asdhbmfit$k0,tdhbmfit$k0))

p1<-ggplot(params_df, aes(color = group, x = k0,fill=group)) +
geom_histogram(aes(y=after_stat(density)), alpha=0.5, 
                 position="identity")+
  labs(fill="",color="")+
  xlab(TeX("$k_{0}$"))+
  geom_density(alpha=.2,show.legend = F)+
  theme_bw()+
  theme(legend.position =c(.8,.7),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p2<-ggplot(params_df, aes(color = group, x = mu0,fill=group)) +
geom_histogram(aes(y=after_stat(density)), alpha=0.5, 
                 position="identity")+
  labs(fill="",color="")+
  xlab(TeX("$\\mu_{0}$"))+
  geom_density(alpha=.2,show.legend = F)+
  theme_bw()+
  theme(legend.position =c(.8,.7),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p3<-ggplot(params_df, aes(color = group, x = sig0,fill=group)) +
geom_histogram(aes(y=after_stat(density)), alpha=0.5, 
                 position="identity")+
  labs(fill="",color="")+
  xlab(TeX("$\\sigma_{0}$"))+
  geom_density(alpha=.2,show.legend = F)+
  theme_bw()+
  theme(legend.position =c(.8,.7),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())



df = alldata[srs_normal == 1 & above_crit == 1 & game_version == "ft"]

df1 = data.frame("k1"=colMeans(asdhbmfit$k1),
                 "b1"=colMeans(asdhbmfit$b1),
                 "b2"=colMeans(asdhbmfit$b2),
                 "bs"=colMeans(asdhbmfit$bs),
                 "gr"="ASD")

df2 = data.frame("k1"=colMeans(tdhbmfit$k1),
                 "b1"=colMeans(tdhbmfit$b1),
                 "b2"=colMeans(tdhbmfit$b2),
                 "bs"=colMeans(tdhbmfit$bs),
                 "gr"="TD")

dt1 = df[binary_group=="ASD"]
dt2 = df[binary_group=="TD"]

dt1$subj_id<-as.numeric(factor(dt1$username, 
                               levels=unique(dt1$username)))

dt2$subj_id<-as.numeric(factor(dt2$username, 
                               levels=unique(dt2$username)))

df1$subj_id = as.numeric(c(1:length(unique(dt1$subj_id))))
df2$subj_id = as.numeric(c(1:length(unique(dt2$subj_id))))

dt1 = merge(dt1,df1,by="subj_id")
dt2 = merge(dt2,df2,by="subj_id")

dt = rbind(dt1,dt2)
dt = unique(dt[,.(username,k1,b1,b2,bs,binary_group)])

rm(dt1,dt2,df1,df2,df)


params_df2 <- dt
colnames(params_df2)[colnames(params_df2)=="binary_group"] <- "group"

# p4<-
ggplot(params_df2, aes(color = group, y = k1, x = group)) +
geom_boxplot(outlier.shape = NA,show.legend = F,alpha=1,width=.5)+
  geom_jitter(width=.1,show.legend = FALSE,size=.5)+
  labs(color="")+xlab("")+
  ylab(TeX("$k_{1}$"))+
  theme_bw()+
  theme(legend.position ="None",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


p5<-ggplot(params_df, aes(color = group, x = mu1,fill=group)) +
geom_histogram(aes(y=after_stat(density)), alpha=0.5, 
                 position="identity")+
  labs(fill="",color="")+
  xlab(TeX("$\\mu_{1}$"))+
  geom_density(alpha=.2,show.legend = F)+
  theme_bw()+
  theme(legend.position =c(.8,.7),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p6<-ggplot(params_df, aes(color = group, x = sig1,fill=group)) +
geom_histogram(aes(y=after_stat(density)), alpha=0.5, 
                 position="identity")+
  labs(fill="",color="")+
  xlab(TeX("$\\sigma_{1}$"))+
  geom_density(alpha=.2,show.legend = F)+
  theme_bw()+
  theme(legend.position =c(.8,.7),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p7<-
ggplot(params_df2, aes(color = group, y = abs(bs), x = group)) +
geom_boxplot(outlier.shape = NA,show.legend = F,alpha=1,width=.5)+
  geom_jitter(width=.1,show.legend = FALSE,size=.5)+
  labs(color="")+xlab("")+
  ylab(TeX("Side-bias"))+
  theme_bw()+
  theme(legend.position ="None",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p8<-
ggplot(params_df2, aes(color = group, y = abs(b1), x = group)) +
geom_boxplot(outlier.shape = NA,show.legend = F,alpha=1,width=.5)+
  geom_jitter(width=.1,show.legend = FALSE,size=.5)+
  labs(color="")+xlab("")+
  ylab(TeX("WS"))+
  theme_bw()+
  theme(legend.position ="None",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p9<-
ggplot(params_df2, aes(color = group, y = abs(b2), x = group)) +
geom_boxplot(outlier.shape = NA,show.legend = F,alpha=1,width=.5)+
  geom_jitter(width=.1,show.legend = FALSE,size=.5)+
  labs(color="")+xlab("")+
  ylab(TeX("LS"))+
  theme_bw()+
  theme(legend.position ="None",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9, nrow=3,ncol = 3,common.legend = TRUE,legend = "top",align = "v") %>% 
ggexport(filename = "results/figures/geodems/hbm_params.pdf")

t.test(params_df2$k1~params_df2$group,var.equal = F)
t.test(abs(params_df2$bs)~params_df2$group)
t.test(abs(params_df2$b1)~params_df2$group)
t.test(abs(params_df2$b2)~params_df2$group)

summary2 = summary[game_version=="ft"]
# rename column binary_group to group
colnames(summary2)[colnames(summary2)=="binary_group"] <- "group"

summary2 = merge(summary2,params_df2,by=c("username","group"),all=TRUE)
colnames(summary2)
head(summary2)

df = alldata[above_crit == 1 & srs_normal == 1 & game_version == "ft"] %>%
group_by(username) %>% summarise(mean_rt = mean(rt,na.rm = TRUE),
mean_iti = mean(iti,na.rm = TRUE),
.groups = "drop")

summary2 = merge(summary2,df,by="username",all=TRUE)
rm(df,dt)
rm(p1,p2,p3,p4,p5,p6,p7,p8,p9,params_df,params_df2,asdhbmfit,tdhbmfit)
rm(asd_subid,td_subid,fitasd,fittd)

###################### individual glms ###############################################

# Fit a GLM model to the choice data for each subject
df <- alldata[trial>10 & srs_normal == 1 & above_crit == 1 & game_version == "ft",c("username","subID","choice","dflash","winstay","loseswitch","binary_group","group_verbal")]

# Function to fit a GLM model to the choice data for a single subject
fit_subject_glm <- function(subject_data) {
  glm(choice ~ dflash + winstay + loseswitch, 
      data = subject_data, 
      family = binomial(link = "logit"))
}
# fit the model for each subject
model_fits <- df %>%
  group_by(subID) %>%
  nest() %>%
  mutate(model = map(data, fit_subject_glm))

# get the parameter estimates for each subject
param_estimates <- model_fits %>%
  mutate(params = map(model, broom::tidy)) %>%
  unnest(params) %>%
  select(subID, term, estimate) %>%
  pivot_wider(names_from = term, values_from = estimate)

# Merge with subject group information
param_estimates <- param_estimates %>%
  left_join(df %>% distinct(subID, binary_group), by = "subID")

# # Plot the parameter estimates by group
# param_long <- param_estimates %>%
#   pivot_longer(cols = c(dflash, winstay, loseswitch), 
#                names_to = "parameter", 
#                values_to = "estimate")

colnames(param_estimates) <- c("subID","Sidebias","Slope","WS","LS","group")

p2<-
ggplot(param_estimates, 
aes(x = group, y = abs(Sidebias), color = group)) +
  geom_boxplot(outlier.shape = NA,show.legend = F,alpha=1,width=.5)+
  geom_jitter(width=.1,show.legend = FALSE,size=.5)+
    labs(x = "", y = "Side-bias") +
  theme_bw()+
  theme(legend.position ="None",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p1<-
ggplot(param_estimates, 
aes(x = group, y = Slope, color = group)) +
  geom_boxplot(outlier.shape = NA,show.legend = F,alpha=1,width=.5)+
  geom_jitter(width=.1,show.legend = FALSE,size=.5)+
    labs(x = "", y = "Slope") +
  theme_bw()+
  theme(legend.position ="None",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p3<-
ggplot(param_estimates, 
aes(x = group, y = abs(WS), color = group)) +
  geom_boxplot(outlier.shape = NA,show.legend = F,alpha=1,width=.5)+
  geom_jitter(width=.1,show.legend = FALSE,size=.5)+
    labs(x = "", y = "WS") +
  theme_bw()+
  theme(legend.position ="None",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p4<-
ggplot(param_estimates, 
aes(x = group, y = abs(LS), color = group)) +
  geom_boxplot(outlier.shape = NA,show.legend = F,alpha=1,width=.5)+
  geom_jitter(width=.1,show.legend = FALSE,size=.5)+
    labs(x = "", y = "LS") +
  theme_bw()+
  theme(legend.position ="None",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


summary2 = merge(summary2,param_estimates,by=c("subID","group"),all=TRUE)
colnames(summary2)

p5 <- 
ggplot(summary2, aes(x = Slope, y = k1, color = group)) +
  geom_point(size=.5) +
  labs(x = "Psychometric slope", y = "Perceptual noise",color="") +
  theme_bw() +
  theme(legend.position ="None",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

cor.test(summary2$Slope,summary2$k1)
cor.test(summary2$k1,abs(summary2$WS))
cor.test(summary2$k1,abs(summary2$LS))
cor.test(summary2$k1,abs(summary2$Sidebias))

ggplot(summary2, aes(x = k1, y = abs(LS), color = group)) +
  geom_point(size=.5) +
  labs(x = "Psychometric slope", y = "Perceptual noise",color="") +
  theme_bw() +
  theme(legend.position ="None",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# p6 <-
# ggplot(summary2, aes(x = abs(LS), y = abs(b2), color = group)) +
#   geom_point() +
#   # labs(title = "Sidebias vs. Performance", x = "Sidebias", y = "Performance",color="") +
#   theme_bw() +
#   theme(legend.position ="None",
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())

ggarrange(p1,p2,p3,p4,p5, nrow=3,ncol = 3,common.legend = TRUE,legend = "top",align = "v",
labels = c("A","B","C","D","E")) %>% 
ggexport(filename = "results/figures/geodems/indglm_params.pdf")

t.test(param_estimates$Slope~param_estimates$group)
t.test(abs(param_estimates$Sidebias)~param_estimates$group)
t.test(abs(param_estimates$WS)~param_estimates$group)
t.test(abs(param_estimates$LS)~param_estimates$group)

rm(p1,p2,p3,p4,p5,param_estimates,model_fits,fit_subject_glm,df)

# # Merge with the summary data
# summary <- summary %>%
#   right_join(param_estimates, by = c("subID","binary_group"))


# # Plot the fitted choice probabilities for selected subjects
#   plot_subject_fit <- function(subject_id, group_data, full_data) {
#   subject_model <- group_data %>% 
#     filter(subject_id == !!subject_id) %>% 
#     pull(model) %>% 
#     .[[1]]
  
#   subject_data <- full_data %>% filter(subject_id == !!subject_id)
  
#   new_data <- expand.grid(
#     dflash = seq(min(subject_data$dflash), max(subject_data$dflash), length.out = 100),
#     winstay = mean(subject_data$winstay),
#     loseswitch = mean(subject_data$loseswitch)
#   )
  
#   new_data$predicted_prob <- predict(subject_model, newdata = new_data, type = "response")
  
#   ggplot(subject_data, aes(x = dflash, y = choice)) +
#     geom_point(alpha = 0.5) +
#     geom_line(data = new_data, aes(y = predicted_prob), color = "red") +
#     labs(title = paste("Subject", subject_id, "(", group_data$group[1], ")"),
#          x = "dflash", y = "Choice Probability") +
#     theme_minimal()
# }

# # Select two subjects from each group
# asd_subjects <- param_estimates %>% filter(binary_group == "ASD") %>% slice_sample(n = 1) %>% pull(subID)
# td_subjects <- param_estimates %>% filter(binary_group == "TD") %>% slice_sample(n = 1) %>% pull(subID)

# # Plot for selected subjects
# selected_plots <- map(c(asd_subjects, td_subjects), 
#                       ~plot_subject_fit(.x, model_fits, df))

# # Arrange plots in a grid
# grid.arrange(grobs = selected_plots, ncol = 2)

# rm(model_fits, param_estimates, param_long, selected_plots,plot_subject_fit,asd_subjects,td_subjects,fit_subject_glm)

#### correlations ####

library(Hmisc)
library(tidyverse)
library(gridExtra)
 
# Select the columns of interest
df1 <- summary2[#binary_group == "ASD"
,c("perf","trainperf","Slope","k1","bs","b1","b2","mean_rt","mean_iti")]
df1$bs <- abs(df1$bs)
df1$b1 <- abs(df1$b1)
df1$b2 <- abs(df1$b2)


df2 <-  summary2[,c("vinabcstd","srs2total","bistotal","aasp_low_reg_raw","aasp_sen_seek_raw","aasp_sen_ses_raw","aasp_sen_avoid_raw","group_verbal","group")]


df <- cbind(df1,df2)
df <- df[group_verbal != "flag" & group_verbal!="flag_low_td",]
df$vinabcstd <- 140 - df$vinabcstd

# cor_test <- cor.test(df$vinabcstd, df$perf)
df$group_verbal <- factor(df$group_verbal, levels = c("TD", "ASD", "ASD (low verbal)"),ordered = T)
p1<-
ggplot(df,aes(x=vinabcstd,y=perf*100))+
geom_point(aes(color=group_verbal),size=.5)+
geom_smooth(method = "lm",se = T,color="black",size=.5)+
stat_cor(method = "pearson",label.y.npc = "bottom")+
labs(color="",x="VABS-3",y="%Correct")+
scale_color_manual(values = c("TD" = "blue", "ASD" = "orange", "ASD (low verbal)" = "red"))+
# annotate("text", x = 75, y = .5, 
#            label = paste("r =", round(cor_test$estimate, 2),
#                          ", p =", round(cor_test$p.value, 6)),
          #  hjust = 1, vjust = 1)+
theme_bw()+
theme(legend.position ="None",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p2 <-
ggplot(df,aes(x=vinabcstd,y=Slope))+
geom_point(aes(color=group_verbal),size=.5)+
geom_smooth(method = "lm",se = T,color="black",size=.5)+
stat_cor(method = "pearson",label.y.npc = "top")+
labs(color="",x="VABS-3",y="Psychometric Slope")+
scale_color_manual(values = c("TD" = "blue", "ASD" = "orange", "ASD (low verbal)" = "red"))+
theme_bw()+
theme(legend.position ="None",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p3 <-
ggplot(df,aes(x=vinabcstd,y=k1))+
geom_point(aes(color=group_verbal),size=.5)+
geom_smooth(method = "lm",se = T,color="black",size=.5)+
stat_cor(method = "pearson",label.y.npc = "top")+
labs(color="",x="VABS-3",y="Perceptual Noise")+
scale_color_manual(values = c("TD" = "blue", "ASD" = "orange", "ASD (low verbal)" = "red"))+
theme_bw()+
theme(legend.position ="None",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


p4 <-
ggplot(df,aes(x=srs2total,y=perf*100))+
geom_point(aes(color=group_verbal),size=.5)+
geom_smooth(method = "lm",se = T,color="black",size=.5)+
stat_cor(method = "pearson",label.y.npc = "bottom")+
labs(color="",x="SRS-2",y="%Correct")+
scale_color_manual(values = c("TD" = "blue", "ASD" = "orange", "ASD (low verbal)" = "red"))+
theme_bw()+
theme(legend.position ="None",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p5 <-
ggplot(df,aes(x=srs2total,y=Slope))+
geom_point(aes(color=group_verbal),size=.5)+
geom_smooth(method = "lm",se = T,color="black",size=.5)+
stat_cor(method = "pearson",label.y.npc = "top")+
labs(color="",x="SRS-2",y="Psychometric Slope")+
scale_color_manual(values = c("TD" = "blue", "ASD" = "orange", "ASD (low verbal)" = "red"))+
theme_bw()+
theme(legend.position ="None",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


p6 <-
ggplot(df,aes(x=srs2total,y=k1))+
geom_point(aes(color=group_verbal),size=.5)+
geom_smooth(method = "lm",se = T,color="black",size=.5)+
stat_cor(method = "pearson",label.y.npc = "top")+
labs(color="",x="SRS-2",y="Perceptual Noise")+
scale_color_manual(values = c("TD" = "blue", "ASD" = "orange", "ASD (low verbal)" = "red"))+
theme_bw()+
theme(legend.position ="None",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggarrange(p1,p2,p3,p4,p5,p6, nrow=3,ncol = 3,common.legend = TRUE,legend = "top",align = "v",
labels = c("A","B","C","D","E","F")) %>% 
ggexport(filename = "results/figures/geodems/correlations.pdf")
rm(df)

###### cross correlation ######
library(corrplot)
library(psych)
library(coin)
library(reshape2)

df <- summary2
# df <- df[complete.cases(df),]

df1 <- df[#binary_group == "ASD"
,c("perf","trainperf","Slope","k1","bs","b1","b2","mean_rt","mean_iti")]
df1$bs <- abs(df1$bs)
df1$b1 <- abs(df1$b1)
df1$b2 <- abs(df1$b2)


df2 <-  df[,c("vinabcstd","srs2total","bistotal","aasp_low_reg_raw","aasp_sen_seek_raw","aasp_sen_ses_raw","aasp_sen_avoid_raw","group_verbal","group")]

df1 <- df1[,-c("trainperf","mean_rt")]
df2 <- df2[,-c("group_verbal","group")]
# df1 <- as.matrix(df1)
# df2 <- as.matrix(df2)


impute_mean <- function(df) {
  for (col in names(df)) {
    df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm = TRUE)
  }
  return(df)
}

# Impute missing values in both datasets
df1_imputed <- impute_mean(df1)
df2_imputed <- impute_mean(df2)

colnames(df1_imputed) <- c("Accuracy","Psychometric Slope","Perceptual Noise","Side-bias","WS","LS","ITI")
colnames(df2_imputed) <- c("VABS-3","SRS-2","BIS","AASP (Low Registration)","AASP (Sensation Seeking)","AASP (Sensory Sensitivity)","AASP (Sensation Avoiding)")


observed_corr <- corr.test(df1_imputed, df2_imputed, method = "spearman", adjust = "none")

set.seed(123)  # For reproducibility

permutation_test <- function(A, B, n_perm = 1000) {
  m <- ncol(A)
  n <- ncol(B)
  p_values <- matrix(NA, nrow = m, ncol = n)
  
  for (i in 1:m) {
    for (j in 1:n) {
      observed_cor <- cor(A[[i]], B[[j]], method = "spearman", use = "complete.obs")
      permuted_cors <- numeric(n_perm)
      
      for (k in 1:n_perm) {
        permuted_A <- A[sample(nrow(A)), ]  # Permute entire rows of A
        permuted_cors[k] <- cor(permuted_A[[i]], B[[j]], method = "spearman", use = "complete.obs")
      }
      
      p_values[i, j] <- mean(abs(permuted_cors) >= abs(observed_cor))
    }
  }
  
  return(p_values)
}

# Run permutation test
p_values <- permutation_test(df1_imputed, df2_imputed)

p_adjusted <- p.adjust(p_values, method = "fdr")

sig_mask <- p_adjusted < 0.05
sig_corr <- observed_corr$r * sig_mask

melted_corr <- melt(sig_corr)
colnames(melted_corr) <- c("VarA", "VarB", "Correlation")




# p1 <- 
ggplot(melted_corr[melted_corr$VarB %in% c("VABS-3","SRS-2","BIS"),], aes(x=VarA, VarB, fill = Correlation)) +
  geom_tile(color="black",lwd = .2,
            linetype = 2) +
  scale_fill_gradient2(low = "yellow", high = "red", mid = "white", 
                       midpoint = 0, 
                       limit = c(-.35, .35), space = "Lab", 
                       name="Spearman\nCorrelation") +
  theme_bw() +
  # coord_fixed()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1)) +
  coord_fixed() +
  labs(x = "", y = "", 
       title = "Significant correlations between game measures and survey scores")




# p2 <- 
ggplot(melted_corr[!(melted_corr$VarB %in% c("VABS-3","SRS-2","BIS")),], aes(x=VarA, VarB, fill = Correlation)) +
  geom_tile(color="black",lwd = .2,
            linetype = 2) +
  scale_fill_gradient2(low = "yellow", high = "red", mid = "white", 
                       midpoint = 0, 
                       limit = c(-.35, .35), space = "Lab", 
                       name="Spearman\nCorrelation") +
  theme_bw() +
  # coord_fixed()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1)) +
  coord_fixed() +
  labs(x = "", y = "")

ggarrange(p1,p2, nrow=2,ncol = 2,common.legend = TRUE,legend = "top",align = "h",labels = c("G")) %>%
ggexport(filename = "results/figures/geodems/cross_correlations.pdf")

############################################################################################################
# Load necessary libraries
# library(tidyverse)
# library(FactoMineR)
# library(factoextra)
# install.packages("leaps", type = "source")

# Assuming your data frame is called 'df'
# and has a column 'group' with values 'ASD' or 'TD'

data <- summary2[,c("group","srs2total","vinabcstd","bistotal","aasp_low_reg_raw","aasp_sen_seek_raw","aasp_sen_ses_raw","aasp_sen_avoid_raw",
"perf","trainperf","Slope","k1",
# "Sidebias","WS","LS",
"bs","b1","b2",
"mean_iti"
# ,"mean_rt"
)]


data <- data[complete.cases(data),]
# data$WS <- abs(data$WS)
# data$LS <- abs(data$LS)
# data$Sidebias <- abs(data$Sidebias)

data$bs <- abs(data$bs)
data$b1 <- abs(data$b1)
data$b2 <- abs(data$b2)
setDT(data)


colnames(data) <- c("group","SRS-2","VABS-3","BIS","AASP (Low Registration)","AASP (Sensation Seeking)","AASP (Sensory Sensitivity)","AASP (Sensation Avoiding)",
"Accuracy","Training Accuracy","Psychometric Slope","Perceptual Noise","Side-bias","WS","LS","Mean ITI"
# ,"Mean RT"
)

# Separate the group column and features
features <- data[,-c("group")]
scaled_features <- scale(features)


condition <- data$group

# 2. Perform PCA
pca_result <- prcomp(features, #center = TRUE, 
scale. = TRUE)

# 3. Summary of PCA results
summary(pca_result)
print(pca_result$rotation)

# 4. Calculate variance explained by each component
var_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)
scree_data <- data.frame(PC = 1:length(var_explained), 
                         VarExplained = var_explained)

p1<-
ggplot(scree_data, aes(x = PC*100, y = VarExplained)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  labs(x = "Principal Component", 
       y = "%Variance Explained",
       title = "Scree Plot") +
  scale_x_continuous(breaks = 1:length(var_explained))


pc_scores <- pca_result$x
pca_data <- data.frame(pc_scores, condition = condition)

p2 <- 
ggplot(pca_data, aes(x = PC1, y = PC2, color = condition)) +
  geom_point(alpha = 0.7,size=.5) +
  theme_bw() +
  labs(x = "PC1", y = "PC2", color="",
       title = "PC1 vs PC2 colored by group") +
  scale_color_manual(values = c("ASD" = "red", "TD" = "blue")) +
  # stat_ellipse(level = 0.95)+
  theme(legend.position =c(.8,.8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


loadings <- data.frame(pca_result$rotation)
# loadings$feature <- rownames(loadings)
loadings_sorted <- loadings %>%
  arrange(desc(abs(PC1)))
# loadings_sorted$PC1<- factor(loadings_sorted$PC1, levels = loadings_sorted$PC1)  
loadings_sorted$feature <- rownames(loadings_sorted)
loadings_sorted$feature <- factor(loadings_sorted$feature,levels = loadings_sorted$feature)

# loadings_long <- tidyr::pivot_longer(loadings, 
#                                      cols = c(PC1, PC2), 
#                                      names_to = "PC", 
#                                      values_to = "loading")

loadings_long <- loadings_sorted %>%
  pivot_longer(cols = c(PC1, PC2), names_to = "PC", values_to = "loading")

# ggplot(loadings_long, aes(x = feature, y = Loading, fill = PC)) +
#   geom_col(position = "dodge") +
#   coord_flip() +
#   labs(x = "Variables", y = "Loadings") +
#   theme_minimal()

p3 <- 
ggplot(loadings_long, aes(x = feature, y = abs(loading), fill = PC)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  labs(x = "Features", y = "Loading", fill="",
       title = "Loadings for PC1 and PC2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("PC1" = "#F8766D", "PC2" = "#00BFC4"))+
  theme(legend.position =c(.8,.8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

biplot_data <- data.frame(pca_result$x[,1:2])
biplot_data$condition <- condition

loadings_scaled <- pca_result$rotation[,1:2] * 
                   (max(abs(pca_result$x[,1:2])) / 
                    max(abs(pca_result$rotation[,1:2])))

p4 <- 
ggplot(biplot_data, aes(x = PC1, y = PC2, color = condition)) +
  geom_point(alpha = 0.7,size=.5) +
  geom_segment(data = data.frame(loadings_scaled), 
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.1, "cm")),
          linewidth = 0.5,
               color = "black") +
               xlim(-10,10) + ylim(-10,10) +
  geom_text(data = data.frame(loadings_scaled),
            aes(x = PC1, y = PC2, label = rownames(loadings_scaled)),
            color = "black", vjust = 1, hjust = 1,size=2) +
  theme_bw() +
  labs(x = "PC1", y = "PC2", title = "Biplot",color="") +
  scale_color_manual(values = c("ASD" = "red", "TD" = "blue")) +
  theme(legend.position ="None",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


t_test_pc1 <- t.test(abs(pc_scores[,1]) ~ condition)
print("T-test results for PC1:")
print(t_test_pc1)

t_test_pc2 <- t.test(abs(pc_scores[,2]) ~ condition)
print("T-test results for PC2:")
print(t_test_pc2)

ggarrange(p1,p2,p3, nrow=2,ncol = 2,common.legend = FALSE,legend = "top",
align = "v",labels = c("A","B","C")) %>%
ggexport(filename = "results/figures/geodems/pca.pdf")




rm(p1,p2,p3,p4,scree_data,pc_scores,pca_data,loadings,loadings_long,biplot_data,loadings_scaled)
rm(data,features,condition,pca_result,var_explained)

##### classification analysis #####

library(caret)
library(pROC)
library(reshape2)
set.seed(123)

data <- summary2[,c("group","perf","trainperf","Slope","k1","mean_iti",
"bs","b1","b2",
# "WS","LS","Sidebias",
"aasp_low_reg_raw",
"aasp_sen_seek_raw","aasp_sen_ses_raw","aasp_sen_avoid_raw"
)]

data <- data[complete.cases(data),]
data$group <- as.factor(data$group)

data$bs <- abs(data$bs)
data$b1 <- abs(data$b1)
data$b2 <- abs(data$b2)

# data$WS <- abs(data$WS)
# data$LS <- abs(data$b1)
# data$b2 <- abs(data$b2)


data2 <- data[,c("group","perf","trainperf","Slope","k1","mean_iti",
# "WS","LS","Sidebias"
"bs","b1","b2"
)]
data3 <- data[,c("group",
"aasp_low_reg_raw",
"aasp_sen_seek_raw","aasp_sen_ses_raw","aasp_sen_avoid_raw"
)]

ctrl <- trainControl(method = "cv", number = 5, classProbs = TRUE, summaryFunction = twoClassSummary,savePredictions = "all")
mdl_name = "rf"
# "rf" - Random Forest
# "svmRadial" - Support Vector Machines with Radial Basis Function Kernel
# "glm" - Generalized Linear Model (including Logistic Regression)
# "knn" - k-Nearest Neighbors
# "nb" - Naive Bayes
# "nnet" - Neural Network
# "gbm" - Gradient Boosting Machine
# "rpart" - Decision Trees
# "lda" - Linear Discriminant Analysis
# "glmnet" - Regularized Generalized Linear Models

mdl <-  train(group ~ ., 
                    data = data, 
                    method = mdl_name, 
                    trControl = ctrl,
                    metric = "ROC")

mdl2 <- train(group ~ ., 
                    data = data2, 
                    method = mdl_name, 
                    trControl = ctrl,
                    metric = "ROC")

mdl3 <- train(group ~ ., 
                    data = data3, 
                    method = mdl_name, 
                    trControl = ctrl,
                    metric = "ROC")


roc_obj <- roc(mdl$pred$obs, mdl$pred$ASD)
roc_obj2 <- roc(mdl2$pred$obs, mdl2$pred$ASD)
roc_obj3 <- roc(mdl3$pred$obs, mdl3$pred$ASD)

roc_data1 = data.frame(
      specificity = roc_obj$specificities,
      sensitivity = roc_obj$sensitivities
    )


# list(
    roc_data2 = data.frame(
      specificity = roc_obj2$specificities,
      sensitivity = roc_obj2$sensitivities
    )
    # auc = auc(roc_obj2)
  # )


# list(
    roc_data3 = data.frame(
      specificity = roc_obj3$specificities,
      sensitivity = roc_obj3$sensitivities
    )#,
    # auc = auc(roc_obj3)
  # )


# Combine ROC data
roc_data_all <- rbind(
  cbind(roc_data1, set = "Game + AASP"),
  cbind(roc_data2, set = "Game"),
  cbind(roc_data3, set = "AASP")
)

aucs <- c(auc(roc_obj),auc(roc_obj2),auc(roc_obj3))
print(aucs)




# Plot ROC curves
p4<- ggplot(roc_data_all, aes(x = 1 - specificity, y = sensitivity, color = set)) +
  geom_line(size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  coord_equal() +
  scale_color_brewer(palette = "Set1") +
  labs(x = "False Positive Rate", y = "True Positive Rate", 
      #  title = "ROC Curves for Different Feature Sets (10-fold CV)",
       color = "") +
  theme_bw() +
  # annotate("text", x = 0.75, y = 0.25, 
  #          label = sprintf("AUC Set 1: %.3f\nAUC Set 2: %.3f\nAUC Set 3: %.3f", 
  #                          aucs[1], aucs[2], aucs[3]),
  #          hjust = 0, vjust = 0, size = 3) +
  theme(legend.position ="bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggarrange(p4, nrow=2,ncol = 2,common.legend = FALSE,legend = "top",
align = "hv",labels = c("D")) %>%
ggexport(filename = "results/figures/geodems/classify.pdf")

##### multiple sessions #####
df1 = summary[game_version=="ft" & above_crit ==1,c("username","subID","binary_group","perf")]
df2 = summary[game_version!="ft" & above_crit ==1,c("username","subID","binary_group","perf")]

df2[, subID := gsub("_2$|_3$", "", subID)]

common_ids = intersect(df1$subID, df2$subID)

df1 = df1[subID %in% common_ids]
df2 = df2[subID %in% common_ids]
df2 = df2[!duplicated(df2$subID),]

df2$perf2 <- df2$perf

subs <- union(df1$username,df2$username)

df1 <- df1[,-c("username")]
df2 <- df2[,-c("username","perf")]

df <- merge(df1,df2,by=c("subID","binary_group"),all=TRUE)
rm(df1,df2)

p1 <- 
ggplot(df,aes(x=perf,y=perf2))+
geom_point(aes(color=binary_group),size=.5)+
geom_smooth(method = "lm",se = T,color="black",size=.5,linetype="dashed")+
stat_cor(method = "pearson",label.y.npc = "bottom")+
labs(title="Accuracy",color="",x="First attempt",y="Second attempt")+
scale_color_manual(values = c("ASD" = "red", "TD" = "blue"))+
theme_bw()+
theme(legend.position ="None",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())



df = alldata[username %in% subs] %>%
group_by(username,subID,binary_group,game_version) %>% summarise(mean_rt = mean(rt,na.rm = TRUE),
.groups = "drop")

df$mean_rt <- df$mean_rt/1000

df1 = df[df$game_version=="ft",]
df2 = df[df$game_version!="ft",]
setDT(df1)
setDT(df2)

df2[, subID := gsub("_2$|_3$", "", subID)]

# rename column mean_rt to mean_rt2
colnames(df2)[colnames(df2)=="mean_rt"] <- "mean_rt2"

df1 <- df1[,-c("username","game_version")]
df2 <- df2[,-c("username","game_version")]

# merge the two dataframes on subID and binary_group, remove username
df = merge(df1,df2,by=c("subID","binary_group"),all=TRUE)
rm(df1,df2)

p2 <- 
ggplot(df,aes(x=mean_rt,y=mean_rt2))+
geom_point(aes(color=binary_group),size=.5)+
geom_smooth(method = "lm",se = T,color="black",size=.5,linetype="dashed")+
stat_cor(method = "pearson",label.y.npc = "bottom")+
labs(title="RT (s)",color="",x="First attempt",y="Second attempt")+
scale_color_manual(values = c("ASD" = "red", "TD" = "blue"))+
theme_bw()+
theme(legend.position ="None",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# make common x and y axis in the grid

rm(df)




ggarrange(p1,p2, nrow=3,ncol = 3,common.legend = TRUE,legend = "top",align = "v",labels = c("A")) %>%
ggexport(filename = "results/figures/geodems/multi_session.pdf")
