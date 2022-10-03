library(readxl)
library(dplyr)
library(rstatix)
library(reshape2)
library(ggplot2)

#------------------------------------------------------------------------------#
#-------------------data preparation and preprocessing-------------------------#
#------------------------------------------------------------------------------#

#Note: the below should work for importing/cleaning any ERP data outputted from Oren's pipeline.
# All that you need to change is the 'dataset_name' to tell it which folder to look into

# Currently I've set it up to take separately data for mismatch negativity and P3a

#------importing data

dataset_name <- "_collapsed_localizer" #input here the name of the folder of whatever dataset you want to look at (minus the 'MMN' or 'p3a' part, which below takes care of)
dataset_name_v2 <- dataset_name

#---MMN
component_to_analyze <- "MMN"
dataset_name <- paste(component_to_analyze, dataset_name, "/", sep="")
sheet_number <- length(excel_sheets(paste("G:/Flinders work/Project 2 MMN/project-2-pipeline/grand averages/", dataset_name, "Processed_meanVoltagePerPerson.xlsx", sep = "")))
spreadsheet_str <- paste("G:/Flinders work/Project 2 MMN/project-2-pipeline/grand averages/", dataset_name, "Processed_meanVoltagePerPerson.xlsx", sep = "")
bin_vars <- c('MMN_Punish_Dev', 'MMN_Reward_Dev', 'MMN_Standard_Val', 'MMN_Punish_Ctrl', 'MMN_Reward_Ctrl', 'MMN_Standard_Ctrl')

spreadsheets <- list()
for (i in 1:sheet_number) {
  df <- read_excel(spreadsheet_str, sheet=i)
  spreadsheets[[i]] <- assign(bin_vars[[i]], df)
}

#---p3a
component_to_analyze <- "p3a"
dataset_name_v2 <- " triggerfix no blink rejection"
dataset_name <- paste(component_to_analyze, dataset_name_v2, "/", sep="")
spreadsheet_str <- paste("G:/Flinders work/Project 2 MMN/project-2-pipeline/grand averages/", dataset_name, "Processed_meanVoltagePerPerson.xlsx", sep = "")
bin_vars <- c('P3_Punish_Dev', 'P3_Reward_Dev', 'P3_Standard_Val', 'P3_Punish_Ctrl', 'P3_Reward_Ctrl', 'P3_Standard_Ctrl')

spreadsheets <- list()
for (i in 1:sheet_number) {
  df <- read_excel(spreadsheet_str, sheet=i)
  spreadsheets[[i]] <- assign(bin_vars[[i]], df)
}

#------data preparation

subject <- MMN_Punish_Dev$PID

#MMN difference wave (mean amplitude)
MMN_Punish <- MMN_Punish_Dev$MeanVoltage - MMN_Standard_Val$MeanVoltage #bin 1 - bin 3
MMN_Rew <- MMN_Reward_Dev$MeanVoltage - MMN_Standard_Val$MeanVoltage #bin 2 - bin 3
MMN_PunCtrl <- MMN_Punish_Ctrl$MeanVoltage - MMN_Standard_Ctrl$MeanVoltage #bin 4 - bin 6
MMN_RewCtrl <- MMN_Reward_Ctrl$MeanVoltage - MMN_Standard_Ctrl$MeanVoltage #bin 5 - bin 6

#p3a difference wave (mean amplitude)
P3_Punish <- P3_Punish_Dev$MeanVoltage - P3_Standard_Val$MeanVoltage
P3_Rew <- P3_Reward_Dev$MeanVoltage - P3_Standard_Val$MeanVoltage
P3_PunCtrl <- P3_Punish_Ctrl$MeanVoltage - P3_Standard_Ctrl$MeanVoltage
P3_RewCtrl <- P3_Reward_Ctrl$MeanVoltage - P3_Standard_Ctrl$MeanVoltage


df <- data.frame(subject,
                 MMN_Punish,
                 MMN_Rew,
                 MMN_PunCtrl,
                 MMN_RewCtrl,
                 P3_Punish,
                 P3_Rew,
                 P3_PunCtrl,
                 P3_RewCtrl)
df <- na.omit(df)

df_long <- reshape2::melt(df,
                          id.vars = c("subject"),
                          measure.vars = c("MMN_Punish", "MMN_Rew", "MMN_PunCtrl", "MMN_RewCtrl",
                                           "P3_Punish", "P3_Rew", "P3_PunCtrl", "P3_RewCtrl"),
                          variable.name = "value", 
                          value.name = "amplitude")

df_long$ERP <- case_when(
  df_long$value == "MMN_Punish" | df_long$value == "MMN_Rew" | df_long$value == "MMN_PunCtrl" | df_long$value == "MMN_RewCtrl" ~ "MMN",
  df_long$value == "P3_Punish" | df_long$value == "P3_Rew" | df_long$value == "P3_PunCtrl" | df_long$value == "P3_RewCtrl" ~ "P3a",
  TRUE            ~ "nope")

levels(df_long$value)
renaming <- c("punish", "reward",
              "punish_control", "reward_control",
              "punish", "reward",
              "punish_control", "reward_control")
levels(df_long$value) <- renaming

factors <- c("subject", "value", "ERP")
df_long[,factors] <- lapply(df_long[,factors], factor)
str(df_long)

df_long$valence <- case_when(
  df_long$value == "punish" | df_long$value == "punish_control" ~ "negative",
  df_long$value == "reward" | df_long$value == "reward_control" ~ "positive",
  TRUE            ~ "nope")

df_long$associated_value <- case_when(
  df_long$value == "punish" | df_long$value == "reward" ~ "valued",
  df_long$value == "punish_control" | df_long$value == "reward_control" ~ "non-valued",
  TRUE            ~ "nope")




#-------------------------------Analysis---------------------------------------#

df.MMN <- df_long[!df_long$ERP=="P3a",]
df.MMN.aov <- anova_test(data = df.MMN,
                         dv = amplitude,
                         wid = subject,
                         within = c(valence, associated_value))
df.MMN.aov

df.p3a <- df_long[!df_long$ERP=="MMN",]
df.p3a.aov <- anova_test(data = df.p3a,
                         dv = amplitude,
                         wid = subject,
                         within = c(valence, associated_value))
df.p3a.aov

p3a.pwc <- df.p3a %>% 
  pairwise_t_test(amplitude ~ valence,
                  p.adjust.method = "bonferroni", paired = TRUE, detailed=TRUE)
p3a.pwc




#-------------------------------Plots------------------------------------------#

df.MMN %>%
  filter(amplitude >= -0.8 & amplitude <= 2.6) %>%
  ggplot() +
  aes(x = valence, y = amplitude) +
  geom_boxplot(fill = "#112446") +
  ggthemes::theme_par()


df.p3a %>%
  filter(amplitude >= -0.8 & amplitude <= 2.6) %>%
  ggplot() +
  aes(x = valence, y = amplitude) +
  geom_boxplot(fill = "#EF562D") +
  ggthemes::theme_par()



#---------------exclusions [checking exclusions as Alycia]

df.MMN$exclude <- df.MMN$subject == 4 |
  df.MMN$subject == 8 |
  df.MMN$subject == 35 |
  df.MMN$subject == 45 
df.MMN_excl <- df.MMN[df.MMN$exclude == FALSE,]

df.MMN_excl.aov <- anova_test(data = df.MMN_excl,
                              dv = amplitude,
                              wid = subject,
                              within = c(valence, associated_value))
df.MMN_excl.aov


df.p3a$exclude <- df.p3a$subject == 4 |
  df.p3a$subject == 8 |
  df.p3a$subject == 35 |
  df.p3a$subject == 45 
df.p3a_excl <- df.p3a[df.p3a$exclude == FALSE,]

df.p3a_excl.aov <- anova_test(data = df.p3a_excl,
                              dv = amplitude,
                              wid = subject,
                              within = c(valence, associated_value))
df.p3a_excl.aov
