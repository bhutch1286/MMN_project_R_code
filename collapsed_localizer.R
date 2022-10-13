library(readxl)
library(dplyr)
library(rstatix)
library(reshape2)
library(ggplot2)

#-----------which do you want to run? 0 for MMN, 1 for P3a

############
analyze <- 0
############

#------------------------------------------------------------------------------#
#-------------------data preparation and preprocessing-------------------------#
#------------------------------------------------------------------------------#

#------importing data

if (analyze == 1){
  look_in <- "MMN triggerfix no blink rejection/"
} else {
  look_in <- "p3a triggerfix no blink rejection/"
}


sheet_number <- length(excel_sheets(paste("G:/Flinders work/Project 2 MMN/project-2-pipeline/grand averages/", look_in, "RawOutput_PIDbySamples.xlsx", sep = "")))
spreadsheet_str <- paste("G:/Flinders work/Project 2 MMN/project-2-pipeline/grand averages/", look_in, "RawOutput_PIDbySamples.xlsx", sep = "")

spreadsheet_list <- list()
for (i in 1:sheet_number) {
  df <- read_excel(spreadsheet_str, sheet=i)
  spreadsheet_list[[i]] <- assign(paste("bin_", i, sep=""), df)
}

bin_list <- list(bin_1, bin_2, bin_3, bin_4, bin_5, bin_6)

bin_output_list <- list()
for (q in 1:length(bin_list)) {
  df <- bin_list[[q]][-c(1),] #remove first row in spreadsheet
  df<- dplyr::rename(df, PID = Times) #rename first column to 'PID'
  df <- na.omit(df) #remove subjects with NA's
  bin_output_list[[q]] <- df #place edited spreadsheet into list for later extraction
}

bin_1 <- bin_output_list[[1]] #punish deviant
bin_2 <- bin_output_list[[2]] #reward deviant
bin_3 <- bin_output_list[[3]] #standard value
bin_4 <- bin_output_list[[4]] #punish control
bin_5 <- bin_output_list[[5]] #reward control
bin_6 <- bin_output_list[[6]] #standard control


#------preparing data

#-difference waves

#the following works because R does element wise for normal operators (but need to remove ID column first)
punish = bin_1[,-c(1)] - bin_3[,-c(1)] 
reward = bin_2[,-c(1)] - bin_3[,-c(1)]
punishctrl = bin_4[,-c(1)] - bin_6[,-c(1)]
rewardctrl = bin_5[,-c(1)] - bin_6[,-c(1)]

#-collapsed localizer (averaging all difference waves together)

collapsed_localizer <- (punish + reward + punishctrl + rewardctrl) / 4 #average together each condition (punish, reward, punish control, reward control)
collapsed_localizer[43,] <- colMeans(collapsed_localizer) #create new row which averages over each row (i.e. participant)

#------finding peak value and time

if (analyze == 1){
  peak_value <- min(collapsed_localizer[43,]) #find the maximum negative voltage value across columns (i.e. time points) for MMN peak
} else {
  peak_value <- max(collapsed_localizer[43,]) #find the maximum positive voltage value across columns (i.e. time points) for p3a peak
}

peak_time <- which(apply(collapsed_localizer, 2, function(x) any(grepl(peak_value, x)))) #find what column (time point) peak value is located
peak_time <- names(peak_time)

#------output

if (analyze == 1){
  MMN_peak_time <- c(peak_time, peak_value)
  write.csv(collapsed_localizer, 'MMN_cl.csv')
  write.csv(MMN_peak_time, 'CL_method_MMN_peak.csv')
} else {
  p3a_peak_time <- c(peak_time, peak_value)
  write.csv(collapsed_localizer, 'p3a_cl.csv')
  write.csv(p3a_peak_time, 'CL_method_p3a_peak.csv')
}
