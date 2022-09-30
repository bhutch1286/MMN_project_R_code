
#script requires eegUtils installed
#use: remotes::install_github("craddm/eegUtils@develop")

library(eegUtils)
library(R.matlab)
library(hdf5r)
library(readxl)
library(ggplot2)


#-----------which do you want to plot? 0 for MMN, 1 for P3a

############
to_plot <- 0
############

#------------------------------------------------------------------------------#


if (to_plot == 0){
  erp_to_plot <- "mmn_regular_bins.xlsx"
} else {
  erp_to_plot <- "p3a_regular_bins.xlsx"
}

sheet_number <- length(excel_sheets(paste("C:/Users/blu12/OneDrive/Documents/Flinders work/Project 2 MMN/plots/topo_plots", erp_to_plot, sep = "")))
topo_spreadsheet_str <- paste("C:/Users/blu12/OneDrive/Documents/Flinders work/Project 2 MMN/plots/topo_plots", erp_to_plot, sep = "")

output_list <- list()
for (i in 1:sheet_number) {
  df <- read_excel(topo_spreadsheet_str, sheet=i)
  output_list[[i]] <- assign(paste("bin_", i, sep=""), df)
}

output_list_2 <- list()
for (i in 1:sheet_number) {
  df <- output_list[[i]]
  df <- electrode_locations(data = df, montage = "biosemi32")
  output_list_2[[i]] <- assign(paste("bin_", i, sep=""), df)
}

######NOTE: if you want to automate generating and saving each plot, wrap each topoplot() with the following:

#setwd("C:/Documents/ProjectMMN/plots") #set to where you want plot to be saved
#svg("bin_X.svg", width=15, height = 7.5) #name of plot + set its width and height, where X = bin number
#topoplot() plotting function goes here
#dev.off()
#setwd("C:/Documents") #return working directory to normal [only use ONCE after the final topoplot call if generating multiple plots]

#size: 550 x 601 seems good
topoplot(bin_1,
         palette = "RdBu", 
         limits = c(-0.75,0.75),
         contour=F,
         r = 105,
         grid_res=300,
         interp_limit="head",
         chan_marker="point",
         scaling=1) + 
  ggtitle("A)")  + xlab("Punish") +
  theme(legend.text = element_text(size=8), 
        legend.position = c(5,0.5), 
        axis.title.x = element_text(colour="black", size=rel(1), face="plain"),
        title = element_text(size=rel(2), face="bold"))

topoplot(bin_2,
         palette = "RdBu", 
         limits = c(-0.75,0.75),
         contour=F,
         r = 105,
         grid_res=300,
         interp_limit="head",
         chan_marker="point",
         scaling=1) + 
  ggtitle("B)")  + xlab("Reward") +
  theme(legend.text = element_text(size=8), 
        legend.position = c(5,0.5), 
        axis.title.x = element_text(colour="black", size=rel(1), face="plain"),
        title = element_text(size=rel(2), face="bold"))

topoplot(bin_3,
         palette = "RdBu", 
         limits = c(-0.75,0.75),
         contour=F,
         r = 105,
         grid_res=300,
         interp_limit="head",
         chan_marker="point",
         scaling=1) + 
  ggtitle("C)")  + xlab("Standard") +
  theme(legend.text = element_text(size=8), 
        legend.position = c(5,0.5), 
        axis.title.x = element_text(colour="black", size=rel(1), face="plain"),
        title = element_text(size=rel(2), face="bold"))

topoplot(bin_4,
         palette = "RdBu", 
         limits = c(-0.75,0.75),
         contour=F,
         r = 105,
         grid_res=300,
         interp_limit="head",
         chan_marker="point",
         scaling=1) + 
  ggtitle("D)")  + xlab("Punish control") +
  theme(legend.text = element_text(size=8), 
        legend.position = c(5,0.5), 
        axis.title.x = element_text(colour="black", size=rel(1), face="plain"),
        title = element_text(size=rel(2), face="bold"))

topoplot(bin_5,
         palette = "RdBu", 
         limits = c(-0.75,0.75),
         contour=F,
         r = 105,
         grid_res=300,
         interp_limit="head",
         chan_marker="point",
         scaling=1) + 
  ggtitle("E)")  + xlab("Reward control") +
  theme(legend.text = element_text(size=8), 
        legend.position = c(5,0.5), 
        axis.title.x = element_text(colour="black", size=rel(1), face="plain"),
        title = element_text(size=rel(2), face="bold"))

topoplot(bin_6,
         palette = "RdBu", 
         limits = c(-0.75,0.75),
         contour=F,
         r = 105,
         grid_res=300,
         interp_limit="head",
         chan_marker="point",
         scaling=1) + 
  ggtitle("F)")  + xlab("Standard control") +
  theme(legend.text = element_text(size=8), 
        legend.position = c(5,0.5), 
        axis.title.x = element_text(colour="black", size=rel(1), face="plain"),
        title = element_text(size=rel(2), face="bold"))
