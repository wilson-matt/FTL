#Create 3 axis "ichthyograph" scatterplot
#based on discharge, temp, and moonphase data from Cedar River, WA

#Dependencies-----
library(plot3D)
library(misc3d)
library(openxlsx)
library(readxl)
library(tidyverse)
library(ggplot2)
library(viridis)

ichthy <- read_excel(("Data/HypotheticalIchthyograph.xlsx"), 
                     sheet = "FTL")

# x, y and z coordinates
x <- ichthy$Temperature
y <- ichthy$Discharge
z <- ichthy$Moonphase


#Colorguide:
# "#e3e309" = yellow, "#0982ba" = blue, "#1c0582" = purple

?scatter3D
par("mar")
#force viewing window - it was not plotting properly:
par(mar=c(3,1,1,1))
#plot
scatter3D(x, y, z, pch = 16, cex = 1, theta = 45, #theta for rotating to best view
          xlab = "Temperature",
          ylab ="Discharge", 
          zlab = "Moon Phase",
          col.var = as.factor(ichthy$LifePhase), 
          col = c("#e3e309", "#0982ba", "#AA4371"),
          ticktype = "detailed",
          colkey = list(at = c(16.5, 50, 83.5), side = 1, #the range is 1:100 for sum unknown reason.
              addlines = TRUE, length = 0.5, width = 0.5,
              labels = c("Smolt", "Fry", "Adult")) )


#Scatter plot: Flow ---------------------------------------------------------------
flow <- ggplot(ichthy,aes(x = Discharge,
                              y = Abundance,
                              fill=LifePhase)) +
  geom_point(size = 5,shape = 21, colour = "black",position=position_dodge2(0.2)) +
  scale_colour_viridis_c()+
  ylab(expression(paste(bold("Fish Abundance")))) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(face = "bold"))
#axis.text.x = element_text(angle=90))

flow+labs(fill="LifePhase")

ggsave(file=paste0("Figures/","Flow Ichthyograph",
                   ".png"), 
       flow+labs(fill="LifePhase"), width = 6.5, 
       height = 4.5, units = "in", dpi=300)
