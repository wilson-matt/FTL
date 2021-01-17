#Create 3 axis "ichthyograph" scatterplot
#based on discharge, temp, and moonphase data from Cedar River, WA

#Dependencies-----
library(plot3D)
library(misc3d)
library(openxlsx)
library(readxl)
library(tidyverse)

ichthy <- read_excel(("Data/HypotheticalIchthyograph.xlsx"), 
                     sheet = "FTL")

# x, y and z coordinates
x <- ichthy$Temperature
y <- ichthy$Discharge
z <- ichthy$Moonphase

#Colorguide:
# "#e3e309" = yellow, "#0982ba" = blue, "#1c0582" = purple

plot3D::scatter3D(x, y, z, pch = 21, cex = 1.5,
          col.var = as.integer(ichthy$LifePhase), 
          col = c("#e3e309", "#0982ba", "#1c0582"),
          ticktype = "detailed",
          colkey = list(at = c(2, 3, 4), side = 1, 
                        addlines = TRUE, length = 0.5, width = 0.5,
                        labels = c("Smolt", "Fry", "Adult")) )
