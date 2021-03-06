# We are going to follow along with a Vignette that explains how to fit response curves using R
# we'll use PEcAN - you'll use PEcAN next week to do some data assimilation
# We will also look at another package written by Duursma


##### Required for PEcAN
#install them
install.packages("devtools")
install.packages("rjags")


#load the packages
library(devtools)


#note you need rjags installed in R and also JAGS (stand alone application) installed on your computer
#note you will also need Rtools installed.

#note you will get an error 
library(rjags)

# #Next Install PEcAN.photosynthesis ... this is not available through CRAN 
# - but you can find it on GITHUB - this code will download and install
### This step might take a while 
if (!require("PEcAn.photosynthesis", character.only = TRUE)) {
  devtools::install_github("PecanProject/pecan/modules/photosynthesis") 
}
knitr::opts_chunk$set(cache = TRUE)
library(PEcAn.photosynthesis)

######
# plantecophys 
# https://cran.r-project.org/web/packages/plantecophys/index.html
#  plantecophys: Modelling and Analysis of Leaf Gas Exchange Data
# Coupled leaf gas exchange model, A-Ci curve simulation and fitting, 
# Ball-Berry stomatal conductance models, leaf energy balance using Penman-Monteith, 
# Cowan-Farquhar optimization, humidity unit conversions.

# IN CASE YOU JUST ABOLUTELY HAVE TO HAVE THE LATEST VERSION
#install_bitbucket("remkoduursma/plantecophys")

#install the package
install.packages("plantecophys")
#load the package
library (plantecophys)
# REFERENCE MANUAL https://cran.r-project.org/web/packages/plantecophys/plantecophys.pdf 


#Some utility packages# if you need to install ... then install them :)
#install.packages("dplyr")
library(dplyr)
#install.packages("tidyr")
library(tidyr)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("grid")
library(grid) #required for 'unit'
#Load data


# You have just downloaded the LICOR files collected by the FLUXCOURSE in 2012 
#... they are included in the PEcAN.photosynthesis Package
#If you want to see the files you can go into your R directory on your computer 
# .... you can find it in the extdata folder within the PEcAN.photosynthesis package
# on my computer it's here : C:\Users\dmoore\Documents\R\R-3.4.0\library\PEcAn.photosynthesis\extdata

# the command system.file() in R allows you to call up folders relevant to a particular package
# This commant looks in the folder, scans for files with aci or aq in the filename and writes these names (and their full path) to the R object "filenames"
## Get list of LI-COR 6400 file names (ASCII not xls)

filenames <- system.file("extdata", paste0("flux-course-",rep(1:6,each=2),c("aci","aq")), package = "PEcAn.photosynthesis")

#filenames is 12 character variables

#These files are licor files ... not CSV files, not XLSX files 
#... just the file that comes right off the machine ... it has no file extension

# PEcAN.photosynthesis contains a command call read_Licor() ...I bet you can guess what it does!

#read_Licor will load an individual licor file ... if you have one. 
## Load files to a list
master = lapply(filenames, read_Licor)

###############
# you can do this with your own data .... you just need to point R to those data and load em up into a big list
# my_data <- "path_to_your_data" # for example: "~/Downloads/FluxCourse_2017_ACi_Curves/07102017PSC0365_CO2response"
# master = lapply(my_data, read_Licor)
# ###############


# The code below performs a set of interactive QA/QC checks on the LI-COR data that's been loaded. 
# Because it's interactive it isn't run when this vignette is knit.
# 
# If you want to get a feel for how the code works you'll want to run it first on just one file, 
# rather than looping over all the files

master[[1]] <- Licor_QC(master[[1]])

