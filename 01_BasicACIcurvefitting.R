
####### this next section uses plantecophys ########
# https://cran.r-project.org/web/packages/plantecophys/index.html
#  plantecophys: Modelling and Analysis of Leaf Gas Exchange Data
# Coupled leaf gas exchange model, A-Ci curve simulation and fitting, 
# Ball-Berry stomatal conductance models, leaf energy balance using Penman-Monteith, 
# Cowan-Farquhar optimization, humidity unit conversions.

# IN CASE YOU JUST ABOLUTELY HAVE TO HAVE THE LATEST VERSION
#install_bitbucket("remkoduursma/plantecophys")

#install the package

#   install.packages("plantecophys")
#load the package
#   library (plantecophys)
# REFERENCE MANUAL https://cran.r-project.org/web/packages/plantecophys/plantecophys.pdf 

#I'm assuming that you have 1 file where the data starts after 18 lines and the headers are on line 16

#Load data
Mydata=read.table("data//filenameforlicorfile",skip = 18,sep="\t", header=FALSE)
print(Mydata)
Mydata_headers=read.table("data//filenameforlicorfile",skip = 16, nrows = 1,
                            stringsAsFactors = FALSE,sep="\t", header=FALSE)

Mydata_columns=as.list(Mydata_headers)
colnames(Mydata)=Mydata_columns
#If you have installed pecan.photosynthesis - you can use read.licor instead 
#Mydata = read.licor("data//filenameforlicorfile")


#Fit an aci curve to the data using measured T values
Sample_aci=fitaci(Mydata, Tcorrect = FALSE)
plot(Sample_aci)

#Summary of the fit
summary(Sample_aci)

#Get Vcmax and Jmax specifically
coef(Sample_aci)

#Compare the modelled A with measured A using a line with slope of 1
with(Sample_aci$df,plot(Amodel,Ameas))
abline(0,1)



#### #### try to do with lots of licor files #### #### 

#tell R where you stored your data
#create objects called filepath & tempFilelist
filepath="./data/" #this is where your Licor Data is stored
tempFilelist = list.files(filepath)

#reading data files using pecan.photosynthesis function read.licor
myfiles = do.call("rbind", lapply(paste0(filepath,tempFilelist), function(x) read.Licor(x)))

# ##Example from Mike Dietze
# ## Get list of LI-COR 6400 file names (ASCII not xls)
# filenames <- system.file("extdata", paste0("flux-course-",rep(1:6,each=2),c("aci","aq")), package = "PEcAn.photosynthesis")
# 
# #create objects called filepath & tempFilelist
# filepath="./data/"

tempFilelist = list.files(filepath,pattern="*.*")

cpath= getwd()
inbetween = "/data/"
licorfiles = paste0(cpath,inbetween,tempFilelist)
licorFiles #prints to screen

#make master list using peacan.photosynthesis tool read.Licor
LicorMaster = lapply(licorFiles, read.Licor)


# Licor QC will read your data and plot graphs that allow you to select points that are not working

#run the QA/QC on just the first file "b2 pop a18 kw 09-18-15" 
LicorMaster[[1]] <- Licor.QC(LicorMaster[[1]], curve = "ACi")

#for all the files in KarenMaster
for(i in 1:length(LicorMaster)){
  LicorMaster[[i]] = Licor.QC(LicorMaster[[i]],curve = "ACi")
}

#drawing some plots with GGPLOT2 --- you can plot your own data using ggplot

#sets up the type of plot
aci_plot <- ggplot(data=myfiles, aes(x=Ci, y=Photo))


#this draws all the curves on one plot
aci_plot + 
  geom_point(colour="black", size = 2.5) +
  theme_classic() +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=22,face="bold")) + 
  theme(panel.border = element_blank(), axis.line = element_line(colour="black", size=2, lineend="square"))+
  theme(axis.ticks = element_line(colour="black", size=2, lineend="square"))+
  ylab("Assimilation (umol/m2/sec)")+
  xlab("Ci")

#this draws each curve in a different frame using "facet_wrap"
aci_plot + facet_wrap(~ fname) + 
  geom_point(colour="black", size = 2.5) +
  theme_classic() +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=22,face="bold")) + 
  theme(panel.border = element_blank(), axis.line = element_line(colour="black", size=2, lineend="square"))+
  theme(axis.ticks = element_line(colour="black", size=2, lineend="square"))+
  ylab("Assimilation (umol/m2/sec)")+
  xlab("Ci")


#Take you master file of all the ACI curves - put it in a data frame
df <- data.frame(matrix(unlist(LicorMaster), nrow=29, byrow=T))
LicorMasterDF=do.call(rbind.data.frame, LicorMaster)

#using fitacis from plantecophys to fit all curves at once.
Myacis= fitacis(LicorMasterDF, "fname")

#plotting your ACI curves using plantecophys 
plot(Myacis, how="manyplots")

#Get Vcmax and Jmax 
coef(Myacis)


