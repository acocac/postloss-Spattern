###############################################################################################
# title         : assessment of Spatial Pattern of Deforested Areas using Datamining techniques;
# purpose       : map deforestation spatial patterns from available 
#                 datasets using data mining techniques;
# producer      : prepared by A. Coca;
# last update   : in London, UK June 2015 / v1 in September 2015 / v2 in October 2016;
# remarks 1     : steps documentation can be accessed in the weblink:  
#                 analysis parts must be run separately;
#                 this is a proof of concept tool so in case of identifying any bug please 
#                 report it to acocac@gmail.com;
################################################################################################

### clean workspace ###
rm(list = ls())
##### end clean ####

### R codes workspace ### #copy the full dir to R scripts#
r.dir = readClipboard()
### end R codes workspace ### 

### Config dir ### #copy the full dir to config file#
conf.file = paste0(readClipboard(),"\\conf.txt")
### end R codes workspace ###

### Install and load R-Libraries ### 
#### libraries ####
pckg = c("caret","car","data.table","doParallel","dplyr","devtools","GGally","ggplot2",
         "grid","gridExtra","maptools", "nnet", "NeuralNetTools", "parallel", "PerformanceAnalytics", 
         "raster","reshape", "rgdal", "rgeos", "rJava","RWeka", "sampling","sp", "snowfall") 

usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

lapply(pckg,usePackage)
##### end libraries ####

### Config dir to temporaster ###  http://stackoverflow.com/questions/25426405/raster-package-taking-all-hard-drive
# pc.name = "server66"
# raster.tmp = paste0(dirname(conf.file),"/temp",pc.name)
# dir.create(raster.tmp, recursive = T, mode = "0777", showWarnings = F)
# rasterOptions(tmpdir = raster.tmp)
### end R codes workspace ###

###################################################
####PART 1 - Create project and fishnet files #####
###################################################

#### load ####
## module ##
source(paste(r.dir,"/modules/1_CreateProjectAndFishnets.R", sep=""))
### functions ##
source(paste(r.dir,"/functions/1_UserArgs.R", sep=""))
### end load ###

{
### user interaction ###
args.part1 = prompt.user.part1()
### end user iteration ###

### run submodule function ###
CreateProjectAndFishnets(args.part1[[1]],args.part1[[2]])
### end run submodule function ###
}

###################################################
####PART 2 - Generate individual GeoTIFF files ####
###################################################

#### load ####
## module ##
source(paste(r.dir,"/modules/2_ExtractUAParallel.R", sep=""))
### functions ##
source(paste(r.dir,"/functions/2a_UserArgs.R", sep=""))
source(paste(r.dir,"/functions/2b_GenUniGrid.R", sep=""))
### end load ###

{
### user interaction ###
args.part2 = prompt.user.part2()
### end user iteration ###

### run submodule function ###
ExtractUA(args.part2[[1]],args.part2[[2]],args.part2[[3]],args.part2[[4]],args.part2[[5]])
### end run submodule function ###
}

###################################################
####PART 3 - Extract FRAGSTAT-like metrics    #####
###################################################

#### load ####
## module ##
source(paste(r.dir,"/modules/3_FragstatExtractionParallel.R", sep=""))
### functions ##
source(paste(r.dir,"/functions/3a_UserArgs.R", sep=""))
source(paste(r.dir,"/functions/3b_RunFragstat.R", sep=""))
### end load ###

{
  ### user interaction ###
  args.part3 = prompt.user.part3()
  ### end user iteration ###
  
  ### run submodule function ###
  FragstatExtraction(args.part3[[1]],args.part3[[2]],args.part3[[3]],args.part3[[4]])
  ### end run submodule function ###
}

###################################################
####PART 4 - Create training set              #####
###################################################

#### load ####
## module ##
source(paste(r.dir,"/modules/4_CreateTrainingSet.R", sep=""))
### functions ##
source(paste(r.dir,"/functions/4_UserArgs.R", sep=""))
### end load ###

### user interaction ###
args.part4 = prompt.user.part4()
### end user iteration ###

### run submodule function ###
do.call("TrainingSet",args.part4)
### end run submodule function ###

###################################################
####PART 5a - Modelling: Exploratory Analysis #####
###################################################

#### load ####
## module ##
source(paste(r.dir,"/modules/5a_ModExploratory.R", sep=""))
### functions ##
source(paste(r.dir,"/functions/5a_UserArgs.R", sep=""))
### end load ###

### user interaction ###
args.part5a = prompt.user.part5a()
### end user iteration ###

### run submodule function ###
do.call("ModExploratory",args.part5a)
### end run submodule function ###

#########################################################
####PART 5b - Modelling: Run Data Mining Algorithms #####
#########################################################

#### load ####
## module ##
source(paste(r.dir,"/modules/5b_ModRunDM.R", sep=""))
### functions ##
source(paste(r.dir,"/functions/5b_UserArgs.R", sep=""))
### end load ###

### user interaction ###
args.part5b = prompt.user.part5b()
### end user iteration ###

### run submodule function ###
do.call("ModRunDM",args.part5b)
### end run submodule function ###

#########################################################
####PART 5c - Modelling: Identify Best DM Models    #####
#########################################################

#### load ####
## module ##
source(paste(r.dir,"/modules/5c_ModBestDM.R", sep=""))
### functions ##
source(paste(r.dir,"/functions/5c_UserArgs.R", sep=""))
### end load ###

{
### user interaction ###
args.part5c = prompt.user.part5c()
### end user iteration ###

### run submodule function ###
ModBestDM(args.part5c[[1]])
### end run submodule function ###
}

#########################################################
####PART 5d - Modelling: Sensitivity analyses       #####
#########################################################

#### load ####
## module ##
source(paste(r.dir,"/modules/5d_ModSenAna.R", sep=""))
### functions ##
source(paste(r.dir,"/functions/5d_UserArgs.R", sep=""))
### end load ###

{
### user interaction ###
args.part5d = prompt.user.part5d()
### end user iteration ###

### run submodule function ###
ModSenAna(args.part5d[[1]],args.part5d[[2]],args.part5d[[3]])
###end run submodule function ###
}

#########################################################
####PART 5e - Modelling: Model use                  #####
#########################################################

#### load ####
## module ##
source(paste(r.dir,"/modules/5e_ModUse.R", sep=""))
### functions ##
source(paste(r.dir,"/functions/5e_UserArgs.R", sep=""))
### end load ###

{
### user interaction ###
args.part5e = prompt.user.part5e()
### end user iteration ###

### run submodule function ###
ModUse(args.part5e[[1]],args.part5e[[2]],args.part5e[[3]])
###end run submodule function ###
}

#########################################################
####PART 5f - Model evaluation                      #####
#########################################################

#### load ####
## module ##
source(paste(r.dir,"/modules/5f_ModEval.R", sep=""))
### functions ##
source(paste(r.dir,"/functions/5f_UserArgs.R", sep=""))
### end load ###

{
### user interaction ###
args.part5f = prompt.user.part5f ()
### end user iteration ###

### run submodule function ###
ModEval(args.part5f[[1]],args.part5f[[2]],args.part5f[[3]],args.part5f[[4]],args.part5f[[5]])
###end run submodule function ###
}
