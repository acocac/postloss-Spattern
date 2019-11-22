#############################################################################
# title         : training dataset for running NN;
# purpose       : create training dataset for running NN;
# producer      : prepared by A. Coca;
# last update   : in London, UK Abr 2015 / Updated in July 2015;
# inputs        : merge file (in RData format) with FRAGSTAT outputs (.class,.land);
# outputs       : training dataset with ID 
# remarks 1     : N/A
##############################################################################

ModExploratory = function(dataset, wsize, check_source)
{
  if (missing(dataset))
    stop("Need to specify dataset name.")
  
  if (missing(wsize))
    stop("Need to specify the fishnet size")
  
  if (missing(check_source))
    stop("Need to specify/recheck if labelled data are collected by a single period or multiperiod raster(s) -type single or multi-")
  
  #### libraries ####
  pckg = c("data.table","PerformanceAnalytics","GGally","caret") 
  
  usePackage <- function(p) {
    if (!is.element(p, installed.packages()[,1]))
      install.packages(p, dep = TRUE)
    require(p, character.only = TRUE)
  }
  
  lapply(pckg,usePackage)
  ##### end libraries ####
  
  ### functions ##
  r.dir <- gsub("\\\\", "/", r.dir)
  source(paste(r.dir,"/functions/0_LoadConfig.R", sep=""))
  ### end load ###
  
  ## paths ##
  #load conf#
  conf.args = LoadConfig(conf.file)
  #associated config info to code variables
  root.dir = conf.args[[1]]
  date.ini = conf.args[[2]]
  date.end = conf.args[[3]]
  proj.acronym = conf.args[[4]]
  class.metrics = conf.args[[9]]
  land.metrics = conf.args[[10]]

  ## load ##
  if (check_source == "multi"){
    #project output plot path
    project.plots.path = paste(root.dir,"/output/results/det_all/traindata/projects/",dataset,"_",wsize,"/plots",sep="")
    dir.create(project.plots.path, showWarnings = TRUE, recursive = FALSE)
      
    ## load train set ##
    db.train = read.csv(paste(root.dir,"/output/results/det_all/traindata/projects/",dataset,"_",wsize,"/",dataset,"_",wsize,"_training.csv",sep=""),header = T)
  } else {
    #project output plot path
    project.plots.path = paste(root.dir,"/output/results/det_",date.ini,"to",date.end,"/processed/projects/",dataset,"_",wsize,"/plots",sep="")
    dir.create(project.plots.path, showWarnings = TRUE, recursive = FALSE)
    
    ## load ##
    ## load train set ##
    db.train = read.csv(paste(root.dir,"/output/results/det_",date.ini,"to",date.end,"/processed/projects/",dataset,"_",wsize,"/",dataset,"_",wsize,"_training.csv",sep=""),header = T)
  }
  
  ##### processing ####
  ### Section 1: Data as numeric and select final predictors
  #variables to select
  #class level
  if (all(class.metrics == "all")){ 
    metrics = colnames(db.train)[grep("c_",colnames(db.train))]
    sel.var.class = paste("^",metrics,"$",sep="",collapse ="|")
  } else if (all(class.metrics != "NA" & class.metrics != "all")){
    sel.var.class = paste("^c_",class.metrics,"$",sep="",collapse ="|")
  } else if (all(class.metrics == "NA")){
    sel.var.class = NULL
  }
  #land level
  if (all(land.metrics == "all")){ 
    metrics = colnames(db.train)[grep("l_",colnames(db.train))]
    sel.var.land = paste("^",metrics,"$",sep="",collapse ="|")
  } else if (all(land.metrics != "NA" & land.metrics != "all")){
    sel.var.land = paste("^l_",land.metrics,"$",sep="",collapse ="|")
  } else if (all(land.metrics == "NA")){
    sel.var.land = NULL
  }
  
  sel.var = paste(sel.var.class,sel.var.land,collapse ="|")
  sel.var = gsub(" ","",sel.var)
  
  #filter using selected variables identified for all datasets
  filter = grepl(sel.var, names(db.train))
  patTrainX = db.train[, filter]
  #preprocessing input data before modelling
  patTrainX.m = preProcess(patTrainX, c("range"))
  patTrainX.n = predict(patTrainX.m,patTrainX)
  patTrainX.n[,nearZeroVar(patTrainX.n)] = NULL

  ################################################################################
  ### Section 2: order and rename columns names (variables)
  db.train.all = data.frame("pattern" = db.train$pattern,"wsize"=wsize,patTrainX.n)
  vars.target <- -grep('^(wsize|pattern)', names(db.train.all))
  
  ################################################################################
  ### Section 3: Exploratory analysis
  # plot the data
  png(file = paste(project.plots.path,"/","train_correlationmatrix","_",dataset,"_",wsize,".png",sep=""), width = 1000, height = 700)
  chart.Correlation(db.train.all[,vars.target], bg=seq(1:length(unique(db.train.all$pattern)))[db.train.all$pattern], pch=21)
  par(xpd=TRUE)
  legend(0, 1, as.vector(unique(db.train.all$pattern)), fill=seq(1:4))
  dev.off()
  ##### end processing ####
  
  ##### output messages ####
  cat(paste("### RESULT: Exploratory plots created from the ",dataset," train set using the ",wsize,"m fishnet were successfully added to the project folder! ###",sep=""))
  ##### end output messages ####
}