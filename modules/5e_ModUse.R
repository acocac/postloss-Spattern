#############################################################################
# title         : training dataset for running NN;
# purpose       : create training dataset for running NN;
# producer      : prepared by A. Coca;
# last update   : in London, UK Abr 2015 / Updated in July 2015;
# inputs        : merge file (in RData format) with FRAGSTAT outputs (.class,.land);
# outputs       : training dataset with ID 
# remarks 1     : N/A
##############################################################################

ModUse = function(dataset, wsize, DMModel)
{
  if (missing(dataset))
    stop("Need to specify dataset name.")
  
  if (missing(wsize))
    stop("Need to specify the fishnet size")
  
  if (missing(DMModel))
    stop("Need to specify the data mining model type for the sensitivity analysis")
  
  #### libraries ####
  pckg = c("caret", "rgdal", "maptools") 
  
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

  #projects root path
  projects.path =paste(root.dir,"/output/results/det_",date.ini,"to",date.end,"/processed/projects",sep="")
  
  #input dir where detection fishnet files (window sizes) will be kept
  fishnet.dir = paste(root.dir,"/","input/geodata/shp/target/fishnet/",proj.acronym,sep="")
  
  #output tb classification path
  output.path = paste(projects.path,"/",dataset,"_",wsize,"/results",sep="")
  dir.create(output.path, recursive = T, mode = "0777", showWarnings = F)
  
  #output shp path
  outshp.path = paste(root.dir,"/output/results/det_",date.ini,"to",date.end,"/geodata/shp/classification",sep="") 
  dir.create(outshp.path, recursive = T, mode = "0777", showWarnings = F)
  
  #analysis plots output
  analysis.plots.path = paste(root.dir,"/output/results/det_",date.ini,"to",date.end,"/analysis/plots",sep="")
  dir.create(analysis.plots.path, recursive = T, mode = "0777", showWarnings = F)

  ## load ##
  ## load ##
  if (DMModel == "C45_models"){
    get(load(paste(projects.path,"/",dataset,"_",wsize,"/model/",DMModel,".rda",sep="")))
  } else if (DMModel == "ANNs_models") {
    get(load(paste(projects.path,"/",dataset,"_",wsize,"/model/",DMModel,".RData",sep=""))) 
  }
  
  fishnet <- readOGR(dsn=fishnet.dir, layer=paste("fishnet_",wsize,sep=""))
  ##### end static ####
  
  ##### processing ####
  ###############################################################################
  ### Section 1: Model predictions over unlabelled set
  #prediction unlabelled dataset to score
  pattern.unlabelled.pred.raw <- predict(fit.model, newdata=db.unlabelled.model, type="raw")
  
  ################################################################################
  ### Section 2: generate classified database for visualisation and inspection
  db.training.classified=cbind("CELLID" = db.train.model$CELLID, as.character(db.train.model$pattern))
  db.unlabelled.classified=cbind("CELLID" = as.character(db.unlabelled$CELLID), as.character(pattern.unlabelled.pred.raw))
  db.classified = data.frame(rbind(db.training.classified,db.unlabelled.classified))
  colnames(db.classified)=c("CELLID","pattern")
  
  # ################################################################################
  ### Section 3: Join classified database with fishnet layer for visualisation#match
  id_m = match(as.character(fishnet$CELLID), as.character(db.classified$CELLID))
  xtra1 = db.classified[id_m,]
  
  #spatial cbind
  fishnet.merge=spCbind(fishnet,xtra1$pattern)
  
  # ################################################################################
  ### Section 4: Plot best model
  ## Best model ##
  if (DMModel == "ANNs_models"){
    #line chart
    png(file = paste(analysis.plots.path,"/","ModelUse_LineChart","_",dataset,"_",wsize,".png",sep=""), width = 700, height = 600)
    print(plot(fit.model, metric = "Kappa"))
    dev.off
    ##### output messages ####
    cat(paste("### RESULT 1 out of 2: The line chart of the best ANN Model used for classifying unlabelled data from ", dataset," was plotted and store in the project analysis folder! ###",sep=""),"\n")
    ##### end output messages ####
  } else if (DMModel == "C45_models") {
  }

  ##### export ####
  writeSpatialShape(fishnet.merge,paste(outshp.path,"/",DMModel,"_",dataset,"_",wsize,sep=""))
  save(db.unlabelled.classified, file=paste(output.path,"/db_unlabelled_classified_",DMModel,".RData",sep=""))
  
  ##### output messages ####
  cat(paste("### RESULT 2 out of 2: The unlabelled classified objects were joint to the fishnet with best model from ", dataset,". It was store in the project folder! ###",sep=""),"\n")
  ##### end output messages ####
  
  ##### end processing ####
}