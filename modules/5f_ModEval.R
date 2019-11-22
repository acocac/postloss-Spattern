#############################################################################
# title         : training dataset for running NN;
# purpose       : create training dataset for running NN;
# producer      : prepared by A. Coca;
# last update   : in London, UK Abr 2015 / Updated in July 2015;
# inputs        : merge file (in RData format) with FRAGSTAT outputs (.class,.land);
# outputs       : training dataset with ID 
# remarks 1     : N/A
##############################################################################

ModEval = function(dataset, wsize, DMModel,n.samples,n.repetitions)
{
  if (missing(dataset))
    stop("Need to specify dataset name.")
  
  if (missing(wsize))
    stop("Need to specify the fishnet size")
  
  if (missing(DMModel))
    stop("Need to specify the data mining model type for the sensitivity analysis")
  
  if (missing(n.samples))
    stop("Need to specify the number of samples by pattern typology")
  
  if (missing(n.repetitions))
    stop("Need to specify the number of test repetitions")
  
  #### libraries ####
  pckg = c("caret", "rgdal", "maptools","raster","sampling") 
  
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
  source(paste(r.dir,"/functions/5g_ValidationStratifiedSampling.R", sep=""))
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
  
  #output project results 
  output.path = paste(projects.path,"/",dataset,"_",wsize,"/results",sep="")
  dir.create(output.path, recursive = T, mode = "0777", showWarnings = F)
  
  #input dir where detection GEOTIFF dataset (e.g. GFC or terra) from the target area will be kept
  det.dir.all = paste(root.dir,"/input/geodata/raster/processed/detection/det_",date.ini,"to",date.end,"/",dataset,"/all/",proj.acronym,"/",sep="")
  det.dataset.file <- list.files(det.dir.all, ".tif$", full.names = T)
  
  #analysis tb output
  analysis.tb.path = paste(root.dir,"/output/results/det_",date.ini,"to",date.end,"/analysis/tb",sep="")
  dir.create(analysis.tb.path, recursive = T, mode = "0777", showWarnings = F)
  
  ## load ##
  get(load(paste(projects.path,"/",dataset,"_",wsize,"/results/db_unlabelled_classified_",DMModel,".RData",sep="")))
  fishnet <- readOGR(dsn=fishnet.dir, layer=paste("fishnet_",wsize,sep=""))
  raster.det <- raster(det.dataset.file)
  db.train = read.csv(paste(projects.path,"/",dataset,"_",wsize,"/",dataset,"_",wsize,"_training.csv",sep=""),header = T)
  ##### end static ####
  
  ##### processing ####
  ###############################################################################
  ### Section 1: Create random sampling from classified objects 
  db.unlabelled.classified=data.frame(db.unlabelled.classified)
  colnames(db.unlabelled.classified)[2] = "pred"
  
  #create repetations and set.seed to replicate
  n.repetitions = as.numeric(n.repetitions)
  n.samples = as.numeric(n.samples)
  set.seed(40)
  seeds <- vector(mode = "list", length = n.repetitions)
  for(i in 1:n.repetitions) seeds[[i]]<- sample.int(n=1000, 1)
  
  #set patterns levels
  u = as.character(unique(db.unlabelled.classified$pred))
  
  db.validation.all=NULL
  for (s in 1:length(seeds)){
    cat(paste("\n ###### repetition no. ",s," ######",sep=""))
    set.seed(as.numeric(seeds[s]))
    sample.tmp = stratified(db.unlabelled.classified,"pred",n.samples)
    cat(paste("\n ###### maximum samples number by pattern is ",max(table(db.unlabelled.classified$pred))," and you digited ",n.samples," samples ######",sep=""))
    db.validation.tmp=db.unlabelled.classified[which(db.unlabelled.classified$CELLID %in% unique(sample.tmp$CELLID)),]
    for (i in 1:dim(db.validation.tmp)[1]){
      CELLID.val=as.character(db.validation.tmp[i,"CELLID"])
      
      # Raster path
      TIFF.fishnet.path=paste(root.dir,"/input/geodata/raster/processed/detection/det_",date.ini,"to",date.end,"/",dataset,"/by_fishnet/",proj.acronym,"/",wsize,sep="")
      rawdet.masked = raster(paste(TIFF.fishnet.path,"/",CELLID.val,"_cummulative_",date.ini,"to",date.end,".tif",sep=""))

      #breakpoints <- c(0,1)
      #colors <- c("black","yellow")
      #plot(rawdet.masked,breaks=breakpoints,col=colors)
      plot(rawdet.masked)
      
      x <- readline("What is the spatial pattern? ") 
      db.validation.tmp[db.validation.tmp$CELLID == CELLID.val,"expert"] = x
      pred.value = as.character(db.validation.tmp[db.validation.tmp$CELLID == CELLID.val,"pred"])
      exp.value = db.validation.tmp[db.validation.tmp$CELLID == CELLID.val,"expert"]
      cat(paste("SAMPLE ",i," out of ",dim(db.validation.tmp)[1], "\n pred: ", pred.value," vs ", "exp: ", exp.value, sep=""))
    }
    db.validation.tmp[,"rep"] = s
    db.validation.all = rbind(db.validation.all,db.validation.tmp)
  }
  
  save(db.validation.all, file=paste(output.path,"/","assessment_modeluse_",DMModel,".rda",sep=""))
  
  #compute Kappa by repetition
  metrics.all = NULL
  cm.all = NULL
  for (r in unique(db.validation.all$rep)){
    kappa.db = subset(db.validation.all, rep == r)
    predicted = kappa.db$pred
    reference = kappa.db$expert
    t = table(factor(predicted, u), factor(reference, u)) #in case a category is missing
    table.cm = confusionMatrix(t)$table
    cm.all = cbind(cm.all,table.cm)
    metrics.tmp = data.frame("rep" = r, t(confusionMatrix(t)$overall[1:2]))
    metrics.all = rbind(metrics.all,metrics.tmp)
  }
  
  #export model use evaluation metrics (overall accuracy and kappa)
  write.csv(metrics.all,paste(analysis.tb.path,"/",dataset,"_bestmodelsCV_modeluse_metrics.csv",sep=""), row.names=T)
  write.csv(cm.all,paste(analysis.tb.path,"/",dataset,"_bestmodelsCV_modeluse_CM.csv",sep=""), row.names=T)
  ##### end processing ####
  
  ##### output messages ####
  cat(paste("### RESULT: The model use from ",dataset," using the ",wsize," was evaluated with ",n.samples," samples and ",n.repetitions," repetitions. Statistics were stored in the analysis folder! ###",sep=""),"\n")
  ##### end output messages ####
}