#############################################################################
# title         : training dataset for running NN;
# purpose       : create training dataset for running NN;
# producer      : prepared by A. Coca;
# last update   : in London, UK Abr 2015 / Updated in July 2015;
# inputs        : merge file (in RData format) with FRAGSTAT outputs (.class,.land);
# outputs       : training dataset with ID 
# remarks 1     : N/A
##############################################################################

TrainingSet = function(dataset, wsize, check_source, fieldtrain=NULL)
{
  if (missing(dataset))
    stop("Need to specify dataset name.")
  
  if (missing(wsize))
    stop("Need to specify the fishnet size")
  
  if (missing(check_source))
    stop("Need to specify/recheck if labelled data are collected by a single period or multiperiod raster(s) -type single or multi-")
    
  if (check_source == "single")
    if (missing(fieldtrain))
      stop("Need to specify the field name created to record patterns train samples")
  
  #### libraries ####
  pckg = c("raster","rgdal","data.table")

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

  #input dir where detection fishnet files (window sizes) will be kept
  fishnet.dir = paste(root.dir,"/","input/geodata/shp/target/fishnet/",proj.acronym,sep="")

  ## load ##
  #load fishnet
  fishnet <- readOGR(dsn=fishnet.dir, layer=paste("fishnet_",wsize,sep=""))
  fishnet.df = as.data.frame(fishnet)

  ##### processing ####
  db.training = NULL

  if (check_source == "multi"){
    #project output training file
    project.train.path = paste(root.dir,"/output/results/det_all/traindata/projects/",dataset,"_",wsize,sep="")
    dir.create(project.train.path, recursive = T, mode = "0777", showWarnings = F)

    target_fields = colnames(fishnet.df[,grep("det*",colnames(fishnet.df))])
    for (t in target_fields){
      #fragstat input merged file
      fragstat.file = paste(root.dir,"/output/results/",t,"/raw/fragstat/",dataset,"_",wsize,".Rdata",sep="")

      ## load ##
      #load database with all metrics extracted
      assign("db.all",get(load(fragstat.file))) #load db.all by dataset x ws
      ##### end static ####

      #subset training cells
      #field attribute name that contains the train samples
      fishnet.training = fishnet.df[!is.na(fishnet.df[t]),]

      #match and extract training dataset
      CELLID_training = c(unique(fishnet.training$CELLID))
      db.temp = db.all[db.all$CELLID %in% CELLID_training,]
      pattern.match = match(db.temp$CELLID,fishnet.training$CELLID)
      xtra.pattern = fishnet.training[pattern.match,]
      db.temp = cbind(xtra.pattern[,t],db.temp, "det"=t)
      setnames(db.temp, old="V1", new="pattern")
      db.training = rbind(db.training, db.temp)
    }
    
    db.training= db.training[order(db.training$pattern,db.training$CELLID),]
    db.training = db.training[!duplicated(db.training$CELLID),]
    
    pattern_names = paste(names(table(db.training$pattern)),collapse = "-")
    freq_samples = paste(table(db.training$pattern),collapse = "-")
    
    #export
    write.csv(db.training,paste(project.train.path,"/",dataset,"_",wsize,"_training.csv",sep=""),row.names=F)
    
  } else {
    #project output training file
    project.train.path = paste(root.dir,"/output/results/det_",date.ini,"to",date.end,"/processed/projects/",dataset,"_",wsize,sep="")
    dir.create(project.train.path, recursive = T, mode = "0777", showWarnings = F)

    #fragstat input merged file
    fragstat.file = paste(root.dir,"/output/results/det_",date.ini,"to",date.end,"/raw/fragstat/",dataset,"_",wsize,".Rdata",sep="")

    #load database with all metrics extracted
    assign("db.all",get(load(fragstat.file))) #load db.all by dataset x ws

    #subset training cells
    #field attribute name that contains the train samples
    fishnet.df = as.data.frame(fishnet)
    fishnet.training = fishnet.df[!is.na(fishnet.df[fieldtrain]),]
    #match and extract training dataset
    CELLID_training = c(unique(fishnet.training$CELLID))
    db.training = db.all[db.all$CELLID %in% CELLID_training,]
    pattern.match = match(db.training$CELLID,fishnet.training$CELLID)
    xtra.pattern = fishnet.training[pattern.match,]
    db.training = cbind(xtra.pattern[,fieldtrain],db.training)
    setnames(db.training, old="V1", new="pattern")
    db.training = db.training[order(db.training$pattern,db.training$CELLID),]
    # u = unlist(db.training[db.training$pattern == "d", "CELLID", with=F])
    # a = fishnet.training[fishnet.training$det_1to4 == "d", "CELLID"]
    pattern_names = paste(names(table(db.training$pattern)),collapse = "-")
    freq_samples = paste(table(db.training$pattern),collapse = "-")
    
    #export
    write.csv(db.training,paste(project.train.path,"/",dataset,"_",wsize,"_training.csv",sep=""),row.names=F)
    ##### end processing ####
  }
  ##### end processing ####

  ##### output messages ####
  cat(paste("### RESULT: The train set file from ", dataset," using the ",wsize,"m fishnet was successfully created with the following frequencies by class: \n ",pattern_names," = ",freq_samples,"\n train dataset were added to the project folder! ###",sep=""))
  ##### end output messages ####
}
