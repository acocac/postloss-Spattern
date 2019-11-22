#############################################################################
# title         : training dataset for running NN;
# purpose       : create training dataset for running NN;
# producer      : prepared by A. Coca;
# last update   : in London, UK Abr 2015 / Updated in July 2015;
# inputs        : merge file (in RData format) with FRAGSTAT outputs (.class,.land);
# outputs       : training dataset with ID 
# remarks 1     : N/A
##############################################################################

ModRunDM = function(dataset, wsize, check_source)
{
  if (missing(dataset))
    stop("Need to specify dataset name.")
  
  if (missing(wsize))
    stop("Need to specify the fishnet size")
  
  if (missing(check_source))
    stop("Need to specify/recheck if labelled data are collected by a single period or multiperiod raster(s) -type single or multi-")
  
  #### libraries ####
  pckg = c("data.table","car","caret","reshape",
           "dplyr","ggplot2","doParallel","rJava","RWeka") 
  
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
  n.repeats = as.numeric(conf.args[[11]])
  n.resampling = as.numeric(conf.args[[12]])
  
  ## load ##
  if (check_source == "multi"){
    #project output model path
    project.model.path = paste(root.dir,"/output/results/det_all/models/projects/",dataset,"_",wsize,sep="")
    dir.create(project.model.path, recursive = T, mode = "0777", showWarnings = F)
    
    ## load train set ##
    db.train = read.csv(paste(root.dir,"/output/results/det_all/traindata/projects/",dataset,"_",wsize,"/",dataset,"_",wsize,"_training.csv",sep=""),header = T)
    
    freq_samples = table(db.train$pattern)
    
    if (!all(freq_samples == max(freq_samples))){
      ##### processing ####
      ################################################################################
      ### Section 1: Data as numeric and select final predictors
      #select final predictors
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
      
      #filter using selected variables identified for the train dataset
      filter = grepl(sel.var, names(db.train))
      patTrainX = db.train[, filter]
      #remove variable(s) with ZeroVariance
      patTrainX[,nearZeroVar(patTrainX)] = NULL
      
      ################################################################################
      ### Section 2: Separate training and unlabelled dataset from final database
      #define input data (train and unlabelled) for model
      db.train.model = data.frame("CELLID"=db.train$CELLID,"pattern"=db.train$pattern,patTrainX)
  
      #training set without ID (CELLID)
      vars.remove <- -grep('^(CELLID)', names(db.train.model))
      model.train.input = db.train.model[,vars.remove]
      
      ################################################################################
      ### Section 3: C4.5 models
      ## Section 3a: C4.5 model settings
      #C4.5  parameters > change v2
      C = seq(0.05, 0.5,by=0.05)
      M = seq(1, 30,by=1)
      
      #tuning grid for train caret function
      my.grid <- expand.grid(.C = C, .M = M)
      
      #create a list of seed, here change the seed for each resampling
      set.seed(40)
      length.seeds = (n.repeats*n.resampling) + 1
      n.tune.parameters = length(C)*length(M)
      seeds <- vector(mode = "list", length = length.seeds)#length is = (n_repeats*nresampling)+1
      for(i in 1:length.seeds) seeds[[i]]<- sample.int(n=1000, n.tune.parameters) #(n.tune.parameters = number of tuning parameters)
      seeds[[length.seeds]]<-sample.int(1000, 1)#for the last model
      
      #create a control object for the models, implementing 10-crossvalidation repeated 10 times
      fit.Control <- trainControl(
        method = "repeatedcv",
        number = n.resampling, ## k-fold CV
        repeats = n.repeats, ## iterations
        savePred = TRUE,
        seeds = seeds
      )
      
      ## Section 3b: Run C4.5 models 
      ## parallel process ##
      #cluster 
      # Calculate the number of cores
      no_cores <- detectCores() - 1
      cl <- makeCluster(no_cores, type = "SOCK")    #create a cluster 
      registerDoParallel(cl)                #register the cluster 
      
      ## foreach or lapply would do this faster
      set.seed(40)
      fit.model <- train(pattern~., 
                         data=model.train.input,
                         trControl = fit.Control,
                         preProcess=c("range"),
                         method = "J48",
                         metric = "Kappa",
                         tuneGrid = my.grid
                         
      )
      
      stopCluster(cl = cl)
      
      #export and store model results
      .jcache(fit.model$finalModel$classifier)
      save(fit.model, file=paste(project.model.path,"/C45_models.rda",sep=""))

      ##### output messages ####
      cat(paste("### RESULT 1 out of 2: The C45 models generated from ", dataset," train set using the ",wsize,"m fishnet and ",n.repeats,"-fold (",n.resampling," repetitions) were successfully created and added to the project folder of the detection period ", date.ini," to ", date.end,"! ###",sep=""),"\n")
      ##### end output messages ####
      
      ################################################################################
      ### Section 4: ANNs models
      ## Section 4a: ANNs model settings
      #ANN parameters
      decay.tune = c(0.0001, 0.001, 0.01, 0.1)
      #decay.tune = c(0.01, 0.1)
      size = seq(1, 50,by=2)
      maxit.nnet = 5000
      rang.nnet = 0.7
      MaxNWts.nnet = 2000
      
      #tuning grid for train caret function
      my.grid <- expand.grid(.decay = decay.tune, .size = size)
      
      #create a list of seed, here change the seed for each resampling
      set.seed(40)
      length.seeds = (n.repeats*n.resampling)+1
      n.tune.parameters = length(decay.tune)*length(size)
      seeds <- vector(mode = "list", length = length.seeds)#length is = (n_repeats*nresampling)+1
      for(i in 1:length.seeds) seeds[[i]]<- sample.int(n=1000, n.tune.parameters) #(n.tune.parameters = number of tuning parameters)
      seeds[[length.seeds]]<-sample.int(1000, 1)#for the last model
      
      #create a control object for the models, implementing 10-crossvalidation repeated 10 times
      fit.Control <- trainControl(
        method = "repeatedcv",
        number = n.resampling, ## k-fold CV
        repeats = n.repeats, ## iterations
        classProbs=TRUE,
        savePred = TRUE,
        seeds = seeds
      )
      
      ## Section 4b: Run ANNs models 
      ## parallel process ##
      #cluster
      # Calculate the number of cores
      no_cores <- detectCores() - 1
      cl <- makeCluster(no_cores, type = "SOCK")    #create a cluster 
      registerDoParallel(cl)                #register the cluster 
      
      set.seed(40)
      ## foreach or lapply would do this faster
      fit.model <- train(pattern~., 
                         data=model.train.input,
                         trControl = fit.Control,
                         method = "nnet",
                         maxit = maxit.nnet, 
                         rang = rang.nnet,
                         MaxNWts = MaxNWts.nnet,
                         tuneGrid = my.grid, 
                         trace = F, 
                         metric = "Kappa",
                         linout = F
      )
      
      stopCluster(cl = cl)
      
      #save and export model results 
      save(fit.model, maxit.nnet, rang.nnet, MaxNWts.nnet, 
           file=paste(project.model.path,"/ANNs_models.RData",sep=""))
      
      ##### output messages ####
      cat(paste("### RESULT 2 out of 2: The ANNs models generated from ", dataset," train set using the ",wsize,"m fishnet and ",n.repeats,"-fold (",n.resampling," repetitions) were successfully created and added to the project folder of the detection period ", date.ini," to ", date.end,"! ###",sep=""))
      ##### end output messages ####
      
      ################################################################################
      ### Section 3: C5.0 models
      ## Section 3a: C5.0 model settings
      trials = c(1:9, (1:10)*10)
      model = c("tree", "rules")
      winnow = c(TRUE, FALSE)
      
      #tuning grid for train caret function
      my.grid <- expand.grid(.trials = trials,
                             .model = model,
                             .winnow = winnow)
      
      #create a list of seed, here change the seed for each resampling
      set.seed(40)
      length.seeds = (n.repeats*n.resampling) + 1
      n.tune.parameters = length(trials)*length(model)*length(winnow)
      seeds <- vector(mode = "list", length = length.seeds)#length is = (n_repeats*nresampling)+1
      for(i in 1:length.seeds) seeds[[i]]<- sample.int(n=1000, n.tune.parameters) #(n.tune.parameters = number of tuning parameters)
      seeds[[length.seeds]]<-sample.int(1000, 1)#for the last model
      
      #create a control object for the models, implementing 10-crossvalidation repeated 10 times
      fit.Control <- trainControl(
        method = "repeatedcv",
        number = n.resampling, ## k-fold CV
        repeats = n.repeats, ## iterations
        savePred = TRUE,
        seeds = seeds
      )
      
      ## Section 3b: Run C4.5 models 
      ## parallel process ##
      #cluster 
      # Calculate the number of cores
      no_cores <- detectCores() - 1
      cl <- makeCluster(no_cores, type = "SOCK")    #create a cluster 
      registerDoParallel(cl)                #register the cluster 
      
      ## foreach or lapply would do this faster
      set.seed(40)
      fit.model <- train(pattern~., 
                         data=model.train.input,
                         trControl = fit.Control,
                         preProcess=c("range"),
                         method = "C5.0",
                         metric = "Kappa",
                         tuneGrid = my.grid
                         
      )
      
      stopCluster(cl = cl)
      
      #export and store model results
      .jcache(fit.model$finalModel$classifier)
      save(fit.model, file=paste(project.model.path,"/C50_models.rda",sep=""))
      
      ##### output messages ####
      cat(paste("### RESULT 1 out of 3: The C5.0♥ models generated from ", dataset," train set using the ",wsize,"m fishnet and ",n.repeats,"-fold (",n.resampling," repetitions) were successfully created and added to the project folder of the detection period ", date.ini," to ", date.end,"! ###",sep=""),"\n")
      ##### end output messages ####
    } else {
      
    }
  } else {
    #project output model path
    project.model.path = paste(root.dir,"/output/results/det_",date.ini,"to",date.end,"/processed/projects/",dataset,"_",wsize,"/model",sep="")
    dir.create(project.model.path, recursive = T, mode = "0777", showWarnings = F)
    
    #fragstat input merged file
    fragstat.file = paste(root.dir,"/output/results/det_",date.ini,"to",date.end,"/raw/fragstat/",dataset,"_",wsize,".Rdata",sep="")
    
    ## load ##
    ## load train set ##
    db.train = read.csv(paste(root.dir,"/output/results/det_",date.ini,"to",date.end,"/processed/projects/",dataset,"_",wsize,"/",dataset,"_",wsize,"_training.csv",sep=""),header = T)
    
    #load all database with all metrics extracted
    assign("db.all",get(load(fragstat.file))) #load db.all by dataset x ws
    
    ##### processing ####
    ################################################################################
    ### Section 1: Data as numeric and select final predictors
    #select final predictors
    if (all(class.metrics == "all")){ 
      metrics = colnames(db.all)[grep("c_",colnames(db.all))]
      sel.var.class = paste("^",metrics,"$",sep="",collapse ="|")
    } else if (all(class.metrics != "NA" & class.metrics != "all")){
      sel.var.class = paste("^c_",class.metrics,"$",sep="",collapse ="|")
    } else if (all(class.metrics == "NA")){
      sel.var.class = NULL
    }
    #land level
    if (all(land.metrics == "all")){ 
      metrics = colnames(db.all)[grep("l_",colnames(db.all))]
      sel.var.land = paste("^",metrics,"$",sep="",collapse ="|")
    } else if (all(land.metrics != "NA" & land.metrics != "all")){
      sel.var.land = paste("^l_",land.metrics,"$",sep="",collapse ="|")
    } else if (all(land.metrics == "NA")){
      sel.var.land = NULL
    }
    
    sel.var = paste(sel.var.class,sel.var.land,collapse ="|")
    sel.var = gsub(" ","",sel.var)
    
    # filter columns based on selected vars
    #filter using selected variables identified for all datasets
    filter = grepl(sel.var, names(db.all))
    patAllX <- db.all[, filter, with = FALSE]
    patAllID <- db.all$CELLID
    
    # all data as numeric
    REG=1:length(patAllX)
    ind <- match(names(patAllX[REG]), names(patAllX))
    ## Convert all variables (columns) to numeric format
    for (i in seq_along(patAllX)) {
      set(patAllX, NULL, ind[i], as.numeric(as.character(patAllX[[ind[i]]])))
    }
    #remove variable(s) with ZeroVariance
    patAllX[,nearZeroVar(patAllX)] = NULL
    
    #filter using selected variables identified for the train dataset
    filter = grepl(sel.var, names(db.train))
    patTrainX = db.train[, filter]
    #remove variable(s) with ZeroVariance
    patTrainX[,nearZeroVar(patTrainX)] = NULL
    
    ################################################################################
    ### Section 2: Separate training and unlabelled dataset from final database
    ###join ID to separate unlabelled data
    db.target = cbind(CELLID = patAllID,patAllX)
    db.unlabelled=db.target[-which(db.target$CELLID %in% unique(db.train$CELLID)),]
    
    #define input data (train and unlabelled) for model
    db.train.model = data.frame("CELLID"=db.train$CELLID,"pattern"=db.train$pattern,patTrainX)
    db.unlabelled.model = cbind("pattern"="null",db.unlabelled[,-1, with = F])
    
    #training set without ID (CELLID)
    vars.remove <- -grep('^(CELLID)', names(db.train.model))
    model.train.input = db.train.model[,vars.remove]
    
    ################################################################################
    ### Section 3: C4.5 models
    ## Section 3a: C4.5 model settings
    #C4.5  parameters > change v2
    C = seq(0.05, 0.5,by=0.05)
    M = seq(1, 30,by=1)
    
    #tuning grid for train caret function
    my.grid <- expand.grid(.C = C, .M = M)
    
    #create a list of seed, here change the seed for each resampling
    set.seed(40)
    length.seeds = (n.repeats*n.resampling) + 1
    n.tune.parameters = length(C)*length(M)
    seeds <- vector(mode = "list", length = length.seeds)#length is = (n_repeats*nresampling)+1
    for(i in 1:length.seeds) seeds[[i]]<- sample.int(n=1000, n.tune.parameters) #(n.tune.parameters = number of tuning parameters)
    seeds[[length.seeds]]<-sample.int(1000, 1)#for the last model
    
    #create a control object for the models, implementing 10-crossvalidation repeated 10 times
    fit.Control <- trainControl(
      method = "repeatedcv",
      number = n.resampling, ## k-fold CV
      repeats = n.repeats, ## iterations
      savePred = TRUE,
      seeds = seeds
    )
    
    ## Section 3b: Run C4.5 models 
    ## parallel process ##
    #cluster 
    # Calculate the number of cores
    no_cores <- detectCores() - 1
    cl <- makeCluster(no_cores, type = "SOCK")    #create a cluster 
    registerDoParallel(cl)                #register the cluster 
    
    set.seed(40)
    ## foreach or lapply would do this faster
    fit.model <- train(pattern~., 
                       data=model.train.input,
                       trControl = fit.Control,
                       preProcess=c("range"),
                       method = "J48",
                       metric = "Kappa",
                       tuneGrid = my.grid
                       
    )
    
    stopCluster(cl = cl)
    
    #export and store model results
    .jcache(fit.model$finalModel$classifier)
    save(fit.model, db.train.model, db.unlabelled.model, db.unlabelled, file=paste(project.model.path,"/C45_models.rda",sep=""))
    #save(fit.model, file=paste(project.model.path,"/C45_models.rda",sep=""))
    
    ##### output messages ####
    cat(paste("### RESULT 1 out of 2: The C45 models generated from ", dataset," train set using the ",wsize,"m fishnet and ",n.repeats,"-fold (",n.resampling," repetitions) were successfully created and added to the project folder of the detection period ", date.ini," to ", date.end,"! ###",sep=""),"\n")
    ##### end output messages ####
    
    ################################################################################
    ### Section 4: ANNs models
    ## Section 4a: ANNs model settings
    #ANN parameters
    decay.tune = c(0.0001, 0.001, 0.01, 0.1)
    #decay.tune = c(0.01, 0.1)
    size = seq(1, 50,by=2)
    maxit.nnet = 5000
    rang.nnet = 0.7
    MaxNWts.nnet = 2000
    
    #tuning grid for train caret function
    my.grid <- expand.grid(.decay = decay.tune, .size = size)
    
    #create a list of seed, here change the seed for each resampling
    set.seed(40)
    length.seeds = (n.repeats*n.resampling)+1
    n.tune.parameters = length(decay.tune)*length(size)
    seeds <- vector(mode = "list", length = length.seeds)#length is = (n_repeats*nresampling)+1
    for(i in 1:length.seeds) seeds[[i]]<- sample.int(n=1000, n.tune.parameters) #(n.tune.parameters = number of tuning parameters)
    seeds[[length.seeds]]<-sample.int(1000, 1)#for the last model
    
    #create a control object for the models, implementing 10-crossvalidation repeated 10 times
    fit.Control <- trainControl(
      method = "repeatedcv",
      number = n.resampling, ## k-fold CV
      repeats = n.repeats, ## iterations
      classProbs=TRUE,
      savePred = TRUE,
      seeds = seeds
    )
    
    ## Section 4b: Run ANNs models 
    ## parallel process ##
    #cluster
    # Calculate the number of cores
    no_cores <- detectCores() - 1
    cl <- makeCluster(no_cores, type = "SOCK")    #create a cluster 
    registerDoParallel(cl)                #register the cluster 
    
    set.seed(40)
    ## foreach or lapply would do this faster
    fit.model <- train(pattern~., 
                       data=model.train.input,
                       trControl = fit.Control,
                       method = "nnet",
                       maxit = maxit.nnet, 
                       rang = rang.nnet,
                       MaxNWts = MaxNWts.nnet,
                       tuneGrid = my.grid, 
                       trace = F, 
                       metric = "Kappa",
                       linout = F
    )
    
    stopCluster(cl = cl)
    
    #save and export model results 
    save(db.train.model, fit.model,  
         db.unlabelled.model, db.unlabelled,
         maxit.nnet, rang.nnet, MaxNWts.nnet, 
         file=paste(project.model.path,"/ANNs_models.RData",sep=""))
    
    ##### output messages ####
    cat(paste("### RESULT 2 out of 2: The ANNs models generated from ", dataset," train set using the ",wsize,"m fishnet and ",n.repeats,"-fold (",n.resampling," repetitions) were successfully created and added to the project folder of the detection period ", date.ini," to ", date.end,"! ###",sep=""))
    ##### end output messages ####
  }

}