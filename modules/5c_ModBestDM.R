#############################################################################
# title         : training dataset for running NN;
# purpose       : create training dataset for running NN;
# producer      : prepared by A. Coca;
# last update   : in London, UK Abr 2015 / Updated in July 2015;
# inputs        : merge file (in RData format) with FRAGSTAT outputs (.class,.land);
# outputs       : training dataset with ID 
# remarks 1     : N/A
##############################################################################

ModBestDM = function(dataset)
{
  if (missing(dataset))
    stop("Need to specify dataset name.")
  
  #### libraries ####
  pckg = c("data.table","PerformanceAnalytics","GGally",
           "caret","nnet","plyr") 
  
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
  
  #list fishnet sizes 
  split.fishnets = strsplit(list.dirs(projects.path, recursive = F, full.names = F),"_")
  fishnets = c(unique(sapply(split.fishnets, "[", 2)))
  
  #list DM models
  ref.project = paste(root.dir,"/output/results/det_",date.ini,"to",date.end,"/processed/projects/",dataset,"_",fishnets[1],"/model",sep="")
  split.DMModels = strsplit(list.files(ref.project, recursive = T, full.names = F),"[.]")
  DMModels = c(unique(sapply(split.DMModels, "[", 1)))
  
  #analysis tb output
  analysis.tb.path = paste(root.dir,"/output/results/det_",date.ini,"to",date.end,"/analysis/tb",sep="")
  dir.create(analysis.tb.path, recursive = T, mode = "0777", showWarnings = F)
  
  #analysis plots output
  analysis.plots.path = paste(root.dir,"/output/results/det_",date.ini,"to",date.end,"/analysis/plots",sep="")
  dir.create(analysis.plots.path, recursive = T, mode = "0777", showWarnings = F)
  ##### end static ####
  
  ##### processing ####
  #merge models results
  model.parameters = NULL
  for (DMModel in DMModels){
    for (fishnet in fishnets){
      #project name
      project = paste(dataset,"_",fishnet,sep="")
      
      ## load ##
      if (DMModel == "C45_models"){
        get(load(paste(projects.path,"/",project,"/model/",DMModel,".rda",sep="")))
        results = fit.model$results
        best.results = results[which(fit.model$results$C == fit.model$bestTune$C & fit.model$results$M == fit.model$bestTune$M),]
        best.model = data.frame("fishnet" = fishnet, "model" = DMModel, best.results)  
        model.parameters = rbind.fill(model.parameters, best.model)
        assign(paste(strsplit(DMModel,"_")[[1]][1],"_",fishnet,sep=""), fit.model)
      } else if (DMModel == "ANNs_models") {
        get(load(paste(projects.path,"/",project,"/model/",DMModel,".RData",sep="")))
        best.model = fit.model$results
        best.model = subset(best.model, decay == fit.model$bestTune$decay & size == fit.model$bestTune$size)
        best.model = data.frame("fishnet" = fishnet, "model" = DMModel, best.model, "weights" = length(fit.model$finalModel$wts), "convergence" = fit.model$finalModel$value)  
        model.parameters = rbind.fill(model.parameters, best.model)
        assign(paste(strsplit(DMModel,"_")[[1]][1],"_",fishnet,sep=""), fit.model)
      }
    }
  }
  #export models best combination parameters and features
  write.csv(model.parameters,paste(analysis.tb.path,"/",dataset,"_bestmodelsCV_parameters.csv",sep=""), row.names=T)

  ##### output messages ####
  cat(paste("### RESULT 1 out of 4: The best models parameters and perfomance results were generated to the ", dataset," and store as tabular data in the project analysis folder! ###",sep=""),"\n")
  ##### end output messages ####
  
  ANNs.Models = objects(pattern="*ANNs_")
  C45.Models = objects(pattern="*C45_")
  DM.Models = c(ANNs.Models,C45.Models)
  
  #create list for resampling
  resampling.list <- list()
  for(DM.Model in DM.Models){
    a <- get(DM.Model)
    name <- DM.Model
    resampling.list[[name]] <- a
  }

  #resampling for model comparison
  resamps <- resamples(resampling.list)

  #comparison statistics
  models.comparison = data.frame(summary(resamps)$statistics)
  #export models comparison (boxplot)
  write.csv(models.comparison,paste(analysis.tb.path,"/",dataset,"_bestmodelsCV_boxplots.csv",sep=""), row.names=T)
  
  ##### output messages ####
  cat(paste("### RESULT 2 out of 4: The best models statistics of perfomance results were generated to the ", dataset," and store as tabular data in the project analysis folder! ###",sep=""),"\n")
  ##### end output messages ####
  
  #boxplots charts
  png(file = paste(analysis.plots.path,"/","boxplots","_",dataset,".png",sep=""), width = 700, height = 600)
  print(bwplot(resamps, layout = c(2, 1), box.ratio = 1, auto.key = T)) 
  dev.off()
  
  ##### output messages ####
  cat(paste("### RESULT 3 out of 4: The boxplots to compare the best models according to perfomance variables were generated to the ", dataset," and store as chart in the project analysis folder! ###",sep=""),"\n")
  ##### end output messages ####
  
  #determining statistical differences
  statistics.models = NULL
  for (fishnet in fishnets){
    C45.model = get(paste("C45_",fishnet,sep=""))
    ANN.model = get(paste("ANNs_",fishnets,sep=""))
    sta.results = compare_models(C45.model, ANN.model, metric = C45.model$metric[1])
    sta.results = data.frame(fishnet = fishnet, p_value = sta.results$p.value)
    statistics.models = rbind.fill(statistics.models, sta.results)
  }
  #export models statistics (p-values)
  write.csv(statistics.models,paste(analysis.tb.path,"/",dataset,"_bestmodelsCV_pvalue.csv",sep=""), row.names=T)
  ##### end processing ####
  
  ##### output messages ####
  cat(paste("### RESULT 4 out of 4: The statistical comparison between DM models at the same fishnet were generated to the ", dataset," and store as tabular data in the project analysis folder! ###",sep=""),"\n")
  ##### end output messages ####
}