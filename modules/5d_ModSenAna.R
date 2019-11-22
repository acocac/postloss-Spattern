#############################################################################
# title         : training dataset for running NN;
# purpose       : create training dataset for running NN;
# producer      : prepared by A. Coca;
# last update   : in London, UK Abr 2015 / Updated in July 2015;
# inputs        : merge file (in RData format) with FRAGSTAT outputs (.class,.land);
# outputs       : training dataset with ID 
# remarks 1     : N/A
##############################################################################

ModSenAna = function(dataset, wsize, DMModel)
{
  if (missing(dataset))
    stop("Need to specify dataset name.")
  
  if (missing(wsize))
    stop("Need to specify the fishnet size")
  
  if (missing(DMModel))
    stop("Need to specify the data mining model type for the sensitivity analysis")
  
  #### libraries ####
  pckg = c("data.table","devtools","caret","NeuralNetTools",
           "nnet","reshape","dplyr","ggplot2","gridExtra",
           "grid") 
  
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
  
  #analysis plots output
  analysis.plots.path = paste(root.dir,"/output/results/det_",date.ini,"to",date.end,"/analysis/plots",sep="")
  dir.create(analysis.plots.path, recursive = T, mode = "0777", showWarnings = F)
  ##### end static ####
  
  if (DMModel == "C45_models"){
        get(load(paste(projects.path,"/",dataset,"_",wsize,"/model/",DMModel,".rda",sep="")))
        
      } else if (DMModel == "ANNs_models") {
        get(load(paste(projects.path,"/",dataset,"_",wsize,"/model/",DMModel,".RData",sep="")))
        
        #extract best ANN model parameters 
        wts <- fit.model$finalModel$wts
        decay <- fit.model$finalModel$decay
        struct <- fit.model$finalModel$n
        
        # recreate
        set.seed(40) #seed set for reproducing the same results
        recmod <- nnet(pattern~., 
                       data=db.train.model[,-1], 
                       Wts = wts, decay = decay, 
                       size = struct[2], 
                       maxit = maxit.nnet, 
                       rang = rang.nnet,
                       MaxNWts = MaxNWts.nnet, linout = F,
                       trave = T)
        
        ##plotting 
        #theme
        bar.theme_nolegend = theme_classic(base_size = 12, base_family = "Arial") + theme(plot.title = element_text(face="bold", colour="#000000", size=14, vjust=1.3), axis.title.x = element_text(face="plain", colour="#000000", size=15, vjust=-0.3),
                                                                                            axis.text.x  = element_text(angle=90, vjust=0.5, size=9), axis.title.y = element_text(face="plain", colour="#000000", size=15, vjust=0.8), axis.text.y  = element_text(angle=0, vjust=0.5, size=12)) + 
          theme(plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank()) + 
          theme(axis.line = element_line(color = 'black')) + theme(legend.position = "none")
        
        #loop to create individual plots
        plots.name = NULL
        for (pattern in levels(db.train.model$pattern)){
          plot.temp = olden(recmod, pattern) + bar.theme_nolegend + theme(legend.position = 'none') 
          plot.temp = plot.temp + geom_bar(colour="black", fill="gray", stat="identity") +
            annotate("text", x=7, y=1, label=pattern, color="red",
                     angle = 0) 
          
          #save plot
          assign(paste("g_",pattern,sep=""), ggplotGrob(plot.temp))
          rm(plot.temp)
        }
        
        plots.sen = list.files(pattern = "g_*")
        
        png(file = paste(analysis.plots.path,"/","ANNs_sensitivity","_",dataset,"_",wsize,".png",sep=""), width = 900, height = 600)
        grid.arrange(g_de, g_di, g_g, ncol=3)
        dev.off()
        ##### end processing ####
      }
}