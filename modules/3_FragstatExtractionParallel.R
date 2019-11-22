#############################################################################
# title         : FBT files as input in Fragstat;
# purpose       : generate FBT files by GeoTIFF unit of analysis file to run fragstat;
# producer      : prepared by A. Coca;
# last update   : in London, UK Abr 2015 / Updated in July 2015;;
# inputs        : GeoTIFF extracted by unit of analysis;
# outputs       : FBT input files to run Fragstat using parallel processing;
# remarks 1     : n/o;
##############################################################################

FragstatExtraction = function(dataset, wsize, processors, rerun)
{
  if (missing(dataset))
    stop("Need to specify dataset name.")
  
  if (missing(wsize))
    stop("Need to specify window or fishnet size")
  
  if (missing(processors))
    stop("Need to specify processors numbers for parallel processing")
  
  #### libraries ####
  pckg = c("snowfall", "data.table") 
  
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
  FragstatEXE = conf.args[[7]]
  FragstatFCAfile = conf.args[[8]]
  
  #input path with GeoTIFF files by unit of analysis
  grid.path = paste(root.dir,"/input/geodata/raster/processed/detection/det_",date.ini,"to",date.end,"/",dataset,"/by_fishnet/",proj.acronym,"/",wsize,sep="")
  grid.path.fragstat = gsub("/", "\\\\", grid.path)
  
  #fragstat root input path
  fragstat.path = paste(root.dir,"/input/fragstat/input",sep="")
  dir.create(fragstat.path, recursive = T, mode = "0777", showWarnings = F)
  
  #fragstat project input path
  project.input.path = paste(root.dir,"/input/fragstat/input/det_",date.ini,"to",date.end,"/",dataset,"_",wsize,sep="")
  dir.create(project.input.path, recursive = T, mode = "0777", showWarnings = F)
  
  #fragstat project output path
  project.output.path = paste(root.dir,"/input/fragstat/output/det_",date.ini,"to",date.end,"/",dataset,"_",wsize,sep="")
  dir.create(project.output.path, recursive = T, mode = "0777", showWarnings = F)

  #copy the FCA file to fragstat path 
  fragstat.FCA.path = paste(root.dir,"/input/fragstat/settings",sep="")
  dir.create(fragstat.FCA.path, recursive = T, mode = "0777", showWarnings = F)
  file.copy(FragstatFCAfile, fragstat.FCA.path) 
  
  #output tabular merge Rdata
  fragstat.out.path = paste(root.dir,"/output/results/det_",date.ini,"to",date.end,"/raw/fragstat",sep="")
  dir.create(fragstat.out.path, recursive = T, mode = "0777", showWarnings = F)
  
  ## specific settings ##
  #split size (unit of analysis)
  split.size = 1

  ##### processing ####
  ##PART 1: generate fragstat input file##
  #list grids
  list.grids=list.files(grid.path,pattern="*.tif",full.names=F)
  
  #generate dataframe (background converted as class 0)
  db.fragstat = data.frame("dir"=paste(grid.path.fragstat,"\\",list.grids,sep=""),"x","999","x","x","1","x","IDF_GeoTIFF")
  
  #split by unit of analysis
  list.byrows = split(db.fragstat, (1:nrow(db.fragstat) %/% split.size))

  #export
  for (i in 1:length(list.byrows)){
    temp.db = data.frame(list.byrows[i])
    cellid = strsplit(basename(as.character(temp.db[,1])),"_")[[1]][1]
    write.table(temp.db, file = paste(project.input.path,"\\",cellid,".fbt",sep=""), row.names=F, col.names=F, sep=",",quote = F)
    }

  # ##### output messages ####
  cat(paste("### RESULT 1 out of 3: ", length(list.byrows)," individual FRAGSTAT input files from ", length(list.grids)," GeoTIFF files using a fishnet size of ", wsize," for detection period ", date.ini," to ", date.end," were successfully created! ###",sep=""))
  # ##### end output messages ####
  #
  # ##END PART 1##
  #
  ##PART 2: Run fragstat##

  ## parallel process ##
  require(snowfall)
  sfInit(parallel=T,cpus=processors)
  sfLibrary(snowfall)
  sfExport("RunFragstat")
  sfExport("FragstatEXE")
  sfExport("FragstatFCAfile")
  sfExport("project.output.path")

  if (rerun == "yes"){
    #remove duplicate files
    list.duplicates=list.files(project.output.path,pattern="*_bk*",full.names=T)
    file.remove(list.duplicates)

    #check missed files
    log.files <- list.files(project.output.path, ".class$", full.names = F)
    if (length(log.files) != length(list.grids)){
        repeat {
          split.files = strsplit(log.files,"[.]")
          files.done = as.numeric(c(unique(sapply(split.files, "[", 1))))
          split.files = strsplit(list.grids,"_")
          files.all = as.numeric(c(unique(sapply(split.files, "[", 1))))
          files.left=files.all[which(is.na(match(files.all,files.done)))]

          #list missed FBT input batch files
          period = ".fbt"
          list.batch = sapply(project.input.path, paste, files.left, sep="/")
          list.batch = sapply(list.batch, paste, period, sep="")

          sfExport("list.batch")
          sfLapply(1:length(list.batch),fun=RunFragstat)

          #check log.files
          log.files <- list.files(project.output.path, ".class$", full.names = F)
          if (length(log.files) == length(list.grids)) break
        }
    }
  } else {
    #list FBT input batch files
    list.batch=list.files(project.input.path,pattern="*.fbt",full.names=T)
    sfExport("list.batch")
    sfLapply(1:length(list.batch),fun=RunFragstat)
  }

  sfStop()
  #### end processing #####

  #remove duplicate files
  list.duplicates=list.files(project.output.path,pattern="*_bk*",full.names=T)
  file.remove(list.duplicates) 
  
  ##### checkpoint ####
  list.batch=list.files(project.output.path,pattern="*",full.names=F)
  fragstat.outfiles = length(unique(na.omit(as.numeric(unlist(strsplit(unlist(list.batch), "[^0-9]+"))))))
  
  ##### output messages ####
  cat(paste("### RESULT 2 out of 3: ", fragstat.outfiles," FRAGSTAT grid-based data from ", length(list.byrows)," GeoTIFF files using a fishnet size of ", wsize," for detection period ", date.ini," to ", date.end," were successfully extracted! ###",sep=""),"\n")
  ##### end output messages ####
  
  ##END PART 2##
  
  ##PART 3: Merge fragstat results##

  #list levels types
  list.levels=list.files(project.output.path,pattern="*",full.names=F)
  split.levels = strsplit(list.levels,"[.]")
  levels = c(unique(sapply(split.levels, "[", 2)))
 
  if (levels[1] == "class" & levels[2] =="land")
  {
  ##### processing ####
  ## class level ##
  #read class files
  fileNames.class <- list.files(project.output.path, pattern = "*.class", full.names = T) #list patch files  
  class.temp <- rbindlist(lapply(fileNames.class, fread, header = TRUE, sep = ",", na.strings=c("NA", '')))  
  class.temp <- class.temp[!apply(class.temp, 1, function(x) {any(x == "ERR ")}),] #remove error from previous line caused by FRAGSTAT
  class.id.replace=chartr("\\","_", as.character(class.temp$LID))
  class.id.split = strsplit(class.id.replace,"_")
  index.CELLID = which(class.id.split[[1]] %in% wsize)+1
  class.id=sapply(strsplit(class.id.replace, "_"), "[[", index.CELLID)
  class.temp[,"CELLID"] = class.id
  setnames(class.temp,make.names(paste("c",colnames(class.temp), sep = "_")))
  class.subsets<-split(class.temp, class.temp$c_TYPE, drop=TRUE) #extract only class 1 (non-forest) #update V2
  class.class1<-class.subsets$cls_1 #update V2
  
  ## landscape level ##
  #read landscape
  fileNames.land <- list.files(project.output.path, pattern = "*.land", full.names = T) #list patch files  
  land.temp <- rbindlist(lapply(fileNames.land, fread, header = TRUE, sep = ",", na.strings=c("NA", '')))  
  land.temp <- land.temp[!apply(land.temp, 1, function(x) {any(x == "ERR ")}),] #remove error from previous line caused by FRAGSTAT
  land.id.replace=chartr("\\","_", as.character(land.temp$LID))
  land.id.split = strsplit(land.id.replace,"_")
  land.id=sapply(strsplit(land.id.replace, "_"), "[[", index.CELLID)
  land.temp[,"CELLID"] = land.id
  setnames(land.temp,make.names(paste("l",colnames(land.temp), sep = "_")))
  
  ## merge class and landscape level ##
  #match by CELLID
  id_pl = match(class.class1$c_CELLID,land.temp$l_CELLID) #update V2
  xtra_pl = land.temp[id_pl,]
  #merge dataframes
  merge.file = cbind(class.class1,xtra_pl)
  #remove irrelevant variables 
  db.all = cbind(CELLID = merge.file$l_CELLID, merge.file)
  drops = c("c_LID","c_TYPE","c_CELLID","l_LID","l_CELLID")
  db.all = db.all[,!drops, with=FALSE] 
  
  ## export as RData ##
  save(db.all, file = paste(fragstat.out.path,"/",dataset,"_",wsize,".RData",sep=""))
  ##### end processing ####
  }
  
  ##### output messages ####
  cat(paste("### RESULT 3 out of 3: ", length(fileNames.class)," class and ", length(fileNames.land)," land FRAGSTAT levels files using a fishnet size of ", wsize," for detection period ", date.ini," to ", date.end," were successfully merged! ###",sep=""))
  ##### end output messages ####
  
    }
    