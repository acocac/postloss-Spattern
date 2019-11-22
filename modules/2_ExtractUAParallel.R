##############################################################################
# title         : grid files (inputs) to extract fragstat and fractal metrics;
# purpose       : create separated grids (unit of analysis) from detection grid for fragstat and 
#                 fractal analyses;
# producer      : prepared by A. Coca;
# last update   : in London, UK June 2015 / Updated in September 2015;
# inputs        : deforestation grid by year, fishnet (windows) shapefile;
# outputs       : split detection grid using  in GeoTIFF format (FRAGSTAT/FRACTAL INPUT);
# remarks 1     : detection grid must be in projected projection (i.e IGH or LAE);
###############################################################################

ExtractUA = function(dataset, wsize, processors,bkg_value, rerun)
{
  if (missing(dataset))
    stop("Need to specify dataset name.")
  
  if (missing(wsize))
    stop("Need to specify the fishnet size")

  if (missing(processors))
    stop("Need to specify processors numbers for parallel processing")
  
  if (missing(bkg_value))
    stop("Need to specify background raster value e.g. 999")
  
  if (missing(rerun))
    stop("Need to specify if you rerun yes or no")
  
  #### libraries ####
  pckg = c("raster", "maptools", "rgdal", "snowfall", "parallel") 
  
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

  #input dir where detection GEOTIFF dataset (e.g. GFC or terra) from the target area will be kept
  det.dir.all = paste(root.dir,"/input/geodata/raster/processed/detection/det_all/",dataset,"/",proj.acronym,"/",sep="")
  det.dataset.file <- list.files(det.dir.all, ".tif$", full.names = T)

  #input dir where detection fishnet files (window sizes) will be kept
  fishnet.dir = paste(root.dir,"/","input/geodata/shp/target/fishnet/",proj.acronym,sep="")

  #output
  output.TIFF=paste(root.dir,"/input/geodata/raster/processed/detection/det_",date.ini,"to",date.end,"/",dataset,"/by_fishnet/",proj.acronym,"/",wsize,sep="")
  dir.create(output.TIFF, recursive = T, mode = "0777", showWarnings = F)
  
  ## load data ##
  #load fishnet
  myshp <- readOGR(dsn=fishnet.dir, layer=paste("fishnet_",wsize,sep=""))
  
  #load and export analysed raster
  det.data=raster(det.dataset.file)
  
  #create empty log DF
  log.path=paste0(root.dir,"/input/geodata/raster/processed/detection/det_",date.ini,"to",date.end,"/",dataset,"/by_fishnet/",proj.acronym,"/log_",wsize,sep="")
  dir.create(log.path, recursive = T, mode = "0777", showWarnings = F)
  log.files <- list.files(log.path, ".csv$", full.names = T)
  
  ##### end static ####
  
  ##### processing ####
  # ## parallel process ##
  sfInit(parallel=T,cpus=processors)
  sfLibrary(snowfall)
  sfLibrary(raster)
  sfLibrary(maptools)
  sfLibrary(rgdal)
  sfLibrary(rgeos)
  sfExport("bkg_value")
  sfExport("date.ini")
  sfExport("date.end")
  sfExport("GenUniGrid")
  sfExport("myshp")
  sfExport("det.data")
  sfExport("output.TIFF")
  sfExport("log.path")
  if (rerun == "yes"){
    if (length(log.files) > 0){
      #check log files 
      split.files = strsplit(basename(log.files),"[.]")
      files.done = as.numeric(c(unique(sapply(split.files, "[", 1))))
      files.left=which(is.na(match(myshp$CELLID,files.done)))
      if (length(files.left) > 0){
        cat("missed files exist... \n")
        sfLapply(files.left,fun=GenUniGrid)
      } 
      repeat {
      #check missclassified rasters
      list.grids = list.files(output.TIFF, pattern = "*.tif", full.names = F)
      split.files = strsplit(list.grids,"_")
      files.done = as.numeric(c(unique(sapply(split.files, "[", 1))))
      split.files = strsplit(basename(log.files),"[.]")
      files.all = as.numeric(c(unique(sapply(split.files, "[", 1))))
      files.left=files.all[which(is.na(match(files.all,files.done)))]
      if (length(files.left) > 0){
        cat("missclassified files might exist... \n")
        sfLapply(files.left,fun=GenUniGrid)
      }
      list.grids.new = list.files(output.TIFF, pattern = "*.tif", full.names = F)
      update.grids = length(list.grids.new) - length(list.grids) 
      if (update.grids == 0) break
    }
  }} else {
    sfLapply(1:length(myshp),fun=GenUniGrid)
  }
  sfStop()
  ##### end processing ####
  
  ##### output messages ####
  n.output.files <- length(list.files(output.TIFF, pattern = "*.tif", full.names = F)) #list patch files  
  cat(paste("### RESULT: ", n.output.files," individual geoTIFF files using a fishnet size of ", wsize," for detection period ", date.ini," to ", date.end," were successfully created! ###",sep=""))
  ##### end output messages ####
  
}
