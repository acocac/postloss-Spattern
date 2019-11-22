##############################################################################
# title         : grid files (inputs) to extract fragstat and fractal metrics;
# purpose       : create separated grids (unit of analysis) from detection grid for fragstat and 
#                 fractal analyses;
# producer      : prepared by A. Coca;
# last update   : in London, UK June 2015 / Updated in March 2017;
# inputs        : deforestation grid by year, fishnet (windows) shapefile;
# outputs       : split detection grid using  in GeoTIFF format (FRAGSTAT/FRACTAL INPUT);
# remarks 1     : detection grid must be in projected projection (i.e IGH or LAE);
###############################################################################

CreateProjectAndFishnets = function(dataset.path, det.reference.user){ 
  if (missing(dataset.path))
    stop("Need to specify the path with deforestation dataset(s) to analise.")
 
  if (missing(det.reference.user))
    stop("Need to specify the reference dataset name to create fishnet(s) (it is usually the dataset with the coarsest resolution).")
  
  #### libraries ####
  pckg = c("raster", "maptools", "sp", "rgeos","rgdal") 
  
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
  proj.CRS = conf.args[[5]]
  fishnet.sizes = conf.args[[6]]
  
  #identify datasets and copy in a project folder
  split.datasets = strsplit(list.dirs(dataset.path, recursive = F),"/")
  datasets = c(unique(sapply(split.datasets, "[", length(split.datasets[[1]]))))
  # for (dataset in datasets){
  #   det.dir.all = paste(root.dir,"/input/geodata/raster/processed/detection","/det_all/",dataset,"/",proj.acronym,"/",sep="")
  #   dir.create(det.dir.all, recursive = T, mode = "0777", showWarnings = F)
  #   dataset.user.files = list.files(paste(dataset.path,"/",dataset, sep=""),".tif$", full.names = T)
  #   det.data=raster(dataset.user.files)
  #   writeRaster(det.data, filename=paste0(det.dir.all,"/",basename(dataset.user.files)), format="GTiff", overwrite=TRUE)
  # }
  
  #input dir where detection fishnet files (window sizes) will be kept
  fishnet.dir = paste(root.dir,"/","input/geodata/shp/target/fishnet/",proj.acronym,sep="")
  dir.create(fishnet.dir, recursive = T, mode = "0777", showWarnings = F)
  
  ## load data ##
  #load CRS
  proj.CRS <- CRS(proj.CRS)
  
  #load reference detection raster
  ref.dataset.path <- paste(root.dir,"/input/geodata/raster/processed/detection/det_all/",dataset,"/",proj.acronym,"/",sep="")
  ref.dataset.file <- list.files(ref.dataset.path, ".tif$", full.names = T)
  ##### end static ####
  
  ##### processing ####
  ## create fishnets ##
  for (fs in fishnet.sizes){
    #resample reference detection raster dataset by fishnet size
    r <- raster(ref.dataset.file)
    #get raster extent 
    e_area = extent(r)
    #continue resampling
    res(r) <- as.numeric(fs)
    r[] <- 1
    r <- extend(r, c(1,1))
    r[is.na(r)] <- 1
    pol <- rasterToPolygons(r)
    #transform to same projection used in the target area
    pol.IGH <- spTransform(pol, proj.CRS)
    pol.IGH$CELLID = 1:length(row.names(pol.IGH))
    pol.IGH[[1]] = NULL #remove column
    #export final fishnet objects selected by target area extent
    writeOGR(pol.IGH,dsn = fishnet.dir,layer = paste("fishnet_",as.character(fs),sep = ""), driver="ESRI Shapefile", overwrite_layer = T)
  }
  ##### end processing ####
  
  ##### output messages ####
  cat(paste("### RESULT: Project folder was successfully created and contains ",length(datasets)," datasets (", paste(datasets, collapse = ' & '),") and ",length(fishnet.sizes)," fishnet sizes (", paste(fishnet.sizes, collapse = ' & '),"m) ###",sep=""))
  ##### end output messages ####
  
}
  