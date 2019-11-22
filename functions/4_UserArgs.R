##############################################################################
# title         : user dirs prompt;
# purpose       : identify users dirs to copy them then in an unique project dir;
# producer      : prepared by A. Coca;
# last update   : in Bogota, UK September 2015;
# inputs        : NA;
# outputs       : user directories and processed files;
# remarks 1     : ;
###############################################################################

prompt.user.part4 <- function()#get arguments from user
{
  ### functions ##
  r.dir <- gsub("////", "/", r.dir)
  source(paste(r.dir,"/functions/0_LoadConfig.R", sep=""))
  ### end load ###
  conf.args = LoadConfig(conf.file)
  #associated config info to code variables
  root.dir = conf.args[[1]]
  datasets = dir(paste0(root.dir,"/input/geodata/raster/processed/detection/det_all"))
  fishnets = conf.args[[6]]

  repeat{
    a <- readline(prompt=paste0("Please type a valid deforestation dataset name (***valid values: ",paste(datasets,collapse = " or "),") ::>>> "))
    if (a %in% datasets || a == "exit") break
  }

  repeat{
    b <- readline(prompt=paste0("Please type a valid fishnet size (***valid values: ",paste(fishnets,collapse = " or "),") ::>>> "))
    if (b %in% fishnets || b == "exit") break
  }
  
  repeat{
    c <- readline(prompt=paste0("Please type a valid label collection approach  (***valid values: single or multi) ::>>> "))
    if (c %in% c("single","multi") || c == "exit") break
  }
  
  if (c == "single"){
    repeat{
      d <- readline(prompt=paste0("Please type a valid field name created to record the training samples for a single period ::>>> "))
      if (d != "" || d == "exit") break
    }
    newlist = list(a,b,c,d)
  } else {
    newlist = list(a,b,c)
  }
  return(newlist)
  }