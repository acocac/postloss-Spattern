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

LoadConfig = function(x)
{ 
conf.list <- lapply(strsplit(readLines(x)," "), as.character)

#read target lines
root.index <- grep("*path",conf.list)
root.path = conf.list[[root.index]][[length(conf.list[[root.index]])]]

date.ini.index <- grep("*det.ini",conf.list)
date.ini = conf.list[[date.ini.index]][[length(conf.list[[date.ini.index]])]]

date.end.index <- grep("*det.end",conf.list)
date.end = conf.list[[date.end.index]][[length(conf.list[[date.end.index]])]]

proj.acronym.index <- grep("*proj.acronym",conf.list)
proj.acronym = conf.list[[proj.acronym.index]][[length(conf.list[[proj.acronym.index]])]]

proj.CRS.index <- grep("*proj.CRS",conf.list) 
proj.CRS = paste(conf.list[[proj.CRS.index]][3:length(conf.list[[proj.CRS.index]])],collapse = " ")

fishnet.sizes.index <- grep("*.sizes",conf.list) 
fishnet.sizes = conf.list[[fishnet.sizes.index]][[length(conf.list[[fishnet.sizes.index]])]]
fishnet.list <- unlist(strsplit(fishnet.sizes, ";"))

fragstat.exe.index <- grep("*fragstat.exe",conf.list) 
fragstat.exe = conf.list[[fragstat.exe.index]][[length(conf.list[[fragstat.exe.index]])]]

fragstat.fca.index <- grep("*fragstat.fca.file",conf.list) 
fragstat.fca = conf.list[[fragstat.fca.index]][[length(conf.list[[fragstat.fca.index]])]]

fragstat.class.index <- grep("*class.metrics",conf.list) 
fragstat.class = conf.list[[fragstat.class.index]][[length(conf.list[[fragstat.class.index]])]]
class.list <- unlist(strsplit(fragstat.class, ";"))

fragstat.land.index <- grep("*land.metrics",conf.list) 
fragstat.land = conf.list[[fragstat.land.index]][[length(conf.list[[fragstat.land.index]])]]
land.list <- unlist(strsplit(fragstat.land, ";"))

model.CV.repeats.index <- grep("*CV.n.repeats",conf.list) 
model.CV.repeats = conf.list[[model.CV.repeats.index]][[length(conf.list[[model.CV.repeats.index]])]]

model.CV.resampling.index <- grep("*CV.n.resampling",conf.list) 
model.CV.resampling = conf.list[[model.CV.resampling.index]][[length(conf.list[[model.CV.resampling.index]])]]

newlist = list(root.path,date.ini,date.end,proj.acronym,proj.CRS, fishnet.list, 
               fragstat.exe,fragstat.fca,class.list,land.list,model.CV.repeats,model.CV.resampling)
return(newlist)
}

LoadConfig(conf.file)
