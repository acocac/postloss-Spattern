##############################################################################
# title         : function to generate input data to fragstat and fractal analyses;
# purpose       : create separated grids by unit of analysis for fragstat and 
#                 fractal analyses;
# producer      : prepared by A. Coca;
# last update   : in London, UK June 2015 / Updated in July 2015;;
# inputs        : deforestation dataset by year, fishnet shapefile;
# outputs       : split grids in GeoTIFF format (FRAGSTAT/FRACTAL INPUT);
# remarks 1     : associated with the 0_extract_grids_by_ua_parallel.R code;
###############################################################################

GenUniGrid=function(i){
  
  ei = as(extent(det.data), "SpatialPolygons")
  db=data.frame(myshp[i,])
 
  if (startsWith(gRelate(ei, myshp[i,]),"2")){ #check geometry
    
    # CROP
    tryPoly <- myshp@polygons[[i]]@Polygons[[1]]@coords
    MaxY <- max(tryPoly[,2])
    MaxX <- max(tryPoly[,1])
    MinY <- min(tryPoly[,2])
    MinX <-min(tryPoly[,1])
    ext <- extent(cbind(c(MinX,MinY), c(MaxX,MaxY)))
    
    rawdet.masked <- raster::intersect(det.data,ext)
    
    if (!(round(extent(rawdet.masked)) == round(ext))) {
      rawdet.masked = extend(rawdet.masked, ext)
    }
    
    #settings (to avoid scientific notation in the filename when is exported)
    options(scipen=999)
    
    freq_table = freq(rawdet.masked,useNA='no')
    
    if (length(freq_table) > 0){ #avoid rasters with spontaneous NAs
      
      #check point for selecting raster with target values
      nonvalues_idx = which(freq_table[,1] %in% c(0,999)) #check indexes of non-target values
      check_p = length(freq_table[,1]) - length(nonvalues_idx)
        
      if (length(check_p) > 0){
        prop_nontarget= sum(freq_table[nonvalues_idx,2]) / sum(freq_table[,2])
        
        if (prop_nontarget < 1){
          
          index_accum = which(freq_table[,1] >= date.ini & freq_table[,1] <= date.end)
          values_accum = freq_table[index_accum,1]
          
          det.math.value0 <- function(x) { x[x %in% values_accum] <- 1; return(x) }
          det.initial.1 <- calc(rawdet.masked, det.math.value0)
          
          det.math.value1 <- function(x) { x[!x %in% values_accum & x != as.numeric(bkg_value)] <- 0; return(x) }
          det.initial.1 <- calc(det.initial.1, det.math.value1)
          det.initial.1[is.na(det.initial.1)] <- as.numeric(bkg_value)
          
          #export raster
          writeRaster(det.initial.1, filename=paste(output.TIFF,"/",db$CELLID,"_cummulative_",date.ini,"to",date.end,".tif",sep=""), format="GTiff", overwrite=TRUE)
          
          #save to log
          log.file <- paste0(log.path,"/",db$CELLID,".csv")
          write.table(data.frame("CELLID" = db$CELLID, "type" = "data"), file=log.file, append = F, sep=",", col.names=T, row.names = F)
        
          } else {
          #save to log
          log.file <- paste0(log.path,"/",db$CELLID,".csv")
          write.table(data.frame("CELLID" = db$CELLID, "type" = "nodata"), file=log.file, append = F, sep=",", col.names=T, row.names = F)
        }
      } else {
        #save to log
        log.file <- paste0(log.path,"/",db$CELLID,".csv")
        write.table(data.frame("CELLID" = db$CELLID, "type" = "nodata"), file=log.file, append = F, sep=",", col.names=T, row.names = F)
      }
    } else {
    #save to log
      log.file <- paste0(log.path,"/",db$CELLID,".csv")
      write.table(data.frame("CELLID" = db$CELLID, "type" = "NA"), file=log.file, append = F, sep=",", col.names=T, row.names = F)
    }
    } else {
      #save to log
      log.file <- paste0(log.path,"/",db$CELLID,".csv")
      write.table(data.frame("CELLID" = db$CELLID, "type" = "NA"), file=log.file, append = F, sep=",", col.names=T, row.names = F)
    }
}
