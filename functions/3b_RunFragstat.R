#############################################################################
# title         : run fragstat functions;
# purpose       : run fragstat by command line and parallel processing;
# producer      : prepared by A. Coca;
# last update   : in London, UK Abr 2015 / Updated in July 2015;
# inputs        : unit of analysis files in FBT format (FRAGSTAT BATCH);
# outputs       : FRAGSTAT at class and landscape level (.class, .land); 
# remarks 1     : associated with the 2_runfragstat.R code;
##############################################################################

RunFragstat=function(i){
  filename = as.character(strsplit(basename(list.batch[i]),".fbt")[1])
  command.line=paste(FragstatEXE,"-m",FragstatFCAfile,"-b",list.batch[i],"-o",paste(project.output.path,"/",filename,sep=""),sep=" ")
  system(command.line)
}