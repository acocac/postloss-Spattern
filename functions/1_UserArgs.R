##############################################################################
# title         : user dirs prompt;
# purpose       : identify users dirs to copy them then in an unique project dir;
# producer      : prepared by A. Coca;
# last update   : in Bogota, UK September 2015;
# inputs        : NA;
# outputs       : user directories and processed files;
# remarks 1     : ;
###############################################################################

prompt.user.part1 <- function()#get arguments from user
{
  message(prompt="Please type the path with deforestation dataset(s) to analise:>>> ") 
  a <- readLines(n = 1)
  a <- gsub("\\\\", "/", a)
  
  message(prompt="Please type the reference dataset name to create fishnet(s) (usually the dataset with the coarsest resolution):>>>")
  b <- readLines(n = 1)
  
  newlist = list(a,b)
  return(newlist)
}