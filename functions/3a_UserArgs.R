##############################################################################
# title         : user dirs prompt;
# purpose       : identify users dirs to copy them then in an unique project dir;
# producer      : prepared by A. Coca;
# last update   : in Bogota, UK September 2015;
# inputs        : NA;
# outputs       : user directories and processed files;
# remarks 1     : ;
###############################################################################

prompt.user.part3 <- function()#get arguments from user
{
  message(prompt="Please type the deforestation dataset name::>>> ") 
  a <- readLines(n = 1)
  
  message(prompt="Please type the fishnet size:>>> ") 
  b <- readLines(n = 1)
  
  message(prompt="Please type number of processors for parallel processing:>>> ") 
  c <- readLines(n = 1)
  
  message(prompt="Please type if process will be run again - yes or no -:>>> ") 
  d <- readLines(n = 1)
  
  newlist = list(a,b,c,d)
  return(newlist)
}