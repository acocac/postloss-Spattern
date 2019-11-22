##############################################################################
# title         : user dirs prompt;
# purpose       : identify users dirs to copy them then in an unique project dir;
# producer      : prepared by A. Coca;
# last update   : in Bogota, UK September 2015;
# inputs        : NA;
# outputs       : user directories and processed files;
# remarks 1     : ;
###############################################################################

prompt.user.part5f <- function()#get arguments from user
{
  message(prompt="Please type the deforestation dataset name::>>> ") 
  a <- readLines(n = 1)
  
  message(prompt="Please type the fishnet size:>>> ") 
  b <- readLines(n = 1)
  
  message(prompt="Please type the data mining model type for the sensitivity analysis:>>> ") 
  c <- readLines(n = 1)
  
  message(prompt="Please type the number of samples by pattern typology:>>> ") 
  d <- as.numeric(readLines(n = 1))
  
  message(prompt="Please type the number of test repetitions:>>> ") 
  e <- as.numeric(readLines(n = 1))
  
  newlist = list(a,b,c,d,e)
  return(newlist)
}