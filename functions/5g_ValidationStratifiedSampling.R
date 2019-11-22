#############################################################################
# title         : function for stratified sampling of classified unlabelled grid-based objects;
# purpose       : sample with equal number by class;
# producer      : Tommy Levi;
# last update   : May 2013;
# inputs        : classified unlabelled grid-based objects;
# outputs       : sampling dataframe;
# remarks 1     : taken from website
# http://stackoverflow.com/questions/16493920/
# how-can-i-ensure-that-a-partition-has-representative-observations-from-each-leve
##############################################################################

stratified <- function(df, group, size) {
  # USE: * Specify your data frame and grouping variable (as column
  # number) as the first two arguments.
  # * Decide on your sample size. For a sample proportional to the
  # population, enter "size" as a decimal. For an equal number
  # of samples from each group, enter "size" as a whole number.
  #
  # Example 1: Sample 10% of each group from a data frame named "z",
  # where the grouping variable is the fourth variable, use:
  #
  # > stratified(z, 4, .1)
  #
  # Example 2: Sample 5 observations from each group from a data frame
  # named "z"; grouping variable is the third variable:
  #
  # > stratified(z, 3, 5)
  #
  require(sampling)
  temp = df[order(df[group]),]
  colsToReturn <- ncol(df)
  
  #Don't want to attempt to sample more than possible
  dfCounts <- table(df[group])
  if (size > min(dfCounts)) {
    size <- min(dfCounts)
  }
  
  if (size < 1) {
    size = ceiling(table(temp[group]) * size)
  } else if (size >= 1) {
    size = rep(size, times=length(table(temp[group])))
  }
  strat = strata(temp, stratanames = names(temp[group]),
                 size = size, method = "srswor")
  (dsample = getdata(temp, strat))
  
  dsample <- dsample[order(dsample[1]),]
  dsample <- data.frame(dsample[,1:colsToReturn], row.names=NULL)
  return(dsample)
  
}