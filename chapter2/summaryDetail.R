##This function provides a detailed summary of various statistics in regard to a single
##numeric variable. These statistics were collected based on the output of the STATA
##summary, detail

summaryDetail = function(inputVec){
  PERCENTILE = quantile(inputVec,probs=c(0.01,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99))
  sortedInput = sort(inputVec)
  SMALL4 = sortedInput[1:4]
  BIG4 = sortedInput[(length(sortedInput)-3):length(sortedInput)]
  NUM_OB = length(inputVec)
  MEAN = mean(inputVec)
  SD = sd(inputVec)
  VAR = var(inputVec)
  m3 = sum((inputVec-MEAN)^3)/NUM_OB
  s3 = sqrt((SD)^2)^3
  SKEW = m3/s3
  m4 = sum((inputVec-MEAN)^4)/NUM_OB
  s4 = SD^4
  KURTOSIS = m4/s4
  NA_NUM = length(which(is.na(inputVec)))
  structure(list(Percentiles = PERCENTILE, Smallest = SMALL4, Largest = BIG4, Obs = NUM_OB, Mean = MEAN, Std.Dev = SD, Variance = VAR, Skewness = SKEW, Kurtosis = KURTOSIS, NAs = NA_NUM ))
  
}