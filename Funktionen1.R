
# Funktionen zum direkten Abrufen von Daten für Good and Bad Responder

BR = function(x) 
{
  gruppiert = x[which(Gruppe == 2)]
  return(gruppiert)
}

GR = function(x) 
{
  gruppiert = x[which(Gruppe == 1)]
  return(gruppiert)
}


# nützliche Variablen zur Ausgabe in Plots
groupnames1 = c("Good Responder", "Bad Responder")
groupnames2 = c("GR", "BR")
groupcol1 = c("green3", "red2")

medkat1 = c("seit OP nicht mehr", 
            "seit OP Dosis verringert",
            "keine Änderung durch OP", 
            "seit OP Dosis erhöht", 
            "Einnahme erst nach OP begonnen")

medkat2 = c("Not since surgery", 
            "smaller dose since Surgery",
            "no change due to surgery", 
            "higher dose since surgery", 
            "only since surgery")

medkat3 = c("No Medication", 
            "Smaller Dose",
            "No Change", 
            "Higher Dose", 
            "New Medication")



# Color Scheme ------------------------------------------------------------
library(colorspace)

stand.col <- rainbow_hcl(2, start = 60, end = 240) 

# Functions for Statistics ------------------------------------------------

logisticPseudoR2s <- function(LogModel) {
  dev <- LogModel$deviance 
  nullDev <- LogModel$null.deviance 
  modelN <-  length(LogModel$fitted.values)
  R.l <-  1 -  dev / nullDev
  R.cs <- 1- exp ( -(nullDev - dev) / modelN)
  R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
  cat("Pseudo R^2 for logistic regression\n")
  cat("Hosmer and Lemeshow R^2  ", round(R.l, 3), "\n")
  cat("Cox and Snell R^2        ", round(R.cs, 3), "\n")
  cat("Nagelkerke R^2           ", round(R.n, 3),    "\n")
}


# Outlier exclusion on a 2.2 IQR criterion
outliers <- function(score, int = FALSE) {
  
  # score should be a vector
  score <- as.numeric(score)
  
  # calcutate quantiles: NAs will be removed
  Q3.score <- as.numeric(quantile(score,0.75, na.rm = TRUE))
  Q1.score <- as.numeric(quantile(score,0.25, na.rm = TRUE))
  
  # calculate upper and lower boundaries
  upper.score = Q3.score + (2.2 * (Q3.score - Q1.score))
  lower.score = Q1.score - (2.2 * (Q3.score - Q1.score))
  
  # subset the score
  score_noOutlier <- score[score >= lower.score & score <= upper.score]
  
  ifelse(length(score_noOutlier) == length(score), 
         print(paste0("no outliers in the Score", names(score))),
         print(paste0((length(score)-length(score_noOutlier)), " outliers were detected Remaining: ", length(score_noOutlier)))
  )
  # return the cleaned score
  if(int == FALSE){ 
    ifelse(length(score_noOutlier) == length(score), return(score), return(score_noOutlier))
    }
  
  # if stated, returnn integer list of remaining values after exclusion instead
  if(int == TRUE) {
    return(ifelse(score >= lower.score & score <= upper.score, TRUE, FALSE))
  }
}

# Scaling to use in dplyr chains
scale.this <- function(x) as.vector(scale(x))


cor.own <- function(df, method = "pearson") 
{
  cor.est <- lapply(c(1:ncol(df)), function(n) {
    apply(df,2, function(col) {cor.test(col, df[,n], method = method, exact=FALSE)$estimate})
  }) 
  cor.p <- lapply(c(1:ncol(df)), function(n) {
    apply(df,2, function(col) {cor.test(col, df[,n], method = method, exact=FALSE)$p.value})
  }) 
  
  cor.est <- do.call(rbind.data.frame, cor.est)
  cor.p <- do.call(rbind.data.frame, cor.p)
  colnames(cor.est) <- colnames(df)
  rownames(cor.est) <- colnames(df)
  colnames(cor.p) <- colnames(df)
  rownames(cor.p) <- colnames(df)  
  
  
  print(cor.est, digits = 3)
  print(cor.p, digits = 3)
}


# produce 95% Confidence Intervals drom mean and SE
conf.int <- function(mean, se) {
  lower <- mean - 1.96 * se
  upper <- mean + 1.96 * se
  df <- data.frame("lower 95% CI" = lower, 
                   "upper 95% CI" = upper)
  print(df)
}
# 
# 
# library(Hmisc)
# 
# 
# ## testdata
#  df <- data.frame(id = c(1:6), 
#                 p.code = c(1, 5, 4, NA, 0, 5),  
#                 p.label = c('Optometrists', 'Nurses', 
#                               'Financial analysts', '<NA>', 
#                               '0', 'Nurses'), 
#                   foo = LETTERS[1:6])
# # Add some variable labels using label from the Hmisc package
#  require(Hmisc)
#  label(df) <- "Sweet sweet data"
#  label(df$id) <- "id !@#$%^" 
#  label(df$p.label) <- "Profession with human readable information"
#  label(df$p.code) <- "Profession code"
#  label(df$foo) <- "Variable label for variable x.var"
# 
# # USAGE
#  write.Hmisc.SPSS(df, datafile="df.sav", codefile="df.sps")
# #
# # Original "write.SPSS" function taken from:
# # https://stat.ethz.ch/pipermail/r-help/2006-January/085941.html
# 
# 
# label
# 
# ## calling the function..
#  a = do.call(llist, df)
#  tempout = vector("list", length(a))
#  
#  for (i in 1:length(a)) {
#    tempout[[i]] = label(a[[i]])
#  }
#  b = unlist(tempout)
#  label.temp = structure(c(b), .Names = names(data))
#  attributes(data)$variable.labels = label.temp
# # source("http://dl.dropbox.com/u/2556524/R%20Functions/writeSPSS.R")
#  write.SPSS(data, datafile, codefile)
#  }

