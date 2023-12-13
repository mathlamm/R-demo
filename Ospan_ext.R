###  Computing Operational Span Task scores +++
###  
### Ospan-Data from external datasets
###

###
###  Script by Mathis Lammert (lammert@cbs.mpg.de)
###
###  last edit: 2018-10-16
###


# Preperation -------------------------------------------------------------

# Clean start when reload
rm(list = ls())

# Load config, additional functions
source("config.R")
if(!exists("foo", mode="function")) source("Funktionen1.R")

# Set working directory
setwd("./Ospan")

# Load librarys
library(tidyverse)
library(EnvStats)
library(psych)
library(car)
library(pastecs)
library(psych)  

# Data loading & manipulation ---------------------------------------------

# List of all Ospan datasets that came by Engle et al
files <- list.files(path = "./Ospan Engle/", pattern=".txt")
path <- paste0("./Ospan Engle/",files[4])


# Reading out data
Rawdata <- read.table(path, sep = ",", 
                   stringsAsFactors = FALSE, 
                   header = TRUE,
                   na.strings = "")

# List of subject-ids within the raw-data
ids <- unique(as.list(Rawdata$Subject))
length(ids)

# Create a list of raw-data by subjects and name the elements by its IDs
data <- lapply(ids, function(x) {subset(Rawdata, Subject == x)})
names(data) <- ids



# Mining: Extracting Ospan sets from raw files ------------------------------------
Ospan_sets_f <- function(x) 
{
  # Stop funtion if x is not part of data (e.g. excluded subjects)
  if(!length(data[[as.character(x)]]) > 0) stop("Error: requested ID is not part of the dataset")
  
  # Start new trial-based dataframe, one for each subject
  Ospan_set <- data.frame(Trial = unique(data[[as.character(x)]]$Trial))
  
  # Looping through all 15 Trials
  for(i in 1:nrow(Ospan_set))
  { 
    # Set ID
    Ospan_set[i,"ID"] <- as.character(x)
    
    # Extract Setsize
    Ospan_set[i,"Setgroesse"] <- subset(data[[as.character(x)]], 
                                             Procedure.Block. == "SessionProc" & 
                                               Trial == i & !is.na(letterstimuli), 
                                             select = "setsz") %>% 
      unlist() %>% unique()
    
    # Extract: Nr. of accuracy errors
    Ospan_set[i,"Err.Acc"] <- subset(data[[as.character(x)]], 
                                       Procedure.Block. == "SessionProc" & 
                                         Trial == i & OPERATION.ACC == 0 & 
                                         showProblem.RT != 0, 
                                       select = "OPERATION.ACC") %>% nrow()
    
    # Extract: Nr. of speed errors
    Ospan_set[i,"Err.Speed"] <- subset(data[[as.character(x)]], 
                                              Procedure.Block. == "SessionProc" & 
                                                Trial == i & !is.na(showProblem.RT) & 
                                                showProblem.RT == 0, 
                                              select = "showProblem.RT") %>% nrow()
    # Note: The may be a bug in the logfile: Accuracy-Errors are sometimes also reported, if a speed error occured. Work around: Count only those errors as accuracy errors, that do not coincide with speed errors. 
    
    # Extract: Nr. of correct calculations (SequLength - SpeedErrors - AccuraccyErrors)
    Ospan_set[i,"Correct.Calc"] <- subset(data[[as.character(x)]], 
                                              (Procedure.Block. == "SessionProc" & Trial == i) & 
                                                ((!is.na(showProblem.RT) & showProblem.RT > 0) &
                                                (!is.na(OPERATION.ACC) & OPERATION.ACC == 1 )), 
                                              select = c(OPERATION.ACC, showProblem.RT)) %>% nrow()
    
    # Extract: Nr. of correct calculations (like above) AND substraction of the first calculation
    Ospan_set[i,"Correct.Calc.MinusOne"] <- subset(data[[as.character(x)]], 
                                                   (Procedure.Block. == "SessionProc" & Trial == i & SubTrial != 1) & 
                                                     ((!is.na(showProblem.RT) & showProblem.RT > 0) &
                                                        (!is.na(OPERATION.ACC) & OPERATION.ACC == 1 )), 
                                                   select = c(OPERATION.ACC, showProblem.RT)) %>% nrow()
    
    # Extract overall correct calculations in percent. This is only the overall ratio, but stated at every trial.
    Ospan_set[i,"Correct.Calc.intern"] <- round((75 - as.numeric(tail(data[[as.character(x)]]$MathErrorTotal, n=1)) ) / 0.75, digits = 2)
    
    # Extract given sequence
    Ospan_set[i,"Seq.given"] <- subset(data[[as.character(x)]], 
                                       Procedure.Block. == "SessionProc" & 
                                         Trial == i & !is.na(letterstimuli), 
                                       select = "letterstimuli") %>% 
      unlist() %>% paste(collapse = "")
    
    # Extract stated Sequence
    # conditions: 
    # a) Replace "blank" with "X". 
    # b) Remove substring until "clear". 
    # c) Remove "InvalidResponse".
    Ospan_set[i,"Seq.stated"] <- subset(data[[as.character(x)]], 
                                             Procedure.Block. == "SessionProc" & 
                                               Trial == i & !is.na(WordSelection) & 
                                               WordSelection != "Exit", 
                                             select = "WordSelection") %>% 
      unlist() %>% 
      gsub(pattern = "blank", replacement =  "X") %>%
      paste(collapse = "") %>%
      gsub(pattern = "^.*clear", replacement =  "") %>%
      gsub(pattern = "InvalidResponse", replacement =  "") 
    
    # Extract stated Sequence with no X
    # conditions: 
    # a) Replace "blank" with "". 
    # b) Remove substring until "clear". 
    # c) Remove "InvalidResponse".
    Ospan_set[i,"Seq.stated_noX"] <- subset(data[[as.character(x)]], 
                                        Procedure.Block. == "SessionProc" & 
                                          Trial == i & !is.na(WordSelection) & 
                                          WordSelection != "Exit", 
                                        select = "WordSelection") %>% 
      unlist() %>% 
      gsub(pattern = "blank", replacement =  "X") %>%
      paste(collapse = "") %>%
      gsub(pattern = "^.*clear", replacement =  "") %>%
      gsub(pattern = "InvalidResponse", replacement =  "") %>%
      gsub(pattern = "X", replacement =  "") 

  }
  
  return(Ospan_set)
}



# Produce a list with all Ospan sets to speed up further calls, making use of the above function.
Ospan_sets <- lapply(ids,Ospan_sets_f)
names(Ospan_sets) = ids 


# Ospan Score (script internal) -------------------------------------------
#
# Ospan Scores (All-or-Nothing and Total Score), as stated within the original logfiles
Ospan_scores <- data.frame(
  ID = unlist(ids),
  Ospan.Total = as.numeric(sapply(ids, function(x) {tail(data[[as.character(x)]]$OspanTotal, n=1)})),
  Ospan.Score = as.numeric(sapply(ids, function(x) {tail(data[[as.character(x)]]$OspanScore, n=1)})),
  Math.Error.Total = as.numeric(sapply(ids, function(x) {tail(data[[as.character(x)]]$MathErrorTotal, n=1)})),
  Acc.Error.Total = as.numeric(sapply(ids, function(x) {tail(data[[as.character(x)]]$AccErrorTotal, n=1)})),
  Speed.Error.Total = as.numeric(sapply(ids, function(x) {tail(data[[as.character(x)]]$SpeedErrorTotal, n=1)})),
  stringsAsFactors = FALSE
)



# Validity-Test -----------------------------------------------------------
# 
# Testing script-validity: Is my error-rate-computation equal to the error-summary of the logfile-internal error-summary?

Errors.Comparison <- function(x)
{
  
  Errors.Comparison <- data.frame(  # int = the error-summary as stated within the raw-data. ext = The item-based comptation of errors by this script. 
    ID = ids[x],
    Acc.Err.int = Ospan_scores[x,"Acc.Error.Total"],
    Acc.Err.ext = subset(data[[x]], Procedure.Block. == "SessionProc" & OPERATION.ACC ==  0 & showProblem.RT != 0, 
                          select = c(OPERATION.ACC, showProblem.RT ),
                          by = showProblem.RT) %>% nrow(),
    Speed.Err.int = Ospan_scores[x,"Speed.Error.Total"],
    Speed.Err.ext = subset(data[[x]], Procedure.Block. == "SessionProc" & !is.na(showProblem.RT) & showProblem.RT == 0, 
                              select = c(showProblem.RT),
                              by = showProblem.RT) %>% nrow(),
    Math.Err.int = Ospan_scores[x,"Math.Error.Total"],
    Math.Err.int2 = subset(data[[x]], Procedure.Block. == "SessionProc" & SubTrial == 1, select = c(numberwrong)) %>% unlist() %>% sum()
  )
  
  Errors.Comparison$Math.Err.ext <- Errors.Comparison$Speed.Err.ext + Errors.Comparison$Acc.Err.ext
  Errors.Comparison$compAcc <- Errors.Comparison$Acc.Err.int == Errors.Comparison$Acc.Err.ext
  Errors.Comparison$compSpeed <- Errors.Comparison$Speed.Err.int == Errors.Comparison$Speed.Err.ext
  Errors.Comparison$compMath <- Errors.Comparison$Math.Err.int == Errors.Comparison$Math.Err.ext
  Errors.Comparison$compMathSets <- Errors.Comparison$Math.Err.int == (75 - sum(Ospan_sets[[x]]$Correct.Calc))
  
  return(Errors.Comparison)
}

t(sapply(1:length(ids), Errors.Comparison))

# they are similiar.



# Computation: All-or-Nothing Load Scoring (ANL) --------------------------
comp_ANL <- function (x)
{
  # Stop Funtion if x is not part of data (e.g. excluded subjects)
  if(!length(Ospan_sets[[as.character(x)]]) > 0) stop("Error: requested ID is not part of the dataset")
  
  # Compare the given to the stated sequence. If stated correctly, the full value (e.g. 5 points for 5 correctly recalled letters) of the sequence is given. If there is one ore more mistake, 0 points are given. 
  
  # ANL Score: Summed up trial-based score
  ANL_ov <- sum(mapply(
    function(y,z) {
      # cut off longer sequences
      z = substr(z, start = 1, stop = nchar(y))
      # Score, if correct
      ifelse(y == z, nchar(y), as.numeric(0))
    },
    y = Ospan_sets[[as.character(x)]]$Seq.given,
    z = Seq_temp <- Ospan_sets[[as.character(x)]]$Seq.stated))
  
  
  #return values
  temp <- data.frame(
    ID = x,
    ANL_ov = ANL_ov,
    Overall.Calc.Error = tail(Ospan_sets[[as.character(x)]]$Correct.Calc.intern, n = 1))
  
  return(temp)
}



# Computation: Partial-Credit Load Scoring (PCL) --------------------------

# Partial Credit Load Scoring gives one point for each stated letter that is equal to the given letter in the same position.
# Conditions: 
# a) Longer given letter sequences are shortened to the length of the given sequence.
# b) Shorter given letter sequences are filled with wrong letters; the first given letter is interpreted as to reflect the first letter in the given sequence.  
comp_PCL <- function (x)
{
  # Stop funtion if x is not part of data (e.g. excluded subjects)
  if(!length(Ospan_sets[[as.character(x)]]) > 0) stop("Error: requested ID is not part of the dataset")
  
  # PCL Score: Summed up trial-based score
  PCL_ov <- sum(mapply(
    function(y,z) {
      # Split up the Sequences
      Seq_giv_temp <- unlist(strsplit(y,""))
      Seq_sta_temp <- unlist(strsplit(z,""))
      
      # Cut the overlength and fill the blanks 
      Seq_sta_temp <- Seq_sta_temp[1:length(Seq_giv_temp)]
      Seq_sta_temp[which(is.na(Seq_sta_temp))] <-"A"
      
      # Compare the letters and sum up the results
      sum(Seq_giv_temp == Seq_sta_temp)
      
    },
    y = Ospan_sets[[as.character(x)]]$Seq.given,
    z = Ospan_sets[[as.character(x)]]$Seq.stated
  ))
  
  #return values
  temp <- data.frame(
    ID = x,
    PCL_ov = PCL_ov,
    Overall.Calc.Error = tail(Ospan_sets[[as.character(x)]]$Correct.Calc.intern, n = 1)
  )
  return(temp)
}

# Computation: Partial-Credit Unit Scoring (PCU) --------------------------
comp_PCU <- function (x)
{
  # Stop funtion if x is not part of data (e.g. excluded subjects)
  if(!length(Ospan_sets[[as.character(x)]]) > 0) stop("Error: requested ID is not part of the dataset")
  
  # PCU Score: Summed up trial-based score
  PCU_ov <- sum(mapply(
    function(y,z) {
      # Split up the Sequences
      Seq_giv_temp <- unlist(strsplit(y,""))
      Seq_sta_temp <- unlist(strsplit(z,""))
      
      # Cut the overlength and fill the blanks 
      Seq_sta_temp <- Seq_sta_temp[1:length(Seq_giv_temp)]
      Seq_sta_temp[which(is.na(Seq_sta_temp))] <-"A"
      
      # Compare the letters and sum up the results, then divide it by the sequence's length 
      sum(Seq_giv_temp == Seq_sta_temp) / length(Seq_giv_temp)
      
    },
    y = Ospan_sets[[as.character(x)]]$Seq.given,
    z = Ospan_sets[[as.character(x)]]$Seq.stated
  ))
  
  #return values
  temp <- data.frame(
    ID = x,
    PCU_ov = PCU_ov,
    Overall.Calc.Error = tail(Ospan_sets[[as.character(x)]]$Correct.Calc.intern, n = 1)
  )
  return(temp)
}


# Computation: Script-Consistency Test: ANL / PCL ----------------------------

# Testing consistency of Ospan-Score (All-or-Nothing-Load-Scoring) as calculated by the original script (scores taking from logfile) and by my script (trial-based-calculation)

# dataframe containing both ways of calculation of All-or-Nothing Scores.
bothscore <- data.frame(
   Ospan_scores[,c("ID","Ospan.Score", "Ospan.Total")],
  "Ospan.sets.ANL" = do.call("rbind.data.frame", lapply(ids, comp_ANL))$ANL_ov,
  "Ospan.sets.PCL" = do.call("rbind.data.frame", lapply(ids, comp_PCL))$PCL_ov)
bothscore["comparison.ANL"] <- bothscore[,"Ospan.Score"] == bothscore[,"Ospan.sets.ANL"]
bothscore["comparison.PCl"] <- bothscore[,"Ospan.Total"] == bothscore[,"Ospan.sets.PCL"]

# The script seems to work smoothly! 
# Only: Engle seemed to have given points to correctly stated sequences, even if additional letters appeared at the end of the stated sequence.
# We go with Engle's way, by cutting of longer sequences at the stated length, before comparing sequences.

# Graphical Demonstration
ggplot(data = bothscore,
       aes(y = Ospan.Score,
           x = Ospan.sets.ANL)) + 
  geom_point()



# Computation: MATH-ITEM-SEQUENCE (MIS) score - a new approach --------------------------
#
## This method gives the same credit to the ratio of correctly recalled calculation and the ratio of correctly stated Sequences (Items), weighing higher those, that are in the correct order
## the formula would be:
## Calc(cor-1st) / calc(tot-1st) * ( Items(cor) / Items(tot) + Order(cor) / Order(tot) ) / 2

comp_MIS <- function (x, df = FALSE)
{
  # Stop funtion if x is not part of data (e.g. excluded subjects)
  if(!length(Ospan_sets[[as.character(x)]]) > 0) stop("Error: requested ID is not part of the dataset")

  MIS_ov <- data.frame(Ospan_sets[[as.character(x)]], t(mapply(
    function(CalcCor,CalcTot, Seq.given, Seq.stated) {
      
      # Ratio of correct over total results (the first calculation is ignored, as it does not influence the Working Memory)
      RatioCalc <- CalcCor / CalcTot
      RatioCalcSqu <- RatioCalc^2
      
      # Ratio of correct over total letters (every letter from the given sequence is checked within the stated sequence)
      RatioItem <- sum(unlist(strsplit(Seq.given,"")) %in% unlist(strsplit(Seq.stated,""))) / nchar(Seq.given)
      
      # Ratio of the (sum of the) longest stated sequences in the right order over the longest possible sequence.
      # Only count sequences of at least 2 letters
      
      # a) first define start and end points for splitting the given sequence in all possible and needed combinations: 
      # e.g. for a 4-letter-sequence this: 1:2, 1:3, 1:4, 2:3; 3:4
      txx <- expand.grid(Start = 1:nchar(Seq.given), Stop = 1:nchar(Seq.given))
      txx <- txx[which(txx$Start < txx$Stop),]
      txx <- txx[with(txx, order(txx$Start)), ]
      
      # b) creating a list with all possible sequences
      MIS_seq <- data.frame(AllSubstr = mapply(function(SubstrStart, SubstrStop) {
        substr(Seq.given, start = SubstrStart, stop = SubstrStop)
      },
      SubstrStart = txx$Start, 
      SubstrStop = txx$Stop), 
      stringsAsFactors = FALSE)
      
      # c) looking for existance of each possible sequence within the stated Sequence
      MIS_seq$exist <- !sapply(sapply(MIS_seq$AllSubstr, grep, x = Seq.stated), identical, y = integer(0) )
      MIS_seq$length <- nchar(MIS_seq$AllSubstr)
      
      # d) delete all non matched sequences
      MIS_seq <- subset(MIS_seq, exist == TRUE)
      
      # e) Finding the longest sequence, than delete all sequences from the list, that contain letters from this sequence
      MIS_seq_res <- ifelse(nrow(MIS_seq) > 0,max(MIS_seq$length),0)  
      
      # f) calculation the Order ratio:
      RatioOrder <- MIS_seq_res / nchar(Seq.given)
      
      # MIS Formula, as described above
      res_MIS <- RatioCalc * ( RatioItem + RatioOrder ) / 2
      
      # MIS Load 
      MIS_load <- res_MIS * nchar(Seq.given)
      
      # The combination of ITEM and ORDER Ratio without the CALC Ratio
      res_MIS_noCalc <- ( RatioItem + RatioOrder ) / 2
      
      
      return(data.frame(RatioCalc, RatioCalcSqu, RatioItem, RatioOrder, res_MIS, MIS_load, res_MIS_noCalc))
      
      
    },
    CalcCor <- Ospan_sets[[as.character(x)]]$Correct.Calc.MinusOne,
    CalcTot <- Ospan_sets[[as.character(x)]]$Setgroesse - 1,
    Seq.given <- Ospan_sets[[as.character(x)]]$Seq.given,
    Seq.stated <- Ospan_sets[[as.character(x)]]$Seq.stated
  )))
  
  
  #return values
  temp <- data.frame(
    ID = x,
    MIS_ov = sum(unlist(MIS_ov["res_MIS"])),
    MIS_load = sum(unlist(MIS_ov["MIS_load"])),
    MIS_ov_noCalc = sum(unlist(MIS_ov["res_MIS_noCalc"])),
    MIS_math = sum(unlist(MIS_ov["RatioCalc"])),
    MIS_squmath = sum(unlist(MIS_ov["RatioCalcSqu"])),
    MIS_item = sum(unlist(MIS_ov["RatioItem"])),
    MIS_sequ = sum(unlist(MIS_ov["RatioOrder"])),
    Overall.Calc.Error = tail(Ospan_sets[[as.character(x)]]$Correct.Calc.intern, n = 1)
  )
  
  ifelse(df == FALSE, return(temp), return(MIS_ov)) 
}


# Validity Check: MIS bs MIS_noX --------------------------------------------

# Comparison between MIS calculated with or withon the X.
# (works only if, within the MIS_comp function, the Seq.stated is prolonged with "_noX". Then, both score need to be computed and compared.)
# Ospan_AllScores_noX <- Ospan_AllScores
# 
# data.frame(MIS.orig = Ospan_AllScores$MIS_ov,
#           MIS.noX = Ospan_AllScores_noX$MIS_ov,
#           comp = Ospan_AllScores$MIS_ov == Ospan_AllScores_noX$MIS_ov )


# Computation: Merging all methods ----------------------------------------
Ospan_AllScores <- cbind.data.frame(
  do.call("rbind.data.frame", lapply(ids, comp_ANL))[1:2],
  do.call("rbind.data.frame", lapply(ids, comp_PCL))[2],
  do.call("rbind.data.frame", lapply(ids, comp_PCU))[2],
  do.call("rbind.data.frame", lapply(ids, comp_MIS))[2:9])

# define PCU_85
Ospan_AllScores$PCU_85 <- ifelse(Ospan_AllScores$Overall.Calc.Error < 85, NA, Ospan_AllScores$PCU_ov)
# Testing assumptions -----------------------------------------------------

# ANL
ggplot(Ospan_AllScores,
       aes(ANL_ov)) +
  geom_histogram(aes(y = ..density..),
                 colour = "black",
                 fill = "white") + 
  stat_function(fun = dnorm,
                args = list(mean = mean(Ospan_AllScores$ANL_ov, na.rm = TRUE),
                            sd = sd(Ospan_AllScores$ANL_ov, na.rm = TRUE)),
                colour = "black",
                size = 1)


# PCL
ggplot(Ospan_AllScores,
       aes(PCL_ov)) +
  geom_histogram(aes(y = ..density..),
                 colour = "black",
                 fill = "white") + 
  stat_function(fun = dnorm,
                args = list(mean = mean(Ospan_AllScores$PCL_ov, na.rm = TRUE),
                            sd = sd(Ospan_AllScores$PCL_ov, na.rm = TRUE)),
                colour = "black",
                size = 1)

stat.desc(Ospan_AllScores$PCL_ov, basic = FALSE, norm = TRUE) %>% round( digits = 5)


# PCU
ggplot(Ospan_AllScores,
       aes(PCU_ov)) +
  geom_histogram(aes(y = ..density..),
                 colour = "black",
                 fill = "white") + 
  stat_function(fun = dnorm,
                args = list(mean = mean(Ospan_AllScores$PCU_ov, na.rm = TRUE),
                            sd = sd(Ospan_AllScores$PCU_ov, na.rm = TRUE)),
                colour = "black",
                size = 1)

stat.desc(Ospan_AllScores$PCU_ov, basic = FALSE, norm = TRUE) %>% round( digits = 5)


# MIS 
ggplot(Ospan_AllScores,
       aes(MIS_ov)) +
  geom_histogram(aes(y = ..density..),
                 colour = "black",
                 fill = "white") + 
  stat_function(fun = dnorm,
                args = list(mean = mean(Ospan_AllScores$MIS_ov, na.rm = TRUE),
                            sd = sd(Ospan_AllScores$MIS_ov, na.rm = TRUE)),
                colour = "black",
                size = 1)

stat.desc(Ospan_AllScores$MIS_ov, basic = FALSE, norm = TRUE) %>% round( digits = 5)


# MIS_noCalc
ggplot(Ospan_AllScores,
       aes(MIS_ov_noCalc)) +
  geom_histogram(aes(y = ..density..),
                 colour = "black",
                 fill = "white") + 
  stat_function(fun = dnorm,
                args = list(mean = mean(Ospan_AllScores$MIS_ov_noCalc, na.rm = TRUE),
                            sd = sd(Ospan_AllScores$MIS_ov_noCalc, na.rm = TRUE)),
                colour = "black",
                size = 1)

stat.desc(Ospan_AllScores$MIS_ov_noCalc, basic = FALSE, norm = TRUE) %>% round( digits = 5)


# MATH 
ggplot(Ospan_AllScores,
       aes(MIS_math)) +
  geom_histogram(aes(y = ..density..),
                 colour = "black",
                 fill = "white") + 

   stat_function(fun = dnorm,
                args = list(mean = mean(Ospan_AllScores$MIS_math, na.rm = TRUE),
                            sd = sd(Ospan_AllScores$MIS_math, na.rm = TRUE)),
                colour = "black",
                size = 1)

stat.desc(Ospan_AllScores$MIS_math, basic = FALSE, norm = TRUE) %>% round( digits = 5)


# ITEM 
ggplot(Ospan_AllScores,
       aes(MIS_item)) +
  geom_histogram(aes(y = ..density..),
                 colour = "black",
                 fill = "white") + 
  
  stat_function(fun = dnorm,
                args = list(mean = mean(Ospan_AllScores$MIS_item, na.rm = TRUE),
                            sd = sd(Ospan_AllScores$MIS_item, na.rm = TRUE)),
                colour = "black",
                size = 1)

stat.desc(Ospan_AllScores$MIS_item, basic = FALSE, norm = TRUE) %>% round( digits = 5)

# SEQUENCE 
ggplot(Ospan_AllScores,
       aes(MIS_sequ)) +
  geom_histogram(aes(y = ..density..),
                 colour = "black",
                 fill = "white") + 
  
  stat_function(fun = dnorm,
                args = list(mean = mean(Ospan_AllScores$MIS_sequ, na.rm = TRUE),
                            sd = sd(Ospan_AllScores$MIS_sequ, na.rm = TRUE)),
                colour = "black",
                size = 1)

stat.desc(Ospan_AllScores$MIS_sequ, basic = FALSE, norm = TRUE) %>% round( digits = 5)


# normality for all scores
stat.desc(Ospan_AllScores[, c("ANL_ov", "PCL_ov", "PCU_ov", "MIS_ov", "MIS_ov_noCalc", "MIS_math", "MIS_item", "MIS_sequ")], basic = TRUE, norm = TRUE) %>% round( digits = 2) %>% .[ c("median", "min", "max"), c("MIS_ov", "MIS_item", "MIS_sequ", "MIS_math", "PCU_ov")] #%>% write.table("./Ospan MIS/temp.csv")

# Exclusion criterion: Statisticly possible? ------------------------------
# 
# Possible Criterion for excluding bad math performers on a statistical level

# Number of excluded subjects with a criterion of: MATH-ratio < mean - 2.5 SD of MATH-ratio
unlist(subset(Ospan_AllScores, MIS_math < (mean(MIS_math) - 2.5 * sd(MIS_math)), c(ID, Overall.Calc.Error, MIS_math)))  %>% length()

# Number of excluded subjects with a traditional 85% Score
Ospan_AllScores[which(Ospan_AllScores$Overall.Calc.Error < 85),] %>% nrow()


# 
# # Outlier exclusion per score, based on a 2.2 IQR 
# for(x in c("ANL_ov", "PCL_ov", "PCU_ov", "MIS_ov", "MIS_ov_noCalc", "MIS_math", "MIS_item", "MIS_sequ")) {
#   Ospan_AllScores[!outliers(Ospan_AllScores[,x], int = TRUE), x] <- NA}


# Correlation of the Main Scores: MIS, PCL, PCU, ANL -------------------------------------------------------------

# PCL over ANL
ggplot(data = Ospan_AllScores,
       aes(x = ANL_ov,
           y = PCL_ov)) + 
  geom_point() +
  geom_smooth(method="lm")

# PCL over PCU
ggplot(data = Ospan_AllScores,
       aes(x = PCU_ov,
           y = PCL_ov)) + 
  geom_point() +
  geom_smooth(method="lm")



# Scatterplot: PCU over MIS, with line of best fit and 95% CI. Subjects with less than 85% correct calculation marked red. 
ggplot(data = Ospan_AllScores,
       aes(x = MIS_ov,
           y = PCU_ov)) + 
  geom_point() +   # aes(color = Overall.Calc.Error)
  geom_point(data = Ospan_AllScores[which(Ospan_AllScores$Overall.Calc.Error < 85),], size = 1, colour = "red") + 
  # scale_fill_gradient(low = "#132B43", high = "#56B1F7", space = "Lab", guide = "colourbar") + 
  geom_smooth(method="lm") 

# Correlation analysis: Pearson's correlation of PCL and MIS
cor.test(Ospan_AllScores$PCL_ov, Ospan_AllScores$MIS_ov, method = "pearson", conf.level = 0.95)


# Scatterplot: PCU over MIS, with line of best fit and 95% CI. Subjects with less than 85% correct calculation marked red. 
ggplot(data = Ospan_AllScores,
       aes(x = MIS_ov,
           y = PCU_ov)) + 
  geom_point() +   # aes(color = Overall.Calc.Error)
  geom_point(data = Ospan_AllScores[which(Ospan_AllScores$Overall.Calc.Error < 85),], size = 1, colour = "red") + 
  # scale_fill_gradient(low = "#132B43", high = "#56B1F7", space = "Lab", guide = "colourbar") + 
  geom_smooth(method="lm") 

# Correlation analysis: Pearson's correlation of PCL and MIS
cor.test(Ospan_AllScores$PCU_ov, Ospan_AllScores$MIS_ov, method = "pearson", conf.level = 0.95)
cor.test(Ospan_AllScores$PCU_ov, Ospan_AllScores$MIS_ov, method = "spearman", conf.level = 0.95)




# Scatterplot: ANL over MIS, with line of best fit and 95% CI. Subjects with less than 85% correct calculation marked red. 
# windows()
ggplot(data = Ospan_AllScores,
       aes(x = MIS_ov,
           y = ANL_ov)) + 
  geom_point() +   # aes(color = Overall.Calc.Error)
  geom_point(data = Ospan_AllScores[which(Ospan_AllScores$Overall.Calc.Error < 85),], size = 1, colour = "red") + 
  # scale_fill_gradient(low = "#132B43", high = "#56B1F7", space = "Lab", guide = "colourbar") + 
  geom_smooth(method="lm") 

# Correlation analysis: Pearson's correlation of PCL and MIS
cor.test(Ospan_AllScores$ANL_ov, Ospan_AllScores$MIS_ov, method = "pearson", conf.level = 0.95)



# Scatterplot: Combination of all 4 above
pick <- function(condition){
  function(d) d %>% filter_(condition)
}

library(ggpubr)
windows()
Ospan_AllScores %>%
  dplyr::select(ID, PCU = PCU_ov, "a) MIS" = MIS_ov, "b) MIS ITEM" = MIS_item, "c) MIS SEQUENCE" = MIS_sequ, "d) MIS MATH" = MIS_math, Overall.Calc.Error) %>%
  gather(Score, value, -ID, -PCU, -Overall.Calc.Error) %>%
ggplot(aes(x = value,
           y = PCU)) + 
  geom_point(alpha = 0.5) +   
  geom_point(data = pick(~Overall.Calc.Error < 85), size = 1, colour = "red", alpha = .5) + 
  geom_line(stat="smooth", method="lm", color = "blue", alpha = 0.7, size = 1) +
  geom_smooth(method="lm", alpha = 0.4, size = 0) +
  facet_wrap(~Score, scales = "free_x") +
  xlab("Score") + 
  ylab("PCUall") + 
  theme(strip.text = element_text(hjust = -0.00),
        strip.background = element_blank()) +
  stat_cor(aes(label = paste(..r.label.., gsub("p=", "p = ", cut(..p.., 
                                                                 breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                                                                 labels = c("<0.001", "<0.1", "<0.5", ">0.5"))), sep = ", ")),
           method = "spearman", label.sep = ", ", output.type = "text")

ggsave(filename=paste("./Ospan Engle/MISall_PCU_",Sys.Date(),".png", sep=""), width = 7, height = 5.5)
  



# Correlation: Each Ratio to PCL, PCU and MIS ------------------------------------------------

# MATH over PCL
ggplot(data = Ospan_AllScores,
       aes(x = MIS_math,
           y = PCL_ov)) + 
  geom_point() +   # aes(color = Overall.Calc.Error)
  geom_point(data = Ospan_AllScores[which(Ospan_AllScores$Overall.Calc.Error < 85),], size = 1, colour = "red") + 
  # scale_fill_gradient(low = "#132B43", high = "#56B1F7", space = "Lab", guide = "colourbar") + 
  geom_smooth(method="lm") 

# Correlation analysis: Pearson's correlation of PCL and MATH
cor.test(Ospan_AllScores$PCL_ov, Ospan_AllScores$MIS_math, method = "pearson", conf.level = 0.95) 

# MATH over PCU
ggplot(data = Ospan_AllScores,
       aes(x = MIS_math,
           y = PCU_ov)) + 
  geom_point() +   # aes(color = Overall.Calc.Error)
  geom_point(data = Ospan_AllScores[which(Ospan_AllScores$Overall.Calc.Error < 85),], size = 1, colour = "red") + 
  # scale_fill_gradient(low = "#132B43", high = "#56B1F7", space = "Lab", guide = "colourbar") + 
  geom_smooth(method="lm") 

# Correlation analysis: Pearson's correlation of PCL and MATH
cor.test(Ospan_AllScores$PCU_ov, Ospan_AllScores$MIS_math, method = "pearson", conf.level = 0.95) 

# MATH over MIS
ggplot(data = Ospan_AllScores,
       aes(x = MIS_math,
           y = MIS_ov)) + 
  geom_point() +   # aes(color = Overall.Calc.Error)
  geom_point(data = Ospan_AllScores[which(Ospan_AllScores$Overall.Calc.Error < 85),], size = 1, colour = "red") + 
  # scale_fill_gradient(low = "#132B43", high = "#56B1F7", space = "Lab", guide = "colourbar") + 
  geom_smooth(method="lm") 

# Correlation analysis: Pearson's correlation of MATH and MIS
cor.test(Ospan_AllScores$MIS_ov, Ospan_AllScores$MIS_math, method = "pearson", conf.level = 0.95)




# ITEM over PCL
ggplot(data = Ospan_AllScores,
       aes(x = MIS_item,
           y = PCL_ov)) + 
  geom_point() +   # aes(color = Overall.Calc.Error)
  geom_point(data = Ospan_AllScores[which(Ospan_AllScores$Overall.Calc.Error < 85),], size = 1, colour = "red") + 
  # scale_fill_gradient(low = "#132B43", high = "#56B1F7", space = "Lab", guide = "colourbar") + 
  geom_smooth(method="lm") 

# Correlation analysis: Pearson's correlation of PCL and ITEM
cor.test(Ospan_AllScores$PCL_ov, Ospan_AllScores$MIS_item, method = "pearson", conf.level = 0.95) 


# ITEM over PCU
ggplot(data = Ospan_AllScores,
       aes(x = MIS_item,
           y = PCU_ov)) + 
  geom_point() +   # aes(color = Overall.Calc.Error)
  geom_point(data = Ospan_AllScores[which(Ospan_AllScores$Overall.Calc.Error < 85),], size = 1, colour = "red") + 
  # scale_fill_gradient(low = "#132B43", high = "#56B1F7", space = "Lab", guide = "colourbar") + 
  geom_smooth(method="lm") 

# Correlation analysis: Pearson's correlation of PCL and ITEM
cor.test(Ospan_AllScores$PCU_ov, Ospan_AllScores$MIS_item, method = "pearson", conf.level = 0.95) 

# ITEM over MIS
ggplot(data = Ospan_AllScores,
       aes(x = MIS_item,
           y = MIS_ov)) + 
  geom_point() +   # aes(color = Overall.Calc.Error)
  geom_point(data = Ospan_AllScores[which(Ospan_AllScores$Overall.Calc.Error < 85),], size = 1, colour = "red") + 
  # scale_fill_gradient(low = "#132B43", high = "#56B1F7", space = "Lab", guide = "colourbar") + 
  geom_smooth(method="lm") 

# Correlation analysis: Pearson's correlation of MIS and ITEM
cor.test(Ospan_AllScores$MIS_ov, Ospan_AllScores$MIS_item, method = "pearson", conf.level = 0.95)




# SEQUENCE over PCL
ggplot(data = Ospan_AllScores,
       aes(x = MIS_sequ,
           y = PCL_ov)) + 
  geom_point() +   # aes(color = Overall.Calc.Error)
  geom_point(data = Ospan_AllScores[which(Ospan_AllScores$Overall.Calc.Error < 85),], size = 1, colour = "red") + 
  # scale_fill_gradient(low = "#132B43", high = "#56B1F7", space = "Lab", guide = "colourbar") + 
  geom_smooth(method="lm") 

# Correlation analysis: Pearson's correlation of PCL and SEQUENCE
cor.test(Ospan_AllScores$PCL_ov, Ospan_AllScores$MIS_sequ, method = "pearson", conf.level = 0.95) 


# SEQUENCE over PU
ggplot(data = Ospan_AllScores,
       aes(x = MIS_sequ,
           y = PCU_ov)) + 
  geom_point() +   # aes(color = Overall.Calc.Error)
  geom_point(data = Ospan_AllScores[which(Ospan_AllScores$Overall.Calc.Error < 85),], size = 1, colour = "red") + 
  # scale_fill_gradient(low = "#132B43", high = "#56B1F7", space = "Lab", guide = "colourbar") + 
  geom_smooth(method="lm") 

# Correlation analysis: Pearson's correlation of PCL and SEQUENCE
cor.test(Ospan_AllScores$PCU_ov, Ospan_AllScores$MIS_sequ, method = "pearson", conf.level = 0.95) 


# SEQUENCE over MIS
ggplot(data = Ospan_AllScores,
       aes(x = MIS_sequ,
           y = MIS_ov)) + 
  geom_point() +   # aes(color = Overall.Calc.Error)
  geom_point(data = Ospan_AllScores[which(Ospan_AllScores$Overall.Calc.Error < 85),], size = 1, colour = "red") + 
  # scale_fill_gradient(low = "#132B43", high = "#56B1F7", space = "Lab", guide = "colourbar") + 
  geom_smooth(method="lm") 

# Correlation analysis: Pearson's correlation of MIS and SEQUENCE
cor.test(Ospan_AllScores$MIS_ov, Ospan_AllScores$MIS_sequ, method = "pearson", conf.level = 0.95)
cor.test(Ospan_AllScores$MIS_ov, Ospan_AllScores$MIS_sequ, method = "spearman", conf.level = 0.95)





# PCL over MIS_noCalc (= MIS, but no Calculation Ratio in the formula), with Line of best fit and 95% CI. Subjects with less than 85% correct calculation marked red. 
ggplot(data = Ospan_AllScores,
       aes(x = MIS_ov_noCalc,
           y = PCL_ov)) + 
  geom_point() +
  geom_point(data = Ospan_AllScores[which(Ospan_AllScores$Overall.Calc.Error < 85),], size = 1, colour = "red") +
  geom_smooth(method="lm")

# Correlation Analysis: Pearson's correlation of PCL and MIS_noCalc
cor.test(Ospan_AllScores$PCL_ov, Ospan_AllScores$MIS_ov_noCalc, method = "pearson", conf.level = 0.95)


# PCU over MIS_noCalc
ggplot(data = Ospan_AllScores,
       aes(x = MIS_ov_noCalc,
           y = PCU_ov)) + 
  geom_point() +
  geom_point(data = Ospan_AllScores[which(Ospan_AllScores$Overall.Calc.Error < 85),], size = 1, colour = "red") + 
  geom_smooth(method="lm")

# Correlation Analysis: Pearson's correlation of PCL and MIS_noCalc
cor.test(Ospan_AllScores$PCU_ov, Ospan_AllScores$MIS_ov_noCalc, method = "pearson", conf.level = 0.95)

# MIS over MIS_noCalc, with line of best fit and 95% CI. Subjects with less than 85% correct calculation marked red. 
ggplot(data = Ospan_AllScores,
       aes(x = MIS_ov_noCalc,
           y = MIS_ov)) + 
  geom_point() +
  geom_point(data = Ospan_AllScores[which(Ospan_AllScores$Overall.Calc.Error < 85),], size = 1, colour = "red") + 
  geom_smooth(method="lm")

# Correlation Analysis: Pearson's correlation of MIS and MIS_noCalc
cor.test(Ospan_AllScores$MIS_ov, Ospan_AllScores$MIS_ov_noCalc, method = "pearson", conf.level = 0.95)




# MATH over MIS_noCalc, with line of best fit and 95% CI. Subjects with less than 85% correct calculation marked red. 
ggplot(data = Ospan_AllScores,
       aes(x = MIS_ov_noCalc,
           y = MIS_math)) + 
  geom_point() +
  # scale_fill_gradient(low = "#132B43", high = "#56B1F7", space = "Lab", guide = "colourbar") + 
  geom_point(data = Ospan_AllScores[which(Ospan_AllScores$Overall.Calc.Error < 85),], size = 1, colour = "red") + 
  geom_smooth(method="lm")

# Correlation analysis: Pearson's correlation of MATH and MIS_noCalc
cor.test(Ospan_AllScores$MIS_math, Ospan_AllScores$MIS_ov_noCalc, method = "pearson", conf.level = 0.95)




# correlation matrix ------------------------------------------------------

rel.scores <- as.matrix(Ospan_AllScores[c("PCU_ov", "MIS_ov", "MIS_item", "MIS_sequ", "MIS_math")])

  rcorr(x = rel.scores, type = "spearman")
  stats::cor(x = rel.scores, method = "spearman")
  
  corr.test(x = as.matrix(rel.scores), method = "spearman", use = "pairwise", adjust = "none", minlength = 7)
  

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
 
cor.own(rel.scores, method = "spearman")
cor.own(Ospan_AllScores[,c("PCU_85", "MIS_ov")], method = "spearman")
cor.test(Ospan_AllScores$PCU_85, Ospan_AllScores$MIS_ov, method = "spearman")
cor.test(Ospan_AllScores$PCU_ov, Ospan_AllScores$MIS_ov, method = "spearman")


 # Internal consistency ----------------------------------------------------

# create a list with 
Ospan_sets_scores <- lapply(ids, comp_MIS, df = TRUE)
names(Ospan_sets_scores) = ids 

ordered.score <- function(x, score = "no") {
  # x must be the subject ID
  if(score == "no") stop("score must be written in parentheses and take one of the following values: res_MIS, RatioCalc, RatioItem, RatioOrder, MIS_load, res_MIS_noCalc")
  Ospan_sets_scores[[as.character(x)]] %>%
    arrange(Setgroesse) %>% 
    dplyr::select(one_of(score)) %>% t() %>% unlist()
}

# define value of interest
score_cron_all <-c("MIS_ov" = "res_MIS",
               "MIS_item" = "RatioItem",
               "MIS_sequ" = "RatioOrder",
               "MIS_math" = "RatioCalc")
score_cron <- score_cron_all[4]

# compute item-based dataframe
Ospan_AllScores_ordered <- do.call("rbind.data.frame", lapply(ids, ordered.score, score = score_cron)) 
names(Ospan_AllScores_ordered) <- paste0("Item_", c(1:15)) 
rownames(Ospan_AllScores_ordered) <- ids
Ospan_AllScores_ordered$ID <- ids

# exclude outliers if need be
Ospan_AllScores_ordered <- subset(Ospan_AllScores_ordered, ID %in% Ospan_AllScores[!is.na(Ospan_AllScores[, names(score_cron)]), "ID"] )

# compute Cronbach's alpha
alpha(Ospan_AllScores_ordered[,-16])
