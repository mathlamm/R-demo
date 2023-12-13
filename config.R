# 
# 
# ++ Configuration file for the whole Bariresp Dataset ++
# 
# 
# Script: Mathis Lammert (lammert@cbs.mpg.de)
# 


# Setting the basic working directory, depending on the use of the work station (Home or Work)
#if(length(grep("home/raid2/lammert", getwd())) > 0) setwd("/home/raid2/lammert/myOwncloud/Medizin/Promotion/Auswertung")

#if(length(grep("D:/nextcloud", getwd())) > 0) setwd("D:/Nextcloud/Medizin/Promotion/Auswertung")

#if(length(grep("C:/Users/nix-n/Documents", getwd())) > 0) setwd("D:/Nextcloud/Medizin/Promotion/Auswertung")

setwd("D:/Nextcloud/Coding/R-demo")
getwd()
