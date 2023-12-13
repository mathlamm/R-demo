###  +++ Relate cognitive and hormonal data +++
###
###

###
###  Script by Mathis Lammert (lammert@cbs.mpg.de)
###
###  last edit: 2023-27-06
###


# Preperation -------------------------------------------------------------

# Clean Environment & load standard functions & packages
rm(list = ls())

# Load config, additional functions and set WD
source("config.R")
if(!exists("foo", mode="function")) source("Funktionen1.R")


# load all data file
source("./allData_load.R")

# Loading libraries
library(tidyverse)
library(Hmisc)
library(ggpubr)
library(psych)
library(car)
library(pastecs)
library(effects)
# library(ez)
library(multcomp)
library(nlme)
library(WRS2)
library(emmeans)
# library(lsmeans)
library(broom)
library(ggfortify)
library(mediation)
library(MASS)
library(caret)
library(lmtest)

# Correlation: AUC VAS and AUC & Hormones  ------------------------------------
corr.AUCs <- allData[,c("AUC_H","AUC_S", "AUC_GLP1","AUC_PYY", "AUC_Ghrelin", "AUC_Insulin", "AUC_Leptin")]
corr.iAUCs <- allData[,c("iAUC_H","iAUC_S", "iAUC_GLP1","iAUC_PYY", "iAUC_Ghrelin", "iAUC_Insulin", "iAUC_Leptin")]

# AUC
corr.test(x = as.matrix(corr.AUCs[1:2]), y = as.matrix(corr.AUCs[3:7]), method = "spearman", use = "pairwise", adjust = "none")
cor.own(corr.AUCs, method = "spearman")
corr.test(x = as.matrix(corr.AUCs[1:2]), y = as.matrix(corr.AUCs[3:7]), method = "spearman", use = "pairwise", adjust = "holm")  %>% .$p %>% round(digits = 3)


# iAUC
corr.test(x = as.matrix(corr.iAUCs[1:2]), y = as.matrix(corr.iAUCs[3:7]), method = "spearman", use = "pairwise", adjust = "none")
cor.own(corr.iAUCs, method = "spearman")
corr.test(x = as.matrix(corr.iAUCs[1:2]), y = as.matrix(corr.iAUCs[3:7]), method = "spearman", use = "pairwise", adjust = "holm")  %>% .$p %>% round(digits = 3)



# Correlation: Cognition only  ------------------------------------
corr.cogn <- allData[,c("MIS_ov", "MIS_item", "MIS_sequ", "MIS_math", "DS_forw1", "DS_back1", "EBR90")]

corr.test(as.matrix(corr.cogn), method = "spearman", use = "pairwise", adjust = "none")
cor.own(corr.cogn, method = "spearman")


# Correlation: Cognition and AUC Hormones  ------------------------------------

# Extract relevant variables
corr.AUC <- allData[,c("MIS_ov", "MIS_item", "MIS_sequ", "MIS_math", "AUC_GLP1", "AUC_PYY", "AUC_Ghrelin", "AUC_Insulin", "AUC_Leptin")]
corr.iAUC <- allData[,c("MIS_ov", "MIS_item", "MIS_sequ", "MIS_math", "iAUC_GLP1", "iAUC_PYY", "iAUC_Ghrelin", "iAUC_Insulin", "iAUC_Leptin")]

# Outlier exclusion  <- not necessary with spearman ;) 
# corr.AUC <- mutate_all(corr.AUC, funs(ifelse(outliers(., int = TRUE) == TRUE, ., NA )))
# corr.iAUC <- mutate_all(corr.iAUC, funs(ifelse(outliers(., int = TRUE) == TRUE, ., NA )))

# show correlation matric with plots
pairs.panels(x = corr.iAUC, method="spearman", smooth = FALSE, lm = TRUE, ci = TRUE, stars = FALSE)
pairs.panels(x = corr.AUC, method="spearman", smooth = FALSE, lm = TRUE, ci = TRUE, stars = FALSE)

# correlation matrix, using corr.test from psych-package because of easy usage of p-value-adjustment
# (a) no adjustment
corr.test(x = as.matrix(corr.iAUC[1:4]), y = as.matrix(corr.iAUC[5:9]), method = "spearman", use = "pairwise", adjust = "none")
cor.own(corr.iAUC, method = "spearman")

corr.test(x = as.matrix(corr.AUC[1:4]), y = as.matrix(corr.AUC[5:9]), method = "spearman", use = "pairwise", adjust = "none")
cor.own(corr.AUC, method = "spearman")

# (b) adjustment for relevant values only
corr.test(x = as.matrix(corr.iAUC[1:3]), y = as.matrix(corr.iAUC[5:9]), method = "spearman", use = "pairwise", adjust = "holm") %>% .$p %>% round(digits = 3)
corr.test(x = as.matrix(corr.AUC[1:3]), y = as.matrix(corr.AUC[5:9]), method = "spearman", use = "pairwise", adjust = "holm") %>% .$p %>% round(digits = 3)


# Cognition and IR  ------------------------------------

# correlation analyses: IR and WM
corr.IR <- allData[,c("HOMA.IR_akt", "HOMA.IR_pre", "MIS_ov", "MIS_ov_noCalc")]

corr.test(as.matrix(corr.IR), method = "spearman", use = "pairwise", adjust = "none")
cor.own(corr.IR, method = "spearman")

# Graph: Correlation of IR and WM, groups colored

allData %>% dplyr::select(ID, Gruppe, IR_post = HOMA.IR_akt, IR_pre = HOMA.IR_pre, MIS = MIS_ov, IS = MIS_ov_noCalc) %>% 
  gather(cognition, cognition_score,-Gruppe, -ID, -IR_pre, -IR_post)  %>%
  dplyr::mutate(Gruppe = dplyr::recode(Gruppe, BR = "PR")) %>%
  mutate(cognition = factor(cognition, levels=c("MIS", "IS"))) %>%
  
  ggplot(aes(x=IR_post, y=cognition_score)) + 
  geom_point(aes(color=Gruppe)) + 
  geom_line(stat="smooth", method="lm", color = "black", alpha = 0.3, linewidth = 1) +
  scale_color_manual(values = stand.col, name = "Group") +
  facet_wrap(~cognition, scales = "free_y", ncol= 2) +
  ylab("") + xlab("Insulin resistence (HOMA-IR)") +  
  #ggtitle("") + 
  guides(color = guide_legend(title="Group")) + 
  #theme(plot.title = element_text(size = 10)) + 
  theme_light()

ggsave(filename=paste("./GutBrain/Correlation_IS_IR_",Sys.Date(),".jpg", sep=""), width = 7, height = 3) # one row: height 2.5




# Weight loss and IR

# EWL und IR   --------------------------------

ggplot(allData, aes(x=postOP_EWL, y=HOMA.IR_pre)) + 
  geom_point(aes(color=Gruppe)) +   scale_color_manual(values = stand.col, name = "Group")
length(allData$HOMA.IR_pre[!is.na(allData$HOMA.IR_pre)])

ggplot(allData, aes(x=postOP_EWL, y=HOMA.IR_akt)) +
  geom_point(aes(color=Gruppe)) +   scale_color_manual(values = stand.col, name = "Group")
length(allData$HOMA.IR_akt[!is.na(allData$HOMA.IR_akt)])



# Moderation / Mediation: general preperation + graph ------------------------------------

# Outlier exclusion
gutbrain.var <- c("MIS_ov", "MIS_item", "MIS_sequ", "MIS_math", "AUC_Ghrelin", "AUC_GLP1","AUC_PYY", "AUC_Insulin", "AUC_Leptin", "iAUC_Ghrelin", "iAUC_GLP1", "iAUC_PYY", "iAUC_Leptin", "iAUC_Insulin")

gutbrain.data <- allData %>% dplyr::select(ID, Gruppe, one_of(gutbrain.var)) %>%
mutate_at(vars(-ID, -Gruppe), funs(ifelse(outliers(., int = TRUE) == TRUE, ., NA )))

# Graphic demonstration
windows()
gutbrain.data %>%
  dplyr::select(-MIS_math) %>%
  gather(Hormone, value,-Gruppe, -ID, -MIS_ov, -MIS_item, -MIS_sequ) %>%
  gather(Cognition, value_cog, MIS_ov:MIS_sequ) %>%
  separate(Hormone, c("type", "Hormone")) %>%
  mutate(Hormone = factor(Hormone, levels = c("GLP1", "PYY", "Ghrelin","Insulin", "Leptin")),
         Cognition = factor(Cognition, levels = c("MIS_ov", "MIS_item", "MIS_sequ"), labels = c("MIS", "MIS ITEM", "MIS SEQUENCE")),
         value = value/1000000) %>%
  filter(type == "iAUC",
         Hormone != "Leptin",
         Hormone != "Insulin") %>%  
         
  
  ggplot(aes(x = value_cog,
             y = value)) + 
  geom_point(aes(color = Gruppe)) +   
  geom_line(stat="smooth", method="lm", alpha = 0.7, size = 1,
            aes(color = Gruppe,
                group = Gruppe)) +
#  geom_text(aes(label = ifelse(ID %in% c(4, 6, 8), ID, "")), size = 3.5) + 
  geom_line(stat="smooth", method="lm", color = "black", alpha = 0.3, size = 1) +
  geom_smooth(color = "black", method = "lm", alpha = 0.1, linetype = 0) +
  geom_smooth(aes(color = Gruppe), method = "lm", alpha = 0.1, linetype = 0) + 
  facet_wrap(Cognition ~ Hormone, scales = "free_y", ncol= 3) +
  scale_color_manual(values = stand.col, name = "Group") +
  
  xlab("Cognitive Score") + ylab("Incremental Area under the Curve of plasma levels over 120 min  [ ?g / ml x min]") + 
  # ggtitle("Correlation of cognitive and hormonal outcome" ) + 
  theme(plot.title = element_text(size = 10)) + 
  theme_light()

ggsave(filename=paste("./GutBrain/Moderation_iAUC_",Sys.Date(),".jpg", sep=""), width= 9, height = 7) # width 12 für ncol 5




# Moderation: Cognition and iAUC ------------------------------------------

#### Moderation: MIS and PYY

# Preperation: Delete cases + scale data
gutbrain.data.sel <- gutbrain.data %>% drop_na(iAUC_PYY, MIS_ov) %>% mutate(Gruppe = as.numeric(Gruppe) -1 ) %>% mutate_at(vars(-ID, -c(3:6)), scale.this)

# Data transformation
gutbrain.data.sel$MIS_ov <- gutbrain.data.sel$MIS_ov^2 

# Model
MIS_PYY_Model_iAUC <- lm(MIS_ov ~ iAUC_PYY * Gruppe, data = gutbrain.data.sel) 
Anova(MIS_PYY_Model_iAUC, type = "III") 
summary.lm(MIS_PYY_Model_iAUC)

# different beta and p for Gruppe, if centered, in the modeeration model (and not in the model without the interaction term). Look and understand at https://stats.stackexchange.com/questions/65898/why-could-centering-independent-variables-change-the-main-effects-with-moderatio
gutbrain.data[c(5,7,3), "ID"] # outlier..?

# Data transformation: 
bc <- boxcox(MIS_PYY_Model_iAUC, lambda = seq(-3,3))
bc$x[which.max(bc$y)] #  = ~2

predict(caret::BoxCoxTrans(gutbrain.data.sel$MIS_ov), gutbrain.data.sel$MIS_ov)

# Effect size
effect("Gruppe", MIS_PYY_Model_iAUC, se = TRUE) %>% summary()
effect("Gruppe", MIS_PYY_Model_iAUC, se = TRUE)$se

# Normality?
by(augment(MIS_PYY_Model_iAUC)$.std.resid, augment(MIS_PYY_Model_iAUC)$Gruppe, stat.desc, basic = FALSE, norm = TRUE)
stat.desc(resid(MIS_PYY_Model_iAUC), basic = FALSE, norm = TRUE)
hist(rstudent(MIS_PYY_Model_iAUC))

# Homoscedasticity
autoplot(MIS_PYY_Model_iAUC)
bptest(MIS_PYY_Model_iAUC)
ncvTest(MIS_PYY_Model_iAUC)
leveneTest(residuals(MIS_PYY_Model_iAUC) ~ as.factor(gutbrain.data.sel$Gruppe))
bartlett.test(residuals(MIS_PYY_Model_iAUC) ~ as.factor(gutbrain.data.sel$Gruppe))

# Linearity?
dwt(MIS_PYY_Model_iAUC)

# Multicollinearity
MIS_PYY_Model_iAUC_noInteraction <- lm(MIS_ov ~ iAUC_PYY + Gruppe, data = gutbrain.data.sel) 
vif(MIS_PYY_Model_iAUC_noInteraction)
1/vif(MIS_PYY_Model_iAUC_noInteraction)
mean(vif(MIS_PYY_Model_iAUC_noInteraction))



#### Moderation: MIS and GLP1

# Preperation: Delete cases
gutbrain.data.sel <- gutbrain.data %>% drop_na(iAUC_GLP1, MIS_ov) %>% mutate(Gruppe = as.numeric(Gruppe) -1 ) %>% mutate_at(vars(-ID, -c(3:6)), scale.this)

# Data transformation
gutbrain.data.sel$MIS_ov <- gutbrain.data.sel$MIS_ov^2 

# Model
MIS_GLP1_Model_iAUC <- lm(MIS_ov ~ iAUC_GLP1 * Gruppe, data = gutbrain.data.sel) 
Anova(MIS_GLP1_Model_iAUC, type = "III") 

# Data transformation: 
bc <- boxcox(MIS_GLP1_Model_iAUC, lambda = seq(-3,3))
bc$x[which.max(bc$y)] #  = 1.6 -> ~2

# Effect size
effect("Gruppe", MIS_GLP1_Model_iAUC, se = TRUE) %>% summary()
effect("Gruppe", MIS_GLP1_Model_iAUC, se = TRUE)$se

# Assumptions
by(augment(MIS_GLP1_Model_iAUC)$.std.resid, augment(MIS_GLP1_Model_iAUC)$Gruppe, stat.desc, basic = FALSE, norm = TRUE) # Normality
autoplot(MIS_GLP1_Model_iAUC) # Normality / Homoscedasticity
leveneTest(residuals(MIS_GLP1_Model_iAUC) ~ as.factor(gutbrain.data.sel$Gruppe)) # Homoscedasticity
bartlett.test(residuals(MIS_GLP1_Model_iAUC) ~ as.factor(gutbrain.data.sel$Gruppe)) # Homoscedasticity
bptest(MIS_GLP1_Model_iAUC) # Homoscedasticity
ncvTest(MIS_GLP1_Model_iAUC) # Homoscedasticity
dwt(MIS_GLP1_Model_iAUC) # Linearity?
vif(lm(MIS_ov ~ iAUC_GLP1 + Gruppe, data = gutbrain.data.sel)) # Multicollinearity


#### Moderation: MIS and Ghrelin

# Preperation: Delete cases
gutbrain.data.sel <- gutbrain.data %>% drop_na(iAUC_Ghrelin, MIS_ov) %>% mutate(Gruppe = as.numeric(Gruppe) -1 ) %>% mutate_at(vars(-ID, -c(3:6)), scale.this)

# Data transformation
gutbrain.data.sel$MIS_ov <- gutbrain.data.sel$MIS_ov^2 

# Model
MIS_Ghrelin_Model_iAUC <- lm(MIS_ov ~ iAUC_Ghrelin * Gruppe, data = gutbrain.data.sel) 
Anova(MIS_Ghrelin_Model_iAUC, type = "III") 

# Data transformation: 
bc <- boxcox(MIS_Ghrelin_Model_iAUC, lambda = seq(-3,3))
bc$x[which.max(bc$y)] #  = ~2

# Effect size
effect("Gruppe", MIS_Ghrelin_Model_iAUC, se = TRUE) %>% summary()
effect("Gruppe", MIS_Ghrelin_Model_iAUC, se = TRUE)$se

# Assumptions
by(augment(MIS_Ghrelin_Model_iAUC)$.std.resid, augment(MIS_Ghrelin_Model_iAUC)$Gruppe, stat.desc, basic = FALSE, norm = TRUE) # Normality
autoplot(MIS_Ghrelin_Model_iAUC) # Normality / Homoscadisticity
leveneTest(residuals(MIS_Ghrelin_Model_iAUC) ~ as.factor(gutbrain.data.sel$Gruppe)) # Homoscadisticity
bartlett.test(residuals(MIS_Ghrelin_Model_iAUC) ~ as.factor(gutbrain.data.sel$Gruppe)) # Homoscadisticity
bptest(MIS_Ghrelin_Model_iAUC) # Homoscadisticity
ncvTest(MIS_Ghrelin_Model_iAUC) # Homoscadisticity
dwt(MIS_Ghrelin_Model_iAUC) # Linearity?
vif(lm(MIS_ov ~ iAUC_Ghrelin + Gruppe, data = gutbrain.data.sel)) # Multicollinearity




#### Moderation: MIS ITEM and PYY

# Preperation: Delete cases
gutbrain.data.sel <- gutbrain.data %>% drop_na(iAUC_PYY, MIS_item) %>% mutate(Gruppe = as.numeric(Gruppe) -1 ) %>% mutate_at(vars(-ID, -c(3:6)), scale.this)

# Data transformation
gutbrain.data.sel$MIS_item <- gutbrain.data.sel$MIS_item^3

# Model
MIS_item_PYY_Model_iAUC <- lm(MIS_item ~ iAUC_PYY * Gruppe, data = gutbrain.data.sel) 
Anova(MIS_item_PYY_Model_iAUC, type = "III") 

# Data transformation: 
bc <- boxcox(MIS_item_PYY_Model_iAUC, lambda = seq(-3,3))
bc$x[which.max(bc$y)] #  = ~ 3

# Effect size
effect("Gruppe", MIS_item_PYY_Model_iAUC, se = TRUE) %>% summary()
effect("Gruppe", MIS_item_PYY_Model_iAUC, se = TRUE)$se

# Assumptions
by(augment(MIS_item_PYY_Model_iAUC)$.std.resid, augment(MIS_item_PYY_Model_iAUC)$Gruppe, stat.desc, basic = FALSE, norm = TRUE) # Normality
autoplot(MIS_item_PYY_Model_iAUC) # Normality / Homoscadisticity
leveneTest(residuals(MIS_item_PYY_Model_iAUC) ~ as.factor(gutbrain.data.sel$Gruppe)) # Homoscadisticity
bartlett.test(residuals(MIS_item_PYY_Model_iAUC) ~ as.factor(gutbrain.data.sel$Gruppe)) # Homoscadisticity
bptest(MIS_item_PYY_Model_iAUC) # Homoscadisticity
ncvTest(MIS_item_PYY_Model_iAUC) # Homoscadisticity
dwt(MIS_item_PYY_Model_iAUC) # Linearity?
vif(lm(MIS_item ~ iAUC_PYY + Gruppe, data = gutbrain.data.sel)) # Multicollinearity



#### Moderation: MIS ITEM and GLP1

# Preperation: Delete cases
gutbrain.data.sel <- gutbrain.data %>% drop_na(iAUC_GLP1, MIS_item) %>% mutate(Gruppe = as.numeric(Gruppe) -1 ) %>% mutate_at(vars(-ID, -c(3:6)), scale.this)

# Data transformation
gutbrain.data.sel$MIS_item <- gutbrain.data.sel$MIS_item^3

# Model
MIS_item_GLP1_Model_iAUC <- lm(MIS_item ~ iAUC_GLP1 * Gruppe, data = gutbrain.data.sel) 
Anova(MIS_item_GLP1_Model_iAUC, type = "III") 

# Data transformation: 
bc <- boxcox(MIS_item_GLP1_Model_iAUC, lambda = seq(-3,3))
bc$x[which.max(bc$y)] #  = ~ 3

# Effect size
effect("Gruppe", MIS_item_GLP1_Model_iAUC, se = TRUE) %>% summary()
effect("Gruppe", MIS_item_GLP1_Model_iAUC, se = TRUE)$se

# Assumptions
by(augment(MIS_item_GLP1_Model_iAUC)$.std.resid, augment(MIS_item_GLP1_Model_iAUC)$Gruppe, stat.desc, basic = FALSE, norm = TRUE) # Normality
autoplot(MIS_item_GLP1_Model_iAUC) # Normality / Homoscadisticity
leveneTest(residuals(MIS_item_GLP1_Model_iAUC) ~ as.factor(gutbrain.data.sel$Gruppe)) # Homoscadisticity
bartlett.test(residuals(MIS_item_GLP1_Model_iAUC) ~ as.factor(gutbrain.data.sel$Gruppe)) # Homoscadisticity
bptest(MIS_item_GLP1_Model_iAUC) # Homoscadisticity
ncvTest(MIS_item_GLP1_Model_iAUC) # Homoscadisticity
dwt(MIS_item_GLP1_Model_iAUC) # Linearity?
vif(lm(MIS_item ~ iAUC_GLP1 + Gruppe, data = gutbrain.data.sel)) # Multicollinearity


# Simple Slopes (like posthoc tests) <-- only necessary if interaction term is significant
library(QuantPsyc)
sim.slopes(MIS_item_GLP1_Model_iAUC, meanCenter(gutbrain.data.sel$Gruppe))



#### Moderation: MIS ITEM and Ghrelin

# Preperation: Delete cases
gutbrain.data.sel <- gutbrain.data %>% drop_na(iAUC_Ghrelin, MIS_item) %>% mutate(Gruppe = as.numeric(Gruppe) -1 ) %>% mutate_at(vars(-ID, -c(3:6)), scale.this)

# Data transformation
gutbrain.data.sel$MIS_item <- gutbrain.data.sel$MIS_item^3

# Model
MIS_item_Ghrelin_Model_iAUC <- lm(MIS_item ~ iAUC_Ghrelin * Gruppe, data = gutbrain.data.sel) 
Anova(MIS_item_Ghrelin_Model_iAUC, type = "III") 

# Data transformation: 
bc <- boxcox(MIS_item_Ghrelin_Model_iAUC, lambda = seq(-3,3))
bc$x[which.max(bc$y)] #  = ~ 3

# Effect size
effect("Gruppe", MIS_item_Ghrelin_Model_iAUC, se = TRUE) %>% summary()
effect("Gruppe", MIS_item_Ghrelin_Model_iAUC, se = TRUE)$se

# Assumptions
by(augment(MIS_item_Ghrelin_Model_iAUC)$.std.resid, augment(MIS_item_Ghrelin_Model_iAUC)$Gruppe, stat.desc, basic = FALSE, norm = TRUE) # Normality
autoplot(MIS_item_Ghrelin_Model_iAUC) # Normality / Homoscedasticity
leveneTest(residuals(MIS_item_Ghrelin_Model_iAUC) ~ as.factor(gutbrain.data.sel$Gruppe)) # Homoscedasticity
bartlett.test(residuals(MIS_item_Ghrelin_Model_iAUC) ~ as.factor(gutbrain.data.sel$Gruppe)) # Homoscedasticity
bptest(MIS_item_Ghrelin_Model_iAUC) # Homoscedasticity
ncvTest(MIS_item_Ghrelin_Model_iAUC) # Homoscedasticity
dwt(MIS_item_Ghrelin_Model_iAUC) # Linearity?
vif(lm(MIS_item ~ iAUC_Ghrelin + Gruppe, data = gutbrain.data.sel)) # Multicollinearity



#### Moderation: MIS SEQUENCE and PYY

# Preperation: Delete cases
gutbrain.data.sel <- gutbrain.data %>% drop_na(iAUC_PYY, MIS_sequ) %>% mutate(Gruppe = as.numeric(Gruppe) -1 ) %>% mutate_at(vars(-ID, -c(3:6)), scale.this)

# delete 0 candidate, just to measure Lambda, not in original data
#gutbrain.data.sel[which(gutbrain.data.sel$ID == 4),] <- NA 

# Data transformation
gutbrain.data.sel$MIS_sequ <- gutbrain.data.sel$MIS_sequ^2

# Model
MIS_sequ_PYY_Model_iAUC <- lm(MIS_sequ ~ iAUC_PYY * Gruppe, data = gutbrain.data.sel) 
Anova(MIS_sequ_PYY_Model_iAUC, type = "III") 

# Data transformation: 
bc <- boxcox(MIS_sequ_PYY_Model_iAUC, lambda = seq(-3,3))
bc$x[which.max(bc$y)] #  = ~ 2

# Effect size
effect("Gruppe", MIS_sequ_PYY_Model_iAUC, se = TRUE) %>% summary()
effect("Gruppe", MIS_sequ_PYY_Model_iAUC, se = TRUE)$se

# Assumptions
by(augment(MIS_sequ_PYY_Model_iAUC)$.std.resid, augment(MIS_sequ_PYY_Model_iAUC)$Gruppe, stat.desc, basic = FALSE, norm = TRUE) # Normality
autoplot(MIS_sequ_PYY_Model_iAUC) # Normality / Homoscedasticity
leveneTest(residuals(MIS_sequ_PYY_Model_iAUC) ~ as.factor(gutbrain.data.sel$Gruppe)) # Homoscedasticity
bartlett.test(residuals(MIS_sequ_PYY_Model_iAUC) ~ as.factor(gutbrain.data.sel$Gruppe)) # Homoscedasticity
bptest(MIS_sequ_PYY_Model_iAUC) # Homoscedasticity
ncvTest(MIS_sequ_PYY_Model_iAUC) # Homoscedasticity
dwt(MIS_sequ_PYY_Model_iAUC) # Linearity?
vif(lm(MIS_sequ ~ iAUC_PYY + Gruppe, data = gutbrain.data.sel)) # Multicollinearity


#### Moderation: MIS SEQUENCE and GLP1

# Preperation: Delete cases
gutbrain.data.sel <- gutbrain.data %>% drop_na(iAUC_GLP1, MIS_sequ) %>% mutate(Gruppe = as.numeric(Gruppe) -1 ) %>% mutate_at(vars(-ID, -c(3:6)), scale.this)

# delete 0 candidate, just to measure Lambda, not in original data
# gutbrain.data.sel[which(gutbrain.data.sel$ID == 4),] <- NA 

# Data transformation
gutbrain.data.sel$MIS_sequ <- gutbrain.data.sel$MIS_sequ^2

# Model
MIS_sequ_GLP1_Model_iAUC <- lm(MIS_sequ ~ iAUC_GLP1 * Gruppe, data = gutbrain.data.sel) 
Anova(MIS_sequ_GLP1_Model_iAUC, type = "III") 

# Data transformation: 
bc <- boxcox(MIS_sequ_GLP1_Model_iAUC, lambda = seq(-3,3))
bc$x[which.max(bc$y)] #  = ~ 1.6 / 2

# Effect size
effect("Gruppe", MIS_sequ_GLP1_Model_iAUC, se = TRUE) %>% summary()
effect("Gruppe", MIS_sequ_GLP1_Model_iAUC, se = TRUE)$se

# Assumptions
by(augment(MIS_sequ_GLP1_Model_iAUC)$.std.resid, augment(MIS_sequ_GLP1_Model_iAUC)$Gruppe, stat.desc, basic = FALSE, norm = TRUE) # Normality
autoplot(MIS_sequ_GLP1_Model_iAUC) # Normality / Homoscadisticity
leveneTest(residuals(MIS_sequ_GLP1_Model_iAUC) ~ as.factor(gutbrain.data.sel$Gruppe)) # Homoscadisticity
bartlett.test(residuals(MIS_sequ_GLP1_Model_iAUC) ~ as.factor(gutbrain.data.sel$Gruppe)) # Homoscadisticity
bptest(MIS_sequ_GLP1_Model_iAUC) # Homoscadisticity
ncvTest(MIS_sequ_GLP1_Model_iAUC) # Homoscadisticity
dwt(MIS_sequ_GLP1_Model_iAUC) # Linearity?
vif(lm(MIS_sequ ~ iAUC_GLP1 + Gruppe, data = gutbrain.data.sel)) # Multicollinearity


#### Moderation: MIS SEQUENCE and Ghrelin

# Preperation: Delete cases
gutbrain.data.sel <- gutbrain.data %>% drop_na(iAUC_Ghrelin, MIS_sequ) %>% mutate(Gruppe = as.numeric(Gruppe) -1 ) %>% mutate_at(vars(-ID, -c(3:6)), scale.this)

# delete 0 candidate, just to measure Lambda, not in original data
# gutbrain.data.sel[which(gutbrain.data.sel$ID == 4),] <- NA 

# Data transformation
gutbrain.data.sel$MIS_sequ <- gutbrain.data.sel$MIS_sequ^2

# Model
MIS_sequ_Ghrelin_Model_iAUC <- lm(MIS_sequ ~ iAUC_Ghrelin * Gruppe, data = gutbrain.data.sel) 
Anova(MIS_sequ_Ghrelin_Model_iAUC, type = "III") 

# Data transformation: 
bc <- boxcox(MIS_sequ_Ghrelin_Model_iAUC, lambda = seq(-3,3))
bc$x[which.max(bc$y)] #  = ~ 2 / 1.6

# Effect size
effect("Gruppe", MIS_sequ_Ghrelin_Model_iAUC, se = TRUE) %>% summary()
effect("Gruppe", MIS_sequ_Ghrelin_Model_iAUC, se = TRUE)$se

# Assumptions
by(augment(MIS_sequ_Ghrelin_Model_iAUC)$.std.resid, augment(MIS_sequ_Ghrelin_Model_iAUC)$Gruppe, stat.desc, basic = FALSE, norm = TRUE) # Normality
autoplot(MIS_sequ_Ghrelin_Model_iAUC) # Normality / Homoscadisticity
leveneTest(residuals(MIS_sequ_Ghrelin_Model_iAUC) ~ as.factor(gutbrain.data.sel$Gruppe)) # Homoscadisticity
bartlett.test(residuals(MIS_sequ_Ghrelin_Model_iAUC) ~ as.factor(gutbrain.data.sel$Gruppe)) # Homoscadisticity
bptest(MIS_sequ_Ghrelin_Model_iAUC) # Homoscadisticity
ncvTest(MIS_sequ_Ghrelin_Model_iAUC) # Homoscadisticity
dwt(MIS_sequ_Ghrelin_Model_iAUC) # Linearity?
vif(lm(MIS_sequ ~ iAUC_Ghrelin + Gruppe, data = gutbrain.data.sel)) # Multicollinearity


### produce output table of gathered results
values1 <- c("MIS", "MIS_item", "MIS_sequ")
values2 <- c("GLP1", "PYY", "Ghrelin")
values <- as.vector(t(outer(values1, values2, paste, sep="_")))

allMM_iAUC <- lapply(values, function(x) {
  model <- Anova(get(paste0(x, "_Model_iAUC")), type = "III")
  summary <- glance(get(paste0(x, "_Model_iAUC")))
  temp <- c(summary[c(1,2,4,5)], model$F[2:4], model$P[2:4])
  names(temp) <- c("Model.Rsqu", "Model.RsquAdj", "Model.F", "Model.p","F.Hormone", "F.Gruppe", "F.interaction", "p.Hormone", "p.Gruppe", "p.interaction")
  
  return(temp)
  
}) %>% lapply(., as.data.frame.list) %>% bind_rows()
rownames(allMM_iAUC) <- values 
allMM_iAUC %>% write.table("./GutBrain/allMM_iAUC.csv", row.names = TRUE)


allMM_AUC <- lapply(values, function(x) {
  model <- Anova(get(paste0(x, "_Model_AUC")), type = "III")
  summary <- glance(get(paste0(x, "_Model_AUC")))
  temp <- c(summary[c(1,2,4,5)], model$F[2:4], model$P[2:4])
  names(temp) <- c("Model.Rsqu", "Model.RsquAdj", "Model.F", "Model.p","F.Hormone", "F.Gruppe", "F.interaction", "p.Hormone", "p.Gruppe", "p.interaction")
  
  return(temp)
  
}) %>% lapply(., as.data.frame.list) %>% bind_rows()
rownames(allMM_AUC) <- values 
allMM_AUC %>% write.table("./GutBrain/allMM_AUC.csv", row.names = TRUE)




# Moderation: Cognition and AUC -------------------------------------------

# MIS and PYY
MIS_PYY_Model_AUC <- lm(MIS_ov ~ AUC_PYY * Gruppe, data = gutbrain.data) 
Anova(MIS_PYY_Model_AUC, type = "III")
summary.lm(MIS_PYY_Model_AUC)

effect("Gruppe", MIS_PYY_Model_AUC, se = TRUE) %>% summary()
effect("Gruppe", MIS_PYY_Model_AUC, se = TRUE)$se

by(augment(MIS_PYY_Model_AUC)$.std.resid, augment(MIS_PYY_Model_AUC)$Gruppe, stat.desc, basic = FALSE, norm = TRUE)
hist(rstudent(MIS_PYY_Model_AUC))
autoplot(MIS_PYY_Model_AUC)
dwt(MIS_PYY_Model_AUC)
vif(MIS_PYY_Model_AUC)
1/vif(MIS_PYY_Model_AUC)
mean(vif(MIS_PYY_Model_AUC))


# Test for homogeneity of regression slopes: ANCOVA with interaction
MIS_math_CovModel_AUCInt <- aov(MIS_math ~  dem_eduyears + Test_age + Gruppe + Test_age:Gruppe + dem_eduyears:Gruppe, data = Ospan_AllScores)
Anova(MIS_math_CovModel_AUCInt, type = "III")
summary.lm(MIS_math_CovModel_AUCInt)

# MIS and GLP-1
MIS_GLP1_Model_AUC <- aov(MIS_ov ~ AUC_GLP1 * Gruppe, data = gutbrain.data) 
Anova(MIS_GLP1_Model_AUC, type = "III")
summary.lm(MIS_GLP1_Model_AUC)

effect("Gruppe", MIS_GLP1_Model_AUC, se = TRUE) %>% summary()
effect("Gruppe", MIS_GLP1_Model_AUC, se = TRUE) %>% .$se


# MIS and Ghrelin
MIS_Ghrelin_Model_AUC <- aov(MIS_ov ~ AUC_Ghrelin * Gruppe, data = gutbrain.data) 
Anova(MIS_Ghrelin_Model_AUC, type = "III")
summary.lm(MIS_Ghrelin_Model_AUC)

effect("Gruppe", MIS_Ghrelin_Model_AUC, se = TRUE) %>% summary()
effect("Gruppe", MIS_Ghrelin_Model_AUC, se = TRUE) %>% .$se




# MIS ITEM and PYY
MIS_item_PYY_Model_AUC <- aov(MIS_item ~ AUC_PYY * Gruppe, data = gutbrain.data) 
Anova(MIS_item_PYY_Model_AUC, type = "III")
summary.lm(MIS_item_PYY_Model_AUC)

effect("Gruppe", MIS_item_PYY_Model_AUC, se = TRUE) %>% summary()
effect("Gruppe", MIS_item_PYY_Model_AUC, se = TRUE) %>% .$se

# MIS ITEM and GLP-1
MIS_item_GLP1_Model_AUC <- aov(MIS_item ~ AUC_GLP1 * Gruppe, data = gutbrain.data) 
Anova(MIS_item_GLP1_Model_AUC, type = "III")
summary.lm(MIS_item_GLP1_Model_AUC)

effect("Gruppe", MIS_item_GLP1_Model_AUC, se = TRUE) %>% summary()
effect("Gruppe", MIS_item_GLP1_Model_AUC, se = TRUE) %>% .$se


# MIS ITEM and Ghrelin
MIS_item_Ghrelin_Model_AUC <- aov(MIS_item ~ AUC_Ghrelin * Gruppe, data = gutbrain.data) 
Anova(MIS_item_Ghrelin_Model_AUC, type = "III")
summary.lm(MIS_item_Ghrelin_Model_AUC)

effect("Gruppe", MIS_item_Ghrelin_Model_AUC, se = TRUE) %>% summary()
effect("Gruppe", MIS_item_Ghrelin_Model_AUC, se = TRUE) %>% .$se





# MIS SEQUENCE and PYY
MIS_sequ_PYY_Model_AUC <- aov(MIS_sequ ~ AUC_PYY * Gruppe, data = gutbrain.data) 
Anova(MIS_sequ_PYY_Model_AUC, type = "III")
summary.lm(MIS_sequ_PYY_Model_AUC)

effect("Gruppe", MIS_sequ_PYY_Model_AUC, se = TRUE) %>% summary()
effect("Gruppe", MIS_sequ_PYY_Model_AUC, se = TRUE) %>% .$se

# MIS SEQUENCE and GLP-1
MIS_sequ_GLP1_Model_AUC <- aov(MIS_sequ ~ AUC_GLP1 * Gruppe, data = gutbrain.data) 
Anova(MIS_sequ_GLP1_Model_AUC, type = "III")
summary.lm(MIS_sequ_GLP1_Model_AUC)

effect("Gruppe", MIS_sequ_GLP1_Model_AUC, se = TRUE) %>% summary()
effect("Gruppe", MIS_sequ_GLP1_Model_AUC, se = TRUE) %>% .$se


# MIS SEQUENCE and Ghrelin
MIS_sequ_Ghrelin_Model_AUC <- aov(MIS_sequ ~ AUC_Ghrelin * Gruppe, data = gutbrain.data) 
Anova(MIS_sequ_Ghrelin_Model_AUC, type = "III")
summary.lm(MIS_sequ_Ghrelin_Model_AUC)

effect("Gruppe", MIS_sequ_Ghrelin_Model_AUC, se = TRUE) %>% summary()
effect("Gruppe", MIS_sequ_Ghrelin_Model_AUC, se = TRUE) %>% .$se

# stack together results for table print
values1 <- c("MIS", "MIS_item", "MIS_sequ")
values2 <- c("GLP1", "PYY", "Ghrelin")
values <- as.vector(t(outer(values1, values2, paste, sep="_")))

allMM_iAUC <- lapply(values, function(x) {
  model <- Anova(get(paste0(x, "_Model_iAUC")), type = "III")
  summary <- glance(get(paste0(x, "_Model_iAUC")))
  temp <- c(summary[c(1,2,4,5)], model$F[2:4], model$P[2:4])
  names(temp) <- c("Model.Rsqu", "Model.RsquAdj", "Model.F", "Model.p","F.Hormone", "F.Gruppe", "F.interaction", "p.Hormone", "p.Gruppe", "p.interaction")
  
  return(temp)
  
}) %>% lapply(., as.data.frame.list) %>% bind_rows()
rownames(allMM_iAUC) <- values 
allMM_iAUC %>% write.table("./GutBrain/allMM_iAUC.csv", row.names = TRUE)


allMM_AUC <- lapply(values, function(x) {
  model <- Anova(get(paste0(x, "_Model_AUC")), type = "III")
  summary <- glance(get(paste0(x, "_Model_AUC")))
  temp <- c(summary[c(1,2,4,5)], model$F[2:4], model$P[2:4])
  names(temp) <- c("Model.Rsqu", "Model.RsquAdj", "Model.F", "Model.p","F.Hormone", "F.Gruppe", "F.interaction", "p.Hormone", "p.Gruppe", "p.interaction")
  
  return(temp)
  
}) %>% lapply(., as.data.frame.list) %>% bind_rows()
rownames(allMM_AUC) <- values 
allMM_AUC %>% write.table("./GutBrain/allMM_AUC.csv", row.names = TRUE)



# Mediation: Cognition ~ M(iAUC) + weight loss group  ------------------------------------------------------
#  Hormone-Resonse mediated the association of Weight Loss and cogntition 


# Mediation analysis using mediate() from mediation-package
# following https://data.library.virginia.edu/introduction-to-mediation-analysis/
# and http://davidakenny.net/cm/mediate.htm


# MIS SEQU (Y) ~ PYY (M) + weight loss group (X)

# Preperation: Delete cases
gutbrain.data.sel <- gutbrain.data %>% drop_na(iAUC_PYY, MIS_sequ)
gutbrain.data.sel$Gruppe <- as.numeric(gutbrain.data.sel$Gruppe)

# 1. Step: direct effect (c´). MIS SEQU ~ weight loss group 
MIS_sequ_PYY_Mediat_c <- lm(MIS_sequ ~ Gruppe, data = gutbrain.data.sel) 
summary(MIS_sequ_PYY_Mediat_c)

# 2. step: PYY ~ weight loss group
MIS_sequ_PYY_Mediat_a <- lm(iAUC_PYY ~ Gruppe, data = gutbrain.data.sel)
summary(MIS_sequ_PYY_Mediat_a)

# 3. step: MIS ~ PYY + weight loss group
MIS_sequ_PYY_Mediat_b <- lm(MIS_sequ ~ Gruppe + iAUC_PYY, data = gutbrain.data.sel)
summary(MIS_sequ_PYY_Mediat_b)

# 4. step: Mediation Analysis


MIS_sequ_PYY_Mediat_res <- mediation::mediate(MIS_sequ_PYY_Mediat_a, 
                                              MIS_sequ_PYY_Mediat_b, 
                                              treat = "Gruppe", mediator = "iAUC_PYY",
                                              boot = TRUE, sims = 1000)

summary(MIS_sequ_PYY_Mediat_res)

plot(MIS_sequ_PYY_Mediat_res)

MIS_sequ_PYY_Mediat_res_std <- psych::mediate(MIS_sequ ~ Gruppe + (iAUC_PYY), 
                                              data = gutbrain.data.sel,
                                              n.iter = 10000, std = TRUE)
MIS_sequ_PYY_Mediat_res_std
summary(MIS_sequ_PYY_Mediat_res_std)

# Results:
# Total effect: x --> y without M (estimate X from Step 1)
# ADE / Direct effect:  x --> y after taking into account a mediation (indirect) effect of M (estimate x from Step 3) 
# ACME (Average Causal Mediation Effects) / Mediation effect: total effect minus the direct effect

# for further Analysis, Step 1 is not needed.



# MIS ITEM (Y) ~ PYY (M) + weight loss group (X)

# Preperation: Delete cases
gutbrain.data.sel <- gutbrain.data %>% drop_na(iAUC_PYY, MIS_item)
gutbrain.data.sel$Gruppe <- as.numeric(gutbrain.data.sel$Gruppe)

# Model c', a and b
MIS_item_PYY_Mediat_c <- lm(MIS_item ~ Gruppe, data = gutbrain.data.sel) 
summary(MIS_item_PYY_Mediat_c)
MIS_item_PYY_Mediat_a <- lm(iAUC_PYY ~ Gruppe, data = gutbrain.data.sel)
summary(MIS_item_PYY_Mediat_a)
MIS_item_PYY_Mediat_b <- lm(MIS_item ~ Gruppe + iAUC_PYY, data = gutbrain.data.sel)
summary(MIS_item_PYY_Mediat_b)

# Step 2 is not significant. Mediation not possible. 

# Mediation Analysis
MIS_item_PYY_Mediat_res <- mediation::mediate(MIS_item_PYY_Mediat_a, 
                                              MIS_item_PYY_Mediat_b, 
                                              treat = "Gruppe", mediator = "iAUC_PYY",
                                              boot = TRUE, sims = 1000)
summary(MIS_item_PYY_Mediat_res)
plot(MIS_item_PYY_Mediat_res)

MIS_item_PYY_Mediat_res_std <- psych::mediate(MIS_item ~ Gruppe + (iAUC_PYY), 
                                              data = gutbrain.data.sel,
                                              n.iter = 10000, std = TRUE)
MIS_item_PYY_Mediat_res_std
summary(MIS_item_PYY_Mediat_res_std)



# MIS (Y) ~ PYY (M) + weight loss group (X)

# Preperation: Delete cases
gutbrain.data.sel <- gutbrain.data %>% drop_na(iAUC_PYY, MIS_ov)
gutbrain.data.sel$Gruppe <- as.numeric(gutbrain.data.sel$Gruppe)

# Model c', a and b
MIS_ov_PYY_Mediat_c <- lm(MIS_ov ~ Gruppe, data = gutbrain.data.sel) 
summary(MIS_ov_PYY_Mediat_c)
MIS_ov_PYY_Mediat_a <- lm(iAUC_PYY ~ Gruppe, data = gutbrain.data.sel)
summary(MIS_ov_PYY_Mediat_a)
MIS_ov_PYY_Mediat_b <- lm(MIS_ov ~ Gruppe + iAUC_PYY, data = gutbrain.data.sel)
summary(MIS_ov_PYY_Mediat_b)

# Step 2 is not significant. Mediation not possible. 

# Mediation Analysis
MIS_ov_PYY_Mediat_res <- mediation::mediate(MIS_ov_PYY_Mediat_a, 
                                              MIS_ov_PYY_Mediat_b, 
                                              treat = "Gruppe", mediator = "iAUC_PYY",
                                              boot = TRUE, sims = 1000)
summary(MIS_ov_PYY_Mediat_res)
plot(MIS_ov_PYY_Mediat_res)

MIS_ov_PYY_Mediat_res_std <- psych::mediate(MIS_ov ~ Gruppe + (iAUC_PYY), 
                                              data = gutbrain.data.sel,
                                              n.iter = 10000, std = TRUE)
MIS_ov_PYY_Mediat_res_std
summary(MIS_ov_PYY_Mediat_res_std)


# MIS_sequ (Y) ~ GLP1 (M) + weight loss group (X)

# Preperation: Delete cases
gutbrain.data.sel <- gutbrain.data %>% drop_na(iAUC_GLP1, MIS_sequ)
gutbrain.data.sel$Gruppe <- as.numeric(gutbrain.data.sel$Gruppe)

# Model c', a and b
MIS_sequ_GLP1_Mediat_c <- lm(MIS_sequ ~ Gruppe, data = gutbrain.data.sel) 
summary(MIS_sequ_GLP1_Mediat_c)
MIS_sequ_GLP1_Mediat_a <- lm(iAUC_GLP1 ~ Gruppe, data = gutbrain.data.sel)
summary(MIS_sequ_GLP1_Mediat_a)
MIS_sequ_GLP1_Mediat_b <- lm(MIS_sequ ~ Gruppe + iAUC_GLP1, data = gutbrain.data.sel)
summary(MIS_sequ_GLP1_Mediat_b)

# X explains more y in presence of M (Step 3). No Mediation effect. 

# Mediation Analysis
MIS_sequ_GLP1_Mediat_res <- mediation::mediate(MIS_sequ_GLP1_Mediat_a, 
                                            MIS_sequ_GLP1_Mediat_b, 
                                            treat = "Gruppe", mediator = "iAUC_GLP1",
                                            boot = TRUE, sims = 1000)
summary(MIS_sequ_GLP1_Mediat_res)
plot(MIS_sequ_GLP1_Mediat_res)

MIS_sequ_GLP1_Mediat_res_std <- psych::mediate(MIS_sequ ~ Gruppe + (iAUC_GLP1), 
                                               data = gutbrain.data.sel,
                                               n.iter = 10000, std = TRUE)
MIS_sequ_GLP1_Mediat_res_std
summary(MIS_sequ_GLP1_Mediat_res_std)

# MIS_item (Y) ~ GLP1 (M) + weight loss group (X)

# Preperation: Delete cases
gutbrain.data.sel <- gutbrain.data %>% drop_na(iAUC_GLP1, MIS_item)
gutbrain.data.sel$Gruppe <- as.numeric(gutbrain.data.sel$Gruppe)

# Model c', a and b
MIS_item_GLP1_Mediat_c <- lm(MIS_item ~ Gruppe, data = gutbrain.data.sel) 
summary(MIS_item_GLP1_Mediat_c)
MIS_item_GLP1_Mediat_a <- lm(iAUC_GLP1 ~ Gruppe, data = gutbrain.data.sel)
summary(MIS_item_GLP1_Mediat_a)
MIS_item_GLP1_Mediat_b <- lm(MIS_item ~ Gruppe + iAUC_GLP1, data = gutbrain.data.sel)
summary(MIS_item_GLP1_Mediat_b)

# Mediation Analysis is not valid (thought it was..)

# Mediation Analysis
MIS_item_GLP1_Mediat_res <- mediation::mediate(MIS_item_GLP1_Mediat_a, 
                                             MIS_item_GLP1_Mediat_b, 
                                             treat = "Gruppe", mediator = "iAUC_GLP1",
                                             boot = TRUE, sims = 1000)
summary(MIS_item_GLP1_Mediat_res)
plot(MIS_item_GLP1_Mediat_res)

MIS_item_GLP1_Mediat_res_std <- psych::mediate(MIS_item ~ Gruppe + (iAUC_GLP1), 
                                              data = gutbrain.data.sel,
                                              n.iter = 10000, std = TRUE)
MIS_item_GLP1_Mediat_res_std
summary(MIS_item_GLP1_Mediat_res_std)

  
# MIS (Y) ~ GLP1 (M) + weight loss group (X)

# Preperation: Delete cases
gutbrain.data.sel <- gutbrain.data %>% drop_na(iAUC_GLP1, MIS_ov)
gutbrain.data.sel$Gruppe <- as.numeric(gutbrain.data.sel$Gruppe)

# Model c', a and b
MIS_ov_GLP1_Mediat_c <- lm(MIS_ov ~ Gruppe, data = gutbrain.data.sel) 
summary(MIS_ov_GLP1_Mediat_c)
MIS_ov_GLP1_Mediat_a <- lm(iAUC_GLP1 ~ Gruppe, data = gutbrain.data.sel)
summary(MIS_ov_GLP1_Mediat_a)
MIS_ov_GLP1_Mediat_b <- lm(MIS_ov ~ Gruppe + iAUC_GLP1, data = gutbrain.data.sel)
summary(MIS_ov_GLP1_Mediat_b)

# X explains more y in presence of M (Step 3). No Mediation effetcs. 

# Mediation Analysis
MIS_ov_GLP1_Mediat_res <- mediation::mediate(MIS_ov_GLP1_Mediat_a, 
                                             MIS_ov_GLP1_Mediat_b, 
                                             treat = "Gruppe", mediator = "iAUC_GLP1",
                                             boot = TRUE, sims = 5000)
summary(MIS_ov_GLP1_Mediat_res)
plot(MIS_ov_GLP1_Mediat_res)

MIS_ov_GLP1_Mediat_res_std <- psych::mediate(MIS_ov ~ Gruppe + (iAUC_GLP1), 
                                                  data = gutbrain.data.sel,
                                                  n.iter = 10000, std = TRUE)
MIS_ov_GLP1_Mediat_res_std
mediate.diagram(MIS_sequ_GLP1_Mediat_res_std)




# MIS_sequ (Y) ~ Ghrelin (M) + weight loss group (X)

# Preperation: Delete cases
gutbrain.data.sel <- gutbrain.data %>% drop_na(iAUC_Ghrelin, MIS_sequ)
gutbrain.data.sel$Gruppe <- as.numeric(gutbrain.data.sel$Gruppe)

# Model c', a and b
MIS_sequ_Ghrelin_Mediat_c <- lm(MIS_sequ ~ Gruppe, data = gutbrain.data.sel) 
summary(MIS_sequ_Ghrelin_Mediat_c)
MIS_sequ_Ghrelin_Mediat_a <- lm(iAUC_Ghrelin ~ Gruppe, data = gutbrain.data.sel)
summary(MIS_sequ_Ghrelin_Mediat_a)
MIS_sequ_Ghrelin_Mediat_b <- lm(MIS_sequ ~ Gruppe + iAUC_Ghrelin, data = gutbrain.data.sel)
summary(MIS_sequ_Ghrelin_Mediat_b)

# Mediation Analysis is valid

# Mediation Analysis
MIS_sequ_Ghrelin_Mediat_res <- mediation::mediate(MIS_sequ_Ghrelin_Mediat_a, 
                                               MIS_sequ_Ghrelin_Mediat_b, 
                                               treat = "Gruppe", mediator = "iAUC_Ghrelin",
                                               #boot.ci.type = "bca",
                                               boot = TRUE, sims = 5000)
summary(MIS_sequ_Ghrelin_Mediat_res)
plot(MIS_sequ_Ghrelin_Mediat_res)

MIS_sequ_Ghrelin_Mediat_res_std <- psych::mediate(MIS_sequ ~ Gruppe + (iAUC_Ghrelin), 
                                              data = gutbrain.data.sel,
                                              n.iter = 10000, std = TRUE)
MIS_sequ_Ghrelin_Mediat_res_std
summary(MIS_sequ_Ghrelin_Mediat_res_std)
mediate.diagram(MIS_sequ_Ghrelin_Mediat_res_std)

library(lavaan)
mod1 <- "# a path
         iAUC_Ghrelin ~ a * Gruppe

         # b path
         MIS_sequ ~ b * iAUC_Ghrelin

         # c prime path 
         MIS_sequ ~ cp * Gruppe

         # indirect and total effects
         ab := a * b
         total := cp + ab"
temp1 <- sem(mod1, data = gutbrain.data.sel, se = "bootstrap", bootstrap = 10000)
summary(temp1, standardized = TRUE)

# signifikant!!!!!!!!!!!!!!!


# MIS_item (Y) ~ Ghrelin (M) + weight loss group (X)

# Preperation: Delete cases
gutbrain.data.sel <- gutbrain.data %>% drop_na(iAUC_Ghrelin, MIS_item)
gutbrain.data.sel$Gruppe <- as.numeric(gutbrain.data.sel$Gruppe)

# Model c', a and b
MIS_item_Ghrelin_Mediat_c <- lm(MIS_item ~ Gruppe, data = gutbrain.data.sel) 
summary(MIS_item_Ghrelin_Mediat_c)
MIS_item_Ghrelin_Mediat_a <- lm(iAUC_Ghrelin ~ Gruppe, data = gutbrain.data.sel)
summary(MIS_item_Ghrelin_Mediat_a)
MIS_item_Ghrelin_Mediat_b <- lm(MIS_item ~ Gruppe + iAUC_Ghrelin, data = gutbrain.data.sel)
summary(MIS_item_Ghrelin_Mediat_b)

# Mediation Analysis is valid

# Mediation Analysis
MIS_item_Ghrelin_Mediat_res <- mediation::mediate(MIS_item_Ghrelin_Mediat_a, 
                                               MIS_item_Ghrelin_Mediat_b, 
                                               treat = "Gruppe", mediator = "iAUC_Ghrelin",
                                               boot = TRUE, sims = 5000)
summary(MIS_item_Ghrelin_Mediat_res)
plot(MIS_item_Ghrelin_Mediat_res)

MIS_item_Ghrelin_Mediat_res_std <- psych::mediate(MIS_item ~ Gruppe + (iAUC_Ghrelin), 
                                                  data = gutbrain.data.sel,
                                                  n.iter = 10000, std = TRUE)
MIS_item_Ghrelin_Mediat_res_std
summary(MIS_item_Ghrelin_Mediat_res_std)
# signifikant!!!!!!!!!!!!!!!


# MIS (Y) ~ Ghrelin (M) + weight loss group (X)

# Preperation: Delete cases
gutbrain.data.sel <- gutbrain.data %>% drop_na(iAUC_Ghrelin, MIS_ov)
gutbrain.data.sel$Gruppe <- as.numeric(gutbrain.data.sel$Gruppe)

# Model c', a and b
MIS_ov_Ghrelin_Mediat_c <- lm(MIS_ov ~ Gruppe, data = gutbrain.data.sel) 
summary(MIS_ov_Ghrelin_Mediat_c)
MIS_ov_Ghrelin_Mediat_a <- lm(iAUC_Ghrelin ~ Gruppe, data = gutbrain.data.sel)
summary(MIS_ov_Ghrelin_Mediat_a)
MIS_ov_Ghrelin_Mediat_b <- lm(MIS_ov ~ Gruppe + iAUC_Ghrelin, data = gutbrain.data.sel)
summary(MIS_ov_Ghrelin_Mediat_b)

# Mediation Analysis is valid

# Mediation Analysis
MIS_ov_Ghrelin_Mediat_res <- mediation::mediate(MIS_ov_Ghrelin_Mediat_a, 
                                             MIS_ov_Ghrelin_Mediat_b, 
                                             treat = "Gruppe", mediator = "iAUC_Ghrelin",
                                             boot = TRUE, sims = 5000)
summary(MIS_ov_Ghrelin_Mediat_res)
plot(MIS_ov_Ghrelin_Mediat_res)

MIS_ov_Ghrelin_Mediat_res_std <- psych::mediate(MIS_ov ~ Gruppe + (iAUC_Ghrelin), 
                                                  data = gutbrain.data.sel,
                                                  n.iter = 10000, std = TRUE)
MIS_ov_Ghrelin_Mediat_res_std
summary(MIS_ov_Ghrelin_Mediat_res_std)
# not signifikant!
 

 
summary(MIS_ov_Ghrelin_Mediat_res)
summary.mediate(MIS_ov_Ghrelin_Mediat_res)
plot(MIS_ov_Ghrelin_Mediat_res)


## summarize results
values1 <- c("MIS_ov", "MIS_sequ", "MIS_item")
values2 <- paste0("iAUC_", c("GLP1", "PYY", "Ghrelin"))
values <- as.vector(t(outer(values1, values2, paste, sep="_")))
values_comb <- expand.grid(values1, values2, stringsAsFactors = FALSE)

# indirect effects
# allMed_iAUC <- mapply(function(var1, var2) {
#  #paste0(var1,":" ,var2) %>% print
# 
#   gutbrain.data.sel <- gutbrain.data %>% dplyr::select(ID, Gruppe, cogn = var1, horm = var2) %>% drop_na(cogn, horm)
#   gutbrain.data.sel$Gruppe <- as.numeric(gutbrain.data.sel$Gruppe)
#   
#   model <- psych::mediate(cogn ~ Gruppe + (horm),
#                           data = gutbrain.data.sel,
#                           n.iter = 10000, std = TRUE)
#   
#   temp <- unlist(c(model$boot[1:2], unlist(model$boot[3])))
# 
# print(temp)
#   
# },
# var1 = values_comb[,1],
# var2 = values_comb[,2]) %>% t
# rownames(allMed_iAUC) <- paste(values_comb[,1], values_comb[,2], sep = ":")
# allMed_iAUC

# pathways
allMedPaths_iAUC <- mapply(function(var1, var2) {


  gutbrain.data.sel <- gutbrain.data %>% dplyr::select(ID, Gruppe, cogn = var1, horm = var2) %>% drop_na(cogn, horm)
  gutbrain.data.sel$Gruppe <- as.numeric(gutbrain.data.sel$Gruppe)
  
  
  model <- psych::mediate(cogn ~ Gruppe + (horm),
                          data = gutbrain.data.sel,
                          n.iter = 10000, std = TRUE)
  
  paths <- rbind.data.frame(unlist(model$a.reg),
                            as.data.frame(model$b.reg)[2,],
                            unlist(model$total.reg),
                            as.data.frame(model$b.reg)[1,],
  as.vector(unlist(c(model$boot[1:2], unlist(model$boot[3]), 9999.99))))
  colnames(paths) <- c("B", "SE", "t", "p", "R2")                 
  rownames(paths) <- c("Path a", "Path b", "Total effect", "Direct effect", "Indirect effect")
  
  return(paths)
  
},
var1 = values_comb[,1],
var2 = values_comb[,2],
SIMPLIFY = FALSE)

allMedPaths_iAUC
ames(allMedPaths_iAUC) <- paste(values_comb[,1], values_comb[,2], sep = ":")



# Multiple mediation: All Hormones ------------------------------------------------------

# Preperation
gutbrain.var <- c("MIS_ov", "MIS_item", "MIS_sequ", "MIS_math", "MIS_ov_noCalc" , "AUC_Ghrelin", "AUC_GLP1","AUC_PYY", "AUC_Insulin", "AUC_Leptin", "iAUC_Ghrelin", "iAUC_GLP1", "iAUC_PYY", "iAUC_Leptin", "iAUC_Insulin")

gutbrain.data.sel <- allData %>% dplyr::select(ID, Gruppe, one_of(gutbrain.var)) %>%
  mutate_at(vars(-ID, -Gruppe), funs(ifelse(outliers(., int = TRUE) == TRUE, ., NA ))) %>%
  drop_na(MIS_sequ) %>%   # for case wise deletion only remove NAs in outcome variable iAUC_Ghrelin, iAUC_GLP1, iAUC_PYY 
  mutate(Gruppe = as.numeric(Gruppe)) %>%  # dummycode group variable
  mutate_at(vars(one_of(gutbrain.var[6:15])), funs(./1000 /1000)) # iAUC in µg/ml/min
  # mutate_at(vars(-ID), scale.this) # Standartizing is not necessary due to lavaans options.

library(lavaan)
library(semPlot)

# Model structure
AllHormones.model <- '
    MIS_ov_noCalc ~ b1*iAUC_Ghrelin + b2*iAUC_GLP1 + b3*iAUC_PYY + c*Gruppe     # change MIS type here!
    iAUC_Ghrelin ~ a1*Gruppe
    iAUC_GLP1 ~ a2*Gruppe
    iAUC_PYY ~ a3*Gruppe

    indirect1 := a1*b1
    indirect2 := a2*b2
    indirect3 := a3*b3

    total   := c + (a1*b1) + (a2*b2) + (a3*b3)

    iAUC_GLP1 ~~ iAUC_Ghrelin
    iAUC_Ghrelin ~~ iAUC_PYY
    iAUC_PYY ~~ iAUC_GLP1
    '

# Run model (this will take some time) with bootstrapping
fit <- sem(AllHormones.model, data = gutbrain.data.sel, se = "bootstrap", bootstrap = 5000, missing = "direct") # prefer to set to 5000, maybe missing = "ml"

# summary + CIs
summary(fit, standardized = TRUE, fit.measures=TRUE, rsquare=TRUE, ci = TRUE) 
parameterestimates(fit, boot.ci.type = "bca.simple", standardized = TRUE, rsquare = TRUE) # BCa is recommended by Hayes&Scharkow, when power is of concern, although alpha error might rice to ~ 7%

# graphical description (in a weird way)
semPaths(fit, whatLabels = "std", nCharNodes = 0, layout = "circle", edge.label.cex = 1.2) # , nodeLabels = c("GLP1", "PYY", "Ghrelin", "MIS sequ", "WL")

# kable to markdown / csv-save results
library(knitr)
library(kableExtra)

parameterestimates(fit, boot.ci.type = "bca.simple", standardized = TRUE) %>% kable(format = "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>% 
  cat(., file = paste0("./GutBrain/MultipleMediation_MIS_", Sys.Date(), ".html"))

parameterestimates(fit, boot.ci.type = "bca.simple", standardized = TRUE) %>% 
  write.table(., file = paste0("./GutBrain/MultipleMediation_MIS_", Sys.Date(), ".csv"), row.names = TRUE)


# Number of cases left in modi of case-deletion
(!is.na(gutbrain.data.sel$iAUC_Ghrelin)) %>% sum() # if case-wise deletion
(!is.na(gutbrain.data.sel$iAUC_GLP1)) %>% sum()
(!is.na(gutbrain.data.sel$iAUC_PYY)) %>% sum()
gutbrain.data.sel %>% drop_na(iAUC_Ghrelin, iAUC_GLP1, iAUC_PYY) %>% nrow() # if list-wise deletion

# save selected raw data for export to spss / PROCESS for validation
allData %>% dplyr::select(ID, Gruppe, one_of(gutbrain.var)) %>%
  drop_na(MIS_sequ)  %>%
  mutate(Gruppe = as.numeric(Gruppe)) %>%
  mutate_at(vars(one_of(gutbrain.var[5:14])), funs(./1000000)) %>%
  mutate_if(is.numeric, round, 4) %>%
  write.csv(., file = paste0("./GutBrain/MediationData_", Sys.Date(), ".csv"))

# other packages can only handle 2 Ms
test <- psych::mediate(MIS_sequ ~ Gruppe + (iAUC_Ghrelin), 
                       data = gutbrain.data.sel,
                       n.iter = 10000, std = TRUE)
test



# alternative model composition
multimed.model.alt <- ' # direct effect
                        MIS_sequ ~ c*Gruppe
                      # mediator
                         iAUC_PYY ~ a*Gruppe
                         MIS_sequ ~ b*iAUC_PYY
                      # indirect effect (a*b)
                         ab := a*b
                       # total effect
                        total := c + (a*b)
                     '


multimed.model <- '
    MIS_sequ ~ b1*iAUC_GLP1 + b2*iAUC_PYY + b3*iAUC_Ghrelin + c*Gruppe
iAUC_GLP1 ~ a1*Gruppe
iAUC_PYY ~ a2*Gruppe
iAUC_Ghrelin ~ a3*Gruppe

indirect1 := a1*b1
indirect2 := a2*b2
indirect3 := a3*b3

total   := c + (a1*b1) + (a2*b2) + (a3*b3)
# iAUC_GLP1 ~~ iAUC_PYY
# iAUC_GLP1 ~~ iAUC_Ghrelin
# iAUC_PYY ~~ iAUC_Ghrelin
'

ghrelin.model<- '
MIS_sequ ~ b1*iAUC_Ghrelin + c*Gruppe
iAUC_Ghrelin ~ a1*Gruppe

indirect1 := a1*b1

total   := c + (a1*b1) 
# iAUC_GLP1 ~~ iAUC_PYY
# iAUC_GLP1 ~~ iAUC_Ghrelin
# iAUC_PYY ~~ iAUC_Ghrelin
'

ghrelinPYY.model <- '
    MIS_sequ ~ b1*iAUC_Ghrelin + b2*iAUC_PYY + c*Gruppe
    iAUC_Ghrelin ~ a1*Gruppe
    iAUC_PYY ~ a2*Gruppe

    indirect1 := a1*b1
    indirect2 := a2*b2

    total   := c + (a1*b1) + (a2*b2)
   
    iAUC_PYY ~~ iAUC_Ghrelin
    '
ghrelinGLP1.model <- '
    MIS_sequ ~ b1*iAUC_Ghrelin + b2*iAUC_GLP1 + c*Gruppe
    iAUC_Ghrelin ~ a1*Gruppe
    iAUC_GLP1 ~ a2*Gruppe

    indirect1 := a1*b1
    indirect2 := a2*b2

    total   := c + (a1*b1) + (a2*b2)

    iAUC_GLP1 ~~ iAUC_Ghrelin
    '





# Mediation: Cognition ~ M(AUC) + weight loss group  ------------------------------------------------------

# MIS SEQU (Y) ~ PYY (M) + weight loss group (X)

# Preperation: Delete cases
gutbrain.data.sel <- gutbrain.data %>% drop_na(AUC_PYY, MIS_sequ)
gutbrain.data.sel$Gruppe <- as.numeric(gutbrain.data.sel$Gruppe)

# Model c', a and b
MIS_sequ_PYY_Mediat_c <- lm(MIS_sequ ~ Gruppe, data = gutbrain.data.sel) 
summary(MIS_sequ_PYY_Mediat_c)
MIS_sequ_PYY_Mediat_a <- lm(AUC_PYY ~ Gruppe, data = gutbrain.data.sel)
summary(MIS_sequ_PYY_Mediat_a)
MIS_sequ_PYY_Mediat_b <- lm(MIS_sequ ~ Gruppe + AUC_PYY, data = gutbrain.data.sel)
summary(MIS_sequ_PYY_Mediat_b)

# Go on? No, a is not significant

# Mediation Analysis
MIS_sequ_PYY_Mediat_res <- mediation::mediate(MIS_sequ_PYY_Mediat_a, 
                                              MIS_sequ_PYY_Mediat_b, 
                                              treat = "Gruppe", mediator = "AUC_PYY",
                                              boot = TRUE, sims = 1000)
summary(MIS_sequ_PYY_Mediat_res)
plot(MIS_sequ_PYY_Mediat_res)


# MIS ITEM (Y) ~ PYY (M) + weight loss group (X)

# Preperation: Delete cases
gutbrain.data.sel <- gutbrain.data %>% drop_na(AUC_PYY, MIS_item)
gutbrain.data.sel$Gruppe <- as.numeric(gutbrain.data.sel$Gruppe)

# Model c', a and b
MIS_item_PYY_Mediat_c <- lm(MIS_item ~ Gruppe, data = gutbrain.data.sel) 
summary(MIS_item_PYY_Mediat_c)
MIS_item_PYY_Mediat_a <- lm(AUC_PYY ~ Gruppe, data = gutbrain.data.sel)
summary(MIS_item_PYY_Mediat_a)
MIS_item_PYY_Mediat_b <- lm(MIS_item ~ Gruppe + AUC_PYY, data = gutbrain.data.sel)
summary(MIS_item_PYY_Mediat_b)

# Go on? No, a is not significant

# Mediation Analysis
MIS_item_PYY_Mediat_res <- mediation::mediate(MIS_item_PYY_Mediat_a, 
                                              MIS_item_PYY_Mediat_b, 
                                              treat = "Gruppe", mediator = "AUC_PYY",
                                              boot = TRUE, sims = 1000)
summary(MIS_item_PYY_Mediat_res)
plot(MIS_item_PYY_Mediat_res)


# MIS (Y) ~ PYY (M) + weight loss group (X)

# Preperation: Delete cases
gutbrain.data.sel <- gutbrain.data %>% drop_na(AUC_PYY, MIS_ov)
gutbrain.data.sel$Gruppe <- as.numeric(gutbrain.data.sel$Gruppe)

# Model c', a and b
MIS_ov_PYY_Mediat_c <- lm(MIS_ov ~ Gruppe, data = gutbrain.data.sel) 
summary(MIS_ov_PYY_Mediat_c)
MIS_ov_PYY_Mediat_a <- lm(AUC_PYY ~ Gruppe, data = gutbrain.data.sel)
summary(MIS_ov_PYY_Mediat_a)
MIS_ov_PYY_Mediat_b <- lm(MIS_ov ~ Gruppe + AUC_PYY, data = gutbrain.data.sel)
summary(MIS_ov_PYY_Mediat_b)

# Go on? No, a is not significant

# Mediation Analysis
MIS_ov_PYY_Mediat_res <- mediation::mediate(MIS_ov_PYY_Mediat_a, 
                                            MIS_ov_PYY_Mediat_b, 
                                            treat = "Gruppe", mediator = "AUC_PYY",
                                            boot = TRUE, sims = 1000)
summary(MIS_ov_PYY_Mediat_res)
plot(MIS_ov_PYY_Mediat_res)


# MIS_sequ (Y) ~ GLP1 (M) + weight loss group (X)

# Preperation: Delete cases
gutbrain.data.sel <- gutbrain.data %>% drop_na(AUC_GLP1, MIS_sequ)
gutbrain.data.sel$Gruppe <- as.numeric(gutbrain.data.sel$Gruppe)

# Model c', a and b
MIS_sequ_GLP1_Mediat_c <- lm(MIS_sequ ~ Gruppe, data = gutbrain.data.sel) 
summary(MIS_sequ_GLP1_Mediat_c)
MIS_sequ_GLP1_Mediat_a <- lm(AUC_GLP1 ~ Gruppe, data = gutbrain.data.sel)
summary(MIS_sequ_GLP1_Mediat_a)
MIS_sequ_GLP1_Mediat_b <- lm(MIS_sequ ~ Gruppe + AUC_GLP1, data = gutbrain.data.sel)
summary(MIS_sequ_GLP1_Mediat_b)

# X alone explains equal variance in presence of M (Step 3). No Mediation effetcs. 

# Mediation Analysis
MIS_sequ_GLP1_Mediat_res <- mediation::mediate(MIS_sequ_GLP1_Mediat_a, 
                                               MIS_sequ_GLP1_Mediat_b, 
                                               treat = "Gruppe", mediator = "AUC_GLP1",
                                               boot = TRUE, sims = 1000)
summary(MIS_sequ_GLP1_Mediat_res)
plot(MIS_sequ_GLP1_Mediat_res)


# MIS_item (Y) ~ GLP1 (M) + weight loss group (X)

# Preperation: Delete cases
gutbrain.data.sel <- gutbrain.data %>% drop_na(AUC_GLP1, MIS_item)
gutbrain.data.sel$Gruppe <- as.numeric(gutbrain.data.sel$Gruppe)

# Model c', a and b
MIS_item_GLP1_Mediat_c <- lm(MIS_item ~ Gruppe, data = gutbrain.data.sel) 
summary(MIS_item_GLP1_Mediat_c)
MIS_item_GLP1_Mediat_a <- lm(AUC_GLP1 ~ Gruppe, data = gutbrain.data.sel)
summary(MIS_item_GLP1_Mediat_a)
MIS_item_GLP1_Mediat_b <- lm(MIS_item ~ Gruppe + AUC_GLP1, data = gutbrain.data.sel)
summary(MIS_item_GLP1_Mediat_b)

# Go on? No, a is not significant

# Mediation Analysis
MIS_item_GLP1_Mediat_res <- mediation::mediate(MIS_item_GLP1_Mediat_a, 
                                               MIS_item_GLP1_Mediat_b, 
                                               treat = "Gruppe", mediator = "AUC_GLP1",
                                               boot = TRUE, sims = 1000)
summary(MIS_item_GLP1_Mediat_res)
plot(MIS_item_GLP1_Mediat_res)
# not significant


# MIS (Y) ~ GLP1 (M) + weight loss group (X)

# Preperation: Delete cases
gutbrain.data.sel <- gutbrain.data %>% drop_na(AUC_GLP1, MIS_ov)
gutbrain.data.sel$Gruppe <- as.numeric(gutbrain.data.sel$Gruppe)

# Model c', a and b
MIS_ov_GLP1_Mediat_c <- lm(MIS_ov ~ Gruppe, data = gutbrain.data.sel) 
summary(MIS_ov_GLP1_Mediat_c)
MIS_ov_GLP1_Mediat_a <- lm(AUC_GLP1 ~ Gruppe, data = gutbrain.data.sel)
summary(MIS_ov_GLP1_Mediat_a)
MIS_ov_GLP1_Mediat_b <- lm(MIS_ov ~ Gruppe + AUC_GLP1, data = gutbrain.data.sel)
summary(MIS_ov_GLP1_Mediat_b)

# X alone explains equal variance in presence of M (Step 3). No Mediation effetcs. 

# Mediation Analysis
MIS_ov_GLP1_Mediat_res <- mediation::mediate(MIS_ov_GLP1_Mediat_a, 
                                             MIS_ov_GLP1_Mediat_b, 
                                             treat = "Gruppe", mediator = "AUC_GLP1",
                                             boot = TRUE, sims = 1000)
summary(MIS_ov_GLP1_Mediat_res)
plot(MIS_ov_GLP1_Mediat_res)
# not significant


# MIS_sequ (Y) ~ Ghrelin (M) + weight loss group (X)

# Preperation: Delete cases
gutbrain.data.sel <- gutbrain.data %>% drop_na(AUC_Ghrelin, MIS_sequ)
gutbrain.data.sel$Gruppe <- as.numeric(gutbrain.data.sel$Gruppe)

# Model c', a and b
MIS_sequ_Ghrelin_Mediat_c <- lm(MIS_sequ ~ Gruppe, data = gutbrain.data.sel) 
summary(MIS_sequ_Ghrelin_Mediat_c)
MIS_sequ_Ghrelin_Mediat_a <- lm(AUC_Ghrelin ~ Gruppe, data = gutbrain.data.sel)
summary(MIS_sequ_Ghrelin_Mediat_a)
MIS_sequ_Ghrelin_Mediat_b <- lm(MIS_sequ ~ Gruppe + AUC_Ghrelin, data = gutbrain.data.sel)
summary(MIS_sequ_Ghrelin_Mediat_b)

# Mediation Analysis is valid

# Mediation Analysis
MIS_sequ_Ghrelin_Mediat_res <- mediation::mediate(MIS_sequ_Ghrelin_Mediat_a, 
                                                  MIS_sequ_Ghrelin_Mediat_b, 
                                                  treat = "Gruppe", mediator = "AUC_Ghrelin",
                                                  #boot.ci.type = "bca",
                                                  boot = TRUE, sims = 5000)
summary(MIS_sequ_Ghrelin_Mediat_res)
plot(MIS_sequ_Ghrelin_Mediat_res)

MIS_sequ_Ghrelin_Mediat_res_std <- psych::mediate(MIS_sequ ~ Gruppe + (AUC_Ghrelin), 
                                                  data = gutbrain.data.sel,
                                                  n.iter = 10000, std = FALSE)
MIS_sequ_Ghrelin_Mediat_res_std

# not signifikant :/


# MIS_item (Y) ~ Ghrelin (M) + weight loss group (X)

# Preperation: Delete cases
gutbrain.data.sel <- gutbrain.data %>% drop_na(AUC_Ghrelin, MIS_item)
gutbrain.data.sel$Gruppe <- as.numeric(gutbrain.data.sel$Gruppe)

# Model c', a and b
MIS_item_Ghrelin_Mediat_c <- lm(MIS_item ~ Gruppe, data = gutbrain.data.sel) 
summary(MIS_item_Ghrelin_Mediat_c)
MIS_item_Ghrelin_Mediat_a <- lm(AUC_Ghrelin ~ Gruppe, data = gutbrain.data.sel)
summary(MIS_item_Ghrelin_Mediat_a)
MIS_item_Ghrelin_Mediat_b <- lm(MIS_item ~ Gruppe + AUC_Ghrelin, data = gutbrain.data.sel)
summary(MIS_item_Ghrelin_Mediat_b)

# Mediation Analysis is valid (a shows only trend)

# Mediation Analysis
MIS_item_Ghrelin_Mediat_res <- mediation::mediate(MIS_item_Ghrelin_Mediat_a, 
                                                  MIS_item_Ghrelin_Mediat_b, 
                                                  treat = "Gruppe", mediator = "AUC_Ghrelin",
                                                  boot = TRUE, sims = 5000)
summary(MIS_item_Ghrelin_Mediat_res)
plot(MIS_item_Ghrelin_Mediat_res)

MIS_item_Ghrelin_Mediat_res_std <- psych::mediate(MIS_item ~ Gruppe + (AUC_Ghrelin), 
                                                  data = gutbrain.data.sel,
                                                  n.iter = 10000, std = FALSE)
MIS_item_Ghrelin_Mediat_res_std
# not signifikant :/


# MIS (Y) ~ Ghrelin (M) + weight loss group (X)

# Preperation: Delete cases
gutbrain.data.sel <- gutbrain.data %>% drop_na(AUC_Ghrelin, MIS_ov)
gutbrain.data.sel$Gruppe <- as.numeric(gutbrain.data.sel$Gruppe)

# Model c', a and b
MIS_ov_Ghrelin_Mediat_c <- lm(MIS_ov ~ Gruppe, data = gutbrain.data.sel) 
summary(MIS_ov_Ghrelin_Mediat_c)
MIS_ov_Ghrelin_Mediat_a <- lm(AUC_Ghrelin ~ Gruppe, data = gutbrain.data.sel)
summary(MIS_ov_Ghrelin_Mediat_a)
MIS_ov_Ghrelin_Mediat_b <- lm(MIS_ov ~ Gruppe + AUC_Ghrelin, data = gutbrain.data.sel)
summary(MIS_ov_Ghrelin_Mediat_b)

# Mediation Analysis is valid

# Mediation Analysis
MIS_ov_Ghrelin_Mediat_res <- mediation::mediate(MIS_ov_Ghrelin_Mediat_a, 
                                                MIS_ov_Ghrelin_Mediat_b, 
                                                treat = "Gruppe", mediator = "AUC_Ghrelin",
                                                boot = TRUE, sims = 5000)
summary(MIS_ov_Ghrelin_Mediat_res)
plot(MIS_ov_Ghrelin_Mediat_res)

MIS_ov_Ghrelin_Mediat_res_std <- psych::mediate(MIS_ov ~ Gruppe + (AUC_Ghrelin), 
                                                data = gutbrain.data.sel,
                                                n.iter = 10000, std = TRUE)
MIS_ov_Ghrelin_Mediat_res_std
# not signifikant!



summary(MIS_ov_Ghrelin_Mediat_res)
summary.mediate(MIS_ov_Ghrelin_Mediat_res)
plot(MIS_ov_Ghrelin_Mediat_res)



# Mediation: Cognition ~ M(IR) + Weightlos-group ---------------------------------------------
library(QuantPsyc)

# MIS (Y) ~ IR (M) + weight loss group (X)

# Preperation
gutbrain.data.sel <- allData %>% 
  dplyr::select(ID, Gruppe, IR = HOMA.IR_akt, MIS = MIS_ov, IS = MIS_ov_noCalc) %>%
  drop_na(MIS) %>%
  mutate(Gruppe = as.numeric(Gruppe))

# Model c', a and b
MIS_IR_Mediat_c <- lm(MIS ~ Gruppe, data = gutbrain.data.sel) 
summary(MIS_IR_Mediat_c)
MIS_IR_Mediat_a <- lm(IR ~ Gruppe, data = gutbrain.data.sel)
summary(MIS_IR_Mediat_a)
MIS_IR_Mediat_b <- lm(MIS ~ Gruppe + IR, data = gutbrain.data.sel)
summary(MIS_IR_Mediat_b)
cor.test(gutbrain.data.sel$MIS, gutbrain.data.sel$IR, method = "spearman")
# c',a and b are valid -> progress with mediation analysis

# Mediation Analysis
MIS_IR_Mediat_res <- mediation::mediate(MIS_IR_Mediat_a, MIS_IR_Mediat_b,
                                        treat = "Gruppe", mediator = "IR",
                                        boot = TRUE, sims = 5000)
summary(MIS_IR_Mediat_res)
plot(MIS_IR_Mediat_res)

MIS_IR_Mediat_res_std <- psych::mediate(MIS ~ Gruppe + (IR), 
                                        data = gutbrain.data.sel,
                                        n.iter = 10000, std = T) # change std=F/T
MIS_IR_Mediat_res_std
mediate.diagram(MIS_IR_Mediat_res_std)


# IS (Y) ~ IR (M) + weight loss group (X)

# Preperation
gutbrain.data.sel <- allData %>% 
  dplyr::select(ID, Gruppe, IR = HOMA.IR_akt, MIS = MIS_ov, IS = MIS_ov_noCalc) %>%
  drop_na(MIS) %>%
  mutate(Gruppe = as.numeric(Gruppe))

# Model c', a and b
IS_IR_Mediat_c <- lm(IS ~ Gruppe, data = gutbrain.data.sel)  #unstandardized values
IS_IR_Mediat_c <- lm(scale(IS) ~ scale(Gruppe), data = gutbrain.data.sel) #standardizes values
summary(IS_IR_Mediat_c)
IS_IR_Mediat_a <- lm(IR ~ Gruppe, data = gutbrain.data.sel)
summary(IS_IR_Mediat_a)
IS_IR_Mediat_b <- lm(IS ~ Gruppe + IR, data = gutbrain.data.sel)
summary(IS_IR_Mediat_b)
# c',a and b are valid -> progress with mediation analysis

# Mediation Analysis
IS_IR_Mediat_res <- mediation::mediate(IS_IR_Mediat_a, IS_IR_Mediat_b,
                                        treat = "Gruppe", mediator = "IR",
                                        boot = TRUE, sims = 5000)
summary(IS_IR_Mediat_res)
plot(IS_IR_Mediat_res)

IS_IR_Mediat_res_std <- psych::mediate(IS ~ Gruppe + (IR), 
                                        data = gutbrain.data.sel,
                                        n.iter = 10000, std = FALSE) #change std=F/T
IS_IR_Mediat_res_std
mediate.diagram(IS_IR_Mediat_res_std)



# Logistic Regression: Group ~ Cognition * Hormone-iAUC ---------------------------------------------

MIS_PYY_pred <- glm(Gruppe ~ MIS_ov * iAUC_PYY, data = allData, family = "binomial") 
summary(MIS_PYY_pred)

# does that make sense?!?!




# GLM: Cognition ~ Hormone * Group -----------------------------------------------------------

# Long Data with all Datasets (including incomplete). Empty cells are dropped 
gutbrainData_longit <- allData %>%
  dplyr::select(ID, Gruppe, GLP1_0:Leptin_120, MIS_ov, MIS_item, MIS_sequ, MIS_math) %>%
  gather(Hormone, value, GLP1_0:Leptin_120) %>%
  separate(Hormone, c("Hormone", "t")) %>%
  mutate(t = as.numeric(t),
         t = factor(t, levels = as.character(c(0,15,30,60,120)), labels = c("Baseline", "15 min", "30 min", "60 min", "120 min"))) %>%
  dplyr::filter(Hormone == "PYY") %>%
  drop_na(value, MIS_sequ)



Hormone_Model_BL <- lme(value ~ 1,
                       random = ~1|ID/t,
                        method = "ML",
                        data = gutbrainData_longit)

Hormone_Model_t <- lme(value ~ t,
                        random = ~1|ID/t,
                        method = "ML",
                        data = gutbrainData_longit)

Hormone_Model_tGruppe <- lme(value ~ t + Gruppe,
                             random = ~1|ID/t,
                             method = "ML",
                             data = gutbrainData_longit)

Hormone_Model_full <- lme(value ~ t * Gruppe ,
                          random = ~1|ID/t,
                          method = "ML",
                          data = gutbrainData_longit)

Hormone_Model_full1 <- lme(value ~ t * Gruppe + MIS_ov,
                          random = ~1|ID/t,
                          method = "ML",
                          data = gutbrainData_longit)

Hormone_Model_full2 <- lme(value ~ t * Gruppe * MIS_ov,
                           random = ~1|ID/t,
                           method = "ML",
                           data = gutbrainData_longit)

anova(Hormone_Model_BL, Hormone_Model_t, Hormone_Model_tGruppe, Hormone_Model_full, Hormone_Model_full1,Hormone_Model_full2)

summary(Hormone_Model_full)

# post hoc tests
Hormone_Model_posthoc <- glht(Hormone_Model_full, linfct = mcp(t = "Tukey")) 

summary(Hormone_Model_posthoc)
confint(Hormone_Model_posthoc)


emm = emmeans(Hormone_Model_full, ~ t * Gruppe)
pairs(emm)

lsmeans::lsmeans(Hormone_Model_full, pairwise ~ t |  Gruppe)
lsmeans::lsmeans(Hormone_Model_full, pairwise ~ Gruppe |  t)

leastsquare <- lsmeans::lsmeans(Hormone_Model_full,
                                pairwise ~ Gruppe:t,
                                adjust = "tukey")
leastsquare$contrasts
cld(leastsquare)



# Testing Corner ----------------------------------------------------------

test <- allData %>% 
  dplyr::select(ID, Gruppe, "iAUC_GLP1","iAUC_PYY", "iAUC_Ghrelin", "iAUC_Insulin", "iAUC_Leptin","AUC_GLP1","AUC_PYY", "AUC_Ghrelin", "AUC_Insulin", "AUC_Leptin", IL6_akt, MIS_ov) %>% 
  mutate_at(vars(-ID, -Gruppe), funs(ifelse(outliers(., int = TRUE) == TRUE, ., NA )))

ggplot(data = test, 
       aes(x = iAUC_GLP1,
           y = IL6_akt)) + 
  geom_point(aes(color = Gruppe)) + 
  geom_smooth(method = "lm")

cor.test(test$iAUC_GLP1, test$IL6_akt, method = "spearman")
test.model <- lm(IL6_akt ~ iAUC_GLP1 * Gruppe, data = test)
Anova(test.model, type = "III") 


ggplot(data = test, 
       aes(x = iAUC_PYY,
           y = IL6_akt)) + 
  geom_point() + 
  geom_smooth(method = "lm")
cor.test(test$iAUC_PYY, test$IL6_akt, method = "spearman")
test.model <- lm(IL6_akt ~ iAUC_PYY * Gruppe, data = test)
Anova(test.model, type = "III") 


ggplot(data = test, 
       aes(x = iAUC_Ghrelin,
           y = IL6_akt)) + 
  geom_point(aes(color = Gruppe)) + 
  geom_smooth(method = "lm")
cor.test(test$iAUC_Ghrelin, test$IL6_akt, method = "spearman")
test.model <- lm(IL6_akt ~ iAUC_Ghrelin * Gruppe, data = test)
Anova(test.model, type = "III") 

ggplot(data = test, 
       aes(x = MIS_ov,
           y = IL6_akt)) + 
  geom_point(aes(color = Gruppe)) + 
  geom_smooth(method = "lm")
cor.test(test$MIS_ov, test$IL6_akt, method = "spearman")
test.model <- lm(MIS_ov ~ iAUC_Ghrelin * Gruppe, data = test)
Anova(test.model, type = "III") 

