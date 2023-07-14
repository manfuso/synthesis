# Install latest version of the ORKG R package from Git
install.packages('remotes')

# Connect with ORKG
library(orkg)
orkg <- ORKG(host='https://incubating.orkg.org/')

#retrieve significance testing output dataset for Ricardo's fig. 4a (aphid incidence) (n=42) - here we want the p-value (need to run this code to create my test dataframe and Ricardo's dataframe, which is the one we will actually use for the meta-analysis)
PerezAlvarez2018Fig4aSigTest <- orkg$resources$by_id('R569582')$as_dataframe()
PerezAlvarez2018Fig4aSigTest 
#retrieve significance testing output dataset for Ricardo's fig. 4b (flea beetle abundance) (n=43) - here we want the p-value
PerezAlvarez2018Fig4bSigTest <- orkg$resources$by_id('R570502')$as_dataframe()
PerezAlvarez2018Fig4bSigTest
#retrieve significance testing output dataset for Ricardo's fig. 4c (incidence of lepidopteran larvae) (n=41) - here we want the p-value
PerezAlvarez2018Fig4cSigTest <- orkg$resources$by_id('R571405')$as_dataframe()
PerezAlvarez2018Fig4cSigTest
#retrieve ANOVA output dataset for Daniel's fig. 4 (Bactrocera oleae abundance) (n=25) - here we want the Pr..Chi value
Paredes2022Fig4Anova <- orkg$resources$by_id('R552440')$as_dataframe()
Paredes2022Fig4Anova

#create a master dataframe (named master_df) for the mini-metaanalysis based on ChatGPT instructions - this is just an exercise for fun to test ChatGPT :) We will use Ricardo's dataframe for the actual meta-analysis (see below)
master_df <- data.frame (
  p.value = numeric(),
  statistic.value = numeric(),
  species = character (),
  direction = numeric(),
  study = character (),
  stats.test.type = character (),
  sample.size = integer ()

)

#bring in significance values
value4aSigTest<-PerezAlvarez2018Fig4aSigTest[2,1]
master_df[1,1]<- value4aSigTest
value4bSigTest<- PerezAlvarez2018Fig4bSigTest [2,1]
master_df[2,1]<- value4bSigTest
value4cSigTest<- PerezAlvarez2018Fig4cSigTest [2,1]
master_df[3,1]<- value4cSigTest
valueFig4Anova<- Paredes2022Fig4Anova [2,1]
master_df[4,1]<- valueFig4Anova

#bring in statistics values
value4aStatVal<-PerezAlvarez2018Fig4aSigTest[2,2]
master_df[1,2]<- value4aStatVal
value4bStatVal<-PerezAlvarez2018Fig4bSigTest[2,2]
master_df[2,2]<- value4bStatVal
value4cStatVal<-PerezAlvarez2018Fig4cSigTest[2,2]
master_df[3,2]<- value4cStatVal
valueFig4StatVal<-NA
master_df[4,2]<- valueFig4StatVal

#bring in species information
species_aphids<-"aphids"
master_df[1,3]<- species_aphids
species_fleabeetles<-"flea beetles"
master_df[2,3]<- species_fleabeetles
species_lepidopterans<-"lepidopterans"
master_df[3,3]<- species_lepidopterans
species_Bactroceraoleae<-"Bactrocera oleae"
master_df[4,3]<- species_Bactroceraoleae

#bring in information on the direction of the relationship (i.e., 1 = positive relationship and -1 = negative relationship)
master_df[1,4]<- 1
master_df[2,4]<- 1
master_df[3,4]<- -1
master_df[4,4]<- -1

#bring in information on which study we are referring to
master_df[1,5]<- "Perez Alvarez"
master_df[2,5]<- "Perez Alvarez"
master_df[3,5]<- "Perez Alvarez"
master_df[4,5]<- "Paredes"

#bring in information on which stats test the p-value is associated with
master_df[1,6]<- "t-test"
master_df[2,6]<- "t-test"
master_df[3,6]<- "t-test"
master_df[4,6]<- "chi-square"

#bring in sample size information
master_df[1,7]<- 42
master_df[2,7]<- 43
master_df[3,7]<- 41
master_df[4,7]<- 25


#####CREATING A JOIN DATASET #############################
#how to merge dataframes?: Ricardo's method - use this dataframe for the meta-analysis


#create and merge dataframes

Perez_Fig4a<- as.data.frame(PerezAlvarez2018Fig4aSigTest)
species_Fig4a <- c("aphids", "aphids", "aphids")
Perez_Fig4a["species"] <- species_Fig4a 

Perez_Fig4b<- as.data.frame(PerezAlvarez2018Fig4bSigTest)
species_Fig4b <- c("flea beetles", "flea beetles", "flea beetles")
Perez_Fig4b["species"] <- species_Fig4b 

Perez_Fig4c<- as.data.frame(PerezAlvarez2018Fig4cSigTest)
species_Fig4c <- c("Lepidoptera", "Lepidoptera", "Lepidoptera")
Perez_Fig4c["species"] <- species_Fig4c

Perez_Fig4_Total <-rbind(Perez_Fig4a, Perez_Fig4b,Perez_Fig4c)
#remove unnecessary rows from dataframe
Perez_Fig4_Total2 <-Perez_Fig4_Total[-c(1,3, 4, 6,7,9), ]  
#add  column with direction (for the correlation)
direction_perez <- c(1, 1, -1)
Perez_Fig4_Total2["Direction"] <- direction_perez
#making sure row names are accurate
rownames(Perez_Fig4_Total2) <- c("mead_250","mead_250 (2)","mead_1000")
#add column to the entire dataframe, so we could merge this with other dataframes
study_perez <- c("Perez-Alvarez2018", "Perez-Alvarez2018", "Perez-Alvarez2018")
Perez_Fig4_Total2["Study"] <- study_perez
# removing unnecesary columns from the dataframe
names(Perez_Fig4_Total2)
Perez_total = subset(Perez_Fig4_Total2, select = -c(DF,Std.Error,Value))
#add column with type of test and sample size
stats_test_perez <- c("t-value", "t-value", "t-value")
SampleSize_perez <- c(42, 43, 41)
Perez_total["stats_test"] <- stats_test_perez
Perez_total["Sample size"] <- SampleSize_perez 
#change column names
colnames(Perez_total ) <- c('p.value','Statistic.value','species',"Direction", "Study", "stats_test_Type","Sample_size")

#Now we are going to merge Perez_total with the dataframe from Paredes study
#select relevant columns from Paredes
names (Paredes2022Fig4Anova)

Paredes_total = subset(Paredes2022Fig4Anova, select = c(Pr..Chi.))
colnames(Paredes_total) <- c('p-value')
Paredes_total

#create dataframe from paredes with the same column names than Perez_total
Paredes_full<- data.frame (
  p.value = 0.035,
  Statistic.value = NA,
  species= "Bactrocera oleae", 
  Direction= -1, 
  Study= "Paredes2022",
  stats_test_Type="chisqquared",
  Sample_size= 25)
#change row name
rownames(Paredes_full) <- c("shdi")

#Join Perez and paredes
MetaAnalysis_Data <-rbind(Perez_total, Paredes_full)

#This is the final dataframe that include all data
MetaAnalysis_Data

### ### META-ANALYSYS CODE ##################

#First, we need to calculate coefficients of correlation



#load packages-Don't think we need all this packages, but i don't remember
#what particular functions belong to what package 

library (meta)
library (esc)
# library(tidyverse)
# library(metafor)
# library (ggplot2)
# library (MuMIn)
# library (PerformanceAnalytics)
library (dplyr)
# library(glmulti)
# library(emmeans)
# library (devtools)
# library(remotes)
# library(gt)
# library(orchaRd)
# library (metacor)


#estimating coefficients of correlation
names (MetaAnalysis_Data)
#extracting values when there is p-values and t-values
#remove row with Paredes data because statistics value is based on a chisquare
MetaAnalysis_subsetPerez <-MetaAnalysis_Data[-c(4), ] 


#now we can calculate correlations (es column - these values are also called the r values) based on P-values and Sample size 

EffectSizes_Perez<-effect_sizes(MetaAnalysis_subsetPerez, p = p.value, totaln = Sample_size, study = Study, fun = "esc_t", es.type="r")
EffectSizes_Perez

# now we calculate r values for paredes 

MetaAnalysis_subsetParedes <-MetaAnalysis_Data[c(4), ] 
names(MetaAnalysis_subsetParedes)

#extracting data when there is chi-square values
EffectSizes_Paredes<-effect_sizes(MetaAnalysis_subsetParedes, p = p.value, totaln = Sample_size,
                    study = Study, fun = "chisq", es.type="r")
EffectSizes_Paredes
# removing unnecesary columns from the dataframe
EffectSizes_Paredes2 = subset(EffectSizes_Paredes, select = -c(fishers.z,ci.lo.z,ci.hi.z))

#merge datasets
names (EffectSizes_Perez)
names (EffectSizes_Paredes)
CoefficientsCorrelation_Data <-rbind(EffectSizes_Perez, EffectSizes_Paredes2)

#all r values are positive, so we need to create a variable to indicate the direction
#add column with direction and an "es2" column, which is the coefficient of correlation values with the direction attached
direction_metaanalysis <- c(1, 1, -1, -1)
CoefficientsCorrelation_Data["Direction"] <- direction_metaanalysis
names(CoefficientsCorrelation_Data)
CoefficientsCorrelation_Data2 <- mutate (CoefficientsCorrelation_Data, es2=es*Direction)

# calculate overall effect size 

m.cor <- metacor(cor = es2, 
                 n = sample.size,
                 studlab = study,
                 data = CoefficientsCorrelation_Data2,
                 method.tau = "REML",
                 hakn = TRUE,
                 title = "Mini Meta-analysis")
summary(m.cor)
#interpreting the summary output: We want to focus on the output of the "Random effects model". This is the more conservative model (compared to the fixed effects model) as it considers studies as a random effect (i.e., we expect more variability between observations from different studies than observations within the same study). The COR value is the overall effect size (overall coefficient of correlation). Then you have confidence interval values; when these overlap zero we have a non-significant effect, which here is also confirmed by the p-value. This is the most basic way to run a meta-analysis. If we were working with a real data set we would also, weight the estimated values by sample size or variability, we would also standardize the coefficients of correlation. However, this is not really needed here because this is just a proof-of-concept synthesis to demonstrate how the ORKG can support/facilitate/streamline synthesis research. We are not expecting a meaningful ecological interpretation from the meta-analysis output.


forest(m.cor)
# overall, analysis indicate there is no significant
#correlation between pest abundance/incidence and landscape complexity

