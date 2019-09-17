
# set the working directory
getwd()

setwd("/gabriellechristinemartin/Desktop/AIID Guns Study 1ab data")

# clear the environment
rm(list=ls())

#1. IMPORTING, VIEWING, AND SELECTING THE DATA ----

library(haven)
library(dplyr)

#na.strings = "." turns all . from a spss or csv dataset to NA so that r can use the variable.
#stringsAsFactors = False imports strings as characters rather than factors (which is true by default for read.csv)
data <- data.frame(read_sav("/Users/gabriellechristinemartin/Desktop/AIID Guns Study 1ab data/Study 1b data/UFGunFall2018.11.13.2018.cleaned2.sav"))

class(data)

#provides the structure of the data: number observations, number variables, variable names, variable type/classes 
#plus a preview of the data
glimpse(data)

#pulls out all rows for only the columns in data that we need to examine the primary analyses  
data2 <- data %>%
  select(Csupp=Csupport_1, C1=Csafety_1, C2=Csafety_2, C3=Csafety_3, C4=Csafety_4, Code1, Code2, 
         R1W=race_1, R2B=race_2, R4A=race_3, R5N=race_4, R3H=ethnicity, RaceCNR = race_999, RacePSD = race_1000,
         Why1n, Why2n, Why3n, Politic=pol.ID, sex=gender, age, Educ=education, FSS=uni.affil) %>%
  mutate(sex = as.factor(sex)) %>%
  mutate(Educ = as.factor(Educ)) %>%
  mutate(FSS = as.factor(FSS))
    
#confirming that data2 is a data.frame
class(data2)

head(data2)

# select only participants who repsonded to the primary DV item: support for campus carry
data3 <- data2 %>% 
  filter(Csupp > -99) 

# set factor levels and report demographics
table(data3$sex)
levels(data3$sex) <- c("Male", "Female", "Trans", "Non-binary", "Prefer to self-describe")
summary(data3$sex)

table(data3$Educ)
levels(data3$Educ) <- c("No high school", "High school", "High school graduate", "Some post high school training other than college",
                        "Some college", "Associates degree", "College/University degree", "More than a four year college/university degree")
summary(data3$Educ)

table(data3$FSS)
levels(data3$FSS) <- c("Undergraduate student", "Graduate or professional student", "Staff", "Faculty")

summary(data3$age)
sd(data3$age, na.rm = TRUE)

table(data3$R1W)
table(data3$R2B)
table(data3$R4A)
table(data3$R5N)
table(data3$RaceCNR)
table(data3$RacePSD)
table(data3$R3H)


table(data3$Politic)
summary(data3$Politic)
sd(data3$Politic, na.rm = TRUE)

#2. CREATING THE PREDICTORS AND CONTRASTS PRIOR TO ANALYSIS ----

#both of the below methods of computing the blackvnot var produce 659 Black people in the full sample

#add a new var with Black = .5 and Not-Black = -.5: this is just a transformation of the Black var
data3 <- data3%>%
  mutate(blackvnot = case_when(
    R2B == 1 ~ .5,
    R2B == 0 ~ -.5))

#add a new var with Black = .5 and Not-Black = -.5
data3 <- data3%>%
  mutate(blackvnot2 = case_when(
    R2B == 1 ~ .5,
    R1W == 1 ~ -.5,
    R3H == 1 ~ -.5,
    R4A == 1 ~ -.5,
    R5N == 1 ~ -.5))

table(data3$blackvnot)
table(data3$blackvnot2)

# Use Code1 and Code2 in the data for the gun ownership group contrasts

#checking on the gun ownership categories; neither of the total ns in the below match the 12903 starting n 
table(data3$Code1)#non-owners (-2 = 9107) vs. owners (1 = 2755)
table (data3$Code2) # non-protection (-1 = 414) vs. protection owners (1 = 2341) vs. non-owners (0 = 9107) 

# Codes 1 and 2 were pre-existing in the data but we need to document how we made them
# I ausme they were constructed using the Reason2 var in the data
# I assume that looks something like the below, where we first create reason and then the codes

#creating the gun owner categories before re-coding into contrast codes: protection (2, n = 324), non-protection (1, n = 70), non-owner (0, n = 1377)

data3 <- data3%>%
  mutate(Reason = case_when(
    #if no for protection of self and other,but yes recreation, then non-protection
    Why1n == 0 & Why2n == 0 & Why3n == 1 ~ 1,
    #if yes for protection of self OR others, then protection (dis-regarding recreation)
    Why1n == 1 | Why2n == 1 ~ 2,
    #if no for the two protection reasons and the recreation, then non-owner 
    Why1n == 0 & Why2n == 0 & Why3n == 0 ~ 0))

head(data3, n = 30)

table(data3$Reason)
table(data3$Why1n)
table(data3$Why2n)
table(data3$Why3n)


#create Code1: comparing non-protection and protection-owners to non owners 
#Non-owners = -2; Recreation owners = 1; Protection owners = 1
data3 <- data3%>%
  mutate(Code1 = case_when(
    Reason == 0 ~ -2,
    Reason == 1 ~ 1,
    Reason == 2 ~ 1 ))

#create Code2:comparing protection and non-protection owners, excluding non-owners
#Non-owners = 0; Recreations owners = -1; Protection owners =1
data3 <- data3%>%
  mutate(Code2 = case_when(
    Reason == 0 ~ 0,
    Reason == 1 ~ -1,
    Reason == 2 ~ 1))

table(data3$Code1)
table(data3$Code2)

head(data3, n = 30)


#3. CREATING A 15% RANDOM SAMPLE FOR THE EXPLORATORY ANALYSES ----

# Set random number generator seed value for reproducibility
set.seed(1234)

#randomly sample 15% of the data for these exploratory analyses
# should think about shuffling the dataset first 
data3 <- data3 %>%
  sample_frac(.15)

glimpse(data3)

# export csv with the random sample so we can come back to exactly that sample

write.csv(data3,"/Users/gabriellechristinemartin/Desktop/AIID Guns Study 1ab data/Study 1b data/UFGunJamesVersion_15pctsample.csv", row.names = TRUE)

# read in the 15% sample data to work with

data4=read.csv("/Users/gabriellechristinemartin/Desktop/AIID Guns Study 1ab data/Study 1b data/UFGunJamesVersion_15pctsample.csv")

glimpse(data4)

# random sample stats for race and gun ownership group

table(data4$blackvnot)
table(data4$Code1)
table(data4$Code2)

# the numbers produced by the below code explains why we get "NA" for the interactions in Step 2 below
# for the analyses with gun ownership when using the alread-coded Code1 and Code2

# NA more generally means that the coefficient is not estimable. This can happen due to exact collinearity,
# But, it can also happen due to not having enough observations to estimate the relevant parameters (e.g. if 𝑝>𝑛).
# If your predictors are categorical and you're adding interaction terms, 
# an NA can also mean that there are no observations with that combination of levels of the factors.

data5 <- data4 %>%
  filter(blackvnot == .5) 

table(data5$blackvnot) # n = 102 Black
table(data5$Code1) # we only have 5 Black gun owners in the 15% sample; versus 93 non-owners
table(data5$Code2) # we have NO Black recreation owners; all 5 Black gun owners are protection-owners


#4. CREATING THE INTERACTION TERMS PRIOR TO ANALYSIS ----

#reverse-code the Politic var and add it back to data3. Making it so that conservative is higher.
data4 <- data4%>%
  mutate(Politic_R = 
           8 - Politic)

#checking to make sure Politic_R is numeric. Returns TRUE.
is.numeric(data4$Politic_R)

# creating the function to center with 'scale()' before passing to mutate() below
center_scale <- function(x) {
  scale(x, scale = FALSE)
}

#mean-centering the Politic_R and adding it back
data4 <- data4%>%
  mutate(Pol_C = 
           as.vector(center_scale(Politic_R)))

#shows that the mean of Pol_C is zero because it is mean centered
summary(data4$Pol_C) 

#creating the interaction terms between the three race contrast (blackvnot) and Code1, Code2, and Pol_C

data4 <- data4%>%
  mutate(BO_Code1 =
           blackvnot * Code1) %>%
  mutate(BO_Code2 =
           blackvnot * Code2) %>%
  mutate(BO_PO = 
           blackvnot * Pol_C)

#make sure the values and classes look right
glimpse(data4)


#5. HIERARCHICAL REGRESSION MODELS: BLACK V NOT x POLITICAL ORIENT ----

library(psych)

#Support models ----

# MODEL 1: create the main effects model predicting support from blackvnot and political orientation, Pol_C
support_model1bnp= lm(Csupp~blackvnot+Pol_C, data = data4)

#get the coefficients and summary stats 
summary(support_model1bnp)

# to get partial rs with confidence intervals
# run the pcor.test code below twice; switching the order of the second and third columns
# returns the partial correlation between the first two vectors, controlling for the third
library(RVAideMemoire) 

pcor.test(data4$Csupp, data4$Pol_C, data4$blackvnot, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson", "spearman"))
pcor.test(data4$Csupp, data4$blackvnot, data4$Pol_C, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson", "spearman"))

#get the simple correlation between each predictor and the outcome
cor.test(data4$blackvnot, data4$Csupp)
cor.test(data4$Pol_C, data4$Csupp)

# MODEL 2: predicting support from the main effects and interactions
support_model2bnp= lm(Csupp~blackvnot+Pol_C+BO_PO, data = data4)

#get the coefficients and summary stats 
summary(support_model2bnp)

# partial rs with confidence intervals

# partial r for the political orientation predictor
covs <- data4 %>%
  select(BO_PO, blackvnot)

class(covs)

pcor.test(data4$Csupp, data4$Pol_C, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

# partial r for the race category predictor
covs <- data4 %>%
  select(BO_PO, Pol_C)

class(covs)

pcor.test(data4$Csupp, data4$blackvnot, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson", "spearman"))

# partial r for the interaction, controlling for the two main effects
covs <- data4 %>%
  select(blackvnot, Pol_C)

class(covs)

pcor.test(data4$Csupp, data4$BO_PO, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

#get the simple correlation between the interaction and the outcome
cor.test(data4$BO_PO, data4$Csupp)




#C1 models ----

# MODEL 1: create the main effects model predicting C1 from blackvnot and political orientation, Pol_C
C1_model1bnp= lm(C1~blackvnot+Pol_C, data = data4)

#get the coefficients and summary stats 
summary(C1_model1bnp)

#get partial r for each predictor

pcor.test(data4$C1, data4$Pol_C, data4$blackvnot, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson", "spearman"))
pcor.test(data4$C1, data4$blackvnot, data4$Pol_C, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson", "spearman"))

#get the simple correlation between each predictor and the outcome
cor.test(data4$blackvnot, data4$C1)
cor.test(data4$Pol_C, data4$C1)

# MODEL 2: predicting C1 from the main effects and interactions
C1_model2bnp= lm(C1~blackvnot+Pol_C+BO_PO, data = data4)

#get the coefficients and summary stats 
summary(C1_model2bnp)

# partial rs with confidence intervals

# partial r for the political orientation predictor
covs <- data4 %>%
  select(BO_PO, blackvnot)

class(covs)

pcor.test(data4$C1, data4$Pol_C, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

# partial r for the race category predictor
covs <- data4 %>%
  select(BO_PO, Pol_C)

class(covs)

pcor.test(data4$C1, data4$blackvnot, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson", "spearman"))

# partial r for the interaction, controlling for the two main effects
covs <- data4 %>%
  select(blackvnot, Pol_C)

class(covs)

pcor.test(data4$C1, data4$BO_PO, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

#get the simple correlation between the interactions and the outcome
cor.test(data4$BO_PO, data4$C1)



#C2 models ----

# MODEL 1: create the main effects model predicting C2 from blackvnot and political orientation, Pol_C
C2_model1bnp= lm(C2~blackvnot+Pol_C, data = data4)

#get the coefficients and summary stats 
summary(C2_model1bnp)

#get partial r for each predictor

pcor.test(data4$C2, data4$Pol_C, data4$blackvnot, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson", "spearman"))
pcor.test(data4$C2, data4$blackvnot, data4$Pol_C, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson", "spearman"))

#get the simple correlation between each predictor and the outcome
cor.test(data4$blackvnot, data4$C2)
cor.test(data4$Pol_C, data4$C2)

# MODEL 2: predicting C2 from the main effects and interactions
C2_model2bnp= lm(C2~blackvnot+Pol_C+BO_PO, data = data4)

#get the coefficients and summary stats 
summary(C2_model2bnp)

# partial rs with confidence intervals

# partial r for the political orientation predictor
covs <- data4 %>%
  select(BO_PO, blackvnot)

class(covs)

pcor.test(data4$C2, data4$Pol_C, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

# partial r for the race category predictor
covs <- data4 %>%
  select(BO_PO, Pol_C)

class(covs)

pcor.test(data4$C2, data4$blackvnot, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson", "spearman"))

# partial r for the interaction, controlling for the two main effects
covs <- data4 %>%
  select(blackvnot, Pol_C)

class(covs)

pcor.test(data4$C2, data4$BO_PO, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

#get the simple correlation between the interactions and the outcome
cor.test(data4$BO_PO, data4$C2)


#C3 models ----

# MODEL 1: create the main effects model predicting C3 from blackvnot and political orientation, Pol_C
C3_model1bnp= lm(C3~blackvnot+Pol_C, data = data4)

#get the coefficients and summary stats 
summary(C3_model1bnp)

#get partial r for each predictor

pcor.test(data4$C3, data4$Pol_C, data4$blackvnot, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson", "spearman"))
pcor.test(data4$C3, data4$blackvnot, data4$Pol_C, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson", "spearman"))

#get the simple correlation between each predictor and the outcome
cor.test(data4$blackvnot, data4$C3)
cor.test(data4$Pol_C, data4$C3)

# MODEL 2: predicting C3 from the main effects and interactions
C3_model2bnp= lm(C3~blackvnot+Pol_C+BO_PO, data = data4)

#get the coefficients and summary stats 
summary(C3_model2bnp)

# partial rs with confidence intervals

# partial r for the political orientation predictor
covs <- data4 %>%
  select(BO_PO, blackvnot)

class(covs)

pcor.test(data4$C3, data4$Pol_C, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

# partial r for the race category predictor
covs <- data4 %>%
  select(BO_PO, Pol_C)

class(covs)

pcor.test(data4$C3, data4$blackvnot, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson", "spearman"))

# partial r for the interaction, controlling for the two main effects
covs <- data4 %>%
  select(blackvnot, Pol_C)

class(covs)

pcor.test(data4$C3, data4$BO_PO, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

#get the simple correlation between the interactions and the outcome
cor.test(data4$BO_PO, data4$C3)


#C4 models ----

# MODEL 1: create the main effects model predicting C4 from blackvnot and political orientation, Pol_C
C4_model1bnp= lm(C4~blackvnot+Pol_C, data = data4)

#get the coefficients and summary stats 
summary(C4_model1bnp)

#get partial r for each predictor

pcor.test(data4$C4, data4$Pol_C, data4$blackvnot, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson", "spearman"))
pcor.test(data4$C4, data4$blackvnot, data4$Pol_C, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson", "spearman"))

#get the simple correlation between each predictor and the outcome
cor.test(data4$blackvnot, data4$C4)
cor.test(data4$Pol_C, data4$C4)

# MODEL 2: predicting C4 from the main effects and interactions
C4_model2bnp= lm(C4~blackvnot+Pol_C+BO_PO, data = data4)

#get the coefficients and summary stats 
summary(C4_model2bnp)

# partial rs with confidence intervals

# partial r for the political orientation predictor
covs <- data4 %>%
  select(BO_PO, blackvnot)

class(covs)

pcor.test(data4$C4, data4$Pol_C, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

# partial r for the race category predictor
covs <- data4 %>%
  select(BO_PO, Pol_C)

class(covs)

pcor.test(data4$C4, data4$blackvnot, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson", "spearman"))

# partial r for the interaction, controlling for the two main effects
covs <- data4 %>%
  select(blackvnot, Pol_C)

class(covs)

pcor.test(data4$C4, data4$BO_PO, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

#get the simple correlation between the interactions and the outcome
cor.test(data4$BO_PO, data4$C4)


#6. SIMPLE EFFECTS TESTS TO DECOMPOSE THE SIG INTERACTIONS ABOVE (black versus not-black) ----
#Examine effect of political orientation within black people and, separately, within non-black people
#the effects of black_simple and notblack_simple (and the interactions) should be the same as the effects in the model 2s above
#look for the difference in coefficients for Pol_C: we should see a stronger effect among not-black people than black people

#recode the race indicator variable to prepare for simple effects
#we need two codes: one representing black people as 0 (reference); the other representing non-black people as 0 (reference)
#want black to be coded as zero if we're looking at the effect of black

data4 <- data4%>%
  mutate(blackvnot_v2 = case_when(
    R2B == 1 ~ -.5,
    R2B == 0 ~ .5))

data4 <- data4%>%
  mutate(black_simple = case_when(
    blackvnot_v2 == -.5 ~ 0,
    blackvnot_v2 == .5 ~ 1)) %>%
  mutate(notblack_simple = case_when(
    blackvnot_v2 == -.5 ~ -1,
    blackvnot_v2 == .5 ~ 0)) %>%
  mutate(NBS_PO = Pol_C*notblack_simple) %>%
  mutate(BS_PO = Pol_C*black_simple)


glimpse(data4)

# Support models ---- 
support_black_simple_bn=lm(Csupp~Pol_C+black_simple+BS_PO, data = data4)
summary(support_black_simple_bn)

# partial rs with confidence intervals

# partial r for the political orientation predictor
covs <- data4 %>%
  select(BS_PO, black_simple)

class(covs)

pcor.test(data4$Csupp, data4$Pol_C, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

# partial r for the race category predictor
covs <- data4 %>%
  select(BS_PO, Pol_C)

class(covs)

pcor.test(data4$Csupp, data4$black_simple, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson", "spearman"))

# partial r for the interaction, controlling for the two main effects
covs <- data4 %>%
  select(black_simple, Pol_C)

class(covs)

pcor.test(data4$Csupp, data4$BS_PO, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

support_notblack_simple_bn=lm(Csupp~Pol_C+notblack_simple+NBS_PO, data = data4)
summary(support_notblack_simple_bn)

# partial rs with confidence intervals

# partial r for the political orientation predictor
covs <- data4 %>%
  select(NBS_PO, notblack_simple)

class(covs)

pcor.test(data4$Csupp, data4$Pol_C, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

# partial r for the race category predictor
covs <- data4 %>%
  select(NBS_PO, Pol_C)

class(covs)

pcor.test(data4$Csupp, data4$notblack_simple, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson", "spearman"))

# partial r for the interaction, controlling for the two main effects
covs <- data4 %>%
  select(notblack_simple, Pol_C)

class(covs)

pcor.test(data4$Csupp, data4$NBS_PO, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

#C1 models ----
C1_black_simple_bn=lm(C1~Pol_C+black_simple+Pol_C*black_simple, data = data4)
summary(C1_black_simple_bn)

# partial rs with confidence intervals

# partial r for the political orientation predictor
covs <- data4 %>%
  select(BS_PO, black_simple)

class(covs)

pcor.test(data4$C1, data4$Pol_C, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

# partial r for the race category predictor
covs <- data4 %>%
  select(BS_PO, Pol_C)

class(covs)

pcor.test(data4$C1, data4$black_simple, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson", "spearman"))

# partial r for the interaction, controlling for the two main effects
covs <- data4 %>%
  select(black_simple, Pol_C)

class(covs)

pcor.test(data4$C1, data4$BS_PO, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))


C1_notblack_simple_bn=lm(C1~Pol_C+notblack_simple+Pol_C*notblack_simple, data = data4)
summary(C1_notblack_simple_bn)

# partial rs with confidence intervals

# partial r for the political orientation predictor
covs <- data4 %>%
  select(NBS_PO, notblack_simple)

class(covs)

pcor.test(data4$C1, data4$Pol_C, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

# partial r for the race category predictor
covs <- data4 %>%
  select(NBS_PO, Pol_C)

class(covs)

pcor.test(data4$C1, data4$notblack_simple, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson", "spearman"))

# partial r for the interaction, controlling for the two main effects
covs <- data4 %>%
  select(notblack_simple, Pol_C)

class(covs)

pcor.test(data4$C1, data4$NBS_PO, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))


#C2 models ----
C2_black_simple_bn=lm(C2~Pol_C+black_simple+Pol_C*black_simple, data = data4)
summary(C2_black_simple_bn)

# partial rs with confidence intervals

# partial r for the political orientation predictor
covs <- data4 %>%
  select(BS_PO, black_simple)

class(covs)

pcor.test(data4$C2, data4$Pol_C, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

# partial r for the race category predictor
covs <- data4 %>%
  select(BS_PO, Pol_C)

class(covs)

pcor.test(data4$C2, data4$black_simple, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson", "spearman"))

# partial r for the interaction, controlling for the two main effects
covs <- data4 %>%
  select(black_simple, Pol_C)

class(covs)

pcor.test(data4$C2, data4$BS_PO, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))


C2_notblack_simple_bn=lm(C2~Pol_C+notblack_simple+Pol_C*notblack_simple, data = data4)
summary(C2_notblack_simple_bn)

# partial rs with confidence intervals

# partial r for the political orientation predictor
covs <- data4 %>%
  select(NBS_PO, notblack_simple)

class(covs)

pcor.test(data4$C2, data4$Pol_C, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

# partial r for the race category predictor
covs <- data4 %>%
  select(NBS_PO, Pol_C)

class(covs)

pcor.test(data4$C2, data4$notblack_simple, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson", "spearman"))

# partial r for the interaction, controlling for the two main effects
covs <- data4 %>%
  select(notblack_simple, Pol_C)

class(covs)

pcor.test(data4$C2, data4$NBS_PO, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))


#C3 models ----

C3_black_simple_bn=lm(C3~Pol_C+black_simple+Pol_C*black_simple, data = data4)
summary(C3_black_simple_bn)

# partial rs with confidence intervals

# partial r for the political orientation predictor
covs <- data4 %>%
  select(BS_PO, black_simple)

class(covs)

pcor.test(data4$C3, data4$Pol_C, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

# partial r for the race category predictor
covs <- data4 %>%
  select(BS_PO, Pol_C)

class(covs)

pcor.test(data4$C3, data4$black_simple, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson", "spearman"))

# partial r for the interaction, controlling for the two main effects
covs <- data4 %>%
  select(black_simple, Pol_C)

class(covs)

pcor.test(data4$C3, data4$BS_PO, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

C3_notblack_simple_bn=lm(C3~Pol_C+notblack_simple+Pol_C*notblack_simple, data = data4)
summary(C3_notblack_simple_bn)

# partial rs with confidence intervals

# partial r for the political orientation predictor
covs <- data4 %>%
  select(NBS_PO, notblack_simple)

class(covs)

pcor.test(data4$C3, data4$Pol_C, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

# partial r for the race category predictor
covs <- data4 %>%
  select(NBS_PO, Pol_C)

class(covs)

pcor.test(data4$C3, data4$notblack_simple, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson", "spearman"))

# partial r for the interaction, controlling for the two main effects
covs <- data4 %>%
  select(notblack_simple, Pol_C)

class(covs)

pcor.test(data4$C3, data4$NBS_PO, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))


#C4 models ----

C4_black_simple_bn=lm(C4~Pol_C+black_simple+Pol_C*black_simple, data = data4)
summary(C4_black_simple_bn)

# partial rs with confidence intervals

# partial r for the political orientation predictor
covs <- data4 %>%
  select(BS_PO, black_simple)

class(covs)

pcor.test(data4$C4, data4$Pol_C, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

# partial r for the race category predictor
covs <- data4 %>%
  select(BS_PO, Pol_C)

class(covs)

pcor.test(data4$C4, data4$black_simple, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson", "spearman"))

# partial r for the interaction, controlling for the two main effects
covs <- data4 %>%
  select(black_simple, Pol_C)

class(covs)

pcor.test(data4$C4, data4$BS_PO, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

C4_notblack_simple_bn=lm(C4~Pol_C+notblack_simple+Pol_C*notblack_simple, data = data4)
summary(C4_notblack_simple_bn)

# partial rs with confidence intervals

# partial r for the political orientation predictor
covs <- data4 %>%
  select(NBS_PO, notblack_simple)

class(covs)

pcor.test(data4$C4, data4$Pol_C, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

# partial r for the race category predictor
covs <- data4 %>%
  select(NBS_PO, Pol_C)

class(covs)

pcor.test(data4$C4, data4$notblack_simple, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson", "spearman"))

# partial r for the interaction, controlling for the two main effects
covs <- data4 %>%
  select(notblack_simple, Pol_C)

class(covs)

pcor.test(data4$C4, data4$NBS_PO, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))


#7. HIERARCHICAL REGRESSION MODELS: BLACK V NOT x REASON ----


#b models, with the main effects and interactions for blackvnot with Code1, Code2, and Pol_C

#model1 contains main effects only
#model2 contains main effects and the interactions

#Support models ----

#create the main effects model (model1) predicting support from blackvnot, Code1, and Code2
support_model1bnr= lm(Csupp~blackvnot+Code1+Code2, data = data4)

#get the coefficients and summary stats 
summary(support_model1bnr)

# partial r for the effect of code1, controlling for code2 and race
covs <- data4 %>%
  select(blackvnot, Code2)

class(covs)

pcor.test(data4$Csupp, data4$Code1, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

# partial r for the effect of code2, controlling for code1 and race
covs <- data4 %>%
  select(blackvnot, Code1)

class(covs)

pcor.test(data4$Csupp, data4$Code2, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

# partial r for the effect of race, controlling for code1 and code2
covs <- data4 %>%
  select(Code1, Code2)

class(covs)

pcor.test(data4$Csupp, data4$blackvnot, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))


#get the simple correlation between each predictor and the outcome
cor.test(data4$blackvnot, data4$Csupp)
cor.test(data4$Code1, data4$Csupp)
cor.test(data4$Code2, data4$Csupp)

#create model2 predicting support from the main effects and interactions
support_model2bnr= lm(Csupp~blackvnot+Code1+Code2+BO_Code1+BO_Code2, data = data4)
summary(support_model2bnr)

# partial r for the effect of code1, controlling for code2, race, code1*race, code2*race
covs <- data4 %>%
  select(blackvnot, Code2, BO_Code1, BO_Code2)

class(covs)

pcor.test(data4$Csupp, data4$Code1, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

# partial r for the effect of code2, controlling for code1, race, code1*race, code2*race
covs <- data4 %>%
  select(blackvnot, Code1, BO_Code1, BO_Code2)

class(covs)

pcor.test(data4$Csupp, data4$Code2, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

# partial r for the effect of race, controlling for code1, code2, code1*race, code2*race
covs <- data4 %>%
  select(Code1, Code2, BO_Code1, BO_Code2)

class(covs)

pcor.test(data4$Csupp, data4$blackvnot, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

# partial r for the effect of the interaction between code1 and race, controlling for: 
# code1, code2, race, code2*race
covs <- data4 %>%
  select(Code1, Code2, blackvnot,BO_Code2)

class(covs)

pcor.test(data4$Csupp, data4$BO_Code1, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

# partial r for the effect of the interaction between code2 and race, controlling for: 
# code1, code2, race, code1*race
covs <- data4 %>%
  select(Code1, Code2, blackvnot,BO_Code1)

class(covs)

pcor.test(data4$Csupp, data4$BO_Code2, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

#get the simple correlation between the interactions and the outcome
cor.test(data4$BO_Code1, data4$Csupp)
cor.test(data4$BO_Code2, data4$Csupp)

#C1 models ----

#create the main effects model (model1) predicting C1 from blackvnot, Code1, and Code2
C1_model1bnr= lm(C1~blackvnot+Code1+Code2, data = data4)
summary(C1_model1bnr)

# partial r for the effect of code1, controlling for code2 and race
covs <- data4 %>%
  select(blackvnot, Code2)

class(covs)

pcor.test(data4$C1, data4$Code1, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

# partial r for the effect of code2, controlling for code1 and race
covs <- data4 %>%
  select(blackvnot, Code1)

class(covs)

pcor.test(data4$C1, data4$Code2, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

# partial r for the effect of race, controlling for code1 and code2
covs <- data4 %>%
  select(Code1, Code2)

class(covs)

pcor.test(data4$C1, data4$blackvnot, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

#get the simple correlation between each predictor and the outcome
cor.test(data4$blackvnot, data4$C1)
cor.test(data4$Code1, data4$C1)
cor.test(data4$Code2, data4$C1)

#create model2 predicting C1 from the main effects and interactions
C1_model2bnr= lm(C1~blackvnot+Code1+Code2+BO_Code1+BO_Code2, data = data4)
summary(C1_model2bnr)

# partial r for the effect of code1, controlling for code2, race, code1*race, code2*race
covs <- data4 %>%
  select(blackvnot, Code2, BO_Code1, BO_Code2)

class(covs)

pcor.test(data4$C1, data4$Code1, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

# partial r for the effect of code2, controlling for code1, race, code1*race, code2*race
covs <- data4 %>%
  select(blackvnot, Code1, BO_Code1, BO_Code2)

class(covs)

pcor.test(data4$C1, data4$Code2, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

# partial r for the effect of race, controlling for code1, code2, code1*race, code2*race
covs <- data4 %>%
  select(Code1, Code2, BO_Code1, BO_Code2)

class(covs)

pcor.test(data4$C1, data4$blackvnot, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

# partial r for the effect of the interaction between code1 and race, controlling for: 
# code1, code2, race, code2*race
covs <- data4 %>%
  select(Code1, Code2, blackvnot,BO_Code2)

class(covs)

pcor.test(data4$C1, data4$BO_Code1, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

# partial r for the effect of the interaction between code2 and race, controlling for: 
# code1, code2, race, code1*race
covs <- data4 %>%
  select(Code1, Code2, blackvnot,BO_Code1)

class(covs)

pcor.test(data4$C1, data4$BO_Code2, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

#get the simple correlation between the interactions and the outcome
cor.test(data4$BO_Code1, data4$C1)
cor.test(data4$BO_Code2, data4$C1)


#C2 models ----

#create the main effects model (model1) predicting C2 from blackvnot, Code1, and Code2
C2_model1bnr= lm(C2~blackvnot+Code1+Code2, data = data4)
summary(C2_model1bnr)

# partial r for the effect of code1, controlling for code2 and race
covs <- data4 %>%
  select(blackvnot, Code2)

class(covs)

pcor.test(data4$C2, data4$Code1, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

# partial r for the effect of code2, controlling for code1 and race
covs <- data4 %>%
  select(blackvnot, Code1)

class(covs)

pcor.test(data4$C2, data4$Code2, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

# partial r for the effect of race, controlling for code1 and code2
covs <- data4 %>%
  select(Code1, Code2)

class(covs)

pcor.test(data4$C2, data4$blackvnot, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

#get the simple correlation between each predictor and the outcome
cor.test(data4$blackvnot, data4$C2)
cor.test(data4$Code1, data4$C2)
cor.test(data4$Code2, data4$C2)

#create model2 predicting C2 from the main effects and interactions
C2_model2bnr= lm(C2~blackvnot+Code1+Code2+BO_Code1+BO_Code2, data = data4)
summary(C2_model2bnr)

# partial r for the effect of code1, controlling for code2, race, code1*race, code2*race
covs <- data4 %>%
  select(blackvnot, Code2, BO_Code1, BO_Code2)

class(covs)

pcor.test(data4$C2, data4$Code1, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

# partial r for the effect of code2, controlling for code1, race, code1*race, code2*race
covs <- data4 %>%
  select(blackvnot, Code1, BO_Code1, BO_Code2)

class(covs)

pcor.test(data4$C2, data4$Code2, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

# partial r for the effect of race, controlling for code1, code2, code1*race, code2*race
covs <- data4 %>%
  select(Code1, Code2, BO_Code1, BO_Code2)

class(covs)

pcor.test(data4$C2, data4$blackvnot, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

# partial r for the effect of the interaction between code1 and race, controlling for: 
# code1, code2, race, code2*race
covs <- data4 %>%
  select(Code1, Code2, blackvnot,BO_Code2)

class(covs)

pcor.test(data4$C2, data4$BO_Code1, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

# partial r for the effect of the interaction between code2 and race, controlling for: 
# code1, code2, race, code1*race
covs <- data4 %>%
  select(Code1, Code2, blackvnot,BO_Code1)

class(covs)

pcor.test(data4$C2, data4$BO_Code2, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

#get the simple correlation between the interactions and the outcome
cor.test(data4$BO_Code1, data4$C2)
cor.test(data4$BO_Code2, data4$C2)


#C3 models ----

#create the main effects model (model1) predicting C3 from blackvnot, Code1, and Code2
C3_model1bnr= lm(C3~blackvnot+Code1+Code2, data = data4)
summary(C3_model1bnr)

# partial r for the effect of code1, controlling for code2 and race
covs <- data4 %>%
  select(blackvnot, Code2)

class(covs)

pcor.test(data4$C3, data4$Code1, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

# partial r for the effect of code2, controlling for code1 and race
covs <- data4 %>%
  select(blackvnot, Code1)

class(covs)

pcor.test(data4$C3, data4$Code2, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

# partial r for the effect of race, controlling for code1 and code2
covs <- data4 %>%
  select(Code1, Code2)

class(covs)

pcor.test(data4$C3, data4$blackvnot, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

#get the simple correlation between each predictor and the outcome
cor.test(data4$blackvnot, data4$C3)
cor.test(data4$Code1, data4$C3)
cor.test(data4$Code2, data4$C3)

#create model2 predicting C3 from the main effects and interactions
C3_model2bnr= lm(C3~blackvnot+Code1+Code2+BO_Code1+BO_Code2, data = data4)
summary(C3_model2bnr)

# partial r for the effect of code1, controlling for code2, race, code1*race, code2*race
covs <- data4 %>%
  select(blackvnot, Code2, BO_Code1, BO_Code2)

class(covs)

pcor.test(data4$C3, data4$Code1, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

# partial r for the effect of code2, controlling for code1, race, code1*race, code2*race
covs <- data4 %>%
  select(blackvnot, Code1, BO_Code1, BO_Code2)

class(covs)

pcor.test(data4$C3, data4$Code2, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

# partial r for the effect of race, controlling for code1, code2, code1*race, code2*race
covs <- data4 %>%
  select(Code1, Code2, BO_Code1, BO_Code2)

class(covs)

pcor.test(data4$C3, data4$blackvnot, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

# partial r for the effect of the interaction between code1 and race, controlling for: 
# code1, code2, race, code2*race
covs <- data4 %>%
  select(Code1, Code2, blackvnot,BO_Code2)

class(covs)

pcor.test(data4$C3, data4$BO_Code1, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

# partial r for the effect of the interaction between code2 and race, controlling for: 
# code1, code2, race, code1*race
covs <- data4 %>%
  select(Code1, Code2, blackvnot,BO_Code1)

class(covs)

pcor.test(data4$C3, data4$BO_Code2, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

#get the simple correlation between the interactions and the outcome
cor.test(data4$BO_Code1, data4$C3)
cor.test(data4$BO_Code2, data4$C3)


#C4 models ----

#create the main effects model (model1) predicting C4 from blackvnot, Code1, and Code2
C4_model1bnr= lm(C4~blackvnot+Code1+Code2, data = data4)
summary(C4_model1bnr)

# partial r for the effect of code1, controlling for code2 and race
covs <- data4 %>%
  select(blackvnot, Code2)

class(covs)

pcor.test(data4$C4, data4$Code1, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

# partial r for the effect of code2, controlling for code1 and race
covs <- data4 %>%
  select(blackvnot, Code1)

class(covs)

pcor.test(data4$C4, data4$Code2, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

# partial r for the effect of race, controlling for code1 and code2
covs <- data4 %>%
  select(Code1, Code2)

class(covs)

pcor.test(data4$C4, data4$blackvnot, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

#get the simple correlation between each predictor and the outcome
cor.test(data4$blackvnot, data4$C4)
cor.test(data4$Code1, data4$C4)
cor.test(data4$Code2, data4$C4)

#create model2 predicting C4 from the main effects and interactions
C4_model2bnr= lm(C4~blackvnot+Code1+Code2+BO_Code1+BO_Code2, data = data4)
summary(C4_model2bnr)

# partial r for the effect of code1, controlling for code2, race, code1*race, code2*race
covs <- data4 %>%
  select(blackvnot, Code2, BO_Code1, BO_Code2)

class(covs)

pcor.test(data4$C4, data4$Code1, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

# partial r for the effect of code2, controlling for code1, race, code1*race, code2*race
covs <- data4 %>%
  select(blackvnot, Code1, BO_Code1, BO_Code2)

class(covs)

pcor.test(data4$C4, data4$Code2, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

# partial r for the effect of race, controlling for code1, code2, code1*race, code2*race
covs <- data4 %>%
  select(Code1, Code2, BO_Code1, BO_Code2)

class(covs)

pcor.test(data4$C4, data4$blackvnot, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

# partial r for the effect of the interaction between code1 and race, controlling for: 
# code1, code2, race, code2*race
covs <- data4 %>%
  select(Code1, Code2, blackvnot,BO_Code2)

class(covs)

pcor.test(data4$C4, data4$BO_Code1, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

# partial r for the effect of the interaction between code2 and race, controlling for: 
# code1, code2, race, code1*race
covs <- data4 %>%
  select(Code1, Code2, blackvnot,BO_Code1)

class(covs)

pcor.test(data4$C4, data4$BO_Code2, covs, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson"))

#get the simple correlation between the interactions and the outcome
cor.test(data4$BO_Code1, data4$C4)
cor.test(data4$BO_Code2, data4$C4)





#8. SECONDARY ANALYSES ----

#does the relation between how safe I feel carrying a gun and how safe others would feel if I carried a gun hold between:
#white and non-white people?

#assess overall correlation, among white, black, and not-black

cor.test(data4$C2, data4$C4)

data5 <- data4%>%
  filter(
    R1W == 1) 

cor.test(data5$C2, data5$C4)

data5 <- data4%>%
  filter(
    R2B == 1) 

cor.test(data5$C2, data5$C4)

data5 <- data4%>%
  filter(
    R2B == 0) 

cor.test(data5$C2, data5$C4)

