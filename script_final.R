#+eval=FALSE
##-----Code for Final Lab---------
# 200024980
# Reading the necessary libraries
library(sf)
library(tmap)
library(RColorBrewer)
library(grid)
library(gridExtra)
library(GWmodel)
library(AICcmodavg)

#----------Importing the data previously cleaned in QGIS----------
dataGWR <- sf::read_sf("Lden_ScotGov_SIMD_Roads.shp") # Reading as an sf 
colnames(dataGWR)

# Plotting the average noise levels for each datazone with equal intervals 
tm_shape(dataGWR) + tm_fill("_mean", palette = "Reds", style = "equal", n = 7)

head(dataGWR)

# Exploring the data 
head(dataGWR$DataZone) # Datazones are lines drawn where number of people is between 500 and 1000

#-------Cleaning the general health data from the 2011 census------- 
# Reading General Health CSV
general_health <- read.csv("health_census_2011.csv", header = TRUE, skip = 10)# skipping rows because there is some metadata at first 

str(general_health) # Checking data types 

# converting all people from chr to int 
general_health$All.people <- as.integer(general_health$All.people)

sum(is.na(general_health$All.people)) # Looks like there are 8 datazones where we dont have data for the health

# Calculate percentages for 'Bad' and 'Very.bad' by dividing bad health by the total 
general_health$percent_Bad <- general_health$Bad / general_health$All.people
general_health$percent_VeryBad <- general_health$Very.bad / general_health$All.people

sum(is.na(general_health$percent_VeryBad)) # Just checking, not too many 

head(general_health)
# Subset the data --> weirdly general health is the title of the datazones column, so will need to rename that soon 
general_health <- general_health[, c("General.health", "percent_Bad", "percent_VeryBad")]

# now lets combine percent of bad and very bad health to get a negative health rating 
# Create a new column 'percent_NegativeHealth' that sums 'percent_Bad' and 'percent_VeryBad'
general_health$percent_NegativeHealth <- general_health$percent_Bad + general_health$percent_VeryBad
# Check the result
head(general_health)
# Rename column "General health" to "DataZone"
colnames(general_health)[colnames(general_health) == "General.health"] <- "DataZone"
colnames(general_health)[colnames(general_health) == "percent_Bad"] <- "Bad_Hlth"
colnames(general_health)[colnames(general_health) == "percent_VeryBad"] <- "VeryBad_Hlth"
colnames(general_health)[colnames(general_health) == "percent_NegativeHealth"] <- "Negative_Hlth"

#----------Merge the shapefile with the health data------------
dataGWR <- merge(dataGWR, general_health, by = "DataZone")

head(dataGWR)

sum(is.na(dataGWR$Negative_Hlth)) # Okay, so 3 na values, we will 
# just remove these datazones later on, because 3 is hardly significant out of around 1000 

tm_shape(dataGWR) + tm_fill("Negative_Hlth", palette = "Reds", na.color = "blue") 
# We can see some of the NA values in the map, again, not that important considering size of the data  

#--------Cleaning the Employment / Dependency Data from 2011 Census-------------

#First step is to read it 
emp_dep_csv <- read.csv("employment_and_dependency_2011_census.csv", header = TRUE, skip = 10)

sum(is.na(emp_dep_csv$All.households))
names(emp_dep_csv)

# Get the sum of unemployed adults and the sum of people with long term health problems 
emp_dep_csv$Unemployed <- emp_dep_csv$No.adults.in.employment.in.household..With.dependent.children + emp_dep_csv$No.adults.in.employment.in.household..No.dependent.children

emp_dep_csv$LTH_prob <- emp_dep_csv$One.or.more.persons.in.household.with.a.long.term.health.problem.or.disability..With.dependent.children + emp_dep_csv$One.or.more.persons.in.household.with.a.long.term.health.problem.or.disability..No.dependent.children

# Calculate percentages against the total 
emp_dep_csv$Pct_Unemployed <- emp_dep_csv$Unemployed / emp_dep_csv$All.households
emp_dep_csv$Pct_LTH_prob <- emp_dep_csv$LTH_prob / emp_dep_csv$All.households


emp_dep_csv <- na.omit(emp_dep_csv)

summary(emp_dep_csv)
#Renaming columns 
colnames(emp_dep_csv)[colnames(emp_dep_csv) == "Households.with.no.adults.in.employment.and.dependent.children.status.then.Dependent.children.in.household.then.Dependent.children.in.household.then.Persons.in.household.with.long.term.health.problem.or.disability.and.dependent.chilfren.status.then.Household.size"] <- "DataZone"
#Subsetting
emp_dep_csv <- emp_dep_csv[, c("DataZone", "Pct_Unemployed", "Pct_LTH_prob")]

head(emp_dep_csv)

#----------Adding Employment/Health Problems data to the shapefile--------------------
dataGWR <- merge(dataGWR, emp_dep_csv, by = "DataZone")

colnames(dataGWR)


#--------Cleaning the Education Data from 2011 Census--------------
# While the SIMD data does have indicators for education, such as EduNoQual, I decided to also calculate this based on the metadata for how they caluclated household deprivation stats in the 2011 census
# This means measuring lower educational attainment as below a level 2 qualification 
# https://www.scotlandscensus.gov.uk/metadata/household-deprivation-classification/#panel-1
educsv <- read.csv("Education_Census_2011.csv", header = TRUE, skip = 10)

head(educsv)

# Calculating the Lower Education indicator by adding together the lower school quals, upper school quals, and apprenticeship quals 
# This excluded those have a college degree or other higher education 
educsv$LowerEd <- educsv$No.qualifications + educsv$Lower.school.qualifications + educsv$Upper.school.qualifications +educsv$Apprenticeship.qualifications

# calculating the percentage of this against the total no of people
educsv$All.people.aged.16.and.over <- as.integer(educsv$All.people.aged.16.and.over)
educsv$LowerEd_Pct <- educsv$LowerEd / educsv$All.people.aged.16.and.over

# Checking for NAs 
sum(is.na(educsv$LowerEd_Pct))
educsv <- na.omit(educsv)
sum(is.na(educsv$LowerEd_Pct))

min(educsv$LowerEd_Pct)
# This is good that it is a value of 0.13; it means that all na values are cleaned and we would expect the minimum to be around here 

# Subsetting and renaming the data 
head(educsv)

# renaming the messy title of datazone to then merge later on 
colnames(educsv)[colnames(educsv) == "Highest.level.of.qualification"] <- "DataZone"

educsv <- educsv[, c("DataZone", "LowerEd_Pct")]

#----------Adding educsv to the shapefile---------------

dataGWR <- merge(dataGWR, educsv, by = "DataZone")

colnames(dataGWR)

#-------Subsetting the SIMD and the Extra Data We Just Cleaned From 2011 Census----------

# Subset columns from dataGWR with the variables from SIMD and the census, identified from the literature that might be relevant 
dataGWR <- dataGWR[, c("DataZone", "Vigintilv2", "IncRate", "EmpRate", "HlthDprsPc",  
                       "HlthAlcSR", "HlthSMR", "HlthCIF", "EduAttend", "EduAttain", 
                       "CrimeRate", "HouseOCrat", "HouseNCrat", "Negative_Hlth", "Pct_Unemployed", "Pct_LTH_prob", "LowerEd_Pct", "geometry", 
                       "_mean", "_stdev", "_min", "_max", "Shape_Leng", "Shape_Area", "EduNoQuals",
                       "GAccRank")]


# Renaming the noise columns 
colnames(dataGWR)[colnames(dataGWR) == "_mean"] <- "mean_noise"
colnames(dataGWR)[colnames(dataGWR) == "_stdev"] <- "stdev_noise"
colnames(dataGWR)[colnames(dataGWR) == "_min"] <- "min_noise"
colnames(dataGWR)[colnames(dataGWR) == "_max"] <- "max_noise"

colnames(dataGWR)

summary(dataGWR)

# Let's have a quick look at the data
# Standard Deviation of the noise levels 
tm_shape(dataGWR) + tm_fill("Vigintilv2", palette = "Reds") 
# Mean noise Map
tm_shape(dataGWR) + tm_fill("mean_noise", palette = "Reds") 
# Percent Unemployed 
tm_shape(dataGWR) + tm_fill("Pct_Unemployed", palette = "Reds") 

dev.off()

#-----Cleaning the Data, Converting to Percentages ----------

str(dataGWR)
# So clearly, the data from the SIMD is not all an integer or percentage like I need it to be. 
#Instead, some of the percentages have the symbol next to the number (%2), when I need it to be 0.02. 

# I found the documentation for using gsub here 
# https://www.statology.org/gsub-r/
# Basically, I wanted to search the chr for % and replace it with nothing (""), and then divide the number that remained by 100 

dataGWR$IncRate <- as.numeric(gsub("%", "", dataGWR$IncRate)) / 100
dataGWR$EmpRate <- as.numeric(gsub("%", "", dataGWR$EmpRate)) / 100
dataGWR$HlthDprsPc <- as.numeric(gsub("%", "", dataGWR$HlthDprsPc)) / 100
dataGWR$EduAttend <- as.numeric(gsub("%", "", dataGWR$EduAttend)) / 100
dataGWR$HouseOCrat <- as.numeric(gsub("%", "", dataGWR$HouseOCrat)) / 100
dataGWR$HouseNCrat <- as.numeric(gsub("%", "", dataGWR$HouseNCrat)) / 100

# Check the structure of the modified columns
str(dataGWR)

dataGWR <- na.omit(dataGWR) 
sum(is.na(dataGWR)) # great, there are 0 left 

#--------Correlation Matrix for the Identified Variables-------------------

# First I need to convert dataGWR into a dataframe, not a spatial dataframe 

dataGWR_df <- as.data.frame(dataGWR) # Converting this to a dataframe to avoid issues with the geometry column 
names(dataGWR_df)
# Now I can run the correlation test with all the potential variables 
correlation_matrix <- cor(dataGWR_df[, c("mean_noise", "IncRate","EmpRate", "HlthDprsPc", "HlthAlcSR" , "HlthSMR", "HlthCIF", "Negative_Hlth", 
                                         "HouseOCrat","Pct_LTH_prob" , "EduAttain" ,"EduAttend", "CrimeRate",
                                         "HouseNCrat","LowerEd_Pct" ,  "Pct_Unemployed", "Vigintilv2", "GAccRank" )])

print(correlation_matrix)

# I want to be able to highlight numbers that are above .7
# As such, I can visualise this easily in Excel
# Here is the documentation for where I found the library to download the xlsx package 
# https://cran.r-project.org/web/packages/xlsx/readme/README.html

#install.packages("openxlsx")
library(openxlsx)
write.xlsx(correlation_matrix, file = "correlation_matrix_test.xlsx", rowNames = TRUE)

#--------Variables selected from the correlation analysis-------------
# HlthSMR
# Negative_Hlth
# HouseOCrat
# Pct_LTH_prob
# CrimeRate
# HouseNCrat
# LowerEd_Pct
# Pct_Unemployed
# Vigintilv2

#----------Mapping the dependent variable----------

# Mean Noise is the dependent variable
colnames(dataGWR)

tm_shape(dataGWR) + tm_fill("mean_noise", palette = "Reds", n = 7, style = "equal")

#When using GeoDa to test the different neighborhood definitions, 
# I know I'm going to need a Poly_ID - a unique values for each entry in the datazone 
# As such, I used seq_len function to do this easily in r, as demonstrated here: 
# https://www.rpubs.com/Mentors_Ubiqum/seq_len

dataGWR$POLY_ID <- seq_len(nrow(dataGWR))# This creates a sequence that starts at 1 and ends at a specified value -I set this specified value to the number of rows in dataGWR

# Did this work? 
colnames(dataGWR)
# Great, now we can save to bring into GeoDa
st_write(dataGWR, "Lden_ScotGov_SIMD_Combined_PolyID.shp", append=FALSE)# Now I am ready to calculate weights and conduct the global spatial autocorrelation analysis


#------------AICc Optimisation-----------------

# First, as with any GWmodel call, we need to convert data into a spatial data frame
dataGWRspatial <- as_Spatial(dataGWR)
names(dataGWR)

DeVar <- "mean_noise"

# Again, here are the variables we identified 
# HlthSMR
# Negative_Hlth
# HouseOCrat
# Pct_LTH_prob
# CrimeRate
# HouseNCrat
# LowerEd_Pct
# Pct_Unemployed
# Vigintilv2

# Defining independent variables 
InDeVar <- c( "Vigintilv2","Negative_Hlth", "CrimeRate", "HouseOCrat", 
              "HouseNCrat","Pct_Unemployed", "Pct_LTH_prob", "LowerEd_Pct",
              "HlthSMR")

# Run GWR with the specified independent variables
optimalBW <- bw.gwr(mean_noise ~  Vigintilv2+ Negative_Hlth + 
                      CrimeRate + HouseOCrat + 
                      HouseNCrat+ Pct_Unemployed + Pct_LTH_prob + LowerEd_Pct+HlthSMR, 
                    data = dataGWRspatial, 
                    approach = "AICc", 
                    kernel = "bisquare", 
                    adaptive = TRUE)

# Approach is AICc, kernel is bisquare, adaptive is true 
# KNN = 138, AICc is 6526.232
# Running the Model Selection 
modelSel <- model.selection.gwr(DeVar, InDeVar, data=dataGWRspatial, kernel="bisquare", adaptive=TRUE, bw=optimalBW)

# Extract list of models from the results, this creates a list of the order in which they were generated
sortedModels <- model.sort.gwr(modelSel, numVars <- length(InDeVar), ruler.vector = modelSel[[2]][,2])
modelList <- sortedModels[[1]]

model.view.gwr(DeVar, InDeVar, model.list=modelList) # Viewing the radial model 

# Export in a figure
#png(filename="ModelSelection_RadialView_Final.png", width=800, height = 800)
#model.view.gwr(DeVar, InDeVar, model.list=modelList)
dev.off()

#----------1.3 Determining the impact of variables on AICc--------------------------------------- 
#Prepping the graph that shows the variables with the lowest impact on AICc

# number of independent variables
n <- length(InDeVar)

# Export list of AICc values from the sorted models
AICcList <- sortedModels[[2]][,2]

indices <- rep(n, n) # initialise a list of indicies as n values of n, the first position is already correct

# Based on the lab: for each position we will take the number from previous step (i-1) and add (n-i), then correct 
# by adding another 1, because we started at i=2
for (i in 2:n) {
  indices[i]=indices[i-1]+((n-i)+1)
}
# Checking what this looks like, seems to match with radial plot: 
indices

# Now let's find AICc values for models at these indices
AICcBestModelValues <- AICcList[indices]

AICcBestModelValues
# Result: [1] 6405.122 6379.580 6354.013 6339.046 6322.967 6313.754 6306.834 6298.617 6294.297 6285.674

# To be able to plot the AICc optimisation plot, we need to find out which variable was the one that was added to each best model - we do this by selecting model descriptions from the model selection result.
BestModels <- sortedModels[[1]][indices]
BestModels

# The last model has variables listed in the order of addition, and we will need this for our AICc plot.
BestModels[n]

# With the correct order 
variablesAsAdded <- c("HouseOCrat", "Pct_LTH_prob" , "CrimeRate" , "LowerEd_Pct","Vigintilv2", "HouseNCrat" , "Pct_Unemployed", "HlthSMR", "Negative_Hlth")

#-----------1.31 Plotting the graph with the AICc impact---------------------------------
par(mar = c(8, 4, 4, 2)) # Increase bottom margin (first value)
# Documentation for this margin increase was found here: 
# https://www.r-bloggers.com/2010/06/setting-graph-margins-in-r-using-the-par-function-and-lots-of-cow-milk/
plot(cbind(1:9,AICcBestModelValues), col = "black", pch = 20, lty = 10, 
     main = "AICc optimisation", ylab = "AICc", type = "b", axes=FALSE)
par(las=2) # This will rotate labels on x axis for 90 degrees
axis(1, at=1:9, labels=variablesAsAdded) # this plots variable names as labels on x axis, changed to 9
axis(2, at=NULL, labels=TRUE) # this plots numbers on y axis
dev.off()


# And export the plot as a figure; I left this hashed out on subsequent runs of the data 
# png(filename="AICcOptimisationFinal.png", width=1024)
# plot(cbind(1:9,AICcBestModelValues), col = "black", pch = 20, lty = 5, 
#      main = "AICc optimisation", ylab = "AICc", type = "b", axes=FALSE)
# par(las=2) # This will rotate labels on x axis for 90 degrees
# axis(1, at=1:9, labels=variablesAsAdded) # this plots variable names as labels on x axis
# axis(2, at=NULL, labels=TRUE) # this plots numbers on y axisdev.off()
# dev.off()
#------------1.32 Calculating the AICc differences between the variables----------------------
#Difference between two consecutive AICc: Takes the first n-1 elements (1:(n-1)) and the last n-1 elements (2:n) and subtracts the second list from the first list
AICcDifference <- AICcBestModelValues[1:(n-1)]-AICcBestModelValues[2:n]
# Check how this looks
AICcDifference
#Result: 
#[1] 25.6252613 28.5750158 15.4282422 13.1605979  9.5677228  7.0950348  4.7815375 -0.2510507
# This means that the last variable should be removed because it does not improve the model fit, the AICc is less than 3

#----------------2.1 Creating the new optimised GWR model using 8 variables------------------
# Optimised bandwidth: we take the bisquare adaptive kernel and use AICc for identification of the optimal bandwith
optimalBW7 <- bw.gwr(mean_noise ~ HouseOCrat+Pct_LTH_prob+CrimeRate+LowerEd_Pct+Vigintilv2+HouseNCrat+Pct_Unemployed+HlthSMR, data=dataGWRspatial, approach="AICc", kernel="bisquare", adaptive=TRUE)

# Run the GWR model (basic)
gwrmodel <- gwr.basic(mean_noise ~ HouseOCrat+Pct_LTH_prob+CrimeRate+LowerEd_Pct+Vigintilv2+HouseNCrat+Pct_Unemployed+HlthSMR, data=dataGWRspatial, bw=optimalBW7, kernel="bisquare", adaptive=TRUE) 
# As a note, it is not necessary to run the global linear model: the GWR result below gives both the local and global gwr results. 

print(gwrmodel)
capture.output(gwrmodel, file="GWRmodel_descriptiveResult.txt", append = TRUE)

#----------------2.2 Calculating Global Standardised Residuals ----------------------------------------

#----------------2.21 Calculate global residuals in three steps--------------------------------

# Step 1: calculate predicted mean noise

# Here is the actual equation with the values taken from the results of the GWR 
dataGWR$predictedmean_noise <- 52.1872976 + 21.0141953 * dataGWR$HouseOCrat + -2.2071603 * dataGWR$Pct_LTH_prob +  0.0002369 * dataGWR$CrimeRate + -5.6855664 * dataGWR$LowerEd_Pct + -0.1387607 * dataGWR$Vigintilv2 +  12.1745695* dataGWR$HouseNCrat +  -1.2567261* dataGWR$Pct_Unemployed + 0.0018505* dataGWR$HlthSMR

# Check what this did:
head(dataGWR)

# Step 2: Calculate global residuals by subtracting the predicted value from the actual value 
dataGWR$globalRes <- dataGWR$mean_noise - dataGWR$predictedmean_noise
# Check what this did:
head(dataGWR)

#Step 3: Rescale the global residuals to the 0-1 range using mean and sd 
m <- mean(dataGWR$globalRes)
sd <- sd(dataGWR$globalRes)

#Calculating standardised global residuals 
dataGWR$stGlobalRes <- (dataGWR$globalRes-m)/sd

#------------------2.3 Creating the map of GWR by joining with the data--------------------------------------
# Turning the gwrmodel into a dataframe 
results <- as.data.frame(gwrmodel$SDF)

names(results)
head(results)

#To get the stats on the local regression
summary(results)

# Appending the GWR data and the map data and then renaming variables 
mapGWR <- cbind(dataGWR, as.matrix(results))
head(mapGWR)
names(mapGWR)

# Renaming them from .1 to _beta
names(mapGWR)[which(names(mapGWR)=="Intercept")] <- "Intercept_beta"
names(mapGWR)[which(names(mapGWR)=="HouseOCrat.1")] <- "HouseOCrat_beta"
names(mapGWR)[which(names(mapGWR)=="CrimeRate.1")] <- "CrimeRate_beta"
names(mapGWR)[which(names(mapGWR)=="LowerEd_Pct.1")] <- "LowerEd_Pct_beta"
names(mapGWR)[which(names(mapGWR)=="HouseNCrat.1")] <- "HouseNCrat_beta"
names(mapGWR)[which(names(mapGWR)=="Pct_Unemployed.1")] <- "Pct_Unemployed_beta"
names(mapGWR)[which(names(mapGWR)=="Pct_LTH_prob.1")] <- "Pct_LTH_prob_beta"
names(mapGWR)[which(names(mapGWR)=="HlthSMR.1")] <- "HlthSMR_beta"
names(mapGWR)[which(names(mapGWR)=="Vigintilv2.1")] <- "Vigintilv2_beta"

names(mapGWR)

#------------------------2.31 Setting Up the Bounding Boxes----------------------------------------

# Get the bounding boxes of the parameter est maps 
bbox1 <- st_bbox(mapGWR)
# Range of x and y values
xrange <- bbox1$xmax - bbox1$xmin # range of x values
yrange <- bbox1$ymax - bbox1$ymin # range of y values
# Extend the right dimension by 25% more space
bbox1[3] <- bbox1[3] + (0.25 * xrange)
# Extend the bottom dimension by 25% more space
bbox1[2] <- bbox1[2] - (0.25 * yrange)
# Convert bounding box to a simple feature geometry
bbox1 <- bbox1 %>% st_as_sfc()

# Setting up a second bounding box for the local r2, etc. 
# It's the same but not adjusted on the y axis 
# Get the bounding boxes of the parameter est maps 
bbox2 <- st_bbox(mapGWR)
# Range of x and y values
xrange <- bbox2$xmax - bbox2$xmin # range of x values
yrange <- bbox2$ymax - bbox2$ymin # range of y values
# Extend the right dimension by 25% more space
bbox2[3] <- bbox2[3] + (0.25 * xrange)
# Convert bounding box to a simple feature geometry
bbox2 <- bbox2 %>% st_as_sfc()

#---------------2.4 Mapping parameter estimates and T values for each of the 7 variables----------------

# Based off the loops we learned in the second part of the model, I created loops 
# for each parameter estimate map 

# Here I created a list called variables that includes both the names of the variables 
# and assigned them a color map that matches what group they are in - for example, 
# the two health variables have a red blue color scheme. 

variables <- list(
  HouseOCrat = "BrBG",
  Vigintilv2 = "BrBG",
  Pct_LTH_prob = "RdBu",
  CrimeRate = "PiYG",
  HouseNCrat = "BrBG",
  LowerEd_Pct = "PiYG",
  HlthSMR = "RdBu",
  Pct_Unemployed = "PiYG"
)


for (var in names(variables)) { # names(variables) means that this loop will iterate over the names of the variables 
  # that I just defined (so leaves out the colormap)
  # Dynamically create column names
  t_value_col <- paste0(var, "_TV") # This creates an empty t value column that has the name of the variable with _TV appended to it (which we already defined in the GWR model) 
  # I was able to figure out how to use paste0 from this documentation
  #https://www.digitalocean.com/community/tutorials/paste-in-r
  beta_col <- paste0(var, "_beta") # This does the same for beta 
  beta_sig_col <- paste0(var, "_beta_sig") # Now for beta sig 
  
  # Identify non-significant areas --> because we defined t value col as an empty list that will be filled in iteratively for each variable name, we only need to write this once 
  whereNonSig <- which(mapGWR[[t_value_col]] > -1.96 & mapGWR[[t_value_col]] < 1.96) # 1.96 is the cut off four significant t values 
  
  # Copy original parameter estimates
  mapGWR[[beta_sig_col]] <- mapGWR[[beta_col]]
 # --> it's the same step as here: 
  # mapGWR$Pct_LTH_prob_beta_sig <- mapGWR$Pct_LTH_prob_beta, but instead it has double brackets in order to reference something in a list 
  # the documentation for this double brackets was found here: 
  #https://www.dataquest.io/blog/for-loop-in-r/
  
  # Set non-significant areas to NA
  mapGWR[[beta_sig_col]][whereNonSig] <- NA
  # Same as this line but for the empty list of beta sig col and with double brackets as above:
  # mapGWR$Pct_LTH_prob_beta_sig[whereNonSig_Pct_LTH_prob] <- NA  # Set non-significant areas to NA 

} # End for 

# Creation of maps for loop
maps <- list() # This is an empty list that will store all the maps, otherwise they would delete as soon as we made them 
for (var in names(variables)) { # Beginning of second for loop, same concept as with the first loop 
  beta_sig_col <- paste0(var, "_beta_sig") # finds the variable name with the _beta_sig_col appended to it 
  palette <- variables[[var]] # Earlier, we defined the colormap in the same list as the variables, so now instead of calling name(variables) we can just call variables to get the values we assigned to them 
  
  # Generate map; this is the same process as we did in Assignment 1, but I've replaced the names of each map with maps[[var]], which means it will create the maps and store them in the list for each variable 
  maps[[var]] <- tm_shape(mapGWR, bbox = bbox1) +
    tm_fill(beta_sig_col, style = "equal", palette = palette, # Palette and beta_sig_col we have defined earlier 
            colorNA = "lightgray", textNA = "Non-significant") + # Setting the color of NA to light gray so that we can use a bicolor map
    tm_borders()
} # End for loop 

Map_HouseOCrat_beta_final <- maps[["HouseOCrat"]]# I've renamed every map in the maps list by assigning the individual map name to its own variable
# This helps with plotting them on a grid 
Map_Vigintilv2_beta_final <- maps[["Vigintilv2"]]
Map_Pct_LTH_prob_beta_final <- maps[["Pct_LTH_prob"]]
Map_CrimeRate_beta_final <- maps[["CrimeRate"]]
Map_HouseNCrat_beta_final <- maps[["HouseNCrat"]]
Map_LowerEd_Pct_beta_final <- maps[["LowerEd_Pct"]]
Map_HlthSMR_beta_final <- maps[["HlthSMR"]]
Map_Pct_Unemployed_beta_final <- maps[["Pct_Unemployed"]]


# Create a 2x2 grid for the first three maps
grid.newpage()
pushViewport(viewport(layout=grid.layout(2, 2)))  # 2 rows, 2 columns

# Place the maps in the grid layout
print(Map_HouseOCrat_beta_final, vp=viewport(layout.pos.col=1, layout.pos.row=1))
print(Map_HouseNCrat_beta_final, vp=viewport(layout.pos.col=2, layout.pos.row=1))
print(Map_Pct_LTH_prob_beta_final, vp=viewport(layout.pos.col=1, layout.pos.row=2))
print(Map_HlthSMR_beta_final, vp=viewport(layout.pos.col=2, layout.pos.row=2))


dev.off()

# Create a 2x2 grid for the remaining four maps
grid.newpage()
pushViewport(viewport(layout=grid.layout(2, 2)))  # 2 rows, 2 columns

# Place the maps in the grid layout
print(Map_CrimeRate_beta_final, vp=viewport(layout.pos.col=1, layout.pos.row=1))
print(Map_LowerEd_Pct_beta_final, vp=viewport(layout.pos.col=2, layout.pos.row=1))
print(Map_Pct_Unemployed_beta_final, vp=viewport(layout.pos.col=1, layout.pos.row=2))
print(Map_Vigintilv2_beta_final, vp=viewport(layout.pos.col=2, layout.pos.row=2))
dev.off()

#-------------2.5 Mapping Local R 2------------
tm_shape(mapGWR, bbox=bbox2) + tm_fill("Local_R2", style="equal", n=7, palette="Greens")+tm_borders()
# These values were already calculated when we ran the gwrmodel


# ________________2.6 Mapping Local Residual values with Global Residuals_________________________ 
ming <- min(mapGWR$stGlobalRes)
maxg <- max(mapGWR$stGlobalRes) # Finding the min and max of the global standardised residuals
ming
maxg

breaksGlRes <- c(ming, -2.58, -1.96, 0, 1.96, 2.58, maxg)

head(mapGWR$stGlobalRes) 

# Sort the breaks in ascending order
breaksGlRes <- sort(breaksGlRes, na.last = TRUE)  # This sorts the breaks and removes NAs at the end

globRes<-tm_shape(mapGWR, bbox=bbox1) + tm_fill("stGlobalRes", style="fixed", breaks=breaksGlRes, palette="RdBu") +tm_borders+tm_borders()

# Check if breaks are sorted and contain no NAs
print(breaksGlRes)  # Make sure it is sorted and no NAs

print(globRes)

#Local Residual, which is already calculated for us in the GWRMODEL
minl <- min(mapGWR$Stud_residual)
maxl <- max(mapGWR$Stud_residual)

breaksLRes <- c(minl, -2.58, -1.96, 0, 1.96, 2.58, maxl)

locRes <- tm_shape(mapGWR, bbox=bbox1) + tm_fill("Stud_residual", style="fixed", breaks=breaksLRes, palette="RdBu") +tm_borders()

grid.newpage() # Plotting on a grid to have them side by side 
pushViewport(viewport(layout=grid.layout(1,2)))
print(globRes, vp=viewport(layout.pos.col = 1, layout.pos.row =1))
print(locRes, vp=viewport(layout.pos.col = 2, layout.pos.row =1))
dev.off()
#------Saving to then calculate local residuals in GeoDa-----------
st_write(mapGWR,"GG_Noise_GWR_results_Roads_Scotgov.shp", append=FALSE)
