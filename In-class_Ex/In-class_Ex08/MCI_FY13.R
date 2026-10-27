#The setting directary varies with different file locations
setwd("C:/nlb")
options(useFancyQuotes = FALSE) # this is to solve gibberish issue

library(MCI)


# {PART 0: Read and clean data}

# Read TXN_FY13 data into R separately - due to the large file
# The directaries depend on customized settings
TXN_FY13 <- read.csv("C:/nlb/dataTest/TXN_FY13_cleaned.csv")
#save(TXN_FY13,file="C:/nlb/dataTest/RData/TXN_FY13.rda")

Lib_Info <- read.csv("C:/nlb/dataTest/Lib_Info_13.csv")
# transform banch type to dummy variable
Lib_Info <- transform(Lib_Info, type_if_mall = ifelse(Branch.Type == "Mall",1,0))
Lib_Info <- transform(Lib_Info, type_if_standalone = ifelse(Branch.Type == "Stand-Alone",1,0))

Lib_Data <- read.csv("C:/nlb/dataTest/Lib_Data_13.csv")

# read population by subzone data
#Subzone_Pop_2013 <- read.csv("C:/nlb/dataTest/Subzone_Pop_2013.csv")
Subzone_Pop_simple <- read.csv("C:/nlb/dataTest/Subzone_Pop_2013.csv")[ ,c('Subzone', 'Population')]

# read patron data
Patron_Dataset_FY13_Subzone <- read.csv("C:/nlb/dataTest/Patron_Dataset_FY13_Subzone.csv")
# remove "Bad Value" & "Missing Value" under Locale Planning ADZID
Patron_Dataset_FY13_Subzone <- Patron_Dataset_FY13_Subzone[(Patron_Dataset_FY13_Subzone$Locale.Planning.ADZID != "Bad Value")&
                                                             (Patron_Dataset_FY13_Subzone$Locale.Planning.ADZID != "Missing Value"),]

Subzone_Distance <- read.csv("C:/nlb/dataTest/Subzone_Distance.csv")

# Removing the transactions where uid = 0 (unidentified)
TXN_FY13_ready <- TXN_FY13[(TXN_FY13$Patron.UID != 0),]

# Adding patrons' information
TXN_FY13_wPatron <- merge(TXN_FY13_ready, Patron_Dataset_FY13_Subzone, by.x="Patron.UID", by.y="Patron.UID", all.x = TRUE)
TXN_FY13_wPatron <- TXN_FY13_wPatron[(TXN_FY13_wPatron$Patron.Birthyear != 1900),]
TXN_FY13_wPatron <- merge(TXN_FY13_wPatron, Subzone_Pop_simple, by.x="Subzone", by.y="Subzone", all.x = TRUE)

# Creates an interaction matrix based on the observed frequencies (automatically)
# market shares are corrected when they do not match the MCI standards (pij = 0)
# by adding absolute value 0.1
ijmatrix_TXN_FY13 <- ijmatrix.create(TXN_FY13_wPatron, "Subzone","Branch.Code", 
                                     correctVar = TRUE, correctVar.val = 0.1)

#library(xlsx)
#write.xlsx(TXN_FY13_wPatron, "C:/nlb/dataTest/TXN_FY13_wPatron.xlsx")

# Adding the distances from each subzone to library
ijmatrix_TXNFY13_dist <- merge(ijmatrix_TXN_FY13, Subzone_Distance, by.x="interaction", by.y="route", all.x = TRUE)
# delete duplicate columns
ijmatrix_TXNFY13_dist$from_Subzone <- NULL
ijmatrix_TXNFY13_dist$to_Branch.Code <- NULL

# Removing the rows where no distance is available
# CKSZ07 is not a valid subzone code from the 2014 shape file extracted from data.gov.sg. So the corresponding transaction records 
# should be double-checked with NLB
ijmatrix_TXNFY13_dist_rect <- ijmatrix_TXNFY13_dist[!is.na(ijmatrix_TXNFY13_dist$Distance_km),]
#test1 <- ijmatrix_TXNFY13_dist[is.na(ijmatrix_TXNFY13_dist$Distance_km),]

# merge various dataset to get the attractiveness value ready
# Include population information
ijmatrix_Full_Txn_Data <- merge(ijmatrix_TXNFY13_dist_rect, Subzone_Pop_simple, by.x="Subzone", by.y="Subzone", all.x = TRUE)
# Include library information
ijmatrix_Full_Txn_Data <- merge(ijmatrix_Full_Txn_Data, Lib_Info, by.x="Branch.Code", by.y="Branch.Code", all.x = TRUE)


#-----------------------------------------------------------------------------------------------------#


# { MCI Model - shopping 4 }

# Below data were also updated in the Huff Model part

#-------------------------------
# Correct values by adding 0.01 to elimate 0, otherwise the variables cannot be passed in mci.transmat function
ijmatrix_Full_Txn_Data$No..of.MRT.Within.1KM <- ijmatrix_Full_Txn_Data$No..of.MRT.Within.1KM + 0.01
ijmatrix_Full_Txn_Data$No..of.Malls.Within.1KM <- ijmatrix_Full_Txn_Data$No..of.Malls.Within.1KM + 0.01
ijmatrix_Full_Txn_Data$No..of.Tuition.Centres.Within.1KM <- ijmatrix_Full_Txn_Data$No..of.Tuition.Centres.Within.1KM + 0.01
ijmatrix_Full_Txn_Data$Population <- ijmatrix_Full_Txn_Data$Population + 0.01
#-------------------------------

# Log-centering transformation of the interaction matrix
ijmatrix_TXN13_transf <- mci.transmat(ijmatrix_Full_Txn_Data, "Subzone","Branch.Code","p_ij_obs","Distance_km",
                                      "Branch.Gross.Floor.Area", "Collection.Size", "type_if_mall","type_if_standalone","Population",
                                      "No..of.MRT.Within.1KM","No..of.Malls.Within.1KM", "No..of.Tuition.Centres.Within.1KM")

# # Plot variable Population to see the distribution before & after transformation
# # Before mci.transmat
# d <- density(ijmatrix_Full_Txn_Data$Population)
# plot(d, type="n", main="Population")
# polygon(d, col="lightgray", border="lightgray")

# # After mci.transmat
# d_t <- density(ijmatrix_TXN13_transf$Population_t)
# plot(d_t, type="n", main="Population")
# polygon(d_t, col="lightgray", border="lightgray")



# # Test to see if the transformation generates any zero/negative value
# test <- ijmatrix_TXN13_transf[(ijmatrix_TXN13_transf$Population_t <= 0),] - TRUE!!!
# However, zero/negative values are invalid in MCI model due to the impossibility of negative market share

# MCI model for the library patronage
# shares: "p_ij_obs", 
# explanatory variables: "Branch.Gross.Floor.Area", "Collection.Size", "type_if_mall","type_if_standalone", 
#                        "Distance_km", "No..of.MRT/Malls/Tuition Centres.Within.1KM", "Population"
mcimodel_lib_patronage <- mci.fit(ijmatrix_Full_Txn_Data, "Subzone","Branch.Code", "p_ij_obs", "Distance_km",
                                  "Branch.Gross.Floor.Area", "Collection.Size","type_if_mall","type_if_standalone","Population",
                                  "No..of.MRT.Within.1KM","No..of.Malls.Within.1KM", "No..of.Tuition.Centres.Within.1KM")
# add population in the mci model to check how it contributes to the patronage
# or number of recidents within 1km of the lib
mcimodel_lib_patronage_t <- mci.fit(ijmatrix_TXN13_transf, "Subzone","Branch.Code", "p_ij_obs_t", "Distance_km_t",
                                  "Branch.Gross.Floor.Area_t", "Collection.Size_t","type_if_mall","type_if_standalone","Population_t",
                                  "No..of.MRT.Within.1KM_t","No..of.Malls.Within.1KM_t", "No..of.Tuition.Centres.Within.1KM_t")

# Use like lm
summary(mcimodel_lib_patronage)

# to add the predicted composite value to the data, for huff model
ijmatrix_Full_Txn_Data$predicted_value <- predict.lm(mcimodel_lib_patronage)

#-----------------------------------------------------------------------------------------------------#


# { Huff Model: huff.fit function }

# {huff.fit}
# This function fits the Huff model with a given interaction matrix by optimizing the attractivity values
# of suppliers/locations iteratively and calculates the market shares/market areas
huff_fit_total <- huff.fit(ijmatrix_Full_Txn_Data, "Subzone", "Branch.Code", "predicted_value", "Distance_km",
                           lambda = -2, dtype= "pow", lambda2 = NULL, Subzone_Pop_simple, "Subzone", "Population",
                           Lib_Data, "Branch.Code", "Number.of.Books.Borrowed", tolerance = 1, iterations = 2, output = "matrix",
                           show_proc = TRUE)

# # Eliminate all negative/zero value in the data and run huff.fit again
# ijmatrix_Full_Txn_Data_positive <- ijmatrix_Full_Txn_Data[(ijmatrix_Full_Txn_Data$predicted_value > 0),]
# huff_fit_total_corr <- huff.fit(ijmatrix_Full_Txn_Data_positive, "Subzone", "Branch.Code", "predicted_value", "Distance_km",
#                            lambda = -2, dtype= "pow", lambda2 = NULL, Subzone_Pop_simple, "Subzone", "Population",
#                            Lib_Data, "Branch.Code", "Number.of.Books.Borrowed", tolerance = 1, iterations = 2, output = "matrix",
#                            show_proc = TRUE)

model.fit(huff_total$total_obs, huff_total$sum_E_j, plotVal = TRUE)

# to check if the data frame contains NA in any column
# any(is.na(ijmatrix_Full_Txn_Data)) -> FALSE


# PROBLEMS:
# 1. "CKSZ07" is not a valid subzone code from the "Master Plan 2014 Subzone Boundary (Web)" shape file extracted from data.gov.sg.
#    So the corresponding transaction records should be double-checked and verified with NLB
# 2. For the subzones with population = 0 in year 2013 (data extracted from Statistics Singapore), there are, however, frequency more than 0,
#    meaning there are people from those subzones visiting libraries, which doesn't make sense (e.g. subzone CHSZ03)
# 3. mci.transmat function generates zero/negative values for all numerical variables (excluding dummy ones), so the purpose of this step is unclear
#    because the transformed output data frame "ijmatrix_TXN13_transf" is never used in any further step and yet, the variables in mci summary
#    all have the "_t" in their names indicating they are after-transformation (?)
# 4. In the huff.fit function, the "lambda" represents a single numeric value for the (exponential) weighting of distance (transport
#    costs0, so shall we replace the default -2 with the weightage of Distance_km we got from MCI summary -1.71 ?
# 5. Due to the existing of negative value in variable "predicated_value", huff.fit function runs with error immediately.
#    (NA does not exist in the whole data frame)
# 6. Even all columns contain only positive values, huff.fit still cannot run successfully


