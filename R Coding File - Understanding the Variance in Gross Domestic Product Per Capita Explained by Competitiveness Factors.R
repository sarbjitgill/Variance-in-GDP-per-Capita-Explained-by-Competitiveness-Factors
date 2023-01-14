rm(list=ls(all=T))

setwd("~/Documents/Sorbonne/Ca'Foscari/R Coding/R Project")

# ============================================================ #
# REPORT SECTION 2: DATA DESCRIPTION

# Step 2.1: Import and tidy datasets

# Importing Global Competitiveness Index Report 2018 Data (produced by the World Economic Forum) and tidying it up to have only the data for TFP Score show up 
library(tidyverse)
library(readxl)
as_tibble(GCIndexFullData <- read_excel("GCI_4.0_2018_Dataset.xlsx", 
                       sheet = "Data", 
                       skip = 3,
                       range = "A4:ES1001",
                       col_types = c(rep("skip", 4), "text", rep("skip", 4), rep("numeric", 140))))
View(GCIndexFullData)

# Removing unwanted rows (observations are in columns. I will address this next)
TFPScore <- GCIndexFullData[-c(1:19, 21:243, 245:391, 393:432, 434:458, 460:470, 472:594, 596:686, 688:797, 799:893, 895:917, 919:996), ]

# Transposing the dataframe to have the observations show up correctly
library(sjmisc)
as_tibble(TFPScore <- TFPScore %>% rotate_df(rn = "Country Name", cn = TRUE))
View(TFPScore)

# Importing the second dataset. It comes from the World Bank Website and it contains the 2017 GDP Values (PPP, Constant).
library(readxl)
as_tibble(GDPData <- read_excel("API_NY.GDP.PCAP.PP.KD_DS2_en_excel_v2_3732022.xls", 
                                sheet = "Data", 
                                skip = 3, 
                                col_types = c("text", "skip", "text", rep("skip", 59), "numeric", rep("skip", 3))))
View(GDPData)

# Renaming country names to match between GDPData and TFPScore dataset. Since TFPScore dataframe is shorter, we'll rename using its values and then drop the observations later with missing values after comibing the datasets.
GDPData <- 
  GDPData %>%
  mutate_at("Country Name", str_replace, "Congo, Dem. Rep.", "Congo, Democratic Rep.")

GDPData <- 
  GDPData %>%
  mutate_at("Country Name", str_replace, "Cabo Verde", "Cape Verde")

GDPData <- 
  GDPData %>%
  mutate_at("Country Name", str_replace, "Cote d'Ivoire", "Côte d'Ivoire")

GDPData <- 
  GDPData %>%
  mutate_at("Country Name", str_replace, "Egypt, Arab Rep.", "Egypt")

GDPData <- 
  GDPData %>%
  mutate_at("Country Name", str_replace, "North Macedonia", "Macedonia, FYR")

GDPData <- 
  GDPData %>%
  mutate_at("Country Name", str_replace, "Venezuela, RB", "Venezuela")

GDPData <- 
  GDPData %>%
  mutate_at("Country Name", str_replace, "Vietnam", "Viet Nam")

GDPData <- 
  GDPData %>%
  mutate_at("Country Name", str_replace, "Yemen, Rep.", "Yemen")

View(GDPData)
  
# Joining the datasets so as to work within a single dataframe
GDPTFP <- left_join(GDPData, TFPScore, by = "Country Name")
View(GDPTFP)

# Renaming Columns
GDPTFP <- rename(GDPTFP, '2017 GDP Per Capita' = '2017'
       , 'Income Group' = 'IncomeGroup'
       , 'Institutions' = '1st pillar: Institutions'
       , 'Infrastructure' = '2nd pillar: Infrastructure'
       , 'ICT Adoption' = '3rd pillar: ICT adoption'
       , 'Macroeconomic Stability' = '4th pillar: Macroeconomic stability'
       , 'Health' = '5th pillar: Health'
       , 'Skills' = '6th pillar: Skills'
       , 'Product Market' = '7th pillar: Product market'
       , 'Labor Market' = '8th pillar: Labor market'
       , 'Financial System' = '9th pillar: Financial system'
       , 'Market Size' = '10th pillar: Market size'
       , 'Business Dynamism' = '11th pillar: Business dynamism'
       , 'Innovation Capability' = '12th pillar: Innovation capability')
View(GDPTFP)
tibble(GDPTFP)

# Dropping all observations with missing values as we cannot use these observations for regression treatment and analysis
GDPTFP <- drop_na(GDPTFP)
View(GDPTFP)
tibble(GDPTFP)

# Changing 'Income Group' variable to a factor variable
GDPTFP <- GDPTFP %>% mutate_at(vars(2), factor)
tibble(GDPTFP)

# Step 2.1: Read data summary
summary(GDPTFP)
***** There are a few variables with a high standard deviation and large gaps between the maximum and minimum values, indicating potential of outliers and influential variables in regression ahead. These include gdppercapita (dependent variable), health, ictadoption, marketsize and innovationcapability.

* Step 2.2: Check correlation among explanatory variables 
correlate institutions infrastructure ictadoption macroeconomicstability health skills productmarket labormarket financialsystem marketsize businessdynamism innovationcapability
***** Quite a few of our variables are correralted (0.70+). But since there are no data entry errors and neither is our data from a different population than most of our data, we'll opt to keep the variables for now and investigate further.

* Step 2.3: Generate logs and scatterplots. Check "kdensity"s of all variables and fit of data of explanatory variables against the Y variable. Select log or level variables for full empirical model best explaining our Y variable. Check to see if variables are signficant.

***** 2.3.1. Dependent Variable: GDP
kdensity gdppercapita, normal
generate loggdp = log(gdppercapita)
kdensity loggdp, normal
********** loggdp appears more normal. It will be investigated ahead

***** 2.3.2. ID Variable: Infrastructure (Cross-Checked with GDP)
kdensity infrastructure, normal
generate loginfrastructure = log(infrastructure)
kdensity loginfrastructure, normal

twoway scatter gdppercapita infrastructure || lfit gdppercapita infrastructure
* Not the best fit
twoway scatter gdppercapita loginfrastructure || lfit gdppercapita loginfrastructure
* Not the best fit. gdppercapita to be discarded

twoway scatter loggdp infrastructure || lfit loggdp infrastructure, name(graph1)

twoway scatter loggdp loginfrastructure || lfit loggdp loginfrastructure

********** The last two scatterplots appear to be similar. Running a quick regression to see which variable explains "loggdp" better
reg loggdp infrastructure
reg loggdp loginfrastructure
********** Keep "infrastructure" and drop "loginfrastructure"


***** 2.3.3. ID Variable: Institutions (Cross-Checked with GDP)
kdensity institutions, normal
generate loginstitutions = log(institutions)
kdensity loginstitutions, normal
********** loginstitutions appears more normal

twoway scatter loggdp institutions || lfit loggdp institutions

twoway scatter loggdp loginstitutions || lfit loggdp loginstitutions, name(graph2)

********** Both scatterplots appear to be similar. Running a quick regression to see which variable explains "loggdp" better
reg loggdp institutions
reg loggdp loginstitutions
********** Keep "loginstitutions" and drop "institutions"


***** 2.3.4. ID Variable: ICT Adoption (Cross-Checked with GDP)
kdensity ictadoption, normal
generate logictadoption = log(ictadoption)
kdensity logictadoption, normal

twoway scatter loggdp ictadoption || lfit loggdp ictadoption, name(graph3)

twoway scatter loggdp logictadoption || lfit loggdp logictadoption

********** Both scatterplots appear to be similar. Running a quick regression to see which variable explains "loggdp" better
reg loggdp ictadoption
reg loggdp logictadoption
********** Keep "ictadoption" and drop "logictadoption"


***** 2.3.5. ID Variable: Macroeconomic Stability (Cross-Checked with GDP)
kdensity macroeconomicstability, normal
generate logmacroeconomicstability = log(macroeconomicstability)
kdensity logmacroeconomicstability, normal

twoway scatter loggdp macroeconomicstability || lfit loggdp macroeconomicstability, name(graph4)

twoway scatter loggdp logmacroeconomicstability || lfit loggdp logmacroeconomicstability

********** Both scatterplots appear to be similar. Running a quick regression to see which variable explains "loggdp" better
reg loggdp macroeconomicstability
reg loggdp logmacroeconomicstability
********** Keep "macroeconomicstability" and drop "logmacroeconomicstability"


***** 2.3.6. ID Variable: Health (Cross-Checked with GDP)
kdensity health, normal
generate loghealth = log(health)
kdensity loghealth, normal

twoway scatter loggdp health || lfit loggdp health, name(graph5)
********** "health" appears to be the better explanatory variable than "loghealth". Keep "health", drop the other

twoway scatter loggdp loghealth || lfit loggdp loghealth

* Checking reg just in case
reg loggdp health
reg loggdp loghealth

***** 2.3.7. ID Variable: Skills (Cross-Checked with GDP)
kdensity skills, normal
generate logskills = log(skills)
kdensity logskills, normal

twoway scatter loggdp skills || lfit loggdp skills, name(graph6)
twoway scatter loggdp logskills || lfit loggdp logskills

********** Both scatterplots appear to be similar. Running a quick regression to see which variable explains "loggdp" better
reg loggdp skills
reg loggdp logskills
********** Keep "skills" and drop "logskills"


***** 2.3.8. ID Variable: Product Market (Cross-Checked with GDP)
kdensity productmarket, normal
generate logproductmarket = log(productmarket)
kdensity logproductmarket, normal

twoway scatter loggdp productmarket || lfit loggdp productmarket, name(graph7)
twoway scatter loggdp logproductmarket || lfit loggdp logproductmarket

********** Both scatterplots appear to be similar. Running a quick regression to see which variable explains "loggdp" better
reg loggdp productmarket
reg loggdp logproductmarket
********** Keep "productmarket" and drop "logproductmarket"


***** 2.3.9. ID Variable: Labor Market (Cross-Checked with GDP)
kdensity labormarket, normal
generate loglabormarket = log(labormarket)
kdensity loglabormarket, normal

twoway scatter loggdp labormarket || lfit loggdp labormarket, name(graph8)
twoway scatter loggdp loglabormarket || lfit loggdp loglabormarket

********** Both scatterplots appear to be similar. Running a quick regression to see which variable explains "loggdp" better
reg loggdp labormarket
reg loggdp loglabormarket
********** Keep "labormarket" and drop "loglabormarket"


***** 2.3.10. ID Variable: Financial System (Cross-Checked with GDP)
kdensity financialsystem, normal
generate logfinancialsystem = log(financialsystem)
kdensity logfinancialsystem, normal

twoway scatter loggdp financialsystem || lfit loggdp financialsystem
twoway scatter loggdp logfinancialsystem || lfit loggdp logfinancialsystem, name(graph9)

********** Both scatterplots appear to be similar. Running a quick regression to see which variable explains "loggdp" better
reg loggdp financialsystem
reg loggdp logfinancialsystem
********** Keep "logfinancialsystem" and drop "financialsystem"


***** 2.3.11. ID Variable: Financial System (Cross-Checked with GDP)
kdensity marketsize, normal
generate logmarketsize = log(marketsize)
kdensity logmarketsize, normal

twoway scatter loggdp marketsize || lfit loggdp marketsize
twoway scatter loggdp logmarketsize || lfit loggdp logmarketsize, name(graph10)

********** Both scatterplots are poor, but appear to be similar. Running a quick regression to see which variable explains "loggdp" better
reg loggdp marketsize
reg loggdp logmarketsize
********** Keep "logmarketsize" and drop "marketsize"


***** 2.3.12. ID Variable: Business Dynamism (Cross-Checked with GDP)
kdensity businessdynamism, normal
generate logbusinessdynamism = log(businessdynamism)
kdensity logbusinessdynamism, normal

twoway scatter loggdp businessdynamism || lfit loggdp businessdynamism, name(graph11)
********** "businessdynamism" appears to be a better fit. Keep "businessdynamism" and drop "logbusinessdynamism"
twoway scatter loggdp logbusinessdynamism || lfit loggdp logbusinessdynamism

* Checking reg just in case
reg loggdp businessdynamism
reg loggdp logbusinessdynamism


***** 2.3.13. ID Variable: Innovation Capability (Cross-Checked with GDP)
kdensity innovationcapability, normal
generate loginnovationcapability = log(innovationcapability)
kdensity loginnovationcapability, normal

twoway scatter loggdp innovationcapability || lfit loggdp innovationcapability
twoway scatter loggdp loginnovationcapability || lfit loggdp loginnovationcapability, name(graph12)
********** "loginnovationcapability" appears to be a better fit. Keep "loginnovationcapability" and drop "innovationcapability"

* Checking reg just in case
reg loggdp innovationcapability
reg loggdp loginnovationcapability


* Step 2.4: Generate graphs of our narrowed variables

graph combine graph1 graph2 graph3 graph4 graph5 graph6 graph7 graph8 graph9 graph10 graph11 graph12


* ============================================================ *
* SECTION 3: ECONOMETRIC METHODOLOGY


* Step 3.1: The Empirical Methodology

***** Based on our research question and data transformations above (logs), we'll use the following variables in our econometrics model:
  ***** Y variable: loggdp
***** X variables: infrastructure loginstitutions ictadoption macroeconomicstability health skills productmarket labormarket logfinancialsystem logmarketsize businessdynamism loginnovationcapability


* Step 3.2: Run the naïve regression
reg loggdp infrastructure loginstitutions ictadoption macroeconomicstability health skills productmarket labormarket logfinancialsystem logmarketsize businessdynamism loginnovationcapability
vif
rvfplot, mlabel(countryname) yline(0)

* The first estimation of our full model (see figure 4, page x) reveals that our global model is significant (fisher test p-value < 0.05). It has an adjusted R-square of 89.17%, explaining 89% of the variance in our dependent variable. However, 8 of our variables are not significant post-regression: institutions (log form), macroeconomic stability, health, product market, labor market, financial system (log form), market size (log form), and innovation capability (log form). On the other hand, the variable “business dynamism” was positively correlated before regression, but it is now negatively correlated. The VIF analysis shows that 8 of our 12 variables have a variance inflation factor of more than five and a mean VIF of 6.16. Near multicollinearity may likely be the issue affecting our model. Finally, our residuals appear mostly well-behaved, but there may be a heteroskedasticity issue. We'll investigate our first regression estimate further to refine it. We will not remove any variables at this stage.


* ============================================================ *
* SECTION 4: OUTLIER / INFLUENTIAL OBSERVATION DETECTION + CONTROLS


* Step 4.1: Detect Outliers

***** 4.1.1. Leverage-versus-squared-residual plot test
reg loggdp infrastructure loginstitutions ictadoption macroeconomicstability health skills productmarket labormarket logfinancialsystem logmarketsize businessdynamism loginnovationcapability
lvr2plot, mlabel(country)

* There are several influential observations and outliers in the dataset. Haiti has the highest leverage and Angola has the largest residual squared. We'll investigate the outliers/influential observations further. Chow test, to be conducted later, may also explain influential observations and outliers.


***** 4.1.1. Observations with high leverage
reg loggdp infrastructure loginstitutions ictadoption macroeconomicstability health skills productmarket labormarket logfinancialsystem logmarketsize businessdynamism loginnovationcapability
predict lev, leverage
stem lev
* Generally, a point with leverage greater than (2k+2)/n should be carefully examined.
display (2*12+2)/136
* .19117647
list countryname lev if lev > .19117647

* Haiti and Eswatini have a high leverage


***** 4.1.2. Observations with high studentized residuals
reg loggdp infrastructure loginstitutions ictadoption macroeconomicstability health skills productmarket labormarket logfinancialsystem logmarketsize businessdynamism loginnovationcapability
predict r, rstudent 
list countryname r if r > 2
list countryname r if r < -2

* We should pay attention to studentized residuals that exceed +2 or -2, and get even more concerned about residuals that exceed +2.5 or -2.5 and even yet more concerned about residuals that exceed +3 or -3. Our outliers per rstudent are: Luxembourg, Ireland, Botswana, Nigeria, Brunei, Kuwait, and Angola.


***** 4.1.3. Create outliers dummies to understand the impact of outliers on the model

* Step 4.1.3.1. Testing the impact of observation "Angola," with the highest studentized residual, on the regression line. Coefficients of the two models are compared (one including Angola and the other exluding Angola)

gen dummy_angola=1 if countryname=="Angola"
replace dummy_angola=0 if dummy_angola==.

reg loggdp infrastructure loginstitutions ictadoption macroeconomicstability health skills productmarket labormarket logfinancialsystem logmarketsize businessdynamism loginnovationcapability if dummy_angola!=1 
est store withoutangola

reg loggdp infrastructure loginstitutions ictadoption macroeconomicstability health skills productmarket labormarket logfinancialsystem logmarketsize businessdynamism loginnovationcapability
est store angola

suest angola withoutangola
test [angola_mean]infrastructure=[withoutangola_mean]infrastructure
test [angola_mean]loginstitutions=[withoutangola_mean]loginstitutions
test [angola_mean]ictadoption=[withoutangola_mean]ictadoption
test [angola_mean]macroeconomicstability=[withoutangola_mean]macroeconomicstability
test [angola_mean]health=[withoutangola_mean]health
test [angola_mean]skills=[withoutangola_mean]skills
test [angola_mean]productmarket=[withoutangola_mean]productmarket
test [angola_mean]labormarket=[withoutangola_mean]labormarket
test [angola_mean]logfinancialsystem=[withoutangola_mean]logfinancialsystem
test [angola_mean]logmarketsize=[withoutangola_mean]logmarketsize
test [angola_mean]businessdynamism=[withoutangola_mean]businessdynamism
test [angola_mean]loginnovationcapability=[withoutangola_mean]loginnovationcapability
* Comparing the two regression models, one including all observations and the other excluding "Angola", the difference between all coefficient values of explanatory variables is not signficant on the regression line. Now we move forward and investigate the impact of exluding all potential outliers.


* Step 4.1.3.2. Testing the impact on the regression line after removing all identified outliers. Coefficients of the two models are compared (one with all observations and the other without idefnitied outliers)

* We create dummies for all outlier values and compare the model with and without all outliers

gen dummy_haiti=1 if countryname=="Haiti"
replace dummy_haiti=0 if dummy_haiti==.

gen dummy_eswatini=1 if countryname=="Eswatini"
replace dummy_eswatini=0 if dummy_eswatini==.

gen dummy_kuwait=1 if countryname=="Kuwait"
replace dummy_kuwait=0 if dummy_kuwait==.

gen dummy_brunei=1 if countryname=="Brunei Darussalam"
replace dummy_brunei=0 if dummy_brunei==.

gen dummy_nigeria=1 if countryname=="Nigeria"
replace dummy_nigeria=0 if dummy_nigeria==.

gen dummy_botswana=1 if countryname=="Botswana"
replace dummy_botswana=0 if dummy_botswana==.

gen dummy_china=1 if countryname=="China"
replace dummy_china=0 if dummy_china==.

gen dummy_tajikistan=1 if countryname=="Tajikistan"
replace dummy_tajikistan=0 if dummy_tajikistan==.

gen dummy_luxembourg=1 if countryname=="Luxembourg"
replace dummy_luxembourg=0 if dummy_luxembourg==.

gen dummy_ireland=1 if countryname=="Ireland"
replace dummy_ireland=0 if dummy_ireland==.

gen dummy_outliers = dummy_angola + dummy_kuwait + dummy_haiti + dummy_eswatini + dummy_brunei + dummy_nigeria + dummy_botswana + dummy_china + dummy_tajikistan + dummy_luxembourg + dummy_ireland
replace dummy_outliers=0 if dummy_outliers==.

reg loggdp infrastructure loginstitutions ictadoption macroeconomicstability health skills productmarket labormarket logfinancialsystem logmarketsize businessdynamism loginnovationcapability if dummy_outliers!=1 
est sto withoutoutliers

reg loggdp infrastructure loginstitutions ictadoption macroeconomicstability health skills productmarket labormarket logfinancialsystem logmarketsize businessdynamism loginnovationcapability
est sto withoutliers

suest withoutliers withoutoutliers
test [withoutliers_mean]infrastructure=[withoutoutliers_mean]infrastructure
test [withoutliers_mean]loginstitutions=[withoutoutliers_mean]loginstitutions
test [withoutliers_mean]ictadoption=[withoutoutliers_mean]ictadoption
test [withoutliers_mean]macroeconomicstability=[withoutoutliers_mean]macroeconomicstability
test [withoutliers_mean]health=[withoutoutliers_mean]health
test [withoutliers_mean]skills=[withoutoutliers_mean]skills
test [withoutliers_mean]productmarket=[withoutoutliers_mean]productmarket
test [withoutliers_mean]labormarket=[withoutoutliers_mean]labormarket
test [withoutliers_mean]logfinancialsystem=[withoutoutliers_mean]logfinancialsystem
test [withoutliers_mean]logmarketsize=[withoutoutliers_mean]logmarketsize
test [withoutliers_mean]businessdynamism=[withoutoutliers_mean]businessdynamism
test [withoutliers_mean]loginnovationcapability=[withoutoutliers_mean]loginnovationcapability
test [withoutliers_mean]_cons=[withoutoutliers_mean]_cons

* Comparing the two regression models, one including all observations and the other without identified outliers and influential observations, the difference between coefficient values of all explanatory variables is not signficant. Thus, the regression line is not signficantly impacted. We'll move forward with all observations in our model.


* ============================================================ *
* SECTION 5: COMMMENTS ON REGRESSION

* Step 5.1: Comments on regression
reg loggdp infrastructure loginstitutions ictadoption macroeconomicstability health skills productmarket labormarket logfinancialsystem logmarketsize businessdynamism loginnovationcapability
* We only have one new comment to add to our first regression estimate comments. Our model does contain outliers. However, they don't have a significant impact on the coefficients of the explanatory variables when removed. The line does improves by 4% adjusted R-square. However, to avoid bias, we will not exclude these observations. 

* To continue to improve our model and make it more precise, we'll move forward with heteroskedasticity diagnostics (and adjustment if relevant), examine subsamples through Chow test (and adjust, if needed), and at the end, run a backward regression to clean our model and reduce VIF.


* ============================================================ *
* SECTION 6: DIAGNOSTICS & CORRECTION: HETEROSKEDASTICITY AND/OR AUTOCORRELATION


* Step 6.1: Heteroscedasticity Diasgnositics
reg loggdp infrastructure loginstitutions ictadoption macroeconomicstability health skills productmarket labormarket logfinancialsystem logmarketsize businessdynamism loginnovationcapability
rvfplot, mlabel(countryname) yline(0)
estat hettest
* Since the p-value of our test is 0.0785, the chi-statistic is not significant. We fail to reject the null hypothesis: "constant variance". Therefore, we conclude that our model does not meet the criterion for heteroskedasticity and we do need to make any adjustments to make it homoskedastic.


* Step 6.2: Heteroscedasticity Correction
* Not applicable as our model does not meet the criterion for correction.


* Step 6.3: Autocorrelation Diagnosis
* Not applicable to this project as our data is panel data and not time-series.


* ============================================================ *
* SECTION 7: TESTS & VARIABLE SELECTION

* Step 7.1: Test for normality of regression
reg loggdp infrastructure loginstitutions ictadoption macroeconomicstability health skills productmarket labormarket logfinancialsystem logmarketsize businessdynamism loginnovationcapability
predict e, resid
kdensity e, normal
* Looks close to normal
qnorm e
* The residuals are well-behaved for the most part but they do skew near the edge on the right.
pnorm e
* Looks close to normal
swilk e 
* Shapiro-Wilk W test for normal data. 
* Our p-value is 0.07981. Thus we fail to reject the null hypothesis: "the distribution of the residuals is normal." We conclude that residuals are normally distributed.


* Step 7.2: Global F-test
reg loggdp infrastructure loginstitutions ictadoption macroeconomicstability health skills productmarket labormarket logfinancialsystem logmarketsize businessdynamism loginnovationcapability
* We reject the null-hypothesis that our model is insignficant. We conclude that at least one of our variables is signficant.


* Step 7.3: Coefficient test
* We will save the coefficient testing and variable selection until after running the Chow test for structural change, as else our constrained model may not provide us with a good understanding of structural differences between developed and developing countries.


* Step 7.4: Check for omitted variable bias
reg loggdp infrastructure loginstitutions ictadoption macroeconomicstability health skills productmarket labormarket logfinancialsystem logmarketsize businessdynamism loginnovationcapability
ovtest

* Our p-value is 0.2879. Thus, we cannot reject our hypothesis: "model has no omitted variables." So we conclude that we do not need more variables.


* Step 7.5: Test for model specificiation
reg loggdp infrastructure loginstitutions ictadoption macroeconomicstability health skills productmarket labormarket logfinancialsystem logmarketsize businessdynamism loginnovationcapability
linktest

* The p-value of _hatsq (0.860) is not significant. Thus, we cannot reject our hypothesis: "there is no specification error." So we conclude that our model is correctly specified.


* Step 7.6: Test for multicollinearity
reg loggdp infrastructure loginstitutions ictadoption macroeconomicstability health skills productmarket labormarket logfinancialsystem logmarketsize businessdynamism loginnovationcapability
vif
* The VIF analysis shows that 8 of our 12 variables have a VIF score of more than 5. Near multicollinearity is likely an issue affecting our model. We will revisit multicollinearity after the Chow test and variable selection for our final model.

* Step 7.7: Select variables
* We'll first run the Chow test with all variables to test for structual change and later select final variables. We'll use backward stepwise to clean our model.


* ============================================================ *
* SECTION 8: TEST FOR STRUCTURAL CHANGE, IF RELEVANT

* Now we'll test for structural change in our model using the Chow test

* Step 8.1: Generate dummies for developed and developing economies
tab incomeclassification2018wb, generate(incomeclassification_dum)
generate developed_economy = dummy_highincome + dummy_uppermiddle
generate developing_economy = dummy_lowincome + dummy_lowermiddle

* Step 8.2: Run the Chow test via augmented regression (A single model to compute Chow statistic)

generate ddinfrastructure=developed_economy*infrastructure
generate ddloginstitutions=developed_economy*loginstitutions
generate ddictadoption=developed_economy*ictadoption
generate ddmacroeconomicstability=developed_economy*macroeconomicstability
generate ddhealth=developed_economy*health
generate ddskills=developed_economy*skills
generate ddproductmarket=developed_economy*productmarket
generate ddlabormarket=developed_economy*labormarket
generate ddlogfinancialsystem=developed_economy*logfinancialsystem
generate ddlogmarketsize=developed_economy*logmarketsize
generate ddbusinessdynamism=developed_economy*businessdynamism
generate ddloginnovationcapability=developed_economy*loginnovationcapability      

reg loggdp infrastructure loginstitutions ictadoption macroeconomicstability health skills productmarket labormarket logfinancialsystem logmarketsize businessdynamism loginnovationcapability ddinfrastructure ddloginstitutions ddictadoption ddmacroeconomicstability ddhealth ddskills ddproductmarket ddlabormarket ddlogfinancialsystem ddlogmarketsize ddbusinessdynamism ddloginnovationcapability developed_economy

test ddinfrastructure ddloginstitutions ddictadoption ddmacroeconomicstability ddhealth ddskills ddproductmarket ddlabormarket ddlogfinancialsystem ddlogmarketsize ddbusinessdynamism ddloginnovationcapability developed_economy

* Globally, developed and developing economies differ in explaining the variance in GDP per captia through our explanatory variables. Thus, due to this structural change, our final model will be composed of 2 regression models, one for the developed economies and the other for the developing.


* ============================================================ *
  * SECTION 9: PRESENTATION OF THE FINAL ECONOMETRIC MODEL

******* PLEASE NOTE that our models were not revised after the Chow test. To stick to the requirements of this paper—the instructions and the paper length—we are presenting our final models after merely running the backward stepwise method on our full model for our subsamples. Ideally, we would investigate the new models and subsamples after the Chow test, however, this is out of the scope for this assignment.


* Step 9.1: Econometric Model for the Developed Economies
reg loggdp infrastructure loginstitutions ictadoption macroeconomicstability health skills productmarket labormarket logfinancialsystem logmarketsize businessdynamism loginnovationcapability if developed_economy

stepwise, pr(0.05): reg loggdp infrastructure loginstitutions ictadoption macroeconomicstability health skills productmarket labormarket logfinancialsystem logmarketsize businessdynamism loginnovationcapability if developed_economy

******* FINAL MODEL FOR DEVELOPED ECONOMIES - PRESENTATION ******
  reg loggdp infrastructure loginstitutions ictadoption if developed_economy
vif

* Our final model is globally significant (fisher test p-value < 0.05) and it explains 71.78% (adjusted r-squared) of the variance in our dependent variable: 2017 real GDP per capita (PPP). Each of our final variables is significant (t-statistic p-value < 0.05) and positively correlated with GDP. Our model has a mean VIF of 2.94 and VIF is less than 5 for each of our final variables. Thus near multicollinearity is low in our model. Model coefficient estimates and variables are to be interpreted in the following manner for developed economies:
  
  ***** 1. Infrastructure: One unit of increase in 2017 GCI infrastructure score (scores range: 0-100, continuous), correlates to an estimated 1.88% increase in 2017 GDP per capita (PPP) for developed economies in our sample.

***** 2. ICT Adoption: One unit of increase in 2017 GCI ICT adoption score (scores range: 0-100, continuous), correlates to an estimated 1.39% increase in 2017 GDP per capita (PPP) for developed economies in our sample.

***** 3. Institutions: One percent of the increase in the 2017 GCI institutions score, correlates to an estimated 1.20% increase in 2017 GDP per capita (PPP) for developed economies in our sample.


* Step 9.2: Econometric model for the Developing Economies
reg loggdp infrastructure loginstitutions ictadoption macroeconomicstability health skills productmarket labormarket logfinancialsystem logmarketsize businessdynamism loginnovationcapability if developing_economy

stepwise, pr(0.05): reg loggdp infrastructure loginstitutions ictadoption macroeconomicstability health skills productmarket labormarket logfinancialsystem logmarketsize businessdynamism loginnovationcapability if developing_economy

******* FINAL MODEL FOR DEVELOPING ECONOMIES - PRESENTATION ******
  reg loggdp infrastructure loginstitutions ictadoption if developing_economy
vif
rvfplot, mlabel(countryname) yline(0)
estat hettest
lvr2plot, mlabel(countryname)
display (2*3)/51
avplot loginstitutions, mlabel(countryname)

* Our final model is globally significant (fisher test p-value < 0.05) and it explains 71.30% (adjusted r-squared) of the variance in our dependent variable: 2017 real GDP per capita (PPP). Each of our final variables is significant (t-statistic p-value < 0.05). Two of these variables are positively correlated with GDP: infrastructure and ICT adoption. One is negatively correlated: loginstitutions, which is unexpected as one-on-one it is positively correlated. Ideally, this variable and model would be further investigated and tested. However, due to the above-mentioned constraints, we will not be able to refine the predicted model further but we’ll offer this brief explanation: high leverage observations, those exceeding the leverage value of .11764706 (see figures 17 and 18, page 17), are likely influencing the model fit. We’ll now continue with our final model description.

* Our model has a mean VIF of 2.01 and VIF is less than 5 for each of our final variables. Thus near multicollinearity is low in our model. Model coefficient estimates and variables are to be interpreted in the following manner for developing economies:
  
  ***** 1. Infrastructure: One unit of increase in 2017 GCI infrastructure score (scores range: 0-100, continuous) correlates to an estimated 3.95% increase in 2017 GDP per capita (PPP) for developing economies in our sample.

***** 2. ICT Adoption: One unit of increase in 2017 GCI ICT adoption score (scores range: 0-100, continuous) correlates to an estimated 2.40% increase in 2017 GDP per capita (PPP) for developing economies sample.

***** 3. Institutions: One percent of the increase in the 2017 GCI institutions score correlates to an estimated 1.30% decrease in 2017 GDP per capita (PPP) for developing economies in our sample.


* ============================================================ *
  * SECTION 10: CONCLUSION

* Findings and analysis are in our report.


* ============================================================ *
  * THANK YOU, PROFESSOR, FOR GIVING US THE OPPORTUNITY TO WORK ON THIS FUN PROJECT :-)
