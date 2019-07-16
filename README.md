# PREDICT Behavioral Repository Instructions

***Unless otherwise noted, you will need to follow these instructions (in sequential order) every time you want to create a new report***

First, Mac users may want to install GDAL, which is required to run the site-maps script. If you are not interested in the site maps, there is no need to install GDAL. Window users can ignore, as GDAL is already installed for you. This only needs to be done ONCE.  

  1. Install *brew*:
      In the Terminal, copy and paste the following line of code and press 'enter': 
      `/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)`  
  2. Install *GDAL*:
      In the Terminal, copy and paste the following line of code and press 'enter': 
      `brew install gdal`

**Step 1**: Open user-inputs.R script from the predict-behav-report folder

**Step 2**: Load packages needed to run reports  (line 4)

-   If you get the following message in the console: "Do you want to install from sources the package which needs compilation? (Yes/no/cancel)" ---> respond "no"  

**Step 3**: Load functions (lines 8-10)

**Step 4**: Enter country, illness, and taxa contact variables of interest (lines 14-23)

-   All responses must be in quotations

-   *country*: select ONE country only

-   *illness_outcomes*: select up to 4 illness outcomes (separated by commas; note spelling)
    -   possible outcomes are: "sari", "ili", "encephalitis", "hemorrhagic_fever"
    -   the outcomes you specify will be included in the tabular report and lasso report
    -   there is also the option to not enter any illness (`illness_outcomes <- NULL`)
            
-   *heatmap_taxa_outcomes*: select up to 14 taxa outcomes (separated by commas; note spelling)  
    -   the outcomes you specify will be included in the heatmap report
    -   possible outcomes are: "rodents", "bats", "nhp", "birds", "poultry", "carnivores", "ungulates", "pangolins", "goats_sheep", "camels", "swine", "cattle", "dogs", "cats"
    -   at least one taxa MUST be included
    -   `heatmap_taxa_outcomes <- taxa_names` will include all taxa  
          
-   *tabular_lasso_taxa_outcomes*: select up to 14 taxa types (separated by commas; note spelling)
    -   the outcomes you specify will be included in the tabular report and lasso report
    -   possible outcomes are: "rodents", "bats", "nhp", "birds", "poultry", "carnivores", "ungulates", "pangolins", "goats_sheep", "camels", "swine", "cattle", "dogs", "cats", "goats_sheep", "camels", "swine", "cattle", "dogs", "cats"
    -   there is also the option to not enter any taxa (`tabular_lasso_taxa_outcomes <- NULL`)
             
**Step 5**: Confirm that illness_outcomes, heatmap_taxa_outcomes, tabular_lasso_taxa_outcomes are recognized (lines 29-31): 

**Step 6**: Create discrete categories for continuous variables to run in the lasso (lines 35-38)

**Step 7**: Download EIDITH command (line 43)

-   if TRUE, eidith data will download (this is only necessary if there have been recent updates made to eidith)

-   if FALSE, the most recently downloaded version eidith will be used (recommended)

**Step 8**: Run Lasso command (line 49)

-   if  TRUE, the lasso will run for all selected illness_outcomes and tabular_lasso_taxa_outcomes 

    -   Note: the lasso takes a long time to run (up to several hours in some cases); time to run is increased with more selected outcomes

    -   Once the lasso has been run for a specific taxa or illness, it is saved in the outputs folder and does not need to be run again
    
    -   **WARNING**: If the outcomes of interest (taxa or illness) have low or no positive responses (i.e., low prevalence), then the lasso will ultimately fail (i.e., you will receive an error message when you try to create the lasso report). Prior to running the lasso, check that the prevalence of each outcome you selected is at least 10%. You can check prevalence for all of the taxa and illness outcomes by running lines 59-65.

-   if FALSE, the lasso will not run

    -   Note: You can still view the lasso report if you select FALSE - you just need to make sure that you have already run the lasso with your selected taxa and illness outcomes. This can be checked in the outputs/lasso-figs folder.

**Step 9**: Create csv files of input data to be used in the reports (lines 53-54)

-   Note: if you selected download_fresh <- TRUE, a new version of eidith will download
    -   If this is your first time downloading EIDITH in R, you will be asked for your EIDITH_NAME and EIDTH_PASSWORD. These will only need to be entered once.
    -   After downloading, you will see a message in the console stating that there are missing tables in the local EIDITH database.  Respond that you do not want to download the missing tables by entering `2` into the console.

-   csv files are stored in the data folder

**Step 10**: Run the prevalence report (lines 59-65)

-   This is used to inform which taxa and illness outcomes you select (step 4).  The report is saved in outputs/reports as "[country]-outcome-prevalence.html".
    
**Step 11**: Select which reports you want to run (line 71)

-   Options include: "summary-report", "tabular-report". "site-maps", "heatmaps", "lasso"
  
**Step 12**: Create selected reports (line 76)

   -   All reports can be found in the outputs/reports folder
   
   -   All figures can be found in their associated folder within the outputs folder
    
   -   Note: if you selected TRUE for run_lasso <- TRUE (step 8), the lasso will run at this time (and may take a long time)
  
  
*Additional Information*
Every time the illness_outcomes, heatmap_taxa_outcomes, and/or tabular_lasso_taxa_outcomes are changed in the user-inputs.R script, new Tabular reports, Heatmap reports, and Lasso reports are generated in the outputs/reports folder. Unless you change the country of analysis, the Summary report and Sitemaps report will remain the same.



**APPENDIX**

I. Variable Names

1. New variables (these are variables that we create in the report and that do not explicitly appear in the human questionnaire)

      *crowding index*                             
      Definition: # of people in dwelling (including respondent) /# of rooms in dwelling (excluding bathroom and kitchen)
      Note: higher crowding index indicates a more crowded dwelling                   

      *taxa contact-direct*
      Includes the following contact types: pets; handled; raised; cooked/handled; eaten raw/undercooked; eaten sick; 
      found dead/collected; scratched/bitten; hunted/trapped; slaughtered

      *taxa contact-indirect*
      Includes the following contact types: feces in or near food; in house

      *age group*
      Definition: created 4 age groupings, utilized in the Heatmaps report and Lasso report
      Response options: under 18; 18 to 40; 41 to 60; over 60


2. Questionnaire variables

*Demographics*

-   Question: How many other people live in the dwelling where you live? (Q11)
    -   Summary report name: Number of People Living in Dwelling
    -   Definition: total number of individuals living in the dwelling, excluding the respondent

-   Question: How many in the dwelling are children less than 5 years old? (Q12)
    -   Summary report name: Number of Children in Household
    -   Definition: total number of children under 5 in dwelling, including the respondent (if applicable)

-   Question: How many in the dwelling are male? (Q13)
    -   Summary report name: Number of Males in Household
    -   Definition: total number of males in dwelling, including the respondent (if applicable)

-   Question: How many rooms are there in the dwelling where you live? (Q14)
    -   Summary report name: Number of Rooms in Dwelling
    -   Definition: total number of rooms in dwelling, excluding kitchen and bathroom

-   Question: Is the dwelling a permanent structure (that cannot be moved)? (Q15)
    -   Summary/Tabular report name: Dwelling Type
    -   Lasso name: dwelling
    -   Response options: permanent structure; temporary settlement

-   Question: Do you treat your drinking water? (Q17)
    -   Summary/Tabular report name: Is Water Treated
    -   Lasso name: water treated
    -   Response options: yes; no

-   Question: Is your source of drinking water ever used by animals? (Q19)
    -   Summary/Tabular report name: Drinking Water Source Used by Animals
    -   Lasso name: water used by animals
    -   Response options: yes; no

-   Question: In your dwelling is there a dedicated location for human waste? (Q20)
    -   Summary/Tabular report name: Dedicated Location for Human Waste in Dwelling
    -   Lasso name: dedicated location for waste
    -   Response options: yes; no

-   Question: Since this time last year what are the activities you have done to earn your livelihood? Select all that apply (Q24)
    -   Summary report name: Livelihood Activities (past year) All
    -   Tabular report name: 'livelihood name' (see response options)
    -   Lasso report name: livelihood (with livelihood name - see response options)
    -   Response options: 
          (primary grouping)
        -   extraction of minerals, gas, oil timber
        -   crop production
        -   wildlife restaurant business
        -   wild/exotic animal trade/market business
        -   rancher/farmer animal production business
        -   meat processing, slaughterhouse, abattoir
        -   zoo/sanctuary animal health care; protected area worker
        -   protected area worker
        -   hunter/trapper/fisher
        -   forager/gatherer/non-timber forest product collector
        -   migrant labor
        -   nurse, doctor, traditional healer, community health worker
        -   construction
      
          (secondary grouping)
        -   animal health provider/veterinarian
        -   student
        -   non-animal business
        -   homemaker
        -   unemployed
        -   military
        -   non-wildlife restaurant business
        -   livestock/domestic animal/product trade
        -   child
        -   grocery (vegetables)
        -   other
      
-   Question: If more than one activity was selected, what is the activity on which you spend the most time since last year? Select one option (Q25)
    -   Summary report name: Livelihood Activities (past year) Primary
    -   Response options: 
        -   extraction of minerals, gas, oil timber
        -   crop production
        -   wildlife restaurant business
        -   wild/exotic animal trade/market business
        -   rancher/farmer animal production business
        -   meat processing, slaughterhouse, abattoir
        -   zoo/sanctuary animal health care; protected area worker
        -   hunter/trapper/fisher
        -   forager/gatherer/non-timber forest product collector
        -   migrant labor
        -   nurse, doctor, traditional healer, community health worker
        -   construction
        -   other (includes all livelihoods from secondary grouping in Q24)


*Medical History*

-   Question: Where do you usually get treatment for medical problems? Select all that apply (Q28)
    -   Summary report name: Location of Treatment for Medical Problems
    -   *Tabular report name: expanded out to analyze individual response options
    -   Lasso report name: treatment
    -   *Response options: clinic/health center; hospital; mobile clinic; community health worker; traditional healer; dispensary or pharmacy


*Movement*

-   Question: Whey have you traveled? Select all that apply (Q38)
    -   *Tabular report name: expanded out to analyze individual response options
    -   Lasso report name: travelled
    -   *Response options: work; visit family; moved; religious reasons; holiday/vacation; go to hospital/seek medical care; go to market; other

    -   Note: only refers to travel within previous year


*Type of Animal Contact*

*The following taxa contact questions only refer to contact within the previous year

-   Question: Has an animal lived as a pet in or near your dwelling? (Q39)
    -   Tabular report name: Pet in Dwelling
    -   Heatmap/Lasso name: pets
    -   Response options: yes; no
    -   Type of contact: direct

-   Question: Have you handled live animals? (Q40)
    -   Tabular report name: Handled Animals
    -   Heatmap/Lasso name: handled
    -   Response options: yes; no
    -   Type of contact: direct

-   Question: Have you raised live animals? (Q41)
    -   Tabular report name: Raised Animals
    -   Heatmap/Lasso name: raised
    -   Response options: yes; no
    -   Type of contact: direct

-   Question: Have you shared a water source with animals for washing? (Q43)
    -   Tabular report name: Shared Water for Washing
    -   Heatmap/Lasso name: N/A (is not an option)
    -   Response options: yes; no; don't know
    -   Type of contact: N/A

-   Question: Have you seen animal feces in or near food before you have eaten it?? (Q43)
    -   Tabular report name: Animal Feces in/near Food
    -   Heatmap/Lasso name: feces in or near food
    -   Response options: yes; no
    -   Type of contact: indirect

-   Question: Have you eaten food after an animal has touched or damaged it? (Q44)
    -   Tabular report name: Eaten Food Touched/Damaged by Animals
    -   Heatmap/Lasso name: N/A (is not an option)
    -   Response options: yes; no; don't know
    -   Type of contact: N/A

-   Question: Do any animals come inside the dwelling where you live? (Q45)
    -   Tabular report name: Animals inside Dwelling
    -   Heatmap/Lasso name: in house
    -   Response options: yes; no
    -   Type of contact: indirect

-   Question: Have you cooked or handled meat, organs or blood from a recently killed animal? (Q46)
    -   Tabular report name: Cooked/Handled Meat
    -   Heatmap/Lasso name: cooked/handled
    -   Response options: yes; no
    -   Type of contact: direct

-   Question: Have you eaten raw or undercooked meat or organs or blood? (Q47)
    -   Tabular report name: Eaten Raw/Undercooked Meat
    -   Heatmap/Lasso name: eaten raw/undercooked
    -   Response options: yes; no
    -   Type of contact: direct

-   Question: Have you eaten an animal that you know was not well/sick? (Q48)
    -   Tabular report name: Eaten Sick Animal
    -   Heatmap/Lasso name: eaten sick
    -   Response options: yes; no; don't know
    -   Type of contact: direct

-   Question: Have you found a dead animal and collected it to eat or share? (Q49)
    -   Tabular report name: Found Dead Animal to Eat
    -   *Heatmap/Lasso name: found dead/collected (combined with Q50)
    -   Response options: yes; no
    -   Type of contact: direct

-   Question: Have you found a dead animal and collected it to sell it? (Q50)
    -   Tabular report name: Found Dead Animal to Sell
    -   *Heatmap/Lasso name: found dead/collected (combined with Q49)
    -   Response options: yes; no
    -   Type of contact: direct

-   Question: Have you been scratched or bitten by an animal? (Q51)
    -   Tabular report name: Scratched/Bitten
    -   Heatmap/Lasso name: scratched/bitten
    -   Response options: yes; no
    -   Type of contact: direct

-   Question: Have you hunted or trapped an animal? (Q52)
    -   Tabular report name: Hunted/Trapped Animal
    -   Heatmap/Lasso name: hunted/trapped
    -   Response options: yes; no
    -   Type of contact: direct

-   Question: Have you slaughtered an animal? (Q53)
    -   Tabular report name: Slaughtered Animal
    -   Heatmap/Lasso name: slaughtered
    -   Response options: yes; no
    -   Type of contact: direct


*Knowledge, Attitude, and Skills*

-   Question: The last time you were scratched or bitten, or cut yourself while butchering or slaughtering, what did you do? (Q54)
    -   Summary/Tabular report name: Action Taken Last Time Scratched, Bitten or Cut While Butchering or Slaughtering
    -   Lasso name: scratched bitten
    -   Response options: treated; untreated; N/A (N/A only included in the summary report)
        -   'treated' includes: wash wound with soap and water; visit doctor
        -   'untreated' includes: let someone else take over; rinse wound with water; bandage wound; nothing-kept working
        -   'N/A' includes: N/A; never butcher or slaughter

-   Question: Are there risks associated with slaughtering or butchering with an open wound? (Q55)
    -   Summary/Tabular report name: Risks Associated when Slaughtering or Butchering with Open Wound
    -   Lasso name: risk open wound (indicates "yes" response)
    -   Response options: yes; no; don't know; other (don't know and other only included in the Summary report)
        -   'yes' includes: yes, but I don't know what they are; yes, it can make you sick; yes, it can poison you; yes, it can infect you with a disease
        -   'no' includes: no; don't know; other (Tabular report and Lasso only)

-   Question: Are you worried about disease or disease outbreaks in live animals in your local market? (Q57)
    -   Summary/Tabular report name: Worried about Diseases in Live Animals in Market
    -   Lasso name: worried about disease
    -   Response options: yes; no


**II. Report Information and Interpretation**

*1. Summary Report:*
The summary report provides counts, frequency percentages (for categorical variables) and averages/means (for numerical variables) of multiple site and respondent characteristics. 

-   Note: Any categorical response entered as "N/A", "missing", or "unknown" is included in the count and frequency percentages. A categorical variable with all responses "missing" is indicated by --. Numerical variables without any responses (i.e. all responses are left blank/labeled NA) are also indicated by --. If there are only a few cells with blank/NA numeric values, the mean will still be calculated, but those blank values are dropped from the calculation.


*2. Tabular Report:*
The tabular report consists of 4 sections:

-   a. Categorical Tables
-   b. Confidence Interval Plots
-   c. Numeric Tables
-   d. Median Plots


-   Note 1: variables in the Numeric Tables and Median Plots remain the same, regardless of the outcome variable. However, depending on the type of outcome variable (whether illness or taxa contact), the variables in the Categorical Tables and Confidence Interval Plots do change slightly.

-   Note 2: the specific livelihoods listed in the Categorical Tables and Confidence Interval Plots refers to ANY livelihood indicated within the previous year (see: Q24 in the human behavior questionnaire and/or 'Livelihood Activities (past year) All' in the Summary Report).

-   Note 3: any categorical response entered as "N/A" will be counted as a "no" response (with the exception of Q54 - Action Taken Last Time Scratched, Bitten, or Cut while Butchering or Slaughtering; "N/A" responses are excluded from the analysis). Any categorical response entered as "missing" or is left blank will be excluded from the analysis. Any numeric response that is left blank will also be excluded from the analysis. 

    -   The Categorical Tables provide 3 figures, parsed out by site and aggregated: 
        -   outcome variable count (x): "number of successes" (i.e. number of positive outcomes)
        -   total site/aggregate count (n): "number of trials"
        -   binomial probability with 95% confidence interval: 
            -   binomial probability (p = x/n): the probability of successes (x) given (n) number of               trials
            -   95% confidence interval: the range with which we expect 95% of the binomial                        probability outcomes (p) to occur given a set number of successes (x) and "trials" (n)

    -  The Confidence Interval Plots provide a visual representation of the Categorical Tables. When comparing the confidence interval plots, bars that do NOT overlap generally indicate                statistically significant differences between the levels of the predictor variable with             regards to the outcome variable. Plots with overlapping bars do not necessarily imply that          there is no statistically significant variation between the levels of the predictor variable        (especially if the bars overlap just slightly), but will require additional analyses (for           example: a chi-square test, fisher-exact test, or logistic regression). Bars with a very large       range should be interpreted with caution (especially those where the binomial probability           point is located at the very top or bottom of the range), as they generally refer to variables       with very extreme (high or low) counts.

    -   The Numeric Tables provide 2 figures, parsed out by site and aggregated:
        -   count for each level of the outcome (yes/no)
            -   within each site, the count of each outcome level will equal the total respondent                  count
        -   median with 95% confidence interval
            -   median: we used the median instead of the mean to account for outliers and non-normal distributions
            -   95% confidence interval: range with which we are 95% certain that the true population median occurs

    -   The Median Plots are visual representations of the Numeric Tables. The interpretation of           these plots is similar to the Confidence Interval Plots. 


**Sample tabular report interpretations:**

-   outcome variable: bat contact
-   categorical predictor: gender (male/female)
-   numerical predictor: age

*Sample 1: categorical*

                  bat contact       total count     Binomial Probability (95% CI)  
      
    Gender
        male          20              50                0.40 (0.28 - 0.54)
        female        10              100               0.10 (0.03 - 0.20)


Interpretation: Within the sample, males have between a 40% probability of having had contact with bats (95% CI, 0.28 - 0.54), compared to a 10% probability of bat contact among women (95% CI, 0.03 - 0.20). The probability of bat contact is significantly higher among men, since the 95% confidence intervals do not overlap.

-   Note: the Confidence Interval Plots would show two bars that do not overlap.

*Sample 2: numerical*

                            count       Median (95% CI)  
      
    Age
        bat contact           30          40 (36 - 44)
        no bat contact        120         45 (40 - 51)


Interpretation: Within the sample, individuals with bat contact have a median age of 40 (95% CI, 36 - 44), compared to those without bat contact who have a median age of 45 (95% CI, 40 - 51). Given that the 95% confidence intervals do overlap, there is no statistically significant difference in age among those with and without bat contact.

-   Note: the Median Plots would also show two bars that do overlap.


**3. Lasso Report:**
The lasso report shows which variables are positively (indicated by an orange dot; odds ratio greater than one) and inversely (indicated by a purple dot; odds ratio less than one) associated with the outcome variable of interest. The lasso itself removes predictor variables that are highly correlated and ranks variables based on the coefficients.

-   Note 1: only categorical predictor and outcome variables are included int the lasso. Age is divided into 4 groups (under 18; 18 to 40; 41 to 60; over 60) and crowding index is divided into 3 groups (less than 1; 1 to 3; over 3).

-   Note 2: unlike the tabular report, the lasso report considers any response that is "missing", "N/A" or "unknown" as a "no" response. 

There are 2 coefficients presented in the lasso:
    -   s: shrinkage term
        -   ranks the "importance" of each predictor on the outcome variable
        -   s = 0 indicates that within the entire model (that includes all the predictor                       variables), that particular variable is not a useful predictor
        -   s = 1 and s = -1 indicate a predictor variable that has a high amount of influence                 within the model; 1 indicates a positive association and -1 indicates a negative/inverse             association
    -   n: count
        -   total count where both the predictor variable and outcome variable are positive (i.e.               are both "yes")

The lasso is useful in identifying certain predictor variables that are strongly associated with the outcome variable of interest. Additional analyses with these predictor variables are recommended.
