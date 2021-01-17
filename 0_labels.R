# 0_labels.R
# labels that help with the plots of the regression models
# December 2020
library(dplyr)

## Section 1: ANZCTR labels ##

# make table of estimates and names, and add reference groups
# some results put in order of high to low
g_order = c('continuous', 'Gender', 'Maximum age limit', 'Minimum age limit', 'Phase', 'Endpoint', 'Purpose', 'Masking', 'Assignment', 'Control', 'Intervention', 'Area', 'Status') # for ordering on plot

# full list of all potential variables and if they are a reference or not
table_names_anzctr = read.table(sep='!', header=TRUE, stringsAsFactors = FALSE, text='
group!term!label!reference
continuous!date!Trend per 5 years!FALSE
continuous!n_primary!Double the number of primary outcomes!FALSE
continuous!n_secondary!Double the number of secondary outcomes!FALSE
continuous!n_funding!Number of funders!FALSE
Gender!genderMales!Males!FALSE
Gender!genderFemales!Females!FALSE
Gender!genderBoth males and females!All!TRUE
Maximum age limit!age_limit_maxOver 18!Over 18!FALSE
Maximum age limit!age_limit_maxExactly 18!Exactly 18!FALSE
Maximum age limit!age_limit_maxUnder 18!Under 18!FALSE
Maximum age limit!age_limit_maxNo limit!No limit!TRUE
Maximum age limit!age_limit_maxNot stated!Not stated!FALSE
Maximum age limit!age_limit_maxMissing!Missing!FALSE
Minimum age limit!age_limit_minOver 18!Over 18!FALSE
Minimum age limit!age_limit_minExactly 18!Exactly 18!FALSE
Minimum age limit!age_limit_minUnder 18!Under 18!FALSE
Minimum age limit!age_limit_minNo limit!No limit!TRUE
Minimum age limit!age_limit_minNot stated!Not stated!FALSE
Minimum age limit!age_limit_minMissing!Missing!FALSE
Phase!phasePhase 0!Phase 0!FALSE
Phase!phasePhase 1!Phase 1!FALSE
Phase!phasePhase 1 / Phase 2!Phase 1/2!FALSE
Phase!phasePhase 2!Phase 2!FALSE
Phase!phasePhase 2 / Phase 3!Phase 2/3!FALSE
Phase!phasePhase 3!Phase 3!TRUE
Phase!phasePhase 3 / Phase 4!Phase 3/4!FALSE
Phase!phasePhase 4!Phase 4!FALSE
Phase!phaseNot Applicable!Not applicable!FALSE
Phase!phaseMissing!Missing!FALSE
Endpoint!endpointBio-availability!Bio-availability!FALSE
Endpoint!endpointPharmacodynamics!Pharmacodynamics!FALSE
Endpoint!endpointPharmacokinetics!Pharmacokinetics!FALSE
Endpoint!endpointPharmacokinetics / pharmacodynamics!Pharmacokinetics / pharmacodynamics!FALSE
Endpoint!endpointBio-equivalence!Bio-equivalence!FALSE
Endpoint!endpointSafety!Safety!FALSE
Endpoint!endpointSafety/efficacy!Safety/efficacy!FALSE
Endpoint!endpointMissing!Missing!FALSE
Endpoint!endpointEfficacy!Efficacy!TRUE
Purpose!purposeTreatment!Treatment!TRUE
Purpose!purposeDiagnosis!Diagnosis!FALSE
Purpose!purposeEducational / counselling / training!Educational / counselling / training!FALSE
Purpose!purposePrevention!Prevention!FALSE
Masking!maskingOpen (masking not used)!Open!FALSE
Masking!maskingBlinded (masking used)!Blinded!TRUE
Masking!maskingMissing!Missing!FALSE
Assignment!assignmentCrossover!Crossover!FALSE
Assignment!assignmentFactorial!Factorial!FALSE
Assignment!assignmentSingle group!Single group!FALSE
Assignment!assignmentOther!Other!FALSE
Assignment!assignmentParallel!Parallel!TRUE
Assignment!assignmentMissing!Missing!FALSE
Control!controlMissing!Missing!FALSE
Control!controlUncontrolled!Uncontrolled!FALSE
Control!controlDose comparison!Dose comparison!FALSE
Control!controlPlacebo!Placebo!FALSE
Control!controlActive!Active!TRUE
Control!controlHistorical!Historical!FALSE
Intervention!intervention_codeMissing!Missing!FALSE
Intervention!intervention_codeRehabilitation!Rehabilitation!FALSE
Intervention!intervention_codeTreatment: Devices!Treatment: Devices!FALSE
Intervention!intervention_codeLifestyle!Lifestyle!FALSE
Intervention!intervention_codeTreatment: Other!Treatment: Other!FALSE
Intervention!intervention_codeBehaviour!Behaviour!FALSE
Intervention!intervention_codeNone!None!FALSE
Intervention!intervention_codeTreatment: Drugs!Treatment: Drugs!TRUE
Intervention!intervention_codeDiagnosis / Prognosis!Diagnosis / Prognosis!FALSE
Intervention!intervention_codePrevention!Prevention!FALSE
Intervention!intervention_codeEarly detection / Screening!Early detection / Screening!FALSE
Intervention!intervention_codeTreatment: Surgery!Treatment: Surgery!FALSE
Area!ccode1Human Genetics and Inherited Disorders!Human Genetics and Inherited Disorders!FALSE
Area!ccode1Metabolic and Endocrine!Metabolic and Endocrine!FALSE
Area!ccode1Eye!Eye!FALSE
Area!ccode1Other!Other!FALSE
Area!ccode1Neurological!Neurological!FALSE
Area!ccode1Physical Medicine / Rehabilitation!Physical Medicine / Rehabilitation!FALSE
Area!ccode1Alternative and Complementary Medicine!Alternative and Complementary Medicine!FALSE
Area!ccode1Diet and Nutrition!Diet and Nutrition!FALSE
Area!ccode1Respiratory!Respiratory!FALSE
Area!ccode1Oral and Gastrointestinal!Oral and Gastrointestinal!FALSE
Area!ccode1Renal and Urogenital!Renal and Urogenital!FALSE
Area!ccode1Musculoskeletal!Musculoskeletal!FALSE
Area!ccode1Skin!Skin!FALSE
Area!ccode1Anaesthesiology!Anaesthesiology!FALSE
Area!ccode1Surgery!Surgery!FALSE
Area!ccode1Cardiovascular!Cardiovascular!TRUE
Area!ccode1Mental Health!Mental Health!FALSE
Area!ccode1Cancer!Cancer!FALSE
Area!ccode1Injuries and Accidents!Injuries and Accidents!FALSE
Area!ccode1Infection!Infection!FALSE
Area!ccode1Reproductive Health and Childbirth!Reproductive Health and Childbirth!FALSE
Area!ccode1Public Health!Public Health!FALSE
Area!ccode1Emergency medicine!Emergency medicine!FALSE
Status!study_statusStopped early!Stopped early!FALSE
Status!study_statusSuspended!Suspended!FALSE
Status!study_statusWithdrawn!Withdrawn!FALSE
Status!study_statusCompleted!Completed!TRUE
Status!study_statusActive, not recruiting!Active, not recruiting!FALSE
') %>%
  mutate(group_number = as.numeric(factor(group, levels=g_order))) # add number for ordering

### Section 2: clintrials.gov labels ###

# group order for plot
g_order = c('continuous', 'Gender', 'Maximum age limit', 'Minimum age limit', 'Phase', 'Sponsor', 'Purpose', 'Masking', 'Assignment', 'Allocation') # for ordering on plot

# make table of estimates and names, and add reference groups
# some results put in order of high to low
table_names_clintrials = read.table(sep='!', header=TRUE, stringsAsFactors = FALSE, text='
group!term!label!reference
continuous!date!Trend per 5 years!FALSE
continuous!n_primary!Double the number of primary outcomes!FALSE
continuous!n_secondary!Double the number of secondary outcomes!FALSE
continuous!n_condition!Double the Number of conditions!FALSE
continuous!n_arms!Double the number of arms!FALSE
Gender!genderMale!Males!FALSE
Gender!genderFemale!Females!FALSE
Gender!genderAll!All!TRUE
Maximum age limit!age_limit_maxOver 18!Over 18!FALSE
Maximum age limit!age_limit_maxExactly 18!Exactly 18!FALSE
Maximum age limit!age_limit_maxUnder 18!Under 18!FALSE
Maximum age limit!age_limit_maxNo limit!No limit!TRUE
Maximum age limit!age_limit_maxNot stated!Not stated!FALSE
Maximum age limit!age_limit_maxMissing!Missing!FALSE
Minimum age limit!age_limit_minOver 18!Over 18!FALSE
Minimum age limit!age_limit_minExactly 18!Exactly 18!FALSE
Minimum age limit!age_limit_minUnder 18!Under 18!FALSE
Minimum age limit!age_limit_minNo limit!No limit!TRUE
Minimum age limit!age_limit_minNot stated!Not stated!FALSE
Minimum age limit!age_limit_minMissing!Missing!FALSE
Phase!phaseEarly Phase 1!Early Phase 1!FALSE
Phase!phasePhase 1!Phase 1!FALSE
Phase!phasePhase 1, Phase 2!Phase 1/2!FALSE
Phase!phasePhase 2!Phase 2!FALSE
Phase!phasePhase 2, Phase 3!Phase 2/3!FALSE
Phase!phasePhase 3!Phase 3!TRUE
Phase!phasePhase 4!Phase 4!FALSE
Phase!phaseNot Applicable!Not Applicable!FALSE
Phase!phaseMissing!Missing!FALSE
Sponsor!lead_sponsor_classOTHER!Other!TRUE
Sponsor!lead_sponsor_classFED!FED!FALSE
Sponsor!lead_sponsor_classINDUSTRY!Industry!FALSE
Sponsor!lead_sponsor_classOTHER_GOV!Other government!FALSE
Sponsor!lead_sponsor_classNETWORK!Network!FALSE
Sponsor!lead_sponsor_classNIH!NIH!FALSE
Purpose!purposeDevice Feasibility!Device Feasibility!FALSE
Purpose!purposeBasic Science!Basic Science!FALSE
Purpose!purposeTreatment!Treatment!TRUE
Purpose!purposeOther!Other!FALSE
Purpose!purposeSupportive Care!Supportive Care!FALSE
Purpose!purposeDiagnostic!Diagnostic!FALSE
Purpose!purposePrevention!Prevention!FALSE
Purpose!purposeHealth Services Research!Health Services Research!FALSE
Purpose!purposeScreening!Screening!FALSE
Purpose!purposeMissing!Missing!FALSE
Masking!maskingMissing!Missing!FALSE
Masking!maskingQuadruple!Quadruple!FALSE
Masking!maskingTriple!Triple!FALSE
Masking!maskingDouble!Double!TRUE
Masking!maskingSingle!Single!FALSE
Masking!maskingNone (Open Label)!Open label!FALSE
Assignment!assignmentCrossover!Crossover!FALSE
Assignment!assignmentSingle Group!Single group!FALSE
Assignment!assignmentSequential!Sequential!FALSE
Assignment!assignmentParallel!Parallel!TRUE
Assignment!assignmentFactorial!Factorial!FALSE
Assignment!assignmentMissing!Missing!FALSE
Allocation!allocationNon-Randomized!Non-Randomized!FALSE
Allocation!allocationRandomized!Randomized!TRUE
Allocation!allocationMissing!Missing!FALSE
') %>%
  mutate(group_number = as.numeric(factor(group, levels=g_order))) # add number for ordering

## save
save(table_names_anzctr, table_names_clintrials, file='data/labels.RData')

