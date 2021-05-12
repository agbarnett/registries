# 0_labels.R
# labels that help with the plots of the regression models
# useful page: https://prsinfo.clinicaltrials.gov/definitions.html
# April 2021
library(dplyr)

## Section 1: ANZCTR labels ##

# list of mandatory (for use by 98_summary_table_predictors.Rmd)
mandatory_anzctr = read.table(sep='!', header=TRUE, stringsAsFactors = FALSE, text='
var!mandatory
samplesize_target!Yes
date!Yes
n_primary!Yes
n_secondary!Yes
n_funding!Yes
gender!Yes
age_limit_max!Yes
age_limit_min!Yes
volunteers!Yes
phase!Yes
endpoint!No
purpose!Yes
masking!No
allocation!Yes
assignment!No
control!Yes
intervention_code!Yes
ccode1!Yes
study_status!Yes
purposeobs!No
duration!No
timing!No')
#selection!No

# make table of estimates and names, and add reference groups
# some results put in order of high to low
g_order = c('continuous', 'Gender', 'Maximum age limit', 'Minimum age limit', 'Healthy volunteers', 'Phase', 'Endpoint', 'Purpose', 'Masking', 'Assignment', 'Allocation', 'Control', 'Intervention', 'Purpose observational','Duration','Timing','Status', 'Area') # for ordering on plot

# full list of all potential variables and if they are a reference or not
table_names_anzctr = read.table(sep='!', header=TRUE, stringsAsFactors = FALSE, text='
group!term!label!reference
continuous!samplesize_target!Double target sample size!FALSE
continuous!date!Trend per 5 years!FALSE
continuous!n_primary!Double the number of primary outcomes!FALSE
continuous!n_secondary!Double the number of secondary outcomes!FALSE
continuous!n_funding!Number of funders!FALSE
Gender!genderMales!Males!FALSE
Gender!genderFemales!Females!FALSE
Gender!genderAll!All!TRUE
Maximum age limit!age_limit_max18 or over!18+!FALSE
Maximum age limit!age_limit_maxUnder 18!Under 18!FALSE
Maximum age limit!age_limit_maxNo limit!No limit!TRUE
Maximum age limit!age_limit_maxNot stated!Not stated!FALSE
Maximum age limit!age_limit_maxMissing!Missing!FALSE
Minimum age limit!age_limit_min18 or over!18+!FALSE
Minimum age limit!age_limit_minUnder 18!Under 18!FALSE
Minimum age limit!age_limit_minNo limit!No limit!TRUE
Minimum age limit!age_limit_minNot stated!Not stated!FALSE
Minimum age limit!age_limit_minMissing!Missing!FALSE
Healthy volunteers!volunteersYes!Yes!FALSE
Healthy volunteers!volunteersNo!No!TRUE
Healthy volunteers!volunteersMissing!Missing!FALSE
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
Allocation!allocationNon-randomised trial!Non-randomised trial!FALSE
Allocation!allocationRandomised controlled trial!Randomised controlled trial!TRUE
Allocation!allocationMissing!Missing!FALSE
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
Intervention!intervention_codeNot applicable!Not applicable!FALSE
Area!ccode1Human Genetics and Inherited Disorders!Human Genetics and Inherited Disorders!FALSE
Area!ccode1Metabolic and Endocrine!Metabolic and Endocrine!FALSE
Area!ccode1Eye!Eye!FALSE
Area!ccode1Other!Other!FALSE
Area!ccode1Neurological!Neurological!FALSE
Area!ccode1Physical Medicine / Rehabilitation!Physical Medicine / Rehabilitation!FALSE
Area!ccode1Alternative and Complementary Medicine!Alternative and Complementary Medicine!FALSE
Area!ccode1Diet and Nutrition!Diet and Nutrition!FALSE
Area!ccode1Respiratory!Respiratory!FALSE
Area!ccode1Ear!Ear!FALSE
Area!ccode1Inflammatory and Immune System!Inflammatory and Immune System!FALSE
Area!ccode1Stroke!Stroke!FALSE
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
Purpose observational!purposeobsNatural history!Natural history!TRUE
Purpose observational!purposeobsScreening!Screening!FALSE
Purpose observational!purposeobsPsychosocial!Psychosocial!FALSE
Duration!durationLongitudinal!Longitudinal!FALSE
Duration!durationCross-sectional!Cross-sectional!TRUE
Duration!durationMissing!Missing!FALSE
Timing!timingRetrospective!Retrospective!FALSE
Timing!timingProspective!Prospective!TRUE
Timing!timingBoth!Both!FALSE
') %>%
  mutate(group_number = as.numeric(factor(group, levels=g_order))) # add number for ordering
# dropped:
#Selection!selectionConvenience sample!Convenience sample!FALSE
#Selection!selectionDefined population!Defined population!TRUE
#Selection!selectionRandom sample!Random sample!FALSE
#Selection!selectionCase control!Case control!FALSE
#Selection!selectionMissing!Missing!FALSE

### Section 2: clintrials.gov labels ###

# list of mandatory (for use by 98_summary_table_predictors.Rmd)
# Yes+ = since 2017
mandatory_clintrials = read.table(sep='!', header=TRUE, stringsAsFactors = FALSE, text='
var!mandatory
date!Yes
n_primary!Yes
n_secondary!No
n_condition!Yes
n_arms!Yes
gender!Yes
age_limit_max!Yes
age_limit_min!Yes
volunteers!Yes+
biological!Yes
behavioral!Yes
device!Yes
combination!Yes
diagnostic!Yes
dietary!Yes
drug!Yes
other!Yes
genetic!Yes
procedure!Yes
radiation!Yes
phase!Yes
lead_sponsor_class!Yes
purpose!Yes+
study_design_observational!Yes
study_design_time!Yes
masking!Yes+
assignment!Yes
allocation!Yes+
longitudinal!No
adaptive_trial!No
status!Yes
')

# group order for plot
g_order = c('continuous', 'Gender', 'Maximum age limit', 'Minimum age limit', 'Healthy volunteers', 'Intervention', 'Phase', 'Sponsor', 'Purpose', 'Masking', 'Assignment', 'Allocation', 'Design', 'Observational design', 'Time perspective','Status') # for ordering on plot

# make table of estimates and names, and add reference groups
# note required is at variable level but is put next to each category
table_names_clintrials = read.table(sep='!', header=TRUE, stringsAsFactors = FALSE, text='
group!term!label!reference
continuous!date!Trend per 5 years!FALSE
continuous!n_primary!Double the number of primary outcomes!FALSE
continuous!n_secondary!Double the number of secondary outcomes!FALSE
continuous!n_condition!Double the number of conditions!FALSE
continuous!n_arms!Double the number of arms!FALSE
continuous!samplesize_target!Double target sample size!FALSE
Gender!genderMale!Males!FALSE
Gender!genderFemale!Females!FALSE
Gender!genderAll!All!TRUE
Maximum age limit!age_limit_max18 or over!18+!FALSE
Maximum age limit!age_limit_maxUnder 18!Under 18!FALSE
Maximum age limit!age_limit_maxNo limit!No limit!TRUE
Maximum age limit!age_limit_maxNot stated!Not stated!FALSE
Maximum age limit!age_limit_maxMissing!Missing!FALSE
Minimum age limit!age_limit_min18 or over!18+!FALSE
Minimum age limit!age_limit_minUnder 18!Under 18!FALSE
Minimum age limit!age_limit_minNo limit!No limit!TRUE
Minimum age limit!age_limit_minNot stated!Not stated!FALSE
Minimum age limit!age_limit_minMissing!Missing!FALSE
Healthy volunteers!volunteersYes!Yes!FALSE
Healthy volunteers!volunteersNo!No!TRUE
Healthy volunteers!volunteersMissing!Missing!FALSE
Intervention!biologicalTRUE!Biological!FALSE
Intervention!behavioralTRUE!Behavioral!FALSE
Intervention!deviceTRUE!Device!FALSE
Intervention!combinationTRUE!Combination!FALSE
Intervention!diagnosticTRUE!Diagnostic!FALSE
Intervention!dietaryTRUE!Dietary!FALSE
Intervention!drugTRUE!Drug!FALSE
Intervention!otherTRUE!Other!FALSE
Intervention!geneticTRUE!Genetic!FALSE
Intervention!procedureTRUE!Procedure!FALSE
Intervention!radiationTRUE!Radiation!FALSE
Phase!phaseEarly Phase 1!Early Phase 1!FALSE
Phase!phasePhase 1!Phase 1!FALSE
Phase!phasePhase 1, Phase 2!Phase 1/2!FALSE
Phase!phasePhase 1/Phase 2!Phase 1/2!FALSE
Phase!phasePhase 2!Phase 2!FALSE
Phase!phasePhase 2, Phase 3!Phase 2/3!FALSE
Phase!phasePhase 2/Phase 3!Phase 2/3!FALSE
Phase!phasePhase 3!Phase 3!TRUE
Phase!phasePhase 4!Phase 4!FALSE
Phase!phaseN/A!Not Applicable!FALSE
Phase!phaseNot Applicable!Not Applicable!FALSE
Phase!phaseMissing!Missing!FALSE
Sponsor!lead_sponsor_classOTHER!Other!TRUE
Sponsor!lead_sponsor_classU.S. Fed!US Federal agencies!FALSE
Sponsor!lead_sponsor_classIndustry!Industry!FALSE
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
Observational design!study_design_observationalCase-Control!Case-control!FALSE
Observational design!study_design_observationalCase-Crossover!Case-Crossover!FALSE
Observational design!study_design_observationalDefined Population!Defined Population!FALSE
Observational design!study_design_observationalNatural History!Natural History!FALSE
Observational design!study_design_observationalCase-Only!Case-Only!FALSE
Observational design!study_design_observationalCohort!Cohort!TRUE
Observational design!study_design_observationalEcologic or Community!Ecologic or Community!FALSE
Observational design!study_design_observationalFamily-Based!Family-Based!FALSE
Observational design!study_design_observationalOther!Other!FALSE
Observational design!study_design_observationalMissing!Missing!FALSE
Time perspective!study_design_timeCross-Sectional!Cross-Sectional!FALSE
Time perspective!study_design_timeProspective!Prospective!TRUE
Time perspective!study_design_timeOther!Other!FALSE
Time perspective!study_design_timeRetrospective!Retrospective!FALSE
Time perspective!study_design_timeMissing!Missing!FALSE
Masking!maskingMissing!Missing!FALSE
Masking!maskingQuadruple!Quadruple!FALSE
Masking!maskingTriple!Triple!FALSE
Masking!maskingDouble!Double!FALSE
Masking!maskingSingle!Single!FALSE
Masking!maskingNone (Open Label)!Open label!TRUE
Assignment!assignmentCrossover!Crossover!FALSE
Assignment!assignmentSingle Group!Single group!FALSE
Assignment!assignmentSequential!Sequential!FALSE
Assignment!assignmentParallel!Parallel!TRUE
Assignment!assignmentFactorial!Factorial!FALSE
Assignment!assignmentMissing!Missing!FALSE
Allocation!allocationNon-Randomized!Non-Randomized!FALSE
Allocation!allocationRandomized!Randomized!TRUE
Allocation!allocationMissing!Missing!FALSE
Allocation!allocationN/A!Not applicable!FALSE
Design!longitudinalTRUE!Longitudinal!FALSE
Design!adaptive_trialTRUE!Adaptive/platform trial!FALSE
Status!statusActive, not recruiting!Active, not recruiting!FALSE
Status!statusTerminated!Terminated!FALSE
Status!statusSuspended!Suspended!FALSE
Status!statusStopped early!Stopped early!FALSE
Status!statusUnknown status!Unknown!FALSE
Status!statusWithdrawn!Withdrawn!FALSE
Status!statusCompleted!Completed!TRUE
') %>%
  mutate(group_number = as.numeric(factor(group, levels=g_order))) # add number for ordering

## save
save(mandatory_anzctr, table_names_anzctr, mandatory_clintrials, table_names_clintrials, file='data/labels.RData')

