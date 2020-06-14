# for sample size from clinicaltrials.gov
# Jun 2020

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Sample sizes for trials"),

p("Select the various options to see the sample sizes from trials registered on",  tags$a(href="https://clinicaltrials.gov/", "clinicaltrials.gov"), " and ", tags$a(href="https://www.anzctr.org.au/", "ANZCTR"), ". ",  
  "The maximum number of trials is",  format(nrow(clintrials),big.mark=','), " for clinicaltrials.gov and ", format(nrow(studies),big.mark=','), " for ANZCTR. ",
  "The data were downloaded from clinicaltrials.gov on" , format(censor.date.clintrials, '%d-%b-%Y'), ' and from ANZCTR on ', format(censor.date, '%d-%b-%Y'), '.'),  
  sidebarLayout(
    sidebarPanel(
      
      radioButtons(
        inputId = "database",
        label = "Database:", 
        choices = c('clinicaltrials.gov','ANZCTR'),
        selected = 'clinicaltrials.gov'
      ),
      
      pickerInput(
          inputId = "interval_width",
          label = "Percent spread either side of the median:", 
          choices = c("10%", "20%", "25% (inter-quartile range)", "30%", 
                      "40%", '45%'),
          multiple = FALSE,
          selected = "25% (inter-quartile range)"
        ),

      # select one
      conditionalPanel(
        condition = "input.database == 'clinicaltrials.gov'",
        awesomeRadio(inputId="purpose.c", 
                   label = "Purpose:",
                   choices = c("Treatment" = "Treatment",
                     "Prevention" = "Prevention",
                     "Diagnostic" = 'Diagnostic',
                     'Screening' = 'Screening',
                     'Device Feasibility' = 'Device Feasibility',
                     'Basic Science' = 'Basic Science',
                     "Health Services Research"= "Health Services Research",
                     'Supportive Care' = 'Supportive Care',
                     'Educational/Counseling/Training' = 'Educational/Counseling/Training',
                     'Other' = 'Other',
                     "Missing" = "Missing",
                     'All of the above' = 'all'), 
                   selected='all',
                   status = "warning")),
      
      conditionalPanel(
        condition = "input.database == 'ANZCTR'",
        awesomeRadio(inputId="purpose.a", 
                     label = "Purpose:",
                     choices = c("Treatment" = "Treatment",
                                 "Prevention" = "Prevention",
                                 "Diagnostic" = 'Diagnostic',
                                 'Educational / counselling / training' = 'Educational / counselling / training',
                                 'All of the above' = 'all'), 
                     selected='all',
                     status = "danger")),
      
      conditionalPanel(
        condition = "input.database == 'clinicaltrials.gov'",
        awesomeRadio(inputId="assignment.c", 
                   label = "Assignment:",
                   c("Crossover" = "Crossover",
                     "Factorial" = "Factorial",
                     "Parallel" = 'Parallel',
                     'Sequential' = 'Sequential',
                     'Single Group' = 'Single Group',
                     'All of the above' = 'all'), 
                   selected='all',
                   status = "warning")
      ),
      
      conditionalPanel(
        condition = "input.database == 'ANZCTR'",
        awesomeRadio(inputId="assignment.a", 
                     label = "Assignment:",
                     c("Crossover" = "Crossover",
                       "Factorial" = "Factorial",
                       "Parallel" = 'Parallel',
                       'Single group' = 'Single group',
                       'Other' = 'Other',
                       'All of the above' = 'all'), 
                     selected='all',
                     status = "danger")
      ),
      
      conditionalPanel(
        condition = "input.database == 'clinicaltrials.gov'",
        awesomeRadio(inputId = "phase.c",
                     label = "Phase:",
                     c("Early Phase 1" = "Early Phase 1",
                       "Phase 1" = "Phase 1",
                       "Phase 1/2" = "Phase 1, Phase 2",
                       "Phase 2" = "Phase 2",
                       "Phase 2/3" = "Phase 2, Phase 3",
                       "Phase 3" = "Phase 3",
                       "Phase 4" = "Phase 4",
                       "Not Applicable" = "Not Applicable",
                       'All of the above' = 'all'
                     ), 
                       selected='all',
                     status = "warning")),
      
      conditionalPanel(
        condition = "input.database == 'ANZCTR'",
        awesomeRadio(inputId = "phase.a",
                     label = "Phase:",
                     c("Phase 0" = "Phase 0",
                       "Phase 1" = "Phase 1",
                       "Phase 1/2" = "Phase 1 / Phase 2",
                       "Phase 2" = "Phase 2",
                       "Phase 2/3" = "Phase 2 / Phase 3",
                       "Phase 3" = "Phase 3",
                       "Phase 3/4" = "Phase 3 / Phase 4",
                       "Phase 4" = "Phase 4",
                       "Not Applicable" = "Not Applicable",
                       "Missing" = "Missing",
                       'All of the above' = 'all'
                     ), 
                     selected='all',
                     status = "danger")),
      
      conditionalPanel(
        condition = "input.database == 'clinicaltrials.gov'",
        awesomeRadio(inputId = "alloc",
                   label = "Allocation:",
                   c("Randomized" = "Randomized",
                     "Non-Randomized" = "Non-Randomized",
                     'All of the above' = 'all'), 
                   selected='all',
                   status = "warning")),
      
      conditionalPanel(
        condition = "input.database == 'ANZCTR'",
        awesomeRadio(inputId = "endpoint",
                   label = "Endpoint:",
                   c("Bio-availability" = "Bio-availability",
                     "Bio-equivalence" = "Bio-equivalence",
                     'Efficacy' = 'Efficacy',
                     'Pharmacodynamics' = 'Pharmacodynamics',
                     'Pharmacokinetics' = 'Pharmacokinetics',
                     'Pharmacokinetics / pharmacodynamics' = 'Pharmacokinetics / pharmacodynamics',
                     'Safety' = 'Safety',
                     'Safety/efficacy' = 'Safety/efficacy',
                     'All of the above' = 'all'), 
                   selected='all',
                   status = "danger")),
      
      conditionalPanel(
        condition = "input.database == 'ANZCTR'",
        awesomeRadio(inputId = "control",
                     label = "Control:",
                     c("Active" = "Active",
                       "Dose comparison" = "Dose comparison",
                       'Efficacy' = 'Efficacy',
                       'Historical' = 'Historical',
                       'Placebo' = 'Placebo',
                       'Uncontrolled' = 'Uncontrolled',
                       'All of the above' = 'all'), 
                     selected='all',
                     status = "danger")),
      
      conditionalPanel(
        condition = "input.database == 'ANZCTR'",
        awesomeRadio(inputId = "condition",
                     label = "Condition:",
                     c('Alternative and Complementary Medicine' = 'Alternative and Complementary Medicine',
                       'Anaesthesiology' = 'Anaesthesiology',
                       'Blood' = 'Blood',
                       'Cancer' = 'Cancer',
                       'Cardiovascular' = 'Cardiovascular',
                       'Diet and Nutrition' = 'Diet and Nutrition',
                       'Ear' = 'Ear',
                       'Emergency medicine' = 'Emergency medicine',
                       'Eye' = 'Eye',
                       'Human Genetics and Inherited Disorders' = 'Human Genetics and Inherited Disorders',
                       'Infection' = 'Infection',
                       'Inflammatory and Immune System' = 'Inflammatory and Immune System',
                       'Injuries and Accidents' = 'Injuries and Accidents',
                       'Mental Health' = 'Mental Health',
                       'Metabolic and Endocrine' = 'Metabolic and Endocrine',
                       'Musculoskeletal' = 'Musculoskeletal',
                       'Neurological' = 'Neurological',
                       'Oral and Gastrointestinal' = 'Oral and Gastrointestinal',
                       'Other' = 'Other',
                       'Physical Medicine / Rehabilitation' = 'Physical Medicine / Rehabilitation',
                       'Public Health' = 'Public Health',
                       'Renal and Urogenital' = 'Renal and Urogenital',
                       'Reproductive Health and Childbirth' = 'Reproductive Health and Childbirth',
                       'Respiratory' = 'Respiratory',
                       'Skin' = 'Skin',
                       'Stroke' = 'Stroke',
                       'Surgery' = 'Surgery',
                       'All of the above' = 'all'), 
                     selected='all',
                     status = "danger")),
      
      
      h4('Log (base 10) or linear scales for plots:'),
      
      radioButtons(
          inputId = "log_scale_stats",
          label = "Summary statistics plots", 
          choices = c('Linear','Log'),
          selected = 'Linear'
        ),
        
      radioButtons(
          inputId = "log_scale_dist",
          label = "Distribution plots", 
          choices = c('Linear','Log'),
          selected = 'Log'
        )
      
      ), # end of side bar
    
  mainPanel(
    h3('Target sample size'),
    textOutput(outputId = 'target_sample_size'),
    h3('Actual sample size'),
    textOutput(outputId = 'actual_sample_size'),
    h3('Plot of median and intervals'),
    plotOutput(outputId = 'sample_size_stats'),
    h3('Sample size distributions'),
    p('The grey vertical dotted lines are the percentiles either side of the median.'),
    plotOutput(outputId = 'sample_size_distribution'),
    p("The intervals use the percentiles from the observed distribution.")
    
  ) # end of main panel

))
)
