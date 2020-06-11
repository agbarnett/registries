# for sample size from clinicaltrials.gov
# Jun 2020

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Sample size ranges"),

p("Select the different options to see the median sample size from studies registered on",  tags$a(href="https://clinicaltrials.gov/", "clinicaltrials.gov"), "."),  
p("The data used here were downloaded on" , format(censor.date, '%d-%b-%Y'), '.'),  
  sidebarLayout(
    sidebarPanel(
      
      pickerInput(
        inputId = "interval_width",
        label = "Percent spread either side of the median", 
        choices = c("10%", "20%", "25% (inter-quartile range)", "30%", 
                                   "40%", '%45'),
        multiple = FALSE,
        selected = "25% (inter-quartile range)"
      ),
      
      # check multiple
      awesomeCheckboxGroup(
        inputId = "stype",
        label = "Study type:", 
        choices = c("Interventional", "Observational", "Expanded Access"),
        selected = "Interventional"
      ),
      
      radioButtons(inputId="purpose", 
                   label = "Purpose:",
                   c("Treatment" = "Treatment",
                     "Prevention" = "Prevention",
                     "Diagnostic" = 'Diagnostic',
                     'Screening' = 'Screening',
                     'Device Feasibility' = 'Device Feasibility',
                     'Basic Science' = 'Basic Science',
                     "Health Services Research"= "Health Services Research",
                     'Supportive Care' = 'Supportive Care',
                     'Educational/Counseling/Training' = 'Educational/Counseling/Training',
                     'Other' = 'Other',
                     "Missing" = "Missing"), 
                   selected='Treatment'),

      radioButtons(inputId="assignment", 
                   label = "Assignment:",
                   c("Crossover" = "Crossover",
                     "Factorial" = "Factorial",
                     "Parallel" = 'Parallel',
                     'Sequential' = 'Sequential',
                     'Single Group' = 'Single Group'), 
                   selected='Parallel'),
      
      # only for intervenational
      conditionalPanel(
        condition = "input.stype == 'Interventional'",
        radioButtons(inputId = "phase",
                     label = "Phase:",
                     c("Early Phase 1" = "Early Phase 1",
                       "Phase 1" = "Phase 1",
                       "Phase 1/2" = "Phase 1, Phase 2",
                       "Phase 2" = "Phase 2",
                       "Phase 2/3" = "Phase 2, Phase 3",
                       "Phase 3" = "Phase 3",
                       "Phase 4" = "Phase 4",
                       "Not Applicable" = "Not Applicable"
                     ), 
                       selected='Not Applicable')),
      
      radioButtons(inputId = "alloc",
                   label = "Allocation:",
                   c("Randomized" = "Randomized",
                     "Non-Randomized" = "Non-Randomized",
                     "Not Applicable" = 'Not Applicable'), 
                   selected='Randomized'),
      
      awesomeCheckbox(
        inputId = "log_scale",
        label = "Log scale for plots", 
        value = FALSE
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
    plotOutput(outputId = 'sample_size_distribution')
  ) # end of main panel

))
)

