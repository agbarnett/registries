# 99_make_appendices.R
# make appendices with ordered file names
# April 2021
library(rmarkdown)

# Appendix 1 (does not render well as pdf)
render(input = "98_descriptive.Rmd",
       output_format = "word_document",
       output_file = 'supplement_1.docx')

# Table 1 
render(input = "98_summary_table_predictors.Rmd",
       output_format = "pdf_document",
       output_file = 'supplement_table_1.pdf')

# Table 2
render(input = "98_ratio_regression_table.Rmd",
       output_format = "pdf_document",
       output_file = 'supplement_table_2.pdf')

# Table 3
# see 98_word_frequency.R
 
# Fig 1
render(input = "4_bland_altman_ratio_best_model.Rmd",
       output_format = "pdf_document",
       output_file = 'supplement_figure_1.pdf')

# Fig 2
render(input = "4_plot_jags_chains.Rmd",
       output_format = "pdf_document",
       output_file = 'supplement_figure_2.pdf')

# Fig 3
render(input = "99_plot_excluded.Rmd",
       output_format = "pdf_document",
       output_file = 'supplement_figure_3.pdf')

# Fig 4
render(input = "98_xval_errors.Rmd",
       output_format = "pdf_document",
       output_file = 'supplement_figure_4.pdf')

# Fig 5
render(input = "98_residuals.Rmd",
         output_format = "pdf_document",
         output_file = 'supplement_figure_5.pdf')
