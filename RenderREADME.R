#########################
#  Author-Mark Austin
#
#  Due Date 10/05/21
#
#  Purpose-Render README.md
#    file for 558 Project 1
#  
#########################

#getwd()

#library(rmarkdown)


rmarkdown::render("Vignette.Rmd", 
                  output_format = "github_document",
                  output_file = "README.md",
                  output_options = list(
                    html_preview = FALSE,
                    df_print = "kable",
                    toc = TRUE
                  )
)

