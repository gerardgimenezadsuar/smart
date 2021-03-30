# smartables R package
### Automatically create ready-to-publish summary statistical tables from a data frame. 

Let's look at an example of how it works, make sure to have the *devtools* package installed:

    devtools::install_github("gerardgimenezadsuar/smartables")

And don't forget to load the downloaded package **smartables**:

    library(smartables)

This first version contains two functions: **create_table** and **modeltable**. 

## *create_table* function:

Let's start with an example of how the first one works. We will be using the iris dataset, which is preloaded with R. We want to get both a statistical summary of each variable and also a stratified table by the variable *"Species"*. We can simply type:

    create_table(iris, stratify_by = "Species")
    
Since the function returns a list containing both data frames, you will need to store the output on a variable:

    output_df <- create_table(iris, stratify_by = "Species")

To get the overall summary, just access the first item of the list:

    output_df[1]
And for the stratified table, access the second one:

    output_df[2]

## *modeltable* function:
This function loads the resulting output of either INLA, GLM or HR models, and returns a ready-to-publish summary table from it.
