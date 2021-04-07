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


![Screen Shot 2021-04-07 at 8 38 27 AM](https://user-images.githubusercontent.com/48365704/113822164-62d89780-977d-11eb-9d18-00cebc1dc0e0.png)

And for the stratified table, access the second one:

    output_df[2]

![Screen Shot 2021-04-07 at 8 38 43 AM](https://user-images.githubusercontent.com/48365704/113822186-69670f00-977d-11eb-81c0-7c7ef827f56f.png)


## *modeltable* function:
This function loads the resulting output of either INLA, GLM or HR models, and returns a ready-to-publish summary table from it.

Here's a breif example on how to use it. First, load the results of either of these models in an object.
    
    library(INLA)
    data(Seeds)
    formula <- r~x1+x2+f(plate,model="iid")
    result <- INLA::inla(formula,family="binomial",Ntrials=n,data=Seeds)
    
Once the *result* object is created, we can simply use *smartables* function **modeltable**:

    modeltable(result, "INLA")
    
The output is the following table:

![Screen Shot 2021-04-07 at 8 55 13 AM](https://user-images.githubusercontent.com/48365704/113823556-12623980-977f-11eb-93ef-0d331ede6ce0.png)

This is just a particular example with an INLA model, however, the same procedure should be used to obtain a summary table for either *generalized linear models (GLM)* or *Hazard ratio models (HR)*.

## Acknowledgements

This package has been created by GRECS (Research group in Statistics, Econometry and Health) from the University of Girona. I thank Prof. Marc Saez and Prof. Maria Antonia Barceló for their insightful comments during its development.

    
