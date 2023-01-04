##########
# ABCD Extreme heat and mental health


#### This project uses the following ABCD instruments [version 4.0]:

1. pdem02
2. abcd_lpds01
3. abcd_rhds01
4. acspsw03
5. abcd_ksad01
6. abcd_lt01
7. abcd_ksad501


#### How to run the code:

1. update the [config.R](config.R) to reflect the location of the instruments above.
2. In the data-scripts folder, run scripts in any order. These scripts go over the abcd instruments and create new variables and datasets that are placed in the “data” folder.
3. Run the [merging.R](scripts/merging.R) script to create the dataset.
4. Run the [descriptive_table.Rmd](scripts/descriptive_table.Rmd) to generate table 1 of the main paper.
5. Run the [create_plots.Rmd](scripts/create_plots.Rmd) to create 2 figures for the paper.
6. Run the [analysis.Rmd](scripts/analysis.Rmd) for mixed models.