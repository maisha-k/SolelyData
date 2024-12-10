
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SolelyData

<figure>
<img src="man/figures/solely_data_hex.png" alt="Solely Data" />
<figcaption aria-hidden="true">Solely Data</figcaption>
</figure>

<!-- badges: start -->
<!-- badges: end -->

The goal of **SolelyData** is to provide tools for cleaning, organizing,
and visualizing datasets efficiently. This package is inspired by the
idea of “cleaning the sole” of messy data, ensuring it’s ready for
analysis.

## Background and Motivation

The **SolelyData** package was developed to address common challenges in
data cleaning and preprocessing. Whether you’re working with messy
datasets from real-world sources or preparing data for advanced
analysis, **SolelyData** provides tools to make the process faster,
easier, and more reliable.

Key motivations for developing this package:

- **Time Efficiency**: Minimize manual intervention by automating
  repetitive data cleaning tasks.

- **Data Quality**: Ensure your datasets are free from inconsistencies,
  missing values, and formatting errors.

- **Versatility**: Designed to handle diverse datasets, including animal
  shelter data, survey responses, and other real-world records.

This package is especially useful for analysts, researchers, and data
scientists who want to focus on extracting insights rather than spending
hours cleaning data.

## Installation

You can install the development version of SolelyData like so:

`devtools::install_github("maisha-k/SolelyData")`

## About the Data

The **SolelyData** package includes a built-in dataset sourced from the
**King County Animal Shelter** dataset, which is publicly available on
[King County’s Open Data
Portal](https://data.kingcounty.gov/Pets/Lost-found-adoptable-pets/yaai-7frk/about_data).
This dataset provides valuable insights into animal shelter operations,
including information about lost, found, and adoptable pets in the King
County region.

#### Why Animal Shelter Data?

The **King County Animal Shelter** dataset was chosen as a built-in
example because it represents a real-world scenario with diverse
challenges, including:

- **Categorical Data**: Fields like animal type and breed require
  standardization and typo correction.

- **Date Validations**: Intake and outcome dates need to be consistent
  and accurate.

- **Missing Values**: Some records may have incomplete fields,
  highlighting the importance of handling missing data effectively.

The dataset offers a practical example to demonstrate how **SolelyData**
can be used to clean, organize, and prepare data for analysis.

## Functions in the Package

Here’s an overview of the core functions available in the package:

- `solotypo` : Identify single-occurrence values (typos) in a dataset
  and suggest corrections based on string similarity.
- `validate_dates` : Validate date columns in your dataset to ensure all
  entries conform to the correct format and fall within a specified
  range
- `flagweirdanimals`: Identify rows with unusual or unexpected
  categorical values based on a predefined list of valid options.
- `flagnonnumeric` : Identify non-numeric values in a numeric column and
  optionally replace them with NA.
- `missingsum` : Identify columns with missing values and summarize
  their counts or proportions.
- `plot_completeness` : Create intuitive visualizations to identify
  patterns of missing data.

## Examples

You can find these and more code examples for exploring SolelyData in
`vignettes("intro-to-solelydata")`.

This is a basic example which shows you how to solve a common problem:

``` r
library(SolelyData)

data <- data.frame(
  Fruit = c("apple", "APPLE", "appl", "orange","orange" ,"banana", "BANANA", "banan"),
  stringsAsFactors = FALSE
)

solotypo(data, column = "Fruit")
#> $appl
#> [1] "apple"
#> 
#> $banan
#> [1] "banana"
```

### Contributing

We welcome contributions! Whether it’s reporting a bug, suggesting new
features, or improving documentation, your input helps make
**SolelyData** better for everyone.

To contribute:

1.  Fork the repository:
    [`https://github.com/maisha-k/SolelyData`](https://github.com/maisha-k/SolelyData)

2.  Create a feature branch: `git checkout -b feature-name`

3.  Submit a pull request.

For questions or feedback, feel free to reach out through GitHub issues
or email.

### Getting Help

If you encounter any issues or have questions, check out the following
resources:

- **Documentation**: Detailed function descriptions and examples are
  available in the package’s help files.

- **Vignettes**: Explore step-by-step guides for specific use cases with
  `vignettes("intro-to-solelydata")`.

- **GitHub Repository**: Report bugs or request features at
  [`https://github.com/maisha-k/SolelyData`](https://github.com/maisha-k/SolelyData).

By adding sections like “Motivation,” “Advanced Usage,” “Real-World
Applications,” and “Comparisons with Other Tools,” you’ll significantly
expand the README while providing useful context for potential users.
