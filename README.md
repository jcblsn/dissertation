# dissertation

This repository contains my Master's dissertation along with the code and data used for my analysis.

## Paper

The abstract provides a summary of the dissertation:

>This study examines the repercussions of Germany's 2021 nuclear power plant closures on its fossil fuel electricity generation in 2022 using a dataset documenting European electricity generation from 2015-2022. Four causal inference methodologies were employed: differences-in-differences, synthetic control, Bayesian structural time series, and Gaussian process regression. The goal was to determine the extent of potentially increased reliance on fossil fuels following the nuclear phase-out. The research integrates monthly electricity generation figures from various European countries and incorporates average temperature as a control variable to account for weather-related seasonality.

>The analysis reveals no evidence for an increase in Germany's fossil fuel electricity generation in 2022 as a result of the nuclear plant shutdowns. Among the models applied, Gaussian process regression stood out due to its nonparametric quality and Bayesian uncertainty quantification, complemented by a novel technique for automated feature selection. Contrary to popular anticipation as a result of past research focused on previous closures, the results challenge the preconceived notion of an inherent rise in fossil fuel utilization post-nuclear phase-out.

>Methodologically, the study highlights the utility of combining flexible Bayesian modeling with more established causal inference techniques. In summary, this investigation not only offers valuable insights into the dynamics of energy transitions but also underscores the importance and potential of data-driven causal inference in understanding such shifts. The paper concludes with recommendations for more detailed exploratory studies incorporating richer datasets and enhanced modeling techniques in the future.

## Contents

The dissertation is rendered using [Quarto](https://quarto.org/). The source code and resultant PDF are contained in the `document` folder.

The R and Python code for data cleaning, analysis, and visualization is contained in the `scripts` folder.

The `data` folder contains the raw data used in the analysis. The data was obtained from the following public sources:

- [ENTSO-E Transparency Platform](https://transparency.entsoe.eu/)
- [OpenWeatherMap](https://openweathermap.org/)

The electricity generation data was extracted with R using the [entsoeapi](https://github.com/krose/entsoeapi) package.

## Reproducibility

Resources have been made available to easily reproduce the analysis documented in the paper. To do so, two environments need to be set up.

1. For the Python scripts, a conda environment can be created using the `environment.yml` file:

```bash
conda env create --prefix env -f environment.yml
```

2. For the R scripts, a [renv](https://rstudio.github.io/renv/index.html) environment can be created using the `renv.lock` file:

```r
renv::restore()
```

## License

[![License: CC BY-SA 4.0](https://img.shields.io/badge/License-CC_BY--SA_4.0-lightgrey.svg)](https://creativecommons.org/licenses/by-sa/4.0/)
