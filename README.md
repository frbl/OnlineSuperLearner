# OnlineSuperLearner: SuperLearner with online functionality for time-series analysis

[![TravisCI](https://travis-ci.org/frbl/OnlineSuperLearner.svg?branch=master)](https://travis-ci.org/frbl/OnlineSuperLearner)
[![CircleCI](https://circleci.com/gh/frbl/OnlineSuperLearner.svg?style=svg&circle-token=5badec7bf5e36f14cb653dae793d9495eeb540b3)](https://circleci.com/gh/frbl/onlinesuperlearner)
[![codecov](https://codecov.io/gh/frbl/OnlineSuperLearner/branch/master/graph/badge.svg?token=1s11gjN38m)](https://codecov.io/gh/frbl/OnlineSuperLearner)
[![Codacy Badge](https://api.codacy.com/project/badge/Grade/adb6ba75281742f59ff610611cfad947)](https://www.codacy.com/app/frbl/OnlineSuperLearner?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=frbl/OnlineSuperLearner&amp;utm_campaign=Badge_Grade)

This is the current version of the Online SuperLearner for Time-Series data R package. Note that this version is in active development, and considered to be pre-alpha software. Be very careful interpretting any results from this package. 

## Features

* Automatic optimal predictor ensembling via sequential cross-validatio.
* Can be extended with several algorithms.
* Has several pre-defined summary measures

### Install the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("frbl/OnlineSuperLearner")
```
[devtools]: https://github.com/hadley/devtools

## Examples 

For an example on how to run the OnlineSuperLearner, view the Jupyter notebook, or the `R/OnlineSuperLearner.Simulation.R` file. For a complete guide see [the documentation](https://frbl.eu/OnlineSuperLearner).

[Jupyter]: http://jupyter.org/

You can also run the demos for the project. Run:
``` R
demo('cpp-demo', package = 'OnlineSuperLearner')
```

## TODO

* View the issues page

## References 

Polley EC, van der Laan MJ (2010) Super Learner in Prediction. U.C. Berkeley Division of Biostatistics Working Paper Series. Paper 226. <http://biostats.bepress.com/ucbbiostat/paper266/>

van der Laan, M. J., Polley, E. C. and Hubbard, A. E. (2007) Super Learner. Statistical Applications of Genetics and Molecular Biology, 6, article 25. <http://www.degruyter.com/view/j/sagmb.2007.6.issue-1/sagmb.2007.6.1.1309/sagmb.2007.6.1.1309.xml>

van der Laan, M. J., & Rose, S. (2011). Targeted learning: causal inference for observational and experimental data. Springer Science & Business Media. <http://www.targetedlearningbook.com>

Benkeser, D., Ju, C., Lendle, S. D., & van der Laan, M. J. (2016). Online Cross-Validation-Based Ensemble Learning. U.C. Berkeley Division of Biostatistics Working Paper Series, Paper 355. <http://biostats.bepress.com/ucbbiostat/paper355/>
