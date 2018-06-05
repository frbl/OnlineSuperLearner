# Contributing

1. Fork it
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Run bundle, before starting development.
4. Implement your feature/bugfix and corresponding tests.
5. Make sure your tests run against the latest stable mri.
6. Commit your changes (`git commit -am 'Add some feature'`)
7. Push to the branch (`git push origin my-new-feature`)
8. Create new Pull Request

# Development


## Adding new learners
The learners in the OSL require to be exposed using a predefined format. In this section we will provide a roadmap on how to get started for developing your own learner.

### 1. Create a new learner file
Each of the learners included in the OSL are prefixed with `ML`, to signify the machine learning part. Although this is not strictly enforced, it will be in future versions of the OSL. So start with creating a new learner file:
```bash
  touch R/ML.<YOURLEARNERNAME>.R
```

### 2. Create the R6 Skeleton
OSL heavily relies on the use of R6 classes. So do the wrappers included in the OSL. To include the skeleton in your file, edit the created file:

```bash
  vim R/ML.<YOURLEARNERNAME>.R
```

and paste in the skeleton code
``` R
#' ML.<YOURLEARNERNAME>
#'
#' <DESCRIPTION OF THE LEARNER>
#' 
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom <NEEDED IMPORTS FROM EXTERNAL PACKAGED>
#' @section Methods: 
#' \describe{  
#'   \item{\code{initialize() }}{ 
#'     <DESCRIPTION OF THE HYPERPARAMETERS OF THE NEW LEARNER>
#'   } 
#' }  
#' @export
ML.<YOURLEARNERNAME> <- R6Class("YOURLEARNERNAME",
  inherit = ML.Base,
  public =
    list(
      fitfunname="<A STRING DESCRIBING THE FIT FUNCTION USED>",
      lmclass="<A STRING DESCRIBING THE CLASS>",
      initialize = function() {
      }
    ),
  active =
    list(
    ),
  private =
    list(
      do.fit = function(X_mat, Y_vals, coef = NULL) {
        # Create code to fit the model 
        return(m.fit)
      },

      do.update = function(X_mat, Y_vals, m.fit = NULL, ...) {
        # Create code to update the model
        return(m.fit)
      },

      do.predict = function(X_mat, m.fit = NULL) {
        # Create code to perform the predictions using the model
        return(predictions)
      }
    )
)
```

This should provide you with the basic structure needed to build your own algorithms.

### 3. Include the learner in the factory
In order for the learner to be picked up by the factory, make sure to include it in the `ML.models.allowed` section of the `LibraryFactory` class. Adding the class name of the newly added learner should be enough to use it.

### 4. Write tests
Testing is important for the correct functionality of the OSL. Whenever you create new classes / code, make sure to include a test for the added code. See the part on testing for more details on structuring the tests

## Writing tests
Writing tests is very important for the OSL to function well. As such, always include unit tests for all newly added code. We currently adhere to a structure where we associate one classfile with one testfile. These files have the same name, although the test file is prefixed with `test-`. In order to ensure nice logging, we use several conventions.

1. The first line of the test file always needs to be `context("<YOURCLASSNAME>.R")`. This way the logs nicely show which test is running when, and what it runs on. 

2. For each function that is tested in separation, add a `context(" <FUNCTIONNAME>")` before the set of specs covering that specific function. Note the space after the first set of quotes. This space ensures that the logging nicely indents the output for each of the functions as well.  

3. If you write an integration test, make sure to to use the naming convention `test-i-INTEGRATIONTESTNAME.R` (note the `-i-`), to indicate the test is an integration test.

