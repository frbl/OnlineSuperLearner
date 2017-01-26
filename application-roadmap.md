# Basic roadmap
1. create structure of package

2. set up automatic compiling and testing

3. Use packages:
 - 'R.oxygen'
 - 'R.utils' ('Arguments$getNumeric'  and  the  likes, 'throw',  use  of 'verbose')

```
  ## Example of verbose
  ## Argument 'verbose':
  verbose <- Arguments$getVerbose(verbose)
  verbose <- less(verbose, 10)
  verbose && print(verbose, table(U))
  verbose && enter(verbose, "Simulated  copy number and expression data
  for class 1")
  verbose && str(verbose, V)
  verbose && exit(verbose)
```

4. Keep in mind that we're interested  in oos (online one-step) and otmle (online
tmle) estimators

  4a. new object S3 structure

  4b. high-level description -- flow of the application

    (i) learning step
    (ii) targeting step

  4c. encoding of a generic time series vocabulary:

    - (W(t), A(t), Y(t)) is the t-th block
    - W(t), A(t) and Y(t) are its nodes
    - W(t) in WW and Y(t) in YY, WW and YY possibly multi-dim
    - A(t) is discrete/binary?

  4d. simulation
    (i) characterization of data-generating distribution
    (ii) simulation under the data-generating distribution
    (iii) characterization of the parameter  of interest 
      - => providing intervention nodes, and the corresponding intervention distributions
    (iv) evaluation of the parameter of interest

  4e. encoding of summary measures of the past of each {W,A,Y}-node

  4f. write functions implementing *online* (super-) learning
    - must rely on *online* "prediction" algorithms
    - think about H2O...

  4g. Monte-Carlo procedures to derive
    - the estimates of the parameter from the estimated features
    - the efficient influence curve, used for targeting + CI
    (see draft)
