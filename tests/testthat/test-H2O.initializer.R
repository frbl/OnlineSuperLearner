library('h2o')
context("H2O.initializer.R")

context(' Static methods')
#==========================================================
context(' > H2O.Initializer')
#==========================================================
test_that("it should initialize without params", {
  mock_h2o_init <- mock()
  mock_h2o_clusterup <- mock(TRUE)

  with_mock(h2o.init = mock_h2o_init,
    with_mock(h2o.clusterIsUp = mock_h2o_clusterup,
      expect_error(H2O.Initializer(), NA)
    )
  )
})

test_that("it should disable the progressbar", {
  mock_h2o_noprogress <- mock()
  mock_h2o_clusterup <- mock(TRUE)
  mock_h2o_init <- mock()

  with_mock(h2o.no_progress = mock_h2o_noprogress,
    with_mock(h2o.init = mock_h2o_init,
      with_mock(h2o.clusterIsUp = mock_h2o_clusterup,
        H2O.Initializer()
      )
    )
  )
  expect_called(mock_h2o_noprogress, 1)
})

test_that("it should should check if the cluster is already available, and not start it again if it is", {
  mock_h2o_clusterup <- mock(TRUE)
  mock_h2o_init <- mock()
  mock_h2o_available <- mock(TRUE)

  with_mock(H2O.Available = mock_h2o_available,
    with_mock(h2o.init = mock_h2o_init,
      with_mock(h2o.clusterIsUp = mock_h2o_clusterup,
        H2O.Initializer()
      )
    )
  )
  expect_called(mock_h2o_available, 1)
  expect_called(mock_h2o_init, 0)
  expect_called(mock_h2o_clusterup, 0)

  ## Not yet started
  mock_h2o_available <- mock(FALSE)

  with_mock(H2O.Available = mock_h2o_available,
    with_mock(h2o.init = mock_h2o_init,
      with_mock(h2o.clusterIsUp = mock_h2o_clusterup,
        H2O.Initializer()
      )
    )
  )
  expect_called(mock_h2o_available, 1)
  expect_called(mock_h2o_init, 1)
  expect_called(mock_h2o_clusterup, 1)
})

test_that("it should throw if the cluster is not up after initializing", {
  mock_h2o_clusterup <- mock(FALSE)
  mock_h2o_init <- mock()
  mock_h2o_available <- mock(FALSE)

  with_mock(H2O.Available = mock_h2o_available,
    with_mock(h2o.init = mock_h2o_init,
      with_mock(h2o.clusterIsUp = mock_h2o_clusterup,
        expect_error(H2O.Initializer(host = 'test'), 'Connecting to cluster failed, at host test', fixed = TRUE)
      )
    )
  )
  expect_called(mock_h2o_clusterup, 1)
})


context(' > H2O.Available')
#==========================================================
test_that("it should return true if the cluster is up", {
  mock_h2o_clusterup <- mock(TRUE)

  with_mock(h2o.clusterIsUp = mock_h2o_clusterup,
  result <-  H2O.Available()
  )
  expect_called(mock_h2o_clusterup, 1)
  expect_true(result)
})

test_that("it should return false if its not up (by error)", {
  mock_h2o_clusterup <- mock(FALSE)

  with_mock(h2o.clusterIsUp = mock_h2o_clusterup,
    result <- H2O.Available()
  )
  expect_called(mock_h2o_clusterup, 1)
  expect_false(result)
})

test_that("it should return false if its not up (by boolean)", {
  mock_h2o_clusterup <- mock(throw('h2o DOWN'))

  with_mock(h2o.clusterIsUp = mock_h2o_clusterup,
    result <- H2O.Available()
  )
  expect_called(mock_h2o_clusterup, 1)
  expect_false(result)
})

test_that("it should return false if its not up (by warning)", {
  mock_h2o_clusterup <- mock(warning('h2o DOWN'))

  with_mock(h2o.clusterIsUp = mock_h2o_clusterup,
    expect_warning(result <- H2O.Available(), 'h2o DOWN')
  )
  expect_called(mock_h2o_clusterup, 1)
  expect_false(result)
})
