context("Get")

test_that("get_ilostat includes time and value",{
  skip_on_cran()
  expect_true(all(c("time", "obs_value") %in%
                    names(get_ilostat("UNE_2UNE_SEX_AGE_NB_A"))))
})


test_that("get_ilostat return right classes",{
  skip_on_cran()
  expect_true(all(c("character", "numeric") %in%
                    sapply(get_ilostat("UNE_2UNE_SEX_AGE_NB_A"), class)))

})

context("cache")

test_that("Cache give error if cache dir does not exist", {
  skip_on_cran()
  expect_error(
    get_ilostat("UNE_2UNE_SEX_AGE_NB_A", cache = TRUE, cache_dir = file.path(tempdir(), "test/mytest")))
})

test_that("Cache works", {
  skip_on_cran()
  expect_is({
    t_dir <- file.path(tempdir(), "rilostat")
    dir.create(t_dir)
    k <- get_ilostat("UNE_2UNE_SEX_AGE_NB_A", cache_dir = t_dir)
    },
    "data.frame")
})


context("Search")

test_that("search_ilostat finds",{
  skip_on_cran()
  expect_equal(get_ilostat_toc( search = 
		"KI")$collection[1],
    "KI"
  )
})


context("Label")

test_that("Variable names are labeled",{
  skip_on_cran()
  expect_equal({nrow(get_ilostat('UNE_2UNE_SEX_AGE_NB_A', type = 'label' , filters = list(ref_area.label = 'France'))) > 0}
  
  
  , TRUE)

})

test_that("Dic downloading works", {
  skip_on_cran()
  expect_error(get_ilostat_dic("na_item"))
})

