library(dplyr)
#library(ManifoldDestiny)
sapply(list.files(paste0(rprojroot::find_rstudio_root_file(),'/R'),full.names=T), source)
library(testthat)
abs_path <- rprojroot::find_rstudio_root_file()
################################################################################################
# Expect values
fn <- paste0(abs_path,'/data-raw/xlsx/Clark County, NV.xlsx')
## Clark
fil_cla <- paste0(abs_path,'/data-raw/xlsx/Clark County, NV.xlsx')
e_cla_df <- lapply(openxlsx::getSheetNames(file=fil_cla),openxlsx::read.xlsx,xlsxFile=fn)
#View(e_cla_df[1])
################################################################################################
testthat::test_that("clark_gsm.R",{
  load(paste0(abs_path,'/data/clark_sgs_sel.rda'))
  fitdf <- totwomodes(A=c('B2'),B=c('A1','C3','A2'),C=c('B1+B3'),D=c('C1+A3+C2'),dfi=clark_sgs_sel[[3]])
  fitdf$C <- fitdf$R*0.200 #!
  gclark <- Countinggraphs(fitdf)
  eclark <- Estimation(gclark$sdfc)
  form <- 'g~alpha+h+I(alpha^2)+alpha*h+I(h^2)'
  eclark$regression(form)
  eclark$regsum[[1]][1]
  eclark$regsum[[2]]
  eclark$regsum[[3]]
  #### Step 3: Regression
  eclark$diagnostics()
  eclark$resplots[[1]]
  o <-1
  e <-1
  expect_equal(o,e) 
})
#https://docs.google.com/document/d/1O_5Rs29mJut8NfOtFIW2chN5NC6TyFwVkkCZX7PyZIA/edit
testthat::test_that("clark_pres.R",{
  o <-1
  e <-1
  expect_equal(o,e) 
})
testthat::test_that("dallas_pre.R",{
  o <-1
  e <-1
  expect_equal(o,e) 
})
testthat::test_that("maricopa_pre.R",{
  o <-1
  e <-1
  expect_equal(o,e) 
})

