flp <- c("ArizonaExportByPrecinct_11082022_11092022.txt",
"ArizonaExportByPrecinct_11082022_11112022.txt",
"ArizonaExportByPrecinct_11082022_11122022.txt",
"11-08-2022-2b Final SOV and Official Canvass Report.txt")




fls <- c('09Nov2022_145am/ArizonaExportByPrecinct_11082022_0145am.txt',
'09Nov2022_648pm/ArizonaExportByPrecinct_11082022_11092022.txt',
'10Nov2022_800pm/ArizonaExportByPrecinct_11082022.txt',
'11Nov2022_806pm/ArizonaExportByPrecinct_11082022_11112022.txt',
'12Nov2022_600pm/ArizonaExportByPrecinct_11082022_11122022.txt',
'13Nov2022_619pm/ArizonaExportByPrecinct_11082022_11132022.txt',
'14Nov2022_639pm/ArizonaExportByPrecinct_11082022_11142022.txt',
'15Nov2022_657pm/ArizonaExportByPrecinct_11082022_11152022.txt',
'16Nov2022_907pm/ArizonaExportByPrecinct_11082022_11162022.txt',
'17Nov2022_1209pm/ArizonaExportByPrecinct_11082022_11172022.txt',
'18Nov2022_657pm/ArizonaExportByPrecinct_11082022_11182022.txt',
'19Nov2022_634pm/ArizonaExportByPrecinct_11082022_11192022.txt',
'21Nov2022_139pm/ArizonaExportByPrecinct_11082022_11212022.txt', 
'final/11-08-2022-2b Final SOV and Official Canvass Report.txt')[-14]
pick <- paste0(rprojroot::find_rstudio_root_file(),'/data-raw/Arizona_2022/maricopa/zipbox/3/MaricopaAZ/')
deli <- paste0(rprojroot::find_rstudio_root_file(),'/data-raw/Arizona_2022/maricopa/txt_final/')
sapply(seq(1,length(fls)),function(x)system(paste0('cp ',pick,fls[x],' ',deli)))
pick2 <- paste0(rprojroot::find_rstudio_root_file(),'/data-raw/Arizona_2022/maricopa/zipbox/3/MaricopaAZ/')
deli2 <- paste0(rprojroot::find_rstudio_root_file(),'/data-raw/Arizona_2022/maricopa/txt_final/')


#Note this final data snapshot is identical in vote counts to 21Nov2022_139pm, the county just renamed the file when producing the final report from what I can tell.
#final/11-08-2022-2b\ Final\ SOV\ and\ Official\ Canvass\ Report.txt
#ctx <- system(paste0('cp ',pick,fls[1],' ',deli),T)
