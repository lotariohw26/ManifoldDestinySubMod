




library(ManifoldDestiny)
library(reticulate)
fqs <- reticulate::import("fqs")
p4 <- c(1,2,3,4,5)
fqs$quartic_roots(p4)
p3 <- c(1,2,3,4)
fqs$cubic_roots(p3)
# Dallas
filename <- paste0(ManifoldDestiny::abs_path(),'/data-raw/xlsx/Dallas Texas, Completed.xlsx')
#"/home/joernih/research/ManifoldDestiny/data-raw/xlsx/Dallas Texas, Completed.xlsx"
#openxlsx::getSheetNames(file=filename)
library(dplyr)
library(reticulate)
fqs <- reticulate::import("fqs")
dallas <- openxlsx::read.xlsx(filename, sheet=" Y Dallas Quartic Positive D0") 
dallas_sel <- dallas %>%
	dplyr::select(A,B,C,D,E) %>% 
        dplyr::mutate(pri=row_number()) %>%
	dplyr::group_by(pri) %>%
	dplyr::mutate(test=fqs$quartic_roots(c(A,B,C,D,E))[1]) %>%
	dplyr::mutate(test1r=fqs$quartic_roots(c(A,B,C,D,E))[1]) %>%
	dplyr::mutate(test2r=fqs$quartic_roots(c(A,B,C,D,E))[2]) %>%
	dplyr::mutate(test3r=fqs$quartic_roots(c(A,B,C,D,E))[3]) %>%
	dplyr::ungroup()
View(dallas_sel)
names(dallas_sel)
ds <- cbind(dallas_sel,dallas)
ds; l()
#  [1] "A"                               "B"                               "C"                               "D"                               "E"                              
#  [6] "y"                               "Accept.Final"                    "Resid"                           "v"                               "Force.Real"                     
# [11] "Force.Img"                       "Accept.Root"                     "1.vs.2"                          "3.v.4"                           "Root.1"                         
# [16] "Root.2"                          "Root.3"                          "Root.4"                          "Root.1.Real"                     "Root.1.Img"                     
# [21] "Root1.Img.Zero.Finder"           "Root.2.Real"                     "Root.2.Img"                      "Root2.Img.Zero.Finder"           "Root.3.Real"                    
# [26] "Root.3.Img"                      "Root3.Img.Zero.Finder"           "Root.4.Real"                     "Root.4.Img"                      "Root4.Img.Zero.Finder"          
# [31] "ɑ"                               "β"                               "ɣ"                               "P"                               "Q"                              
# [36] "CubeRootQ"                       "R1"                              "R2,1"                            "R2,2"                            "R2"                             
# [41] "R.Discrim"                       "R.real"                          "R.img"                           "Theta"                           "Mag"                            
# [46] "CuberootMag"                     "1/3Theta"                        "Ureal"                           "Uimg"                            "If.U.=.0.Boolean"               
# [51] "Y.False.Real"                    "Y.False.Img"                     "P/3U.real"                       "P/3U.img"                        "P/3U.denom"                     
# [56] "P/3u.True.Real"                  "P/3u.True.Img"                   "Y.True.Real"                     "Y.True.Img"                      "Y.Accept.Real"                  
# [61] "Y.Accept.Img"                    "W^2.real"                        "W^2.img"                         "Theta"                           "½.Theta"                        
# [66] "Mag.W^2"                         "Mag.W"                           "Real.W"                          "Img.W"                           "Minor.Discrim.Denom"            
# [71] "Minor.Discrim.Num.Real"          "Minor.Discrim.Num.Img"           "Real.λ"                          "Img.λ"                           "Major.Discrim.Real.Root.1.and.2"
# [76] "Major.Discrim.Img.Root.1.and.2"  "Major.Discrim.Real.Root.3.and.4" "Major.Discrim.Img.Root.3.and.4"  "Theta.1,2"                       "Mag.1,2"                        
# [81] "Theta.3,4"                       "Mag.3.4"                         "Root.1,2.Discrim.Real"           "Root.1,2.Img"                    "Root.3,4.Discrim.Real"          
# [86] "Root.3,4.Img"                   
library(ManifoldDestiny)
p4 <- c(1,2,3,4,5)
fqs$quartic_roots(p4)
p3 <- c(1,2,3,4)
fqs$cubic_roots(p3)

