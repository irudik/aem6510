# Makes pdf slides for all lectures
oldwd <- getwd()
setwd("~/Desktop/git/aem6510/lecture-notes/")
lapply(list.files(pattern = "*.html", recursive = T)[14], 
       function(file) {
         xaringan::decktape(
           file, 
           output = paste0(tools::file_path_sans_ext(file), ".pdf")
           )
       }
)
setwd(oldwd)