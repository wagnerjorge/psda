longair <- read.csv('G:/Dropbox_Novo/Dropbox/3.package/psda/data-raw/longair.csv',
                    head = T, sep = '')
devtools::use_data(longair, overwrite = TRUE)
