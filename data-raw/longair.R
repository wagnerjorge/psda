longair <- read.csv('G:/Dropbox_Novo/Dropbox/1. Mestrado/2. Dissertacao/1. SDA/script/projeto/package/psda/data-raw/longair.csv',
                    head = T, sep = '')
devtools::use_data(longair, overwrite = T)
