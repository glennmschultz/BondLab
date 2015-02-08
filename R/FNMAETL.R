library(RCurl)
url <- "https://mbsdisclosure.fanniemae.com/disclosure-docs/monthly/mbs022015.zip"

url <- "https://mbsdisclosure.fanniemae.com/"
userpwd <- "glennmschultz@me.com:Kodiak18j#"
filenames <- getURL(url, 
                    #userpwd = userpwd, 
                    #ftp.use.epsv = FALSE, 
                    #dirlistonly = TRUE,
                    #postfields = c()
                    )