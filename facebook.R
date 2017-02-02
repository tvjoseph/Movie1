## JMJPFU # 
# 25 Jan 2016
# Facebook Mining with R # 
library(Rfacebook)
library(httpuv)
library(RColorBrewer)
library(RCurl)
library(rjson)
library(RJSONIO)
library(httr)
library(devtools)
## 

fb_oauth <- fbOAuth(app_id="939780479392055", app_secret="c266ef4d09fa5cf7850c32351fae6a7f",extended_permissions = FALSE)## When the extended permissions
# were changed from TRUE to FALSE it worked

save(fb_oauth, file="fb_oauth") ## Saved the authorisation file
load("fb_oauth")

me <- getUsers("me",token=fb_oauth) # Got my users

myfriends <- getFriends(token=fb_oauth,simplify = TRUE)


my_fbaccesstoken <- "CAACEdEose0cBAGPtxygYZBfjZAhC4ucKlpEAenVpM0M0uvwzNhdv3rK573HhTSa3Yw69ikbdXKXusyVku9O6pAd10qi3o1fbsPrHlKGqryS8jsOmTZCnpEhF5gZBUtEDlJppFyxZC3bcqw6hCpKzwNnoquAMaz0iZBprEpi6DUhg0v6eZCUmzNBCVDvN94RVId5XJoZBfhB5jWWp1javOqAr"

options(RCurlOptions = list(verbose=FALSE,capatch=system.file("Curlssl","cacert.pem",package="RCurl"),ssl.verifypeer=FALSE))
me <- getUsers("me",token=my_fbaccesstoken)
myfriends <- getFriends(token=my_fbaccesstoken,simplify = TRUE) ## Did not quite work

mat <- getNetwork(my_fbaccesstoken,format="adj.matrix") ## Did not work

posts <- searchFacebook(string = "Christmas", my_fbaccesstoken, n = 500, since = "25 december 2015 00:00", until = "25 december 2015 23:59")
