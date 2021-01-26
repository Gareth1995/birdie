# Trying to the CWAC API
# 11 Oct 2019

# from the CWAC website: http://cwac.adu.org.za/api.php


library(RCurl)
library(digest)
library(rjson)
library(lubridate)
# library(jsonlite)


# Retrieve a full list of all the active CWAC sites registered in the Western Cape
# *******************************************************************************
url='http://api.adu.org.za/cwac/sites/list?province=western%20cape'

myfile <- getURL(url, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)

json_file <- rjson::fromJSON(myfile)

json_file <- lapply(json_file, function(x) {
  x[sapply(x, is.null)] <- NA
  unlist(x)
})

df<-as.data.frame(do.call("rbind", json_file))

write.csv(df, "WC_cwac_sites.csv", row.names = F)
df <- read.csv("WC_cwac_sites.csv")

head(df[,-9])

df$FirstCount <- as.Date(df$FirstCount)
df$LatestCount <- as.Date(df$LatestCount)

summary(df[,-9])

df$LatestYear <- year(df$LatestCount)
df$FirstYear <- year(df$FirstCount)

df$FirstYear[df$FirstYear==15] <- 2015  # a data entry error

hist(df$LatestYear)
hist(df$LatestYear - df$FirstYear)


# Retrieve a full list of all the active CWAC sites per province:
# Options: Northern Province, Mpumalanga, North West, Gauteng, KwaZulu-Natal, 
# Free State, Northern Cape, Western Cape, Eastern Cape, Kenya, Angola, Tanzania, Limpopo

#url='http://api.adu.org.za/cwac/sites/list?province=northern%20cape'
#url='http://api.adu.org.za/cwac/sites/list?province=gauteng'
#url='http://api.adu.org.za/cwac/sites/list?province=kwazulu-natal'
#url='http://api.adu.org.za/cwac/sites/list?province=free%20state'
#url='http://api.adu.org.za/cwac/sites/list?province=mpumalanga'
url='http://api.adu.org.za/cwac/sites/list?province=limpopo'

myfile <- getURL(url, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)

json_file <- rjson::fromJSON(myfile)

json_file <- lapply(json_file, function(x) {
  x[sapply(x, is.null)] <- NA
  unlist(x)
})

df<-as.data.frame(do.call("rbind", json_file))

#write.csv(df, "NC_cwac_sites.csv", row.names = F)
#read.csv("NC_cwac_sites.csv")
#write.csv(df, "GP_cwac_sites.csv", row.names = F)
#df <- read.csv("GP_cwac_sites.csv")
# write.csv(df, "KZN_cwac_sites.csv", row.names = F)
# df <- read.csv("KZN_cwac_sites.csv")
# write.csv(df, "FS_cwac_sites.csv", row.names = F)
# df <- read.csv("FS_cwac_sites.csv")
# write.csv(df, "MP_cwac_sites.csv", row.names = F)
# df <- read.csv("MP_cwac_sites.csv")
write.csv(df, "L_cwac_sites.csv", row.names = F)
df <- read.csv("L_cwac_sites.csv")

head(df[,-9])

df$FirstCount <- as.Date(df$FirstCount)
df$LatestCount <- as.Date(df$LatestCount)

df$LatestYear <- year(df$LatestCount)
df$FirstYear <- year(df$FirstCount)

summary(df[,-9])

hist(df$LatestYear)
hist(df$LatestYear - df$FirstYear)
  

# List all cards submitted for a CWAC site
# *******************************************************************************

url='http://api.adu.org.za/cwac/site/cards/list?locationCode=33561832' 

myfile <- getURL(url, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)

json_file <- rjson::fromJSON(myfile)

json_file <- lapply(json_file$cards, function(x) {
  x[sapply(x, is.null)] <- NA
  unlist(x)
})

df<-as.data.frame(do.call("rbind", json_file))

head(df)



# -- THE CODE BELOW IS NOT WORKING PROPERLY YET -- IGNORE ------

# download full data for a single list
# *******************************************************************************

url='http://api.adu.org.za/cwac/cards/single/get?card=508082'

myfile <- getURL(url, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)

mydata <- read.csv(textConnection(myfile), header=T)

head(mydata)



# A summary of the species reported for a CWAC site during survey
# *******************************************************************************

url='http://api.adu.org.za/cwac/cards/single/list?locationcode=34262023'

myfile <- getURL(url, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)

json_file <- rjson::fromJSON(myfile)

json_file <- lapply(json_file, function(x) {
  x[sapply(x, is.null)] <- NA
  unlist(x)
})

df<-as.data.frame(do.call("rbind", json_file))

write.csv(df, "deHoop.csv")
df <- read.csv("deHoop.csv")


# A dictionary of all the codes used in the CWAC data
# *******************************************************************************

url='http://api.adu.org.za/cwac/dictionary/get'

myfile <- getURL(url, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)

mydata <- read.csv(textConnection(myfile), header=T)

head(mydata)


# Retrieve the boundary polygon coordinates for a CWAC location
# *******************************************************************************

url='http://api.adu.org.za/cwac/site/boundary/get?locationCode=32481810'

myfile <- getURL(url, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)

mydata <- read.csv(textConnection(myfile), header=T)

head(mydata)


# List all cards submitted for a CWAC site
# *******************************************************************************

url='http://api.adu.org.za/cwac/site/cards/list?locationCode=32481810'

myfile <- getURL(url, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)

mydata <- read.csv(textConnection(myfile), header=T)

head(mydata)


# Retrieve the available site information for a CWAC site

url='http://api.adu.org.za/cwac/site/information/get?locationCode=32481810'

myfile <- getURL(url, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)

mydata <- read.csv(textConnection(myfile), header=T)
mydata <- textConnection(myfile)

head(mydata)


# Retrieve a full list of all the active CWAC sites registered in the system
# *******************************************************************************

url='http://api.adu.org.za/cwac/sites/list?province=western%20cape'

myfile <- getURL(url, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)

mydata <- read.csv(textConnection(myfile), header=T)

head(mydata)

head(names(mydata))
str(mydata)


# mark-up copy
