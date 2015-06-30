setwd("T:/FRISS Customer Data/CARGEAS/Data")

rm(list = ls())

#install.packages("permute")
library(permute)

ColNames <- read.csv("CARGEAS_batch.csv", nrows = 1, header = FALSE, quote = "", check.names = FALSE)
ColNames <- as.character(unlist(ColNames))
ColNames <- gsub(pattern = "[",replacement = "",x = ColNames,fixed = TRUE)
ColNames <- gsub(pattern = "]",replacement = "",x = ColNames,fixed = TRUE)

# ColNames[grep("identification",ColNames)]

Data           <- read.csv("C:/Users/hsontrop/Desktop/CARGEAS_batch.csv", skip = 1, header = FALSE, quote = "", check.names = FALSE, as.is = TRUE)

colnames(Data) <- ColNames


SelectedRoles  <- c("ph.identification_number","tp1.identification_number",
                    "tp2.identification_number","tp3.identification_number",
                    "dr.identification_number")


DD             <- Data[, c("sys.claimid","claim.date_occured",SelectedRoles)]

m              <- DD[,SelectedRoles] != ""
m              <- apply(m,1,sum) >= 2 

DD             <- DD[m,]

indRoles       <- t(combn(length(SelectedRoles),2))

roles          <- SelectedRoles[indRoles]
roles          <- matrix(roles,ncol= 2)

DD             <- Data[, c("sys.claimid","claim.date_occured",roles)]

Temp           <- array(dim = c(nrow(DD)*nrow(roles),4))

m1  <- seq(1,nrow(DD)*nrow(roles),by = nrow(roles))
m2  <- m1 + 1
m3  <- m1 + 2
m4  <- m1 + 3
m5  <- m1 + 4
m6  <- m1 + 5
m7  <- m1 + 6
m8  <- m1 + 7
m9  <- m1 + 8
m10 <- m1 + 9


Temp[m1,3] <- DD[,roles[1,1]]
Temp[m1,4] <- DD[,roles[1,2]]
Temp[m2,3] <- DD[,roles[2,1]]
Temp[m2,4] <- DD[,roles[2,2]]
Temp[m3,3] <- DD[,roles[3,1]]
Temp[m3,4] <- DD[,roles[3,2]]
Temp[m4,3] <- DD[,roles[4,1]]
Temp[m4,4] <- DD[,roles[4,2]]
Temp[m5,3] <- DD[,roles[5,1]]
Temp[m5,4] <- DD[,roles[5,2]]
Temp[m6,3] <- DD[,roles[6,1]]
Temp[m6,4] <- DD[,roles[6,2]]
Temp[m7,3] <- DD[,roles[7,1]]
Temp[m7,4] <- DD[,roles[7,2]]
Temp[m8,3] <- DD[,roles[8,1]]
Temp[m8,4] <- DD[,roles[8,2]]
Temp[m9,3] <- DD[,roles[9,1]]
Temp[m9,4] <- DD[,roles[9,2]]
Temp[m10,3] <- DD[,roles[10,1]]
Temp[m10,4] <- DD[,roles[10,2]]

Temp[c(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10),1] <- DD[,"sys.claimid"]
Temp[c(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10),2] <- DD[,"claim.date_occured"]

colnames(Temp) <- c("sys.claimid","claim.date_occured","role1","role2")

Temp[is.na(Temp[,"role1"]),"role1"] = ""
Temp[is.na(Temp[,"role2"]),"role2"] = ""

ind <- Temp[,c("role1","role2")] != ""
ind <- apply(ind,1,sum) == 2

Pairs <- Temp[ind,]

Pairs.sorted <- t(apply(Pairs[,c("role1","role2")],1,sort))

Pairs[,c("role1","role2")] <- Pairs.sorted

ind.duplicated <- duplicated(Pairs[,c("claim.date_occured","role1","role2")])

Pairs <- Pairs[!ind.duplicated,]

# ind.duplicated.pairs <- which(duplicated(Pairs[,c("role1","role2")]))
# duplicate.list <- Pairs[ind.duplicated.pairs,]
# duplicate.list <- duplicate.list[order(duplicate.list[,"role1"]),]
# 
Pairs <- as.data.frame(Pairs)
duplicate.list <- as.data.frame(duplicate.list)
# indPairs <- Pairs$role1 %in% duplicate.list$role1 | Pairs$role2 %in% duplicate.list$role2

final.list <- Pairs[indPairs,]

final.list <- final.list[order(final.list$role1),]

tblRoles1 <- table(final.list$role1)
ind1      <- tblRoles1[tblRoles1>20 | tblRoles1==1]

tblRoles2 <- table(final.list$role2)
ind2      <- tblRoles1[tblRoles2>20 | tblRoles2==1]

final.list$role1 <- as.character(final.list$role1)
final.list$role2 <- as.character(final.list$role2)

final.list <- final.list[!(final.list$role1 %in% names(ind1)),]
final.list <- final.list[!(final.list$role2 %in% names(ind2)),]

indDup <- duplicated(final.list[,c("role1","role2")])

ids1 <- final.list$role1[indDup]
ids2 <- final.list$role2[indDup]

final.list2 <- final.list[final.list$role1 %in% ids1 & final.list$role2 %in% ids2,]

library(xlsx)

final.list <- final.list[duplicated(final.list[,c("role1","role2")]),]

write.xlsx(final.list2,file="pairs_cargeas_ontdubbeld2.xlsx")
