#Author by 2017/08/18	Calculate deviation between theory and fact according CK

input <- "LYG.csv"
CK <- "CK"
col <- 5																	#Target column

df <- read.delim(input, header=T, sep=",", comment.char="", fill=T, stringsAsFactors=F)
list <- which(df$P==CK,arr.ind=F)
output <- paste0("_",input)
df$addtion <- 0
nr <- nrow(df)																#Efficiency line amount
dvt <- vector()

for(i in 1:(length(list)-1)){												#Insert unit distance
	unit <- list[i+1] - list[i]
	for(j in list[i]:(list[i]+unit-1)){
		df[j,ncol(df)] <- unit
	}
}

CKA <- 0																	#Init varible value
CKB <- 0
portion <- 0

for(i in 1:nr){
	step <- df[i,ncol(df)]

	condition <- grepl(CK, df[i,], ignore.case=TRUE, perl=FALSE, fixed=FALSE, useBytes=FALSE)#Distinguish lines
	cond <- condition[1]
	for(j in 2:length(condition)){
		cond <- xor(cond,condition[j])
	}

	if(cond){																#CK lines
		CKA <- df[i,col]
		if(i<nr){
			CKB <- df[i+step,col]
		}
		portion <- 0
		dvt[i] <- 0
	}else{																	#Ordinary lines
		portion = portion + 1
		theory <- (1-portion/step)*CKA + portion/step*CKB
		dvt[i] <- (df[i,col]-theory)/df[i,col]
	}
}

write.csv(dvt, file=output, row.names=F, quote=F)
#Note:
#1.Absence target maybe cluded in data
#2.Same CK strings maybe cluded in CK