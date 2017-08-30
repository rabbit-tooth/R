#Author by 2017/08/20											SNP cluster

input <- "snp.csv"												#Args
output <- "1-.csv"
pct <- 1														#100%

df <- read.delim(input, header=T, row.names=1, sep=",", comment.char="", fill=T, stringsAsFactors=F)
id <- 0
df$binid <- 0													#Flag isbin

for(i in 1:(nrow(df)-1)){
	cat("Progress: ",i," processing...\n")
	if(df[i,ncol(df)]!=0){										#Ommit the processed SNP
		next
	}
	id <- id + 1
	df[i,ncol(df)] <- id

	for(j in (i+1):nrow(df)){									#Process SNPs below current
		if(df[j,ncol(df)]!=0){									#Ommit processed SNPs
			next
		}
		count <- 0												#Record amount of match between 2 SNPs

		for(k in 1:(ncol(df)-1)){								#Compare all individuals
			if(df[i,k]==df[j,k]){
				count = count + 1
			}
		}

		percentage <- count / (ncol(df)-1)						#Judge if 2 SNPs locate in a bin
		if(percentage>=pct){
			df[j,ncol(df)] <- id
		}
	}
}

if(df[nrow(df),ncol(df)]==0){									#The last line
  id <- id + 1
  df[nrow(df),ncol(df)] <- id
}

snpbin <- data.frame(snp=rownames(df),bin=df[,"binid"])			#data.frame equal cbind|[col]/[row]/names(?)[?]:rename col/row
write.csv(snpbin, file=output, row.names=F, quote=F)