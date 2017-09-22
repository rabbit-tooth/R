#Auth by 2017/9/22				R --vanilla --slave --args input < HWE_SNPassoc.R
args <- commandArgs()				#Compute HardyWeinberg p value by SNPassoc
input <- args[5]
library("SNPassoc")

df <- read.delim(input, header=T, row.names=1, sep=",", comment.char="", fill=T, stringsAsFactors=F)
df <- t(df)
hwe <- data.frame(SNP=0,HWE=0)[-1,]

for(i in 1:ncol(df)){
  marker <- snp(df[,i],sep="")
  pv <- summary(marker)[6]
  if(pv>0.05){
    hwe[i,1] <- colnames(df)[i]
    hwe[i,2] <- pv
  }
}

write.csv(hwe, file="HWE.csv", row.names=F, quote=F)

#P>0.05 means accord with HWE. or deviate HWE, which indicates that it's a
#non-random population, sub-populations maybe exists.
#Sample size is one aspect, but not all. 0.05 is a common threshold, sometimes 0.01 also used.
#Multiple test significance level for multiple markers usually more tolerant than single test.
#e.g. it usually be 0.001 or 0.0001 for GWAS.