genotype <- read.delim("data/Y3.txt", header=T, sep="\t", comment.char="", fill=T, stringsAsFactors = F)
phenotype <- read.delim("data/YZ1_3_XX.txt", header=F, sep="\t", comment.char="", fill=T, stringsAsFactors = F, row.names = 1)

geno <- genotype[,-c(1:3)]                                          #-c(1,2,3)
p_data <- as.data.frame(genotype[,1:3], stringsAsFactors = F)       #genotype$probeset_id
names(p_data)[1:3] <- c("Marker","Locus","Site")
pheno <- phenotype[-1,]
title <- phenotype[1,]

for(i in 1:ncol(pheno)){
  y <- as.numeric(pheno[,i])
  y <- data.frame(y = y)                                            #as.data.frame
  
  for(j in 1:nrow(geno)){
    x <- data.frame(t(geno[j,]))
    names(x)[1] <- "x"
    test <- cbind(y,x)
    test <- test[test$x!=-9,]                                       #Delete the CEL of genotype missing
    test <- test[test$y!=-999,]                                     #Delete the CEL of phenotype missing
    t <- t.test(y,x)
    p_data[j,i+3] <- t[[3]]                                         #holes
  }
}

for(k in 1:ncol(title)){
  data1 <-data.frame(Trait=c(title[1,k]))
  data2 <-data.frame(df=c("subpop-1"))
  
  p_branch_data <- subset(p_data, select = c(1:3,k+3))
  p_branch_data <- cbind(data1,p_branch_data[,1:3],data2,p_branch_data[,4])
  names(p_branch_data)[6] <- "p"
  
  output <- paste0("data/",title[1,k],".txt")
  write.table(p_branch_data, file = output, row.names = F, quote = F, sep="\t")
}