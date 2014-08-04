setwd("c:/mkt/")
MKT <- read.csv("MKTordered.csv")

MKTcompleted <- MKT[MKT$数据完整性=="完整数据",]
MKTcompleted$Company.Name <- toupper(MKTcompleted$Company.Name)

Names <- toupper(MKTcompleted$Company.Name)
Names <- Names[!duplicated(Names)]
##nnumlogical <- lapply(as.character(Names),nchar)
##Names[nnumlogical <4]

sub("\\((北京|上海|广州|中国|已换公司)）","（(北京|上海|广州|中国|已换公司)）",Names)
sub("（(北京|上海|广州|中国|已换公司)\\)","（(北京|上海|广州|中国|已换公司)）",Names)

Duplis <- findduplicated <- function(Names=Names,b=700){
  Duplis <- c(NA,NA)
  for(i in 1:b){
    for(j in i+1:min(i+900,length(Names))){
      if(nchar(as.character(Names[i]) >=2))  
        if(grepl(as.character(Names[i]),as.character(Names[j]))&&Names[i]!=""&&Names[j]!="")
              Duplis <- rbind(Duplis,as.character(c(as.character(Names[i]),as.character(Names[j]))))
    }
  }
  return (Duplis)
}

write.csv(Duplis, "Duplis.csv")
##MKT[MKT$Company.Name=="HKU",]
##for(i in 1:length(Names))
##  if(grepl("\\((.*)）",Names[i]))
##    x[i] <- Names[i]
companylist <- read.csv("Duplis.csv",header=T)
Duplicateddd <- function(){
  Duplicateditems <- data.frame(No.=1,Name=NA,Company.Name=NA,Phone=1)
  for(i in 1:length(MKTcompleted$Company.Name)){
    if(MKTcompleted$Company.Name[i] %in% companylist$ShortName){
      dshort <- companylist$ShortName
      for(j in i:length(MKTcompleted$Company.Name))
        if(MKTcompleted$Company.Name[i] %in% companylist$LongName[companylist$ShortName==dshort])
          if(MKTcompleted$Name[i]==MKTcompleted$Name[j])
            Duplicateditems <- rbind(
                  Duplicateditems,
                  c(as.numeric(MKTcompleted$No.[i]),
                    as.character(MKTcompleted$Name[i]),
                    as.character(MKTcompleted$Company.Name[i]),
                    as.numeric(MKTcompleted$电话[i])
                  ),
                  c(as.numeric(MKTcompleted$No.[j]),
                    as.character(MKTcompleted$Name[j]),
                    as.character(MKTcompleted$Company.Name[j]),
                    as.numeric(MKTcompleted$电话[j])
                  ))
    }
  }
}
