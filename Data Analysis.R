setwd("C:/USers/Zohaib/Desktop/Lectures/Udacity/R")
Expenditure<-read.csv("Expenditure.csv",row.names=NULL)
Contributions<-read.csv("Contributions.csv",row.names=NULL,
                        col.names=c("cmte_id","cand_id","cand_nm","contbr_nm","contbr_city","contbr_st","contbr_zip",
                                    "contbr_employer","contbr_occupation","contb_receipt_amt","contb_receipt_dt",
                                    "receipt_desc","memo_cd","memo_text","form_tp","file_num",
                                    "tran_id","election_tp","row"),na.strings="")[,-19]
library(plyr)
library(ggplot2)
for (i in c("cand_nm","election_tp")){print(sort(table(Contributions[i]),decreasing=TRUE))}
Contributions$names<-lapply(as.character(Contributions$cand_nm),function(x) strsplit(x,",")[[1]][1])
Contributions$names<-factor(Contributions$names,levels=sort(as.vector(as.character(unique((Contributions$names))))))
ggplot(aes(x=names,y=contb_receipt_amt),
       data=subset(Contributions,election_tp=="P2016"|election_tp=="G2016"))+
  geom_bar(stat="summary",fun.y=mean)+
  geom_point(stat="summary",fun.y=sum)+
  guides(fill=FALSE)+
  #xlab("Candidate Names")+ylab("Contribution Amount")
  labs(x="Candidate Names",y="Contribution Amount",title="Contributions by Candidate")+
  theme(plot.title = element_text(size=22))

