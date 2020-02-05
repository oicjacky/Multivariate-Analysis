rm(list=ls())

qq=read.csv("(去程)路線11.csv")

qq.old<-qq[which(qq[,3]=="敬老票"),]
dim(qq.old)[1]
# write.csv(qq.old, file = "(去程)路線11敬老票.csv", row.names = F)

qq.stu<-qq[which(qq[,3]=="學生票"),]
dim(qq.stu)[1]
# write.csv(qq.stu, file = "(去程)路線11學生票.csv", row.names = F)

qq.gen<-qq[which(qq[,3]=="一般票"),]
dim(qq.gen)[1]