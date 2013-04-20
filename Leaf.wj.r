dir()
R00 <- read.csv("F:\\DataW\\wjing\\wangjing.csv")
R00 <- R00[R00$干重!=-1,]
head(R00)
str(R00)
summary(R00)
names(R00)
R01 <- R00[R00$月份==0, c("C", "N.", "P.", "k.", "Ca.mg.g.", "Mg.mg.g.",
             "Fe.mg.g.", "Mn.mg.g.", "干重热值.J.g.",
             "Lignin...", "Ash...", "去灰分热值.J.g.")]
ag00 <- aggregate(R01, list(site=R00$地点[R00$月份==0], species=R00$物种[R00$月份==0]),FUN=mean)
ag01 <- aggregate(R01, list(site=R00$地点[R00$月份==0], species=R00$物种[R00$月份==0]),FUN=sd)
ag00
ag02 <- t(ag01)[,c(1,3,2,4)]
ag03 <- matrix(round(as.numeric(ag02[-c(1,2), ]), 3),nrow=12)
row.names(ag03) <- names(R01)
ag03
#################

ag11 <- R00[,c("C","Lignin...","Ash...","N.","Ca.mg.g.", "Mg.mg.g.","月份",
                "地点","物种")]
rowSums(ag11)

head(ag11)
ls(ag11)
ag12 <- ag11[ag11$月份==0,]

ag12 <- R00[R00$月份==0,c("C", "N.", "P.", "k.", "Ca.mg.g.", "Mg.mg.g.",
                        "Fe.mg.g.", "Mn.mg.g.", "干重热值.J.g.",
                        "Lignin...", "Ash...", "去灰分热值.J.g.","月份",
                        "地点","物种")]
#
head(ag12)
#
for(X in 1:12){
  A00 <- aov(ag12[,X]~ag12$地点+ag12$物种)
  print(summary(A00))
             } 
#

for(X in 1:12){
  A00 <- pairwise.t.test(ag12[,X],paste(ag12$地点,ag12$物种),p.adjust.method="holm")
  print(A00)
             } 

##############################################
# 失重率
R11 <- R00[,c('失重率', '月份', '地点', '物种')]

with(subset(R11,失重率>0),plot(月份,失重率))

####
 
 for(XX in 1:12){
 n=3
   XX1 <- 4 + (n*XX-(n-1)) 
   XX2 <- 4 + n*XX
 R12 <- subset(R11,月份>=XX1 & 月份<=XX2)
 #
 A00 <- aov(失重率 ~ 地点 + 物种, data = R12)
 print(summary(A00))
 }
 
 #

for(XX in 1:12){
  n=3
  XX1 <- 4 + (n*XX-(n-1)) 
  XX2 <- 4 + n*XX
  R12 <- subset(R11,地点=='大埠' &月份>=XX1 & 月份<=XX2)
  #
   print( c(mean(R12$失重率),sd(R12$失重率)))
               }
#
################
#
for(XX in 1:12){
  n=3
  XX1 <- 4 + (n*XX-(n-1)) 
  XX2 <- 4 + n*XX
  R12 <- subset(R11,地点=='大埠' &月份>=XX1 & 月份<=XX2)
  #
  print( c(mean(R12$失重率),sd(R12$失重率)))
}

for(XX in 1:12){
  n=3
  XX1 <- 4 + (n*XX-(n-1)) 
  XX2 <- 4 + n*XX
  R12 <- subset(R11,地点=='植物所' &月份>=XX1 & 月份<=XX2)
  #
  print( c(mean(R12$失重率),sd(R12$失重率)))
}

for(XX in 1:12){
  n=3
  XX1 <- 4 + (n*XX-(n-1)) 
  XX2 <- 4 + n*XX
  R12 <- subset(R11,物种=='桂花' &月份>=XX1 & 月份<=XX2)
  #
  print( c(mean(R12$失重率),sd(R12$失重率)))
}

for(XX in 1:12){
  n=3
  XX1 <- 4 + (n*XX-(n-1)) 
  XX2 <- 4 + n*XX
  R12 <- subset(R11,物种=='青冈' &月份>=XX1 & 月份<=XX2)
  #
  print( c(mean(R12$失重率),sd(R12$失重率)))
}

#######################

ag22 <- R00[R00$月份==0,c("C", "N.", "P.", "k.", "Ca.mg.g.", "Mg.mg.g.",
                        "Fe.mg.g.", "Mn.mg.g.", "Lignin...","失重率","月份",
                        "地点","物种")]
#
ag00 <- aggregate(ag22[ , c("C", "N.", "P.", "k.", "Ca.mg.g.", "Mg.mg.g.",
                  "Fe.mg.g.", "Mn.mg.g.", "Lignin...")]
                , list(ag22$地点,ag22$物种), mean)

ag01 <- cbind(ag00[,1:2],ag00[,-c(1,2)]*15)
####################

ag23 <- R00[, c("C", "N.", "P.", "k.", "Ca.mg.g.", "Mg.mg.g.",
                        "Fe.mg.g.", "Mn.mg.g.", "Lignin...")]
ag24 <- R00$干重
################
#
ag25 <- as.data.frame(ag23*ag24)[
            with(R00,月份!=0 & 地点=='大埠' & 物种=='桂花'),]

sw00 <- -sweep(as.data.frame(ag25), 2,as.numeric(ag01[1,-c(1,2)]) , FUN = "-")
sw01 <- R00[with(R00,月份!=0 & 地点=='大埠' & 物种=='桂花'),c('月份','地点','物种',"失重率") ]
sw02 <- cbind(sw00, sw01)
#

ag25 <- as.data.frame(ag23*ag24)[
  with(R00,月份!=0 & 地点=='大埠' & 物种=='青冈'),]

sw00 <- -sweep(as.data.frame(ag25), 2,as.numeric(ag01[1,-c(1,2)]) , FUN = "-")
sw01 <- R00[with(R00,月份!=0 & 地点=='大埠' & 物种=='青冈'),c('月份','地点','物种',"失重率") ]
sw03 <- cbind(sw00, sw01)

ag25 <- as.data.frame(ag23*ag24)[
  with(R00,月份!=0 & 地点=='植物所' & 物种=='桂花'),]

sw00 <- -sweep(as.data.frame(ag25), 2,as.numeric(ag01[1,-c(1,2)]) , FUN = "-")
sw01 <- R00[with(R00,月份!=0 & 地点=='植物所' & 物种=='桂花'),c('月份','地点','物种',"失重率") ]
sw04 <- cbind(sw00, sw01)

ag25 <- as.data.frame(ag23*ag24)[
  with(R00,月份!=0 & 地点=='植物所' & 物种=='青冈'),]

sw00 <- -sweep(as.data.frame(ag25), 2,as.numeric(ag01[1,-c(1,2)]) , FUN = "-")
sw01 <- R00[with(R00,月份!=0 & 地点=='植物所' & 物种=='青冈'),c('月份','地点','物种',"失重率") ]
sw05 <- cbind(sw00, sw01)
##########

sw06 <- rbind(sw02,sw03,sw04,sw05)
# 
write.csv(sw06,'sw00shifang11.csv')
########################
head(sw06)
mode(sw06[,1:9])
sw07<- as.data.frame(sw06[,1:9])
cor(sw07,sw06$失重率,method = "spearman")
cor.test(sw07[,1],sw06$失重率,method = "pearson")


cor.test(sw07[,1],sw06$失重率,method = "kendall")
cor.test(sw07[,1],sw06$失重率,method = "spearman")






















