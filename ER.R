er_data <- read.table(file = 'ER.txt', sep = '\t', header = TRUE)
er_data
str(er_data) #구조

er_data[is.na(er_data)]<-0 #널값을 0 으로 바꿔준다
er_data<-as.data.table(er_data)
str(er_data)

library(data.table)
library(dplyr)

er_data[,admision:=as.Date(inp_date)] #admision 변수를 써주고 data값을 바꿔준다
er_data[,discharge:=as.Date(dis_date)] #discharge 변수를 써주고 data 값을 바꿔준다

er_data$duration = er_data$discharge - er_data$admision + 1 #정확한 los(진찰날짜-퇴원날짜) 값을 계산해준다 
#총 4가지를 제외한다.
er_data[,transfer:=as.integer(Trans)] #전과
er_data[transfer != 1,transfer:=2]
distinct(er_data,transfer) #distinct 1,2로 정해준다
er_data[transfer==1,.N] #필요한 값
er_data[transfer==2,.N] #제외된 값

er_data[,mortality:=as.integer(Death)] #데스
er_data[mortality != 1,mortality:=2]
distinct(er_data,mortality)
er_data[mortality==1,.N]
er_data[mortality==2,.N]

er_data[,DAMA:=as.integer(DAMA)] #다마
er_data[DAMA != 1,DAMA:=2]
distinct(er_data,DAMA)
er_data[DAMA==1,.N]
er_data[DAMA==2,.N]

er_data[,BUN2:=as.integer(BUN_2)] #BUN 2차값
er_data[BUN2==1,.N]

er_data[(er_data$transfer == 2 | er_data$mortality == 2 | er_data$DAMA == 2 | er_data$BUN2 == 1),.N] #4가지 제외된 값  
er_data_exc <- er_data[!(er_data$transfer == 2 | er_data$mortality == 2 | er_data$DAMA == 2 | er_data$BUN2 == 1)]

str(er_data_exc)

er_data_exc[,group:=as.character(group)]
distinct(er_data_exc,group)
er_data_exc[group == "A",group:="1"] #A그룹과 BC 그룹으로 나누어준다
er_data_exc[group == "B",group:="2"]
er_data_exc[group == "C",group:="2"]

er_data_exc[,group:=as.integer(group)]
er_data_exc[group==1,.N] #A, BC환자수의 결과값이 나오고
er_data_exc[group==2,.N]

er_data_exc[,BUN1:=as.character(BUN_1)]
er_data_exc[,BUN2:=as.character(BUN_2)]
er_data_exc$BUN1<-strsplit(er_data_exc$BUN1, "/")
er_data_exc$BUN2<-strsplit(er_data_exc$BUN2, "/")

str(er_data_exc)
er_data_exc <- er_data_exc[,c(1:20)]
write.csv(er_data_exc, file = "er_data_exc.csv", row.names = FALSE)

er_data_exc <- read.table(file = 'er_data_exc.csv', sep = ',', header = TRUE)
str(er_data_exc)

group_a <- subset(er_data_exc,group == 1)
group_b <- subset(er_data_exc,group == 2)

group_a_bun <- group_a %>% select(ID,BUN_1,BUN_2)
group_b_bun <- group_b %>% select(ID,BUN_1,BUN_2)

library(tidyr)
group_a_bun_pd <- gather(group_a_bun,key="GROUP",value="RESULT",-ID) #wide값을 롱타입으로 바꿔준다. 
group_a_bun_pd

d <- group_a_bun_pd$RESULT[group_a_bun_pd$GROUP=="BUN_1"] - group_a_bun_pd$RESULT[group_a_bun_pd$GROUP=="BUN_2"]
shapiro.test(d) #대응표본은 A-B 차값으로 계산한다.
wilcox.test(d,exact = FALSE)

group_b_bun_pd <- gather(group_b_bun,key="GROUP",value="RESULT",-ID)
group_b_bun_pd #long타입으로 변환

d <- group_b_bun_pd$RESULT[group_b_bun_pd$GROUP=="BUN_1"] - group_b_bun_pd$RESULT[group_b_bun_pd$GROUP=="BUN_2"]
shapiro.test(d)
wilcox.test(d,exact = FALSE)

group_a_cr <- group_a %>% select(ID,CR_1,CR_2)
group_b_cr <- group_b %>% select(ID,CR_1,CR_2)

library(tidyr)
group_a_cr_pd <- gather(group_a_cr,key="GROUP",value="RESULT",-ID)
group_a_cr_pd

d <- group_a_cr_pd$RESULT[group_a_cr_pd$GROUP=="CR_1"] - group_a_cr_pd$RESULT[group_a_cr_pd$GROUP=="CR_2"]
shapiro.test(d)
wilcox.test(d,exact = FALSE)

group_b_cr_pd <- gather(group_b_cr,key="GROUP",value="RESULT",-ID)
group_b_cr_pd

d <- group_b_cr_pd$RESULT[group_b_cr_pd$GROUP=="CR_1"] - group_b_cr_pd$RESULT[group_b_cr_pd$GROUP=="CR_2"]
shapiro.test(d)
wilcox.test(d,exact = FALSE)

library(PairedData)

before <- subset(group_a_bun_pd,GROUP == "BUN_1",RESULT,drop = TRUE)
after <- subset(group_a_bun_pd,GROUP == "BUN_2",RESULT,drop = TRUE)
pd_bun_a <- paired(before,after)
pd_bun_a
plot(pd_bun_a,type="profile")

#그래프
install.packages("ggpaired")
library(ggpaired)
ggpaired(pd_bun_a,cond1 = "before",cond2 = "after",fill = "condition",ylab = "BUN",
         palette = "jco",line.color = "gray", title = "BUN of group A")

# ggpaired(pd_bun_a,cond1 = "before",cond2 = "after",fill = "condition",
#          palette = "jco",line.color = "gray")+stat_compare_means(paired = FALSE)

before <- subset(group_b_bun_pd,GROUP == "BUN_1",RESULT,drop = TRUE)
after <- subset(group_b_bun_pd,GROUP == "BUN_2",RESULT,drop = TRUE)
pd_bun_b <- paired(before,after)
pd_bun_b
plot(pd_bun_b,type="profile")

ggpaired(pd_bun_b,cond1 = "before",cond2 = "after",fill = "condition",ylab = "BUN",
         palette = "jco",line.color = "gray", title = "BUN of group B and C")

before <- subset(group_a_cr_pd,GROUP == "CR_1",RESULT,drop = TRUE)
after <- subset(group_a_cr_pd,GROUP == "CR_2",RESULT,drop = TRUE)
pd_cr_a <- paired(before,after)
pd_cr_a
plot(pd_cr_a,type="profile")

ggpaired(pd_cr_a,cond1 = "before",cond2 = "after",fill = "condition",ylab = "CR",
         palette = "jco",line.color = "gray", title = "CR of group A")

# ggpaired(pd_cr_a,cond1 = "before",cond2 = "after",fill = "condition",
#          palette = "jco",line.color = "gray")+stat_compare_means(paired = FALSE)

before <- subset(group_b_cr_pd,GROUP == "CR_1",RESULT,drop = TRUE)
after <- subset(group_b_cr_pd,GROUP == "CR_2",RESULT,drop = TRUE)
pd_cr_b <- paired(before,after)
pd_cr_b
plot(pd_cr_b,type="profile")

ggpaired(pd_cr_b,cond1 = "before",cond2 = "after",fill = "condition",ylab = "CR",
         palette = "jco",line.color = "gray", title = "CR of group B and C")
