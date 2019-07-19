er_add <- read.table(file = "er_adtnl.TXT", sep = '\t', header = TRUE)
er_add
str(er_add)

er_add[is.na(er_add)]<-0 #널 값을 0으로 바꿔줌
er_add<-as.data.table(er_add)
str(er_add)

library(data.table)
library(dplyr)

er_add[,alpha:=as.Date(Inp_Date)] #alpha 변수를 써주고 date값을 바꿔준다. 
er_add[,beta:=as.Date(Dis_Date)] #beta 변수를 써주고 date값을 바꿔준다.

er_add$duration = er_add$beta - er_add$alpha + 1 #정확한 los(진찰날짜-퇴원날짜)값을 계산해준다. 양입법
er_add[,LOS:=as.integer(duration)]
str(er_add)

distinct(er_add,note) #목록들 추출

er_add[,transfer:=as.integer()] #전과   
er_add[note == 'RES/NEP' | note == 'CIR/NEP' | note == 'NEU/NEP' | note == 'OS/NEP' | note == 'END/NEP' | note == 'GS/NEP' 
       | note == 'GAS/NEP' | note == 'NS/NEP' | note == 'NEP/OS', transfer:=1]
er_add[transfer==1,.N] #필요한값
er_add[is.na(er_add)]<-0 #널값 바꿔줌
er_add[transfer==0,.N] #제외된값

er_add[,Death:=as.integer()] #데스
er_add[note == 'D',Death:=1]
er_add[Death==1,.N]
er_add[is.na(er_add)]<-0
er_add[Death==0,.N]

er_add[,DAMA:=as.integer()] #다마
er_add[note =='DAMA',DAMA:=1]
er_add[DAMA==1,.N]
er_add[is.na(er_add)]<-0
er_add[DAMA==0,.N]

a<-distinct(er_add,BUN1)
a<-distinct(er_add,BUN2)
a<-distinct(er_add,Cr1)
a<-distinct(er_add,Cr2)
a<-distinct(er_add,AST)
a<-distinct(er_add,ALT)
a<-distinct(er_add,Na)
a<-distinct(er_add,eGFR)
a<-distinct(er_add,K)
a<-distinct(er_add,albumin)
a<-distinct(er_add,CRP)

distinct(er_add,BUN1)

str(er_add)

er_add[,BUN:=as.integer()]
er_add[BUN2 == 0, BUN:=1]
er_add[BUN==1,.N]
er_add[is.na(er_add)]<-0
er_add[BUN==0,.N]

er_add[,Cr:=as.integer()]
er_add[Cr2 == 0, Cr:=1]
er_add[Cr==1,.N]
er_add[is.na(er_add)]<-0
er_add[Cr==0,.N]

er_add[,CRP1:=as.integer()]
er_add[CRP == 0, CRP1:=1]
er_add[CRP1==1,.N]
er_add[is.na(er_add)]<-0
er_add[CRP1==0,.N]

str(er_add)

er_add[(er_add$transfer == 1 | er_add$Death == 1 | er_add$DAMA == 1 | er_add$BUN == 1 | er_add$Cr == 1 | er_add$CRP1 == 1 | note == 'x'),.N]
er_add_exc <- er_add[!(er_add$transfer == 1 | er_add$Death == 1 | er_add$DAMA == 1 | er_add$BUN == 1 | er_add$Cr == 1 | er_add$CRP1 == 1 | note == 'x')]
str(er_add_exc)

er_add_exc[,group:=as.character(group)]
distinct(er_add_exc,group)
er_add_exc[group == "A",group:="1"] #A그룹과 BC 그룹으로 나누어준다
er_add_exc[group == "B",group:="2"]
er_add_exc[group == "C",group:="2"]

er_add_exc[,group:=as.integer(group)]
er_add_exc[group==1,.N] #A, BC환자수의 결과값이 나오고
er_add_exc[group==2,.N]

### paired t-test
library(PairedData)
library(dplyr)
library(tidyr)
library(dplyr)

group_a <- subset(er_add_exc,group == 1)
group_b <- subset(er_add_exc,group == 2)
#a의BUNpre
group_a_BUN1 <- group_a$BUN1
group_a_BUN2 <- group_a$BUN2
group_a_diff <- group_a_BUN1 - group_a_BUN2

d<-with(er_add_exc, BUN1[group == "1"] - BUN2[group == "1"] )
shapiro.test(d)     #정규성 분포

mean(d)             #A BUN pre mean값
mean(group_a_diff)  #A BUN pre mean값
sd(group_a_diff)    #A BUN pre sd값
length(group_a_diff) 
sd(group_a_diff)/sqrt(length(group_a_diff)) #A BUN pre se값

str(er_add_exc)
res <- wilcox.test(group_a_BUN1, group_a_BUN2,paired = TRUE)
res

#B+C의BUNpre
group_b_BUN1 <- group_b$BUN1
group_b_BUN2 <- group_b$BUN2
group_b_diff <- group_b_BUN1 - group_b_BUN2

b<-with(er_add_exc, BUN1[group == "2"] - BUN2[group =="2"]) #A의 CR값 정규분포
shapiro.test(b)
mean(b)             #B+C의 Mean값
sd(b)               #B+C의 sd값
sd(b)/sqrt(length(group_b_diff)) #B+C의 BUN2 SE값

abc <- wilcox.test(group_b_BUN1, group_b_BUN2, paired = TRUE) #B+C의 BUN의 wilcoxon
abc

mean(group_b_diff)  #B+C Mean값
sd(group_b_diff)    #B+C sd값
length(group_b_diff) #B+C 값 추출
sd(group_b_diff)/sqrt(length(group_b_diff)) #B+C se

###B+C의 Cr2
group_b_Cr1 <- group_b$Cr1
group_b_Cr2 <- group_b$Cr2
group_C_diff <- group_b_Cr1 - group_b_Cr2

def <- wilcox.test(group_a_Cr1, group_a_Cr2, paired = TRUE)
def

c<-with(er_add_exc, Cr1[group =="2"] - Cr2[group == "2"]) #B+C의 Cr2값
shapiro.test(c)

mean(c) #B+C Cr2
sd(c) # B+C Cr2
sd(c)/sqrt(length(group_C_diff)) #B+C Cr2 SE

ghi <- wilcox.test(group_b_Cr1, group_b_Cr2, paired = TRUE)
ghi


### 나머지 8가지 구하기
str(er_add_exc)

group_a <- subset(er_add_exc,group == 1)
group_b <- subset(er_add_exc,group == 2)

group_a_AST <- group_a$AST
group_b_AST <- group_b$AST

mean(group_a_AST) #B+C Cr2
sd(group_a_AST) # B+C Cr2
sd(group_a_AST)/sqrt(length(group_a_AST))

out = lm(AST ~ group, data = er_add_exc)
shapiro.test(resid(out))

wilcox.test(AST ~ group, data = er_add_exc)
#var.test()

f<-with(er_add_exc, AST[group =="2"])
shapiro.test(f)

mean(group_b_AST)
sd(group_b_AST)
sd(group_b_AST)/sqrt(length(group_b_AST))

astt <- wilcox.test(group_a_AST,group_b_AST, paired = TRUE) #V,P값 구하기
astt

group_a_ALT <- group_a$ALT
group_b_ALT <- group_b$AST

mean(group_a_ALT)
sd(group_a_ALT)
sd(group_a_ALT)/sqrt(length(group_a_ALT))

out = lm(ALT ~ group, data = er_add_exc)
shapiro.test(resid(out))

mean(group_b_ALT)
sd(group_b_ALT)

group_a_eGFR <- group_a$eGFR
group_b_eGFR <- group_b$eGFR

mean(group_a_eGFR)
sd(group_a_eGFR)
sd(group_a_eGFR)/sqrt(length(group_a_eGFR))

mean(group_b_eGFR)
sd(group_b_eGFR)
sd(group_b_eGFR)/sqrt(length(group_b_eGFR))

out = lm(eGFR ~ group, data = er_add_exc)
shapiro.test(resid(out))

group_a_Na <- group_a$Na
group_b_Na <- group_b$Na

mean(group_a_Na)
sd(group_a_Na)
sd(group_a_Na)/sqrt(length(group_a_Na))

mean(group_b_Na)
sd(group_b_Na)
sd(group_b_Na)/sqrt(length(group_b_Na))

out = lm(Na ~ group, data = er_add_exc)
shapiro.test(resid(out))

group_a_K <- group_a$K
group_b_K <- group_b$K

mean(group_a_K)
sd(group_a_K)
sd(group_a_K)/sqrt(length(group_a_K))

mean(group_b_K)
sd(group_b_K)
sd(group_b_K)/sqrt(length(group_b_K))

out = lm(K ~ group, data = er_add_exc)
shapiro.test(resid(out))

group_a_CRP <- group_a$CRP
group_b_CRP <- group_b$CRP

mean(group_a_CRP)
sd(group_a_CRP)
sd(group_a_CRP)/sqrt(length(group_a_CRP))

mean(group_b_CRP)
sd(group_b_CRP)
sd(group_b_CRP)/sqrt(length(group_b_CRP))

out = lm(K ~ group, data = er_add_exc)
shapiro.test(resid(out))

group_a_LOS <- group_a$LOS
group_b_LOS <- group_b$LOS

mean(group_a_LOS)
sd(group_a_LOS)
sd(group_a_LOS)/sqrt(length(group_a_LOS))

mean(group_b_LOS)
sd(group_b_LOS)
sd(group_b_LOS)/sqrt(length(group_b_LOS))

out = lm(LOS ~ group, data = er_add_exc)
shapiro.test(resid(out))

group_a_albumin <- group_a$albumin
group_b_albumin <- group_b$albumin

mean(group_a_albumin)
sd(group_a_albumin)
sd(group_a_albumin)/sqrt(length(group_a_albumin))

mean(group_b_albumin)
sd(group_b_albumin)
sd(group_b_albumin)/sqrt(length(group_b_albumin))

out = lm(albumin ~ group, data = er_add_exc)
shapiro.test(resid(out))

group_a_ALT <- group_a$ALT
group_b_ALT <- group_b$ALT

mean(group_b_ALT)
sd(group_b_ALT)
sd(group_b_ALT)/sqrt(length(group_b_ALT))

out = lm(albumin ~ group, data = er_add_exc)
shapiro.test(resid(out))
var.test(albumin~group, data = er_add_exc)
t.test(albumin~group, data = er_add_exc, var.equal=TRUE)

group_a_AST <- group_a$AST
group_b_AST <- group_b$AST

abcd <- wilcox.test(AST~group,data = er_add_exc)
abcd
efgg <- wilcox.test(ALT~group,data = er_add_exc) #V,P값 구하기
efgg

qwer <- wilcox.test(eGFR~group,data = er_add_exc)
qwer

asdf <- wilcox.test(Na~group,data = er_add_exc)
asdf

lkjh <- wilcox.test(K~group,data = er_add_exc)
lkjh

vbnm <- wilcox.test(CRP~group,data = er_add_exc)
vbnm

str(er_add_exc)

out = lm(LOS ~ group, data = er_add_exc)
shapiro.test(resid(out))

tgbn <- wilcox.test(LOS~group,data = er_add_exc)
tgbn
