install.packages("lmtest")
install.packages("plm")
install.packages("ggplot2")
install.packages("fitdistrplus")
install.packages("DescTools")
install.packages("Hmisc")
install.packages("xtable")
install.packages("texreg")

library(lmtest)
library(plm)
library(ggplot2)
library(fitdistrplus)
library(DescTools)
library(xtable)
library(texreg)

setwd("/Users/houping/Dropbox/My Mac (rcbm7256-diii)/Documents/Houping/Crowdsources/Data/results")
# data = read.csv('weight_0720.csv')
# data = read.csv('weight_0719.csv')
data = read.csv('weight_0721.csv')
# gteps, wsceps, eceps, emeps, caeps, ibeseps, 
# catd/_adjust, industry/_adjust, category/_adjust, qid/_adjust

#--------- correlation
ddd = data[,c("wsceps","eceps","emeps","caeps","ibeseps",
              "catd_adjust","industry_adjust","qid_adjust","category_adjust","all_adjust", 
              "wscd","ecd","emd","cad","ibesd",
              "catdd_adjust","industryd_adjust","qidd_adjust","categoryd_adjust","alld_adjust",
              "wsc_w","ec_w","em_w","ca_w","ibes_w",
              "coverage","count","std",
              "sp500","bm","turnover","Ala"
)]

c <- round(cor(data[,c("wsceps","eceps","emeps","caeps","ibeseps",
                       "catd_adjust","industry_adjust","qid_adjust","category_adjust","all_adjust", 
                       "wscd","ecd","emd","cad","ibesd",
                    "catdd_adjust","industryd_adjust","qidd_adjust","categoryd_adjust","alld_adjust",
                    "wsc_w","ec_w","em_w","ca_w","ibes_w",
                    "coverage","count" ,"std",
                    "sp500","bm","turnover","Ala"
)]),4)

c <- round(cor(data[,c("wsc_w","ec_w","em_w","ca_w","ibes_w",
                       "coverage","count" ,"std",
                       "sp500","bm","turnover","Ala"
)]),4)

c = correlation_matrix(df=ddd)
cc <- print(xtable(c))
write(cc, file = '20230213_correlation.tex')


ddd = data[,c("catd_adjust","industry_adjust","qid_adjust","category_adjust","all_adjust", 
              "catdd_adjust","industryd_adjust","qidd_adjust","categoryd_adjust","alld_adjust"
)]

ddd = data[,c("sp1500","sp500"
)]

m <- apply(ddd,2,mean,na.rm = TRUE)
sd <- apply(ddd,2,sd,na.rm = TRUE)
min <- apply(ddd,2,min,na.rm = TRUE)
max <- apply(ddd,2,max,na.rm = TRUE)
q025 <- apply(ddd,2,quantile,probs = 0.25,na.rm = TRUE)
q050 <- apply(ddd,2,quantile,probs = 0.5,na.rm = TRUE)
q075 <- apply(ddd,2,quantile,probs = 0.75,na.rm = TRUE)
ms <- rbind(m,sd,min,q025,q050,q075,max)

cc <- print(xtable(ms))

write(cc, file = '20230213_statistics2.tex')

#----------------------

data$gtepsS = Winsorize(data$gteps, probs = c(0.01,0.99))
data$wscepsS = Winsorize(data$wsceps, probs = c(0.01,0.99))
data$ecepsS = Winsorize(data$eceps, probs = c(0.01,0.99))
data$emepsS = Winsorize(data$emeps, probs = c(0.01,0.99))
data$caepsS = Winsorize(data$caeps, probs = c(0.01,0.99))
data$ibesepsS = Winsorize(data$ibeseps, probs = c(0.01,0.99))
data$catdS = Winsorize(data$catd, probs = c(0.01,0.99))
data$catdSA = Winsorize(data$catd_adjust, probs = c(0.01,0.99))
data$categoryS = Winsorize(data$category, probs = c(0.01,0.99))
data$categorySA = Winsorize(data$category_adjust, probs = c(0.01,0.99))
data$industryS = Winsorize(data$industry, probs = c(0.01,0.99))
data$industrySA = Winsorize(data$industry_adjust, probs = c(0.01,0.99))
data$qidS = Winsorize(data$qid, probs = c(0.01,0.99))
data$qidSA = Winsorize(data$qid_adjust, probs = c(0.01,0.99))
data$allS = Winsorize(data$all, probs = c(0.01,0.99))
data$allSA = Winsorize(data$all_adjust, probs = c(0.01,0.99))
data$bmw = Winsorize(data$bm, probs = c(0.01,0.99), na.rm=TRUE)
data$turnoverw = Winsorize(data$turnover, probs = c(0.01,0.99))
data$Alaw = Winsorize(data$Ala, probs = c(0.01,0.99), na.rm=TRUE)

# ecd: abs(eceps - gteps), ecdd: eceps - gteps, ecdd2: (eceps - gteps)**2
data$ecepsGap = data$eceps - data$wsceps
data$emepsGap = data$emeps - data$wsceps
data$caepsGap = data$caeps - data$wsceps
data$ibesepsGap = data$ibeseps - data$wsceps
data$catdGap = data$catd - data$wsceps
data$categoryGap = data$category - data$wsceps
data$industryGap = data$industry - data$wsceps
data$qidGap = data$qid - data$wsceps
data$allGap = data$all - data$wsceps
data$catdAGap = data$catd_adjust - data$wsceps
data$categoryAGap = data$category_adjust - data$wsceps
data$industryAGap = data$industry_adjust - data$wsceps
data$qidAGap = data$qid_adjust - data$wsceps
data$allAGap = data$all_adjust - data$wsceps

#### abs ~ weight
em1 <- plm(wscd~ wsc_w+coverage+count+std-1, data = data,
          index = c("quarterID",'ticker','industryID'),model = "within")
em2 <- plm(wscd~ wsc_w+coverage+count+std-1+bm+turnover+Ala+sp1500+MOM, data = data,
          index = c("quarterID",'ticker','industryID'),model = "within")
#ec
em3 <- plm(ecd~ ec_w+coverage+count+std-1, data = data,
          index = c("quarterID",'ticker','industryID'),model = "within")
em4 <- plm(ecd~ ec_w+coverage+count+std-1+bm+turnover+Ala+sp1500+MOM, data = data,
           index = c("quarterID",'ticker','industryID'),model = "within")
#em
em5 <- plm(emd~ em_w+coverage+count+std-1, data = data,
          index = c("quarterID",'ticker','industryID'),model = "within")
em6 <- plm(emd~ em_w+coverage+count+std-1+bm+turnover+Ala+sp1500+MOM, data = data,
          index = c("quarterID",'ticker','industryID'),model = "within")
#ca
em7 <- plm(cad~ ca_w+coverage+count+std-1, data = data,
          index = c("quarterID",'ticker','industryID'),model = "within")
em8 <- plm(cad~ ca_w+coverage+count+std-1+bm+turnover+Ala+sp1500+MOM, data = data,
          index = c("quarterID",'ticker','industryID'),model = "within")
#ibes
em9 <- plm(ibesd~ ibes_w+coverage+count+std-1, data = data,
          index = c("quarterID",'ticker','industryID'),model = "within")
em10 <- plm(ibesd~ ibes_w+coverage+count+std-1+bm+turnover+Ala+sp1500+MOM, data = data,
          index = c("quarterID",'ticker','industryID'),model = "within")
summary(em10)

capture.output(texreg(list(em1,em2,em3,em4,em5,em6,em7,em8,em9,em10), 
                      caption = 'Results for the GLS Estimation',stars=c(0.01,0.05,0.1),
                      digits = 4,label='gls_estimation1'), file = '20230213_error.tex')

capture.output(texreg(list(em2,em4,em6,em8,em10), 
                      caption = 'Results for the GLS Estimation',stars=c(0.01,0.05,0.1),
                      digits = 4,label='gls_estimation1'), file = '20230213_error2.tex')


###########
data = read.csv('weight_only_0720.csv')
# wall street persistence
em1 <- plm(wscd~ wsc_w_1+coverage+count+std-1+bm+turnover+Ala+sp1500+MOM, data = data,
          index = c("quarterID",'ticker','industryID'),model = "within")
em2 <- plm(wscd~ wsc_w_2+coverage+count+std-1+bm+turnover+Ala+sp1500+MOM, data = data,
          index = c("quarterID",'ticker','industryID'),model = "within")
em3 <- plm(wscd~ wsc_w_3+coverage+count+std-1+bm+turnover+Ala+sp1500+MOM, data = data,
          index = c("quarterID",'ticker','industryID'),model = "within")
em4 <- plm(wscd~ wsc_w_4+coverage+count+std-1+bm+turnover+Ala+sp1500+MOM, data = data,
          index = c("quarterID",'ticker','industryID'),model = "within")
em8 <- plm(wscd~ wsc_w_8+coverage+count+std-1+bm+turnover+Ala+sp1500+MOM, data = data,
          index = c("quarterID",'ticker','industryID'),model = "within")

capture.output(texreg(list(em1,em2,em3,em4,em8), 
                      caption = 'Results for the GLS Estimation',stars=c(0.01,0.05,0.1),
                      digits = 4,label='gls_estimation1'), file = '20230215_ws_persistence.tex')


# estimize mean persistence
em11 <- plm(emd~ em_w_1+coverage+count+std-1+bm+turnover+Ala+sp1500+MOM, data = data,
          index = c("quarterID",'ticker','industryID'),model = "within")
em12 <- plm(emd~ em_w_2+coverage+count+std-1+bm+turnover+Ala+sp1500+MOM, data = data,
          index = c("quarterID",'ticker','industryID'),model = "within")
em13 <- plm(emd~ em_w_3+coverage+count+std-1+bm+turnover+Ala+sp1500+MOM, data = data,
          index = c("quarterID",'ticker','industryID'),model = "within")
em14 <- plm(emd~ em_w_4+coverage+count+std-1+bm+turnover+Ala+sp1500+MOM, data = data,
          index = c("quarterID",'ticker','industryID'),model = "within")
em18 <- plm(emd~ em_w_8+coverage+count+std-1+bm+turnover+Ala+sp1500+MOM, data = data,
          index = c("quarterID",'ticker','industryID'),model = "within")
# capture.output(texreg(list(em1,em2,em3,em4,em8), 
#                       caption = 'Results for the GLS Estimation',stars=c(0.01,0.05,0.1),
#                       digits = 4,label='gls_estimation1'), file = '20230215_estimizeMean_persistence.tex')



em21 <- plm(ecd~ ec_w_1+coverage+count+std-1+bm+turnover+Ala+sp1500+MOM, data = data,
           index = c("quarterID",'ticker','industryID'),model = "within")
em22 <- plm(ecd~ ec_w_2+coverage+count+std-1+bm+turnover+Ala+sp1500+MOM, data = data,
           index = c("quarterID",'ticker','industryID'),model = "within")
em23 <- plm(ecd~ ec_w_3+coverage+count+std-1+bm+turnover+Ala+sp1500+MOM, data = data,
           index = c("quarterID",'ticker','industryID'),model = "within")
em24 <- plm(ecd~ ec_w_4+coverage+count+std-1+bm+turnover+Ala+sp1500+MOM, data = data,
           index = c("quarterID",'ticker','industryID'),model = "within")
em28 <- plm(ecd~ ec_w_8+coverage+count+std-1+bm+turnover+Ala+sp1500+MOM, data = data,
           index = c("quarterID",'ticker','industryID'),model = "within")
# capture.output(texreg(list(em1,em2,em3,em4,em8), 
#                       caption = 'Results for the GLS Estimation',stars=c(0.01,0.05,0.1),
#                       digits = 4,label='gls_estimation1'), file = '20230215_estimizeConsensus_persistence.tex')

capture.output(texreg(list(em11,em12,em13,em14,em18,em21,em22,em23,em24,em28), 
                      caption = 'Results for the GLS Estimation',stars=c(0.01,0.05,0.1),
                      digits = 4,label='gls_estimation1'), file = '20230215_estimize_persistence.tex')



em11 <- plm(cad~ ca_w_1+coverage+count+std-1+bm+turnover+Ala+sp1500+MOM, data = data,
           index = c("quarterID",'ticker','industryID'),model = "within")
em12 <- plm(cad~ ca_w_2+coverage+count+std-1+bm+turnover+Ala+sp1500+MOM, data = data,
           index = c("quarterID",'ticker','industryID'),model = "within")
em13 <- plm(cad~ ca_w_3+coverage+count+std-1+bm+turnover+Ala+sp1500+MOM, data = data,
           index = c("quarterID",'ticker','industryID'),model = "within")
em14 <- plm(cad~ ca_w_4+coverage+count+std-1+bm+turnover+Ala+sp1500+MOM, data = data,
           index = c("quarterID",'ticker','industryID'),model = "within")
em18 <- plm(cad~ ca_w_8+coverage+count+std-1+bm+turnover+Ala+sp1500+MOM, data = data,
           index = c("quarterID",'ticker','industryID'),model = "within")
# capture.output(texreg(list(em1,em2,em3,em4,em8), 
#                       caption = 'Results for the GLS Estimation',stars=c(0.01,0.05,0.1),
#                       digits = 4,label='gls_estimation1'), file = '20230215_ibesAverage_persistence.tex')


em21 <- plm(ibesd~ ibes_w_1+coverage+count+std-1+bm+turnover+Ala+sp1500+MOM, data = data,
           index = c("quarterID",'ticker','industryID'),model = "within")
em22 <- plm(ibesd~ ibes_w_2+coverage+count+std-1+bm+turnover+Ala+sp1500+MOM, data = data,
           index = c("quarterID",'ticker','industryID'),model = "within")
em23 <- plm(ibesd~ ibes_w_3+coverage+count+std-1+bm+turnover+Ala+sp1500+MOM, data = data,
           index = c("quarterID",'ticker','industryID'),model = "within")
em24 <- plm(ibesd~ ibes_w_4+coverage+count+std-1+bm+turnover+Ala+sp1500+MOM, data = data,
           index = c("quarterID",'ticker','industryID'),model = "within")
em28 <- plm(ibesd~ ibes_w_8+coverage+count+std-1+bm+turnover+Ala+sp1500+MOM, data = data,
           index = c("quarterID",'ticker','industryID'),model = "within")
# capture.output(texreg(list(em1,em2,em3,em4,em8), 
#                       caption = 'Results for the GLS Estimation',stars=c(0.01,0.05,0.1),
#                       digits = 4,label='gls_estimation1'), file = '20230215_ibesConsensus_persistence.tex')
capture.output(texreg(list(em11,em12,em13,em14,em18,em21,em22,em23,em24,em28), 
                      caption = 'Results for the GLS Estimation',stars=c(0.01,0.05,0.1),
                      digits = 4,label='gls_estimation1'), file = '20230215_ibes_persistence.tex')



em1 <- plm(wsc_w~ wsc_w_8-1, data = data,
           index = c("quarterID",'ticker','industryID'),model = "within")
summary(em1)
em1 <- plm(em_w~ em_w_8-1, data = data,
           index = c("quarterID",'ticker','industryID'),model = "within")
summary(em1)
em1 <- plm(ec_w~ ec_w_1+ec_w_2+ec_w_3+ec_w_4+ec_w_8-1, data = data,
           index = c("quarterID",'ticker','industryID'),model = "within")
summary(em1)
em1 <- plm(ca_w~ ca_w_1+ca_w_2+ca_w_3+ca_w_4+ca_w_8-1, data = data,
           index = c("quarterID",'ticker','industryID'),model = "within")
summary(em1)
em1 <- plm(ibes_w~ ibes_w_1+ibes_w_2+ibes_w_3+ibes_w_4+ibes_w_8-1, data = data,
           index = c("quarterID",'ticker','industryID'),model = "within")
summary(em1)


# gteps ~ gap + wsc + dependet variables
# gteps, wsceps, eceps, emeps, caeps, ibeseps, 
# catd/_adjust, industry/_adjust, category/_adjust, qid/_adjust
# em
em1 <- plm(gteps~ emepsGap + wsceps+coverage+count+std-1, data = data,
          index = c("quarterID",'ticker','industryID'),model = "within")
em2 <- plm(gteps~ emepsGap + wsceps+coverage+count+std-1+MOM+bm+turnover+Ala+sp1500, data = data,
          index = c("quarterID",'ticker','industryID'),model = "within")

#ec
em3 <- plm(gteps~ ecepsGap + wsceps+coverage+count+std-1, data = data,
          index = c("quarterID",'ticker','industryID'),model = "within")
em4 <- plm(gteps~ ecepsGap + wsceps+coverage+count+std-1+MOM+bm+turnover+Ala+sp1500, data = data,
          index = c("quarterID",'ticker','industryID'),model = "within")
#ca
em5 <- plm(gteps~ caepsGap + wsceps+coverage+count+std-1, data = data,
          index = c("quarterID",'ticker','industryID'),model = "within")
em6 <- plm(gteps~ caepsGap + wsceps+coverage+count+std-1+MOM+bm+turnover+Ala+sp1500, data = data,
          index = c("quarterID",'ticker','industryID'),model = "within")
#ibes
em7 <- plm(gteps~ ibesepsGap + wsceps+coverage+count+std-1, data = data,
          index = c("quarterID",'ticker','industryID'),model = "within")
em8 <- plm(gteps~ ibesepsGap + wsceps+coverage+count+std-1+MOM+bm+turnover+Ala+sp1500, data = data,
          index = c("quarterID",'ticker','industryID'),model = "within")
capture.output(texreg(list(em11,em12,em13,em14,em18,em21,em22,em23,em24,em28), 
                      caption = 'Results for the GLS Estimation',stars=c(0.01,0.05,0.1),
                      digits = 4,label='gls_estimation1'), file = '20230215_ibes_persistence.tex')



# catd and catd_adjust
em <- plm(gteps~ catdGap + wsceps+coverage+count+std-1, data = data,
          index = c("quarterID",'ticker','industryID'),model = "within")
coeftest(em, vcov. = vcovHC, type="HC1")
summary(em)
em <- plm(gteps~ catdGap + wsceps+coverage+count+std-1+MOM+bm+turnover+Ala+sp1500, data = data,
          index = c("quarterID",'ticker','industryID'),model = "within")
coeftest(em, vcov. = vcovHC, type="HC1")
summary(em)

em <- plm(gteps~ catdAGap + wsceps+coverage+count+std-1+bm+turnover+Ala+sp500, data = data,
          index = c("quarterID",'ticker','industryID'),model = "within")
coeftest(em, vcov. = vcovHC, type="HC1")

# industry and adjust
em <- plm(gteps~ industryGap + wsceps+coverage+count+std-1+bm+turnover+Ala+sp500, data = data,
          index = c("quarterID",'ticker','industryID'),model = "within")
coeftest(em, vcov. = vcovHC, type="HC1")

em <- plm(gteps~ industryAGap + wsceps+coverage+count+std-1+bm+turnover+Ala+sp500, data = data,
          index = c("quarterID",'ticker','industryID'),model = "within")
coeftest(em, vcov. = vcovHC, type="HC1")

# category and adjust
em <- plm(gteps~ categoryGap + wsceps+coverage+count+std-1, data = data,
          index = c("quarterID",'ticker','industryID'),model = "within")
coeftest(em, vcov. = vcovHC, type="HC1")
summary(em)
em <- plm(gteps~ categoryGap + wsceps+coverage+count+std-1+MOM+bm+turnover+Ala+sp1500, data = data,
          index = c("quarterID",'ticker','industryID'),model = "within")
coeftest(em, vcov. = vcovHC, type="HC1")
summary(em)

em <- plm(gteps~ qidGap + wsceps+coverage+count+std-1, data = data,
          index = c("quarterID",'ticker','industryID'),model = "within")
coeftest(em, vcov. = vcovHC, type="HC1")
summary(em)
em <- plm(gteps~ qidGap + wsceps+coverage+count+std-1+MOM+bm+turnover+Ala+sp1500, data = data,
          index = c("quarterID",'ticker','industryID'),model = "within")
coeftest(em, vcov. = vcovHC, type="HC1")
summary(em)

em <- plm(gteps~ industryGap + wsceps+coverage+count+std-1, data = data,
          index = c("quarterID",'ticker','industryID'),model = "within")
coeftest(em, vcov. = vcovHC, type="HC1")
summary(em)
em <- plm(gteps~ industryGap + wsceps+coverage+count+std-1+MOM+bm+turnover+Ala+sp1500, data = data,
          index = c("quarterID",'ticker','industryID'),model = "within")
coeftest(em, vcov. = vcovHC, type="HC1")
summary(em)


em <- plm(gteps~ allGap + wsceps+coverage+count+std-1, data = data,
          index = c("quarterID",'ticker','industryID'),model = "within")
coeftest(em, vcov. = vcovHC, type="HC1")
summary(em)
em <- plm(gteps~ allGap + wsceps+coverage+count+std-1+MOM+bm+turnover+Ala+sp1500, data = data,
          index = c("quarterID",'ticker','industryID'),model = "within")
coeftest(em, vcov. = vcovHC, type="HC1")
summary(em)





## Adjusted
em <- plm(gteps~ catdAGap + wsceps+coverage+count+std-1, data = data,
          index = c("quarterID",'ticker','industryID'),model = "within")
coeftest(em, vcov. = vcovHC, type="HC1")
summary(em)
em <- plm(gteps~ catdAGap + wsceps+coverage+count+std-1+MOM+bm+turnover+Ala+sp1500, data = data,
          index = c("quarterID",'ticker','industryID'),model = "within")
coeftest(em, vcov. = vcovHC, type="HC1")
summary(em)

em <- plm(gteps~ categoryAGap + wsceps+coverage+count+std-1, data = data,
          index = c("quarterID",'ticker','industryID'),model = "within")
coeftest(em, vcov. = vcovHC, type="HC1")
summary(em)
em <- plm(gteps~ categoryAGap + wsceps+coverage+count+std-1+MOM+bm+turnover+Ala+sp1500, data = data,
          index = c("quarterID",'ticker','industryID'),model = "within")
coeftest(em, vcov. = vcovHC, type="HC1")
summary(em)

em <- plm(gteps~ qidAGap + wsceps+coverage+count+std-1, data = data,
          index = c("quarterID",'ticker','industryID'),model = "within")
coeftest(em, vcov. = vcovHC, type="HC1")
summary(em)
em <- plm(gteps~ qidAGap + wsceps+coverage+count+std-1+MOM+bm+turnover+Ala+sp1500, data = data,
          index = c("quarterID",'ticker','industryID'),model = "within")
coeftest(em, vcov. = vcovHC, type="HC1")
summary(em)

em <- plm(gteps~ industryAGap + wsceps+coverage+count+std-1, data = data,
          index = c("quarterID",'ticker','industryID'),model = "within")
coeftest(em, vcov. = vcovHC, type="HC1")
summary(em)
em <- plm(gteps~ industryAGap + wsceps+coverage+count+std-1+MOM+bm+turnover+Ala+sp1500, data = data,
          index = c("quarterID",'ticker','industryID'),model = "within")
coeftest(em, vcov. = vcovHC, type="HC1")
summary(em)


em <- plm(gteps~ allAGap + wsceps+coverage+count+std-1, data = data,
          index = c("quarterID",'ticker','industryID'),model = "within")
coeftest(em, vcov. = vcovHC, type="HC1")
summary(em)
em <- plm(gteps~ allAGap + wsceps+coverage+count+std-1+MOM+bm+turnover+Ala+sp1500, data = data,
          index = c("quarterID",'ticker','industryID'),model = "within")
coeftest(em, vcov. = vcovHC, type="HC1")
summary(em)


###############################










# mktadj_car3, sizeadj_car3, mktadj_car1w, sizeadj_car1w, mktadj_car2w, sizeadj_car2w
data$mktadj_car3A = Winsorize(data$mktadj_car3, probs = c(0.01,0.99), na.rm=TRUE)
data$mktadj_car1wA = Winsorize(data$mktadj_car1w, probs = c(0.01,0.99), na.rm=TRUE)
data$mktadj_car2wA = Winsorize(data$mktadj_car2w, probs = c(0.01,0.99), na.rm=TRUE)

data$sizeadj_car3A = Winsorize(data$sizeadj_car3, probs = c(0.01,0.99), na.rm=TRUE)
data$sizeadj_car1wA = Winsorize(data$sizeadj_car1w, probs = c(0.01,0.99), na.rm=TRUE)
data$sizeadj_car2wA = Winsorize(data$sizeadj_car2w, probs = c(0.01,0.99), na.rm=TRUE)


data$catdAGapA = Winsorize(data$catdAGap, probs = c(0.01,0.99), na.rm=TRUE)

for (id in seq(0,9,by=1))
{
data2 = data[data$categoryID==id,]
# data2 = data[data$industryID==id,]
plmall <- lm(sizeadj_car3A~ wsceps -1, data = data2)
#plmall <- lm(mktadj_car3~ catdAGap + wsceps+MOM+bm+turnover+Ala+sp1500 -1, 
#             data = data2)
print(summary(plmall)$coefficients[1,3])
}

for (id in seq(10,37,by=1))
{
  data2 = data[data$quarterID==id,]
  plmall <- lm(sizeadj_car3~ wsceps -1, data = data2)
  #plmall <- lm(mktadj_car3~ catdAGap + wsceps+MOM+bm+turnover+Ala+sp1500 -1, 
  #             data = data2)
  print(summary(plmall)$coefficients[1,3])
}

data$sign = (data$wsceps + data$eceps )/2
plmall <- lm(mktadj_car3~sign -1, 
             data = data)
summary(plmall)


em <- plm(sizeadj_car3~ catdAGap -1, data = data,
          index = c('ticker'),model = "within")
coeftest(em, vcov. = vcovHC, type="HC1")

em <- plm(sizeadj_car3~ catd+wsceps -1, data = data,
          index = c('ticker'),model = "within")
coeftest(em, vcov. = vcovHC, type="HC1")

em <- plm(sizeadj_car3~ allGap + wsceps+coverage+count+std-1+bm+turnover+Ala+sp500, data = data,
          index = c('ticker','quarterID'),model = "within")
coeftest(em, vcov. = vcovHC, type="HC1")

em <- plm(sizeadj_car1w~ allGap + wsceps+coverage+count+std-1+bm+turnover+Ala+sp500, data = data,
          index = c('ticker','quarterID'),model = "within")
coeftest(em, vcov. = vcovHC, type="HC1")

em <- plm(sizeadj_car2w~ allGap + wsceps+coverage+count+std-1+bm+turnover+Ala+sp500, data = data,
          index = c('ticker','quarterID'),model = "within")
coeftest(em, vcov. = vcovHC, type="HC1")



em1 <- plm(sizeadj_car2w~ allAGap + wsceps+coverage+count+std-1+MOM+bm+Ala+turnover+sp1500, data = data,
          index = c("quarterID",'ticker','industryID'),model = "within")
coeftest(em1, vcov. = vcovHC, type="HC1")
summary(em1)

em2 <- plm(sizeadj_car2w~ ibesepsGap + wsceps+coverage+count+std-1+MOM+bm+turnover+Ala+sp1500, data = data,
          index = c("quarterID",'ticker','industryID'),model = "within")
coeftest(em2, vcov. = vcovHC, type="HC1")
summary(em2)

em3 <- plm(sizeadj_car2w~ caepsGap + wsceps+coverage+count+std-1+MOM+bm+turnover+Ala+sp1500, data = data,
          index = c("quarterID",'ticker','industryID'),model = "within")
coeftest(em3, vcov. = vcovHC, type="HC1")
summary(em3)

em4 <- plm(sizeadj_car2w~ ecepsGap + wsceps+coverage+count+std-1+MOM+bm+turnover+Ala+sp1500, data = data,
          index = c("quarterID",'ticker','industryID'),model = "within")
coeftest(em4, vcov. = vcovHC, type="HC1")
summary(em4)

capture.output(texreg(list(em1,em2,em3,em4), 
                      caption = 'Results for the GLS Estimation',stars=c(0.01,0.05,0.1),
                      digits = 4,label='gls_estimation1'), file = '20230213_car.tex')

