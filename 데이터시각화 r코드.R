#데이터프레임 읽고 행 이름 지정
d2016=read.table("C:/Users/Jeong So Hyeon/Desktop/rdata/d2016.txt", 
                 sep=",")
col2016=read.csv("C:/Users/Jeong So Hyeon/Desktop/rdata/col2016.csv", 
                 header=TRUE)
colnames(d2016)=col2016[is.na(col2016$길이)==FALSE,"내용"]

#국적별 수 분포
countrylb=c("한국계중국", "중국(-한국계)", "베트남", 
            "우즈벡키스탄", "필리핀", "인도네시아", 
            "일본", "태국", "몽골", "기타아시아", "북미", 
            "유럽", "오세아니아", "기타")
countrycount=length(d2016$국적분류)-sum(is.na(d2016$국적분류))
countrybp <- barplot(table(d2016$국적분류), 
                     main="외국인 노동자의 국적", names.arg=countrylb, 
                     space=5, col=rainbow(15), ylab="노동자 수", ylim=c(0,3500))
text(countrybp, table(d2016$국적분류) + 30, format(table(d2016$국적분류)), 
     xpd = TRUE, col = "blue")
text(70, 3400, labels=paste("전체",countrycount, "명"))

#성별 비율
genderlb=c("남","여")
gendercount=length(d2016$성별)-sum(is.na(d2016$성별))
genderpct=round(table(d2016$성별)/gendercount*100)
genderpctlb=paste(genderlb, genderpct, "%(",table(d2016$성별),"명)")
genderpie=pie(table(d2016$성별), labels=genderpctlb, 
              main=paste("외국인 노동자의 성별 (전체",gendercount, "명)"),
              col=cm.colors(2))

#나이분포
agecount=length(d2016$만나이)-sum(is.na(d2016$만나이))
ageplot=plot(table(d2016$만나이), main=paste("외국인 근로자 나이 (전체",agecount, "명)"), 
             xlab="나이", ylab="노동자 수")

#산업별 외국인 수 분포
industrylb=c("농립어업", "광공업", "건설업", 
             "도소매,음식", "전기통신금융", 
             "사업,공공" )
industrycount=length(d2016$산업분류_재분류)-sum(is.na(d2016$산업분류_재분류))

industrybp <- barplot(table(d2016$산업분류_재분류), 
                      main="외국인노동자가 종사하는 산업", names.arg=industrylb, 
                      col=cm.colors(6), ylab="노동자 수", ylim=c(0,3500))
text(industrybp, table(d2016$산업분류_재분류) + 30, format(table(d2016$산업분류_재분류)), 
     xpd = TRUE, col = "blue")
text(5.2, 3200, labels=paste("전체",industrycount, "명") )

#임금
moneylb=c("100만미만","100만~200만미만",
          "200만~300만미만","300만이상" )
moneycount=length(d2016$월평균_임금_임금근로자만해당)-
  sum(is.na(d2016$월평균_임금_임금근로자만해당))
moneypct=round(table(d2016$월평균_임금_임금근로자만해당)/moneycount*100)
moneypctlb=paste(moneylb, moneypct, "%(",table(d2016$월평균_임금_임금근로자만해당),"명)")
install.packages("plotrix")
library(plotrix)
pie3D(table(d2016$월평균_임금_임금근로자만해당),labels=moneypctlb,explode=0.1,
      main=paste("외국인 노동자의 임금 (전체",moneycount, "명)"), 
      col=c("lightblue", "mistyrose", "lightcyan", "lavender"))


moneybp <- barplot(table(d2016$월평균_임금_임금근로자만해당), 
                   main="외국인 노동자의 임금", names.arg=moneylb, 
                   col=cm.colors(4), ylab="노동자 수", ylim=c(0,3000))
text(moneybp, table(d2016$월평균_임금_임금근로자만해당) + 30, format(table(d2016$월평균_임금_임금근로자만해당)), 
     xpd = TRUE, col = "blue")

#성별산업군
par(mfrow=c(1,2))
manindustrybp=barplot(table(d2016[d2016$성별 %in% 1,"산업분류_재분류"]), col=cm.colors(6), 
                      names.arg=industrylb, main=paste("외국인노동자의 성별에 따른 산업군(남자) (전체", sum(table(d2016[d2016$성별 %in% 1,"산업분류_재분류"])), "명)"), 
                      xlab="산업", ylab="노동자수", ylim=c(0,2500))
text(manindustrybp, table(d2016[d2016$성별 %in% 1,"산업분류_재분류"]), 
     format(table(d2016[d2016$성별 %in% 1,"산업분류_재분류"])), xpd = TRUE)
womanindustrybp=barplot(table(d2016[d2016$성별 %in% 2,"산업분류_재분류"]), col=cm.colors(6), 
                        names.arg=industrylb, main=paste("외국인노동자의 성별에 따른 산업군(여자) (전체", sum(table(d2016[d2016$성별 %in% 2,"산업분류_재분류"])), "명)"), 
                        xlab="산업", ylab="노동자수", ylim=c(0,1000))
text(womanindustrybp, table(d2016[d2016$성별 %in% 2,"산업분류_재분류"]), 
     format(table(d2016[d2016$성별 %in% 2,"산업분류_재분류"])), xpd = TRUE)

#성별임금
par(mfrow=c(1,2))
manmoneybp=barplot(table(d2016[d2016$성별 %in% 1,"월평균_임금_임금근로자만해당"]), col=cm.colors(4), 
                   names.arg=moneylb, main=paste("외국인노동자의 성별에 따른 임금(남자) (전체", sum(table(d2016[d2016$성별 %in% 1,"월평균_임금_임금근로자만해당"])), "명)"), 
                   xlab="임금", ylab="노동자수", ylim=c(0,2500))
text(manmoneybp, table(d2016[d2016$성별 %in% 1,"월평균_임금_임금근로자만해당"]), 
     format(table(d2016[d2016$성별 %in% 1,"월평균_임금_임금근로자만해당"])), xpd = TRUE)
womanmoneybp=barplot(table(d2016[d2016$성별 %in% 2,"월평균_임금_임금근로자만해당"]), col=cm.colors(4), 
                     names.arg=moneylb, main=paste("외국인노동자의 성별에 따른 임금(여자) (전체", sum(table(d2016[d2016$성별 %in% 2,"월평균_임금_임금근로자만해당"])), "명)"), 
                     xlab="임금", ylab="노동자수", ylim=c(0,1600))
text(womanmoneybp, table(d2016[d2016$성별 %in% 2,"월평균_임금_임금근로자만해당"]), 
     format(table(d2016[d2016$성별 %in% 2,"월평균_임금_임금근로자만해당"])), xpd = TRUE)

#교욱정도임금
par(mfrow=c(2,2))
elementmoneybp=barplot(table(d2016[d2016$교육정도 %in% 1,"월평균_임금_임금근로자만해당"]), col=cm.colors(4), 
                       names.arg=moneylb, 
                       main=paste("외국인노동자의 교육정도에 따른 임금(초졸이하) (전체", sum(table(d2016[d2016$교육정도 %in% 1,"월평균_임금_임금근로자만해당"])), "명)"), 
                       xlab="임금", ylab="노동자수", ylim=c(0,300))
text(elementmoneybp, table(d2016[d2016$교육정도 %in% 1,"월평균_임금_임금근로자만해당"]), 
     format(table(d2016[d2016$교육정도 %in% 1,"월평균_임금_임금근로자만해당"])), xpd = TRUE)
midmoneybp=barplot(table(d2016[d2016$교육정도 %in% 2,"월평균_임금_임금근로자만해당"]), col=cm.colors(4), 
                   names.arg=moneylb, 
                   main=paste("외국인노동자의 교육정도에 따른 임금(중졸) (전체", sum(table(d2016[d2016$교육정도 %in% 2,"월평균_임금_임금근로자만해당"])), "명)"), 
                   xlab="임금", ylab="노동자수", ylim=c(0,800))
text(midmoneybp, table(d2016[d2016$교육정도 %in% 2,"월평균_임금_임금근로자만해당"]), 
     format(table(d2016[d2016$교육정도 %in% 2,"월평균_임금_임금근로자만해당"])), xpd = TRUE)
highmoneybp=barplot(table(d2016[d2016$교육정도 %in% 3,"월평균_임금_임금근로자만해당"]), col=cm.colors(4), 
                    names.arg=moneylb, 
                    main=paste("외국인노동자의 교육정도에 따른 임금(고졸) (전체", sum(table(d2016[d2016$교육정도 %in% 3,"월평균_임금_임금근로자만해당"])), "명)"), 
                    xlab="임금", ylab="노동자수", ylim=c(0,1300))
text(highmoneybp, table(d2016[d2016$교육정도 %in% 3,"월평균_임금_임금근로자만해당"]), 
     format(table(d2016[d2016$교육정도 %in% 3,"월평균_임금_임금근로자만해당"])), xpd = TRUE)
univmoneybp=barplot(table(d2016[d2016$교육정도 %in% 4,"월평균_임금_임금근로자만해당"]), col=cm.colors(4), 
                    names.arg=moneylb, 
                    main=paste("외국인노동자의 교육정도에 따른 임금(대졸이상) (전체", sum(table(d2016[d2016$교육정도 %in% 4,"월평균_임금_임금근로자만해당"])), "명)"), 
                    xlab="임금", ylab="노동자수", ylim=c(0,800))
text(univmoneybp, table(d2016[d2016$교육정도 %in% 4,"월평균_임금_임금근로자만해당"]), 
     format(table(d2016[d2016$교육정도 %in% 4,"월평균_임금_임금근로자만해당"])), xpd = TRUE)

#교육정도산업
par(mfrow=c(2,2))
elementindusbp=barplot(table(d2016[d2016$교육정도 %in% 1,"산업분류_재분류"]), col=cm.colors(4), 
                       names.arg=industrylb, 
                       main=paste("외국인노동자의 교육정도에 따른 산업(초졸이하) (전체", sum(table(d2016[d2016$교육정도 %in% 1,"산업분류_재분류"])), "명)"), 
                       xlab="산업", ylab="노동자수", ylim=c(0,300))
text(elementindusbp, table(d2016[d2016$교육정도 %in% 1,"산업분류_재분류"]), 
     format(table(d2016[d2016$교육정도 %in% 1,"산업분류_재분류"])), xpd = TRUE)
midindusbp=barplot(table(d2016[d2016$교육정도 %in% 2,"산업분류_재분류"]), col=cm.colors(4), 
                   names.arg=industrylb, 
                   main=paste("외국인노동자의 교육정도에 따른 산업(중졸) (전체", sum(table(d2016[d2016$교육정도 %in% 2,"산업분류_재분류"])), "명)"), 
                   xlab="산업", ylab="노동자수", ylim=c(0,800))
text(midindusbp, table(d2016[d2016$교육정도 %in% 2,"산업분류_재분류"]), 
     format(table(d2016[d2016$교육정도 %in% 2,"산업분류_재분류"])), xpd = TRUE)
highindusbp=barplot(table(d2016[d2016$교육정도 %in% 3,"산업분류_재분류"]), col=cm.colors(4), 
                    names.arg=industrylb, 
                    main=paste("외국인노동자의 교육정도에 따른 산업(고졸) (전체", sum(table(d2016[d2016$교육정도 %in% 3,"산업분류_재분류"])), "명)"), 
                    xlab="산업", ylab="노동자수", ylim=c(0,1600))
text(highindusbp, table(d2016[d2016$교육정도 %in% 3,"산업분류_재분류"]), 
     format(table(d2016[d2016$교육정도 %in% 3,"산업분류_재분류"])), xpd = TRUE)
univindusbp=barplot(table(d2016[d2016$교육정도 %in% 4,"산업분류_재분류"]), col=cm.colors(4), 
                    names.arg=industrylb, 
                    main=paste("외국인노동자의 교육정도에 따른 산업(대졸이상) (전체", sum(table(d2016[d2016$교육정도 %in% 4,"산업분류_재분류"])), "명)"), 
                    xlab="산업", ylab="노동자수", ylim=c(0,800))
text(univindusbp, table(d2016[d2016$교육정도 %in% 4,"산업분류_재분류"]), 
     format(table(d2016[d2016$교육정도 %in% 4,"산업분류_재분류"])), xpd = TRUE)

#이직관련
changelb=c("있었음","없었음")
changecount=length(d2016$직장변경여부)-sum(is.na(d2016$직장변경여부))
changepct=round(table(d2016$직장변경여부)/changecount*100)
changepctlb=paste(changelb, changepct, "%(",table(d2016$직장변경여부),"명)")
changepie=pie(table(d2016$직장변경여부), labels=changepctlb, 
              main=paste("외국인 노동자의 직장변경여부 (전체",changecount, "명)"), 
              col=c("lightblue", "mistyrose"))

#이직이유별 외국인 수 분포
changereasonlb=c("근로조건불만족","개인사유","이전직장폐업","기타")
changereasoncount=length(d2016$이직이유_재분류)-sum(is.na(d2016$이직이유_재분류))
changereasonbp <- barplot(table(d2016$이직이유_재분류), 
                          main="외국인노동자의 직장변경이유", names.arg=changereasonlb, 
                          col=c("lavender", "cornsilk", "lightblue", "mistyrose"), ylab="노동자 수", ylim=c(0,500))
text(changereasonbp, table(d2016$이직이유_재분류) + 10, format(table(d2016$이직이유_재분류)), 
     xpd = TRUE, col = "blue")
text(3.8, 450, labels=paste("전체",changereasoncount, "명") )

changereasonpct=round(table(d2016$이직이유_재분류)/changereasoncount*100)
changereasonpctlb=paste(changereasonlb, changereasonpct, "%(",table(d2016$이직이유_재분류),"명)")
changereasonpie=pie(table(d2016$이직이유_재분류), labels=changereasonpctlb, 
                    main=paste("외국인 노동자의 직장변경이유 (전체",changereasoncount, "명)"), 
                    col=c("lightblue", "mistyrose", "lavender", "cornsilk"))

#국내와 비교
kor2016=read.csv("C:/Users/Jeong So Hyeon/Desktop/rdata/kor2016.txt", 
                 header=TRUE, sep=",")
par(mfrow=c(1,2))
nkormoneybp=barplot(table(d2016$월평균_임금_임금근로자만해당), col=cm.colors(4), 
                    names.arg=moneylb, main=paste("국내외국인근로자 임금 (전체", sum(table(d2016$월평균_임금_임금근로자만해당)), "명)"), 
                    xlab="임금", ylab="노동자수", ylim=c(0,3000))
text(nkormoneybp, table(d2016$월평균_임금_임금근로자만해당), 
     format(table(d2016$월평균_임금_임금근로자만해당)), xpd = TRUE)
kormoneybp=barplot(kor2016[c(2:5),2], col=cm.colors(4), 
                   names.arg=moneylb, main=paste("국내전체근로자 임금 (전체", sum(kor2016[c(2:5),2]), "명)"), 
                   xlab="임금", ylab="노동자수", ylim=c(0,4600000))
text(kormoneybp, kor2016[c(2:5),2], 
     format(kor2016[c(2:5),2]), xpd = TRUE)
