
library(readr)
library(dplyr)
library(ggplot2)
library(jsonlite)
library(readr)

#1

library(readr)
library(dplyr)
Country_103<-read_csv("C:/Users/HO/Downloads/103_ab103_C.csv")
Country_104<-read_csv("C:/Users/HO/Downloads/104_ab104_C.csv")
Country_105<-read_csv("C:/Users/HO/Downloads/105_ab105_C.csv")
Country_106<-read_csv("C:/Users/HO/Downloads/106_ab105_C.csv")

names(Country_103)[3:10]<-gsub("-","_",names(Country_103)[3:10])
names(Country_104)[3:10]<-gsub("-","_",names(Country_104)[3:10])


Country14151617<-rbind(Country_103,Country_104,Country_105,Country_106)

Country14151617$學生總數 <- rowSums(Country14151617[3:11])


Country14151617%>%
 group_by(國別)%>%
 summarise(totalPeople=sum(學生總數))%>%
 arrange(desc(totalPeople))%>%
 head(10)      
          
School_103<-read_csv("C:/Users/HO/Downloads/103_ab103_S.csv")       
School_104<-read_csv("C:/Users/HO/Downloads/104_ab104_S.csv")
School_105<-read_csv("C:/Users/HO/Downloads/105_ab105_S.csv")
School_106<-read_csv("C:/Users/HO/Downloads/106_ab105_S.csv")

names(School_103)[4:11]<-gsub("-","_",names(School_103)[4:11])
names(School_104)[4:11]<-gsub("-","_",names(School_104)[4:11])

School_14151617<-rbind(School_103,School_104,School_105,School_106)

School_14151617$非學位生_大陸研修生<-gsub("…","0",School_14151617$非學位生_大陸研修生)
School_14151617$非學位生_大陸研修生<-as.numeric(School_14151617$非學位生_大陸研修生)

School_14151617$學生總數 <- rowSums(School_14151617[4:12])


School_14151617%>%
 group_by(學校名稱)%>%
 filter(學校名稱!="無法區分校別")%>%
 summarise(totalPeople=sum(學生總數))%>%
 arrange(desc(totalPeople))%>%
 head(10)     

#2

library(ggplot2)

Total<-Country14151617%>%
  group_by(國別)%>%
  summarise(totalPeople=sum(學生總數))%>%
  arrange(desc(totalPeople))

Total1_other<-group_by(Country14151617, 國別) %>%
  tally(學生總數, sort = TRUE) %>%
  group_by(國別 = factor(c(國別[1:45], rep("Other", n() - 45)),
                       levels = c(國別[1:45], "Other"))) %>%
  tally(n) 

ggplot()+
  geom_bar(data=Total1_other,
           aes(x=國別,y=nn),
           stat = "identity")+
  labs(x="國家",y="來台灣唸書的學生人數")+
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5))


#3
承1，請用面量圖呈現各個國家來台灣唸書的學生人數，人數越多顏色越深(10分)。

library(jsonlite)
library(readr)
library(dplyr)

MaptoEng <- fromJSON("C:/Users/HO/Downloads/countries.json")  
names(Total) <- c("country","totalpeople") 

for(i in 1:nrow(Total)){
  for(j in 1:nrow(MaptoEng)){
    if(Total$country[i]==MaptoEng$Taiwan[j]){
      Total$country[i] <- MaptoEng$ISO2[j]
    }
    else if(substr(Total$country[i],1,2)==substr(MaptoEng$Taiwan[j],1,2)){
      Total$country[i] <- MaptoEng$ISO2[j]
    }
  }
}

Total$country[50] <- "HK"
Total$country[67] <- "BA"
Total$country[72] <- "RO"
Total$country[80] <- "KR"
Total$country[87] <- "VG"
Total$country[108] <- "MX"
Total$country[112] <- "RS"
Total$country[167] <- "AW"
Total$country[173] <- "CV"


Country_lat_long <- read_csv("~/A.csv")

library(rworldmap)
library(ggplot2)

Map_comeTW <- left_join(Total,Country_lat_long, by = "country")

Map_comeTW$latitude[123]<-12.178361
Map_comeTW$longitude[123]<-68.238534
Map_comeTW$latitude[81]<-6.876992
Map_comeTW$longitude[81]<-31.306979
Map_comeTW$latitude[90]<-19.282319
Map_comeTW$longitude[90]<-166.647047

Map_comeTW$longitude <- as.numeric(Map_comeTW$longitude)
Map_comeTW$latitude <- as.numeric(Map_comeTW$latitude)


map.world <- map_data(map="world")

ggplot()+ 
  theme(legend.position="none")+
  geom_map(data=map.world, map=map.world, aes(map_id=region, x=long, y=lat,fill=group))+ 
  geom_point(data=Map_comeTW,
             aes(x=longitude, y=latitude,color=totalpeople),size=3)+
  scale_color_continuous(low = "sandybrown",high = "red")+ 
  guides(size=FALSE)



#4
Country<-read.csv("C:/Users/HO/Downloads/A.csv")

Country_1<-filter(Country,學年度!=101,!(學年度==102&學期==1))

Like_Con<-Country_1%>%
  group_by(對方學校.機構.國別.地區.)%>%
  summarise(人數=n())%>%
  arrange(desc(人數))%>%
  head(10)   

Like_Sch<-Country_1%>%
  group_by(學校名稱)%>%
  summarise(人數=n())%>%
  arrange(desc(人數))%>%
  head(10)    

#5
total_1<-Country_1%>%
  group_by(學校名稱)%>%
  summarise(人數=n())%>%
  arrange(desc(人數))

ggplot()+
  geom_bar(data=total_1,
           aes(x=學校名稱,y=人數),
           stat = "identity")+
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5))

#6
承4，請用面量圖呈現台灣大專院校的學生去各國家進修交流人數，人數越多顏色越深(10分)。

AAA <- read.csv("C:/Users/HO/Downloads/AAA.csv")
AAA <- AAA[,c(1:4)]
Taiwan_university <- left_join(total_1,AAA,by="學校名稱")

BBB <- read.csv("C:/Users/HO/Downloads/BBBB.csv")

BBB <- BBB[,c(1:4)]

Taiwan_university$地址[85]<-"[807]新北市板橋區四川路二段58號"
Taiwan_university$地址[93]<-"[807]彰化縣彰化市介壽北路1號"
  


for(p in 1:nrow(Taiwan_university)){
Taiwan_university$New_地址[p]<-substr(Taiwan_university$地址[p],6,25)  
Taiwan_university$New_地址_區[p]<-regexpr("區",Taiwan_university$New_地址[p])
Taiwan_university$New_地址_區<-as.numeric(Taiwan_university$New_地址_區)

if(Taiwan_university$New_地址_區[p]>0){
Taiwan_university$行政區名[p]<-substr(Taiwan_university$New_地址[p],start = 1,stop = Taiwan_university$New_地址_區[p])
}
else if(Taiwan_university$New_地址_區[p]<0){
Taiwan_university$New_地址_區[p]<-regexpr("市|鎮|鄉",Taiwan_university$New_地址[p])
Taiwan_university$行政區名[p]<-substr(Taiwan_university$New_地址[p],start = 1,stop = Taiwan_university$New_地址_區[p])
}
}


Finally_TU <- left_join(Taiwan_university,BBB,by="行政區名")
Finally_TU <- Finally_TU[,c(1:11)]

library(ggmap)

TW<-get_googlemap(center = c(lon=120.58,lat=23.58), 
                       zoom = 7,
                       language = "zh-TW",maptype = "roadmap")
ggmap(TW)+ 
  geom_point(data=Finally_TU,
             aes(x=lat, y=long,color=人數),size=3)+
  scale_color_continuous(low = "sandybrown",high = "red")+ 
  guides(size=FALSE)

#7

台灣學生最喜歡去哪些國家留學呢？請取出前十名的國家與總人數，由大到小排序(5分)。

abroad<-read_csv("C:/Users/HO/Downloads/105___ .csv")

abroad%>%
  group_by(國別)%>%
  summarise(totalPeople=sum(總人數))%>%
  arrange(desc(totalPeople))%>%
  head(10)     

#8
承7，請用面量圖呈現台灣學生去各國家留學人數，人數越多顏色越深(10分)。
library(jsonlite)
library(readr)
library(dplyr)

MaptoEng <- fromJSON("C:/Users/HO/Downloads/countries.json")  

for(i in 1:nrow(abroad)){
  for(j in 1:nrow(MaptoEng)){
    if(abroad$國別[i]==MaptoEng$Taiwan[j]){
      abroad$名子[i] <- MaptoEng$ISO2[j]
    }
  }
}

abroad<-abroad[,c(1,3,7)]
names(abroad)<-c("州","people","country")
Country_lat_long <- read_csv("~/A.csv")

library(rworldmap)
library(ggplot2)

Map_abroad <- left_join(abroad,Country_lat_long, by = "country")

Map_abroad$longitude <- as.numeric(Map_abroad$longitude)
Map_abroad$latitude <- as.numeric(Map_abroad$latitude)

map.world <- map_data(map="world")

ggplot()+ 
  theme(legend.position="none")+
  geom_map(data=map.world, map=map.world, aes(map_id=region, x=long, y=lat,fill=group))+ 
  geom_point(data=Map_abroad,
             aes(x=longitude, y=latitude,color=people),size=3)+
  scale_color_continuous(low = "sandybrown",high = "red")+ 
  guides(size=FALSE)



#9
請問來台讀書與離台讀書的來源國與留學國趨勢是否相同(5分)？
想來台灣唸書的境外生，他們的母國也有很多台籍生嗎？請圖文並茂說明你的觀察(10分)。



