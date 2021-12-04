#读取数据
datafile<-read.csv('/home/mw/project/air_data.csv',header=T,sep=',',fileEncoding='GBK')
#选择数据探索变量
col=c(15:18,20:29)
#输出数据概览
summary(datafile[,col])
#找出票价为缺失值的数据
na_index<-is.na(datafile$SUM_YR_1) | is.na(datafile$SUM_YR_2)
na_datafile<-datafile[which(na_index==1),]
#缺失值数据占总数据的比例
nrow(na_datafile)/nrow(datafile) *100
#丢弃票价为空的记录
#先识别观测窗口第一年、第二年票价收缺失值所在的行，然后删除；$符号表示中文“的”意思， “，”前面表示行，后面表示列。
delet_na = datafile[-which(is.na(datafile$SUM_YR_1) |
                             is.na(datafile$SUM_YR_2)),]
#查看相应记录及所占比例
index<-((delet_na$SUM_YR_1==0 & delet_na$SUM_YR_2==0)
        *(delet_na$avg_discount!=0)
        *(delet_na$SEG_KM_SUM>0))
nrow(delet_na[which(index==1),])/nrow(datafile) *100
#丢弃异常的记录
deletdata<-delet_na[-which(index==1),]
#保存数据
write.csv(deletdata,'./cleanedfile.csv',row.names = FALSE)



#读取数据
cleanedfile<-read.csv('./cleanedfile.csv',header = T)
#names(cleanedfile)
#选取6个相关属性变量
LRFMC<-c('FFP_DATE','LOAD_TIME','FLIGHT_COUNT','SEG_KM_SUM','LAST_FLIGHT_DATE','avg_discount')
reduceddata<-cleanedfile[,LRFMC]
#保存数据
write.csv(reduceddata,'./reducedfile.csv',row.names = FALSE)
#读取数据
cleanedfile<-read.csv('./cleanedfile.csv',header = T)
#查看字段名
#names(cleanedfile)
#选取6个相关属性变量
LRFMC<-c('FFP_DATE','LOAD_TIME','FLIGHT_COUNT','SEG_KM_SUM','LAST_FLIGHT_DATE','avg_discount')
reduceddata<-cleanedfile[,LRFMC]
#保存数据
write.csv(reduceddata,'./reducedfile.csv',row.names = FALSE)
#数据读取
reducedfile<-read.csv('./reducedfile.csv',header = T)
#查看数据字段类型
#str(reducedfile)
#因子型数据转换为日期格式
reducedfile$FFP_DATE<-as.Date(reducedfile$FFP_DATE)
reducedfile$LOAD_TIME<-as.Date(reducedfile$LOAD_TIME)
reducedfile$LAST_FLIGHT_DATE<-as.Date(reducedfile$LAST_FLIGHT_DATE)
#添加L和R  L为离当前时间的入会月数 R为离当前时间的最近坐飞机的时间
reducedfile<-transform(reducedfile,L=difftime(LOAD_TIME,FFP_DATE,units = 'days')/30,
                       R=difftime(LOAD_TIME,LAST_FLIGHT_DATE,units = 'days')/30)
#查看概要
summary(reducedfile)
#删除缺失值 na.omit为删除所有包含缺失值的行
reducedfile<-na.omit(reducedfile)
#摘取有用的数据 并对指标进行重命名
transformeddata<-reducedfile[,c('L','R','FLIGHT_COUNT','SEG_KM_SUM','avg_discount')]
colnames(transformeddata)<-c('L','R','F','M','C')
#保存数据
write.csv(transformeddata,'./transformedfile.csv',row.names = FALSE)


#数据读取
transformeddata<-read.csv('/home/mw/project/transformedfile.csv',header = T)
#数据标准化
zscoreddata<-scale(transformeddata)
#字段重命名
colnames(zscoreddata)<-c('ZL','ZR','ZF','ZM','ZC')
#查看表转化后的数据
head(zscoreddata)
#保存标准化后的数据
write.csv(zscoreddata,'./zscoredfile.csv',row.names = FALSE)


#数据读取
inputfile<-read.csv('./zscoredfile.csv',header = T)
#聚类分析
result<-kmeans(inputfile,5)
#结果输出 cluster表示各类别编号 center表示各类的中心
type<-result$cluster
centervec<-result$centers
#查看类别分布
table(type)
#查看类别中心
centervec
math <- c(419, 993, 2655, 792, 2995)
barplot(math,col=c("red","orange","lightblue","yellow","lightgreen"))

install.packages('fmsb')
#绘制雷达图
#导入数据包
library(fmsb)
#设置各变量的最大值最小值
#centervec为矩阵对象，2表示列操作，max/min为处理数据函数
max<-apply(centervec,2,max)
min<-apply(centervec,2,min)
#把变量取值范围和数据组合成为新的数据集
df=data.frame(rbind(max,min,centervec))
#绘制雷达图
#seg=5表示分为五个等分，plty=1表示使用实线，vlcex为坐标字体大小
radarchart(df=df,seg=5,plty=1,vlcex=0.7)

