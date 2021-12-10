# Analysis-1
Airline customer value analysis based on R language

# 分析航空公司现状  

1.行业内竞争  
民航的竞争除了三大航空公司之间的竞争之外，还将加入新崛起的各类小型航空公司、民营航空公司，甚至国外航空巨头。航空产品生产过剩，产品同质化特征愈加明显，于是航空公司从价格、服务间的竞争逐渐转向对客户的竞争。

2.行业外竞争
随着高铁、动车等铁路运输的兴建，航空公司受到巨大冲击。  


1、数据清洗 (COMMIT-ID:2415b77a7fe1dc14824372dcc70e889b92ae7bb4)

数据分析，找出数据存在的问题（例如：异常值、缺失值），给出处理办法. 
通过对数据观察发现原始数据中存在票价为空值，票价最小值为0，折扣率最小值为0，总飞行公里数大于0的记录。票价为空值的数据可能是客户不存在乘机记录造成。  
处理方法：丢弃票价为空的记录。  
其他的数据可能是客户乘坐0折机票或者积分兑换造成。由于原始数据量大，这类数据所占比例较小，对于问题影响不大，因此对其进行丢弃处理。  
处理方法：丢弃票价为0，平均折扣率不为0，总飞行公里数大于0的记录。  
使用R语言对数据进行处理，给出代码、对使用的函数进行说明.   

2、构建航空客户价值分析的关键特征(COMMIT-ID:064574cba161e77081b5c774f783aa484d90733c)

LRFMC模型：将客户关系长度L，消费时间间隔R，消费频率F，飞行里程M和折扣系数的平均值C作为航空公司识别客户价值的关键特征记为LRFMC模型。  
L；会员入会时间距观测窗口结束的月数。  
R：客户最近一次乘坐公司飞机距观测窗口结束的月数。  
F：客户在观测窗口内乘坐公司飞机的次数。     
M：客户在观测窗口内累计的飞行里程。  
C：客户在观测窗口内乘坐舱位所对应的折扣系数的平均值。  

3、数据标准化(COMMIT-ID:8c2b547e3286992d91d9fed5bd4d38dd244ee1a0)

（1）数据标准化)的目的是什么  
五个特征的取值范围数据差异较大，数据标准化可以消除数据量级对数据带来的影响。  
（2）对于2中选出的变量进行标准化. 

4、对数据进行聚类分群-kmeans(COMMIT-ID:e5dede58d975ddb98d4cb6be74b53fc32f72ef0c)


基于特征描述，聚类为5个类别，定义五个等级的客户类别：

重要保持客户. 

重要发展客户. 

重要挽留客户. 

一般客户. 

低价值客户.  

客户群1，R最大已经很久没有登机了，同时F、L、M、G最小，也就是说这个客户极可能是折扣率很低的时候才选择坐飞机，属于低价值客户。  
客户群2，公里数M、登机的频次F比较高，最近登机的时间间隔R以及折扣C比较低，这样的客户是重要保持客户。  
客户群3，入会时间L最长，但是距最近登记时间间隔R比较短，折扣率C比较低，公里数M和频次F也比较低，属于重要挽留客户。  
客户群4，尽管公里数M和频次F比较小，但是入会时间L和最近登机的间隔R很短，同时折扣率C最高，属于重要发展客户。  
客户群5，R、F、L、M、C五个指标都很小，属于一般客户。  
