#オブジェクトの消去
rm(list=ls(all=TRUE));

#サンプルデータの読み込み(日経平均の月次データを使用)
url<-"http://indexes.nikkei.co.jp/nkave/historical/nikkei_stock_average_monthly_jp.csv";
nikkei225<-read.csv(url,fileEncoding="CP932");
nikkei225<-nikkei225[-nrow(nikkei225),];#最後にディスクレーマが入っているので排除する。
colnames(nikkei225)<-c("date","end","start","high","low");#カラム名を修正する。
nikkei225$date<-as.Date(nikkei225$date);#日付型に変換しておく。
#write.csv(nikkei225,"nikkei225.csv",row.names=F);#csvに保存する。
print(head(nikkei225))

#ライブラリの読み込み
library(ggplot2);
library(reshape2);
library(data.table);
library(plyr);
library(knitr);

################
#1.apply系の復習
################
#1.1.apply
################
mat1<-as.matrix(nikkei225[-1]);#dateを抜いてmatrixにする。
#列方向の平均を計算する。
mat1_clm_mean<-apply(mat1,2,mean);
#行方向の平均を計算する。
mat1_row_mean<-apply(mat1,1,mean);
################
#1.2.sapply
################
vec1<-nikkei225$high;#高値のみを抽出してベクトルにする。
vec1.mean<-mean(vec1);
vec1.sd<-sd(vec1);
#スケーリングを行う。
vec1.scale<-sapply(vec1,
                   function(x){
                     (x-vec1.mean)/vec1.sd;
                   });
################
#1.3.lapply
################
df1<-nikkei225[-1];#dateを抜いてデータフレームにする。
#終値、始値、高値、低値に対してスケーリングを行う。
ret.list<-lapply(df1,scale);#scale関数でスケーリングが可能
df1.scale<-as.data.frame(ret.list);#lapplyの戻り値はlistになるのでデータフレームに変換する。

################
#2.ggplot & reshape2
################
#2.1.時系列プロット
################
melt.nikkei225<-melt(nikkei225,id="date");#縦方向へと変換する。
print(head(melt.nikkei225));

g<-ggplot(melt.nikkei225);
g<-g+geom_line(aes(x=date,y=value,colour=factor(variable)));
g<-g+ggtitle("Nikkei225 Time Series");
print(g);

################
#2.2.散布図
################
g<-ggplot(nikkei225);
g<-g+geom_point(aes(x=high,y=low));
g<-g+geom_smooth(aes(x=high,y=low));
g<-g+ggtitle("High and Low");
print(g);

################
#3.data.table
################
#3.1.data.tableにする。
################
nikkei225.dt<-data.table(nikkei225);
#キーの設定(複数のキーも設定可能。その場合はベクトルで指定する。)
setkeyv(nikkei225.dt,"date");#setkey(nikkei225.dt,date)でも可能。
#キーによる抽出
nikkei225.dt.20040101<-nikkei225.dt[J(as.Date("2004-01-01"))];
#カラム名の変更
setnames(nikkei225.dt.20040101,c("DATE","END","START","HIGH","LOW"));#colnames()=だとおこられます。
print(nikkei225.dt.20040101);
#カラムの抽出
nikkei225.dt.high.low<-nikkei225.dt[,c("date","high","low"),with=F];
print(head(nikkei225.dt.high.low));
#highの年ごとの平均を計算してみる。
nikkei225.dt[["year"]]<-year(nikkei225.dt[["date"]]);
nikkei225.dt[["month"]]<-month(nikkei225.dt[["date"]]);
nikkei225.dt.y<-nikkei225.dt[,c("year","high"),with=F];
print(head(nikkei225.dt.y));
high.mean<-nikkei225.dt.y[,list(high.mean=mean(high)),by=list(year)];#カラムを指定しないといけない。これを解消するのがplyr
print(head(high.mean));

################
#4.plyr
################
#先の年平均をやってみる。end,start,high,lowについて平均を計算する。
nikkei225.dt.y2<-nikkei225.dt[,c("year","high","low","end","start"),with=F];
year.mean.df<-ddply(nikkei225.dt.y2,.(year),colwise(mean));#データフレームになるので注意
print(head(year.mean.df));
year.mean.list<-dlply(nikkei225.dt.y2,.(year),colwise(mean))#リストにもできます。

################
#5.knitr
################
#右上のボタンでレポートがでる。
#あと便利なのがkable関数。
kable(year.mean.df);