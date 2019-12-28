install.packages("sqldf")
library("sqldf")

stock <- read.csv("C:\\Users\\USER\\Documents\\nasdaq.csv")
stock <- sqldf("SELECT * FROM stock WHERE SYMBOL IN (SELECT SYMBOL FROM stock GROUP BY SYMBOL HAVING MAX(dates)=20160318 AND COUNT(dates)>=50)")

stock$dates <- paste(substr(stock$dates,1,4),substr(stock$dates,5,6),substr(stock$dates,7,8),sep="-") 
dateList <- sqldf("select distinct dates from stock") 
dateList$date_seq <- 1:dim(dateList)[1] 

stock1 <- sqldf("select a.*, b.date_seq from stock a, dateList b where a.dates=b.dates") 
stock2 <- stock1[,-1] 

stock2 <- sqldf("select a.*, 0 as m from stock2 a")

dl <- sqldf("select symbol from stock2 group by symbol having max(date_seq)-min(date_seq)+1 != count(date_seq)")
stock2 <- stock2[!(stock2$symbol %in% dl$symbol),]

symbolList <- sqldf("select distinct symbol from stock2 order by symbol")
symbolList$sym_seq <- 1:dim(symbolList)[1]
stock2 <- sqldf("select a.*, b.sym_seq from stock2 a, symbolList b where a.symbol=b.symbol order by symbol, dates")

count_date = sqldf("select symbol, count(dates) cnt from stock2 group by symbol")

cum_seq = 0
for (i in 1:2385){
  for (j in (cum_seq+1):(cum_seq+count_date$cnt[i]-30)){
    max_margin = 0
    for (k in (j+1):(j+30)){
      margin = stock2$high[k]/stock2$close[j]
      if (margin > max_margin){
        max_margin = margin
      }
    }
    stock2$m[j] = max_margin
  } 
  cum_seq <- cum_seq + count_date$cnt[i]
}

write.csv(stock2, file="KIHAN2.csv")

name <- sqldf("select symbol from stock2 where m>=1.1 group by symbol order by symbol")

m1 <- sqldf("select symbol, count(symbol) as num from stock2 where m>=1.1 group by symbol order by symbol")
m2 <- sqldf("select b.symbol, count(date_seq)-30 as datenum from stock2 a, name b where a.symbol=b.symbol group by a.symbol order by a.symbol")
m3 <- sqldf("select a.*, b.datenum from m1 a, m2 b group by a.symbol")
m4 <- sqldf("select a.*, (a.num)/(b.datenum) from m3 a, m3 b where a.symbol=b.symbol")

## 분석코드
stock2 <- read.csv("KIHAN2.csv")

m1 <- sqldf("select symbol, count(symbol) as num from stock2 where m>=1.1 group by symbol order by symbol")
m2 <- sqldf("select b.symbol, count(date_seq)-30 as datenum from stock2 a, name b where a.symbol=b.symbol group by a.symbol order by a.symbol")
m3 <- sqldf("select a.*, b.datenum, 0 ratio from m1 a, m2 b where a.symbol=b.symbol")

for(i in 1:2137)
  m3$ratio[i] = m3$num[i] / m3$datenum[i]

m3 <- sqldf("select a.* from m3 a order by ratio desc")

answer <- read.csv("C:\\Users\\USER\\Documents\\answer_march.csv")
our_answer <- sqldf("select symbol from m3 where ratio > 0.258")
dim(our_answer)
y_answer <- sqldf("select symbol from answer where YN == 'O'")
compare <- sqldf("select a.symbol from y_answer a, our_answer b where a.symbol=b.symbol")
dim(compare)
1069/1420
465/573
## 신뢰구간 코드 

t1 <- sqldf("select distinct symbol, avg(m) ave, stdev(m) std, 0 YN from stock2 where m > 0 group by symbol")
t1 <- sqldf("select a.*, count(date_seq)-30 cnt from t1 a, stock2 b where a.symbol = b.symbol group by a.symbol") 

z = 2.58
for (i in 1:2385){
  if ((t1$ave[i] - z*t1$std[i]/sqrt(t1$cnt[i]))>= 1.1)
    t1$YN[i] = 'O'
  else t1$YN[i] = 'X'
  
}

sqldf("select count(*) from t1 where YN=='O'")

answer <- read.csv("C:\\Users\\USER\\Documents\\answer_march.csv")
answer <- sqldf("select a.* from answer a where YN=='O'")
our_answer <- sqldf("select symbol from t1 where YN =='O'")
dim(our_answer)
dim(answer)
compare <- sqldf("select a.symbol from answer a, our_answer b where a.symbol=b.symbol")
dim(compare)