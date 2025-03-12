getwd() #mengakses lokasi direktori saat ini disimpan
setwd("D:/REYHAN/KULIAH-2/3. PAML/TUGAS") #memilih lokasi direktori yang baru

df <- read.csv("cust.csv") #mengakses file csv di direktori
dim(df) #mengetahui jumlah baris dan kolom data frame
head(df, 10) #menampilkan 10 baris pertama dari data frame yang sudah dibaca
print(names(df), quote = FALSE) #membuat daftar kolom agar mudah dianalisa
head(df$BALANCE_FREQUENCY,10) #menampilkan 10 baris pertana dari satu variabel
head(df$ONEOFF_PURCHASES,10) #menampilkan 10 baris pertana dari satu variabel

df_segment<-df[, c("BALANCE",
                   "PURCHASES",
                   "ONEOFF_PURCHASES",
                   "CASH_ADVANCE",
                   "PURCHASES_FREQUENCY",
                   "CREDIT_LIMIT",
                   "PAYMENTS",
                   "MINIMUM_PAYMENTS",
                   "PRC_FULL_PAYMENT",
                   "TENURE"
)]

#Menghitung jumlah NA
jumlah_na <- sum(is.na(df_segment))
print(jumlah_na)

#Inisialisasi jumlah NaN
jumlah_nan_total <- 0

for (col in names(df_segment)) {
  for (val in df_segment[[col]]) {
    if (is.nan(val)) {
      jumlah_nan_total <- jumlah_nan_total + 1
    }
  }
}

print(jumlah_nan_total)

#Install library zoo untuk mengatasi data NA dengan mean
install.packages("zoo")
library(zoo)
df_segment <- na.aggregate(df_segment)

#Install library psych untuk mengidentifikasi nilai variabel dengan describe()
install.packages("psych")
library(psych)
describe(df_segment)

#Install library factoextra untuk running fviz_nbclust
install.packages("factoextra")
library(factoextra)
fviz_nbclust(df_segment, FUNcluster = kmeans, method = "wss",k.max = 7)+geom_vline(xintercept = 4,linetype=2)
fviz_nbclust(df_segment, FUNcluster = kmeans, method = "silhouette",k.max = 7)

set.seed(123) #mengatur generator acak
km.res <- kmeans(df_segment,4,nstart=25) #mendapatkan analisis k-mean cluster
print(km.res)
aggregate(df_segment, by=list(cluster=km.res$cluster),mean) #alternatif mendapatkan mean dari k-mean cluster
print(km.res$centers) #mean/centers, #alternatif mendapatkan mean dari k-mean cluster

#visualisasi hasil k-mean cluster
fviz_cluster(km.res, df_segment, ellipse.type = "norm")
