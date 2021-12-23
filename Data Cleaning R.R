#Mengimport library yang digunakan untuk proses processing data 
library (caret)


#Membaca data dengan format file CSV
data = read.csv('C:/Users/USER/Documents/Dibimbing/train.csv')
View(data)


#Melihat jumlah missing value dalam sebuah dataset
View(colSums(is.na(data)))


#Menghandle missing value pada kolom BsmtQual
#BsmtQual merupakan kondisi ketinggian ruang bawah tanah
#Type data pada kolom BsmtQual merupakan character
#Mengatasi missing value dengan 'Other'

data[is.na(data$BsmtQual),'BsmtQual'] = 'Other'
View(colSums(is.na(data)))


#Melakukan normalisasi/standarization pada satu variable
hist(data$MSSubClass)


#Dari grafik histogram dapat dilihat bahwa data tersebut tidak normal
#Maka kita melakukan normalisasi pada variable tersebut
std = preProcess(data['MSSubClass'],method = c('range'))
data['MSSubClass'] = predict(std,data['MSSubClass'])
View(data)


#Melakukan data transformasi dengan Box Cox transformation pada variable SalePrice
sp = data$SalePrice
hist(sp)#Tampilan grafik histogram sebelum di transformasi skew kanan karna mean>median

log(sp)
summary(sp)
BoxCoxTrans(sp)$lambda

lambda = BoxCoxTrans(sp)$lambda
boxcox_adr = (sp**lambda - 1)/lambda
hist(boxcox_adr)#Tampilan garik histogram setelah di transformasi


#Outlier handling pada variable Sale Price dengan IQR methode
sp = data$SalePrice
boxplot(sp)

q1 = quantile(sp,0.25)
q3 = quantile(sp,0.75)

iqr = q3 - q1

upper_bound = q3 + 1.5 * iqr
lower_bound = q1 - 1.5 * iqr


sp_iqr = data[sp > lower_bound & sp < upper_bound,]
boxplot(sp_iqr$SalePrice)


#Data categirical handling pada kolom data BsmtQual
#Menggunakan One Hot Encoding karna datanya lebih dari 2 lebel
table(data$BsmtQual)

dummy = dummyVars("~.",data=data['BsmtQual'])
dummy_var = data.frame(predict(dummy, newdata = data['BsmtQual']))
View(dummy_var)

data = cbind(data,dummy_var)
View(data)


#Membuat data frame dari data - data yang sudah di preprocessing
data_new = rbind('BsmtQual' = data$BsmtQual, #Data yg di handle missing value
                 'MSSubClass' = data$MSSubClass,#Data yang di normalisasi
                 'SalePrice' = sp_iqr$SalePrice,#Data yang di handle Outliernya
                 'BsmtQualEx' = data$BsmtQualEx,#Data One Hot Encoding handling
                 'BsmtQualFa' = data$BsmtQualFa, 
                 'BsmtQualGd' = data$BsmtQualGd, 
                 'BsmtQualOther' = data$BsmtQualOther, 
                 'BsmtQualTa' = data$BsmtQualTA)

#Note : #akan muncul warning ketika di run 
#Warning dikarenakan data yang dimasukan ke dalam DF merupakan data yg telah di handle ouliernya
#Tetapi data yg di tampilkan full lengkap dan tidak ada missing value

View(t(data_new))
colSums(is.na(t(data_new)))

#Mengimport data menjadi file CSV
write.csv(t(data_new),'C:/Users/USER/Documents/Dibimbing/train_new.csv', row.names = FALSE)