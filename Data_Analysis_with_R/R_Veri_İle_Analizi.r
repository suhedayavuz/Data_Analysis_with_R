encok_satan_bilimkurgu_kitaplari <- data.frame(ID = 1:10,
Ad = c("Kadınlar Ülkesi", "Doktor Moreau'nun Adası", "Görünmez Adam", "Otostopçunun Galaksi
Rehberi", "Frankenstein", "Liste", "Evrenin Sonundaki Restoran", "Çocukluğun Sonu", "Vakıf", "Elveda
ve Bütün O Balıklar İçin Teşekkürler"))

encok_satan_bilimkurgu_kitaplari2 <- data.frame(ID = 1:10,
Yayınevi = c("İthaki Yayınları", "İthaki Yayınları", "İthaki Yayınları", "Alfa Yatınları", "İthaki Yayınları",
"Genç Timaş", "Alfa Yayınları", "İthaki Yayınları", "İthaki Yayınları", "Alfa Yayınları"),
ciltli_ciltsiz = c( "ciltsiz", "ciltsiz", "ciltsiz", "ciltsiz", "ciltsiz", "ciltsiz", "ciltsiz", "ciltsiz", "ciltsiz",
"ciltsiz"),
yazar_isimleri = c("Charlotte Perkins Gilman", "H. G. Wells", "H. G. Wells", "Douglas Adams", "Mary
Sheley", "Patricia Forde", "Douglas Adams", "Arthur C. Clarke", "Isaac Asimov", "Douglas Adams"),
indirimsiz_fiyatlar = c(45, 38, 52, 89, 50, 120, 95, 90, 104, 75),
indirimli_fiyatlar = c(24.75, 20.90, 28.60, 57.85, 27.50, 84, 61.75, 49.50, 57.20, 48.75))
combined_data <- merge(x = encok_satan_bilimkurgu_kitaplari,
y = encok_satan_bilimkurgu_kitaplari2,
by = "ID")
combined_data
combined_data$yayimlanma_yili <- c(2018, 2017, 2013, 2017, 2012, 2018, 2017, 2015, 2017, 2017)
yeni_satir <- data.frame(ID = 11,
Ad = "Hayat, Evren ve Her Şey",
Yayınevi = "Alfa Yayınları",
ciltli_ciltsiz = "ciltsiz",
yazar_isimleri = "Douglas Adams",
indirimsiz_fiyatlar = 85,
indirimli_fiyatlar = 55.25,
yayimlanma_yili = 2017)
combined_data <- rbind(combined_data, yeni_satir)
combined_data
carpim_hesapla <- function(combined_data){
combined_data$carpim <- combined_data[,"indirimsiz_fiyatlar"]*
combined_data[,"indirimli_fiyatlar"]*combined_data[,"yayimlanma_yili"]
return(combined_data)
}
combined_data <- carpim_hesapla(combined_data)
getwd() 
setwd("C:/Users/suhed/OneDrive/Masaüstü")
write.csv(combined_data, file= "combined data CSV")
install.packages("dplyr")
library(dplyr)
t_test <- combined_data %>%
select(Ad, indirimsiz_fiyatlar) %>%
filter(Ad == "Görünmez Adam" | Ad == "Evrenin Sonundaki Restoran")
t_test2 <- combined_data %>%
select(Ad, indirimli_fiyatlar) %>%
filter(Ad == "Görünmez Adam" | Ad == "Evrenin Sonundaki Restoran")
t.test(t_test$indirimsiz_fiyatlar, t_test2$indirimli_fiyatlar) 
t_test_sonuc <- t.test(t_test$indirimsiz_fiyatlar, t_test2$indirimli_fiyatlar)
if (t_test_sonuc$p.value < 0.05) {
print("İki veri seti arasında anlamlı bir fark bulundu.")
} else {
print("İki veri seti arasında anlamlı bir fark bulunamadı.")
}
install.packages("corrplot")
library(corrplot)
a <- combined_data[,c(6,7)]
cor(a)
cor(combined_data$indirimsiz_fiyatlar, combined_data$indirimli_fiyatlar) 
corrplot(cor(a),method= "pie")