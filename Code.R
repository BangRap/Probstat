#==========================================================
# Memahami Dataset
#==========================================================

# Membaca dataset
data <- read.csv("pembangunan_wilayah_missing_outlier.csv")

# Menampilkan beberapa data awal
head(data)

# Melihat struktur data
str(data)

# Ringkasan data
summary(data)

# Melihat jumlah baris dan kolom
dim(data)

#==========================================================
# Statistika Deskriptif
#==========================================================

library(dplyr)

# Memilih variabel numerik

data_num <- data %>%
  select(where(is.numeric))

# Statistik deskriptif umum
summary(data_num)

# Mean
sapply(data_num, mean, na.rm = TRUE)

# Median
sapply(data_num, median, na.rm = TRUE)

# Standar deviasi
sapply(data_num, sd, na.rm = TRUE)

# Varians
sapply(data_num, var, na.rm = TRUE)

# Kuartil
sapply(data_num, quantile, na.rm = TRUE)

#==========================================================
# 1.3.3 Analisis Missing Value
#==========================================================


# Menghitung jumlah missing value setiap variabel
colSums(is.na(data))


# Menampilkan total missing value
sum(is.na(data))

# Persentase missing value
colSums(is.na(data))/nrow(data)*100

# Penanganan missing value dengan median imputation
data_bersih <- na.omit(data)

# Memastikan tidak ada missing value lagi
colSums(is.na(data_bersih))

#==========================================================
# 1.3.4 Analisis Outlier 
#==========================================================

# Memilih variabel numerik
data_num <- data_bersih %>%
  select(where(is.numeric))

# Membuat boxplot seluruh variabel numerik
boxplot(data_num,
        main = "Deteksi Outlier Variabel Numerik",
        las = 2)

# Membuat boxplot untuk PDRB per kapita untuk menganalisis lebih lanjut
boxplot(data_bersih$pdrb_perkapita,
        main = "Boxplot PDRB Per Kapita")

# Membuat boxplot untuk kemiskinan untuk menganalisis lebih lanjut
boxplot(data_bersih$kemiskinan,
        main = "Boxplot Kemiskinan")

# Membuat boxplot untuk Pengangguran untuk menganalisis lebih lanjut
boxplot(data_bersih$pengangguran,
        main = "Boxplot Pengangguran")




#==========================================================
# 1.3.5 Visualisasi Data 
#==========================================================

# Histogram PDRB Per Kapita
hist(data$pdrb_perkapita,
     main = "Distribusi PDRB Per Kapita",
     xlab = "PDRB Per Kapita",
     col = "skyblue",
     border = "black")

# Boxplot Tingkat Kemiskinan
boxplot(data$kemiskinan,
        main = "Boxplot Tingkat Kemiskinan",
        ylab = "Persentase Kemiskinan",
        col = "orange")

# Scatter Plot IPM dan Kemiskinan
plot(data$ipm,
     data$kemiskinan,
     main = "Hubungan IPM dan Kemiskinan",
     xlab = "IPM",
     ylab = "Kemiskinan (%)",
     pch = 19,
     col = "blue")

abline(lm(kemiskinan ~ ipm, data = data),
       col = "red",
       lwd = 2)

# Bar Chart Rata-rata IPM per Provinsi

library(dplyr)

ipm_prov <- data %>%
  group_by(provinsi) %>%
  summarise(rata_ipm = mean(ipm, na.rm = TRUE))

barplot(ipm_prov$rata_ipm,
        names.arg = ipm_prov$provinsi,
        las = 2,
        col = "green",
        main = "Rata-rata IPM per Provinsi",
        ylab = "IPM")

# Pie Chart Kategori IPM

# Membuat kategori IPM
kategori_ipm <- cut(data$ipm,
                    breaks = c(0, 60, 75, 100),
                    labels = c("Rendah", "Sedang", "Tinggi"))

# Menghitung frekuensi
freq_ipm <- table(kategori_ipm)

# Membuat pie chart
pie(freq_ipm,
    main = "Proporsi Kategori IPM Wilayah",
    col = rainbow(length(freq_ipm)))

#==========================================================
# 1.3.6 Analisis Probabilitas dan Distribusi Data 
#==========================================================

# Menguji Distribusi Data
shapiro.test(data$akses_internet)

# Menentukan apakah data mendekati distribusi normal
mean_akses <- mean(data$akses_internet)
sd_akses <- sd(data$akses_internet)

pnorm(mean_akses,
      mean = mean_akses,
      sd = sd_akses)

#Menjelaskan hubungan konsep probabilitas dengan dataset

rata_akses <- mean(data$akses_internet)

jumlah_diatas <- sum(data$akses_internet > rata_akses)

total_wilayah <- sum(data$akses_internet)

probabilitas <- jumlah_diatas / total_wilayah

probabilitas
