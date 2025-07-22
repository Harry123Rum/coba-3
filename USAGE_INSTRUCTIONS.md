# SIVANA Dashboard - Panduan Penggunaan

## 🚀 Cara Menjalankan Dashboard

### Opsi 1: Versi Lengkap (SIVANA_Dashboard.R)
```r
# Di R atau RStudio
source("SIVANA_Dashboard.R")
```

### Opsi 2: Versi Minimal (SIVANA_Minimal.R) - Direkomendasikan
```r
# Di R atau RStudio
source("SIVANA_Minimal.R")
```

### Opsi 3: Script Otomatis
```r
# Instalasi otomatis dan menjalankan dashboard
source("run_dashboard.R")
```

## 📋 Persyaratan Sistem

### Data yang Diperlukan:
- ✅ `sovi_data.csv` - Dataset SOVI utama
- ✅ `sovi_administrasi_kabupaten.shp` - File peta (opsional untuk versi minimal)

### Software:
- R versi 4.0 atau lebih baru
- RStudio (opsional tapi direkomendasikan)

## 🎯 Fitur Dashboard

### 🏠 **Beranda**
- Informasi umum dashboard
- Status data
- Panduan navigasi

### 📊 **Manajemen Data**
- Transformasi variabel kontinyu ke kategorik
- Metode kategorisasi:
  - **Equal Intervals**: Membagi data menjadi interval yang sama
  - **Quantiles**: Pembagian berdasarkan kuartil/persentil
  - **Standard Deviation**: Kategorisasi berdasarkan standar deviasi

**Cara Penggunaan:**
1. Pilih variabel yang ingin ditransformasi
2. Tentukan jumlah kategori (2-10)
3. Pilih metode kategorisasi
4. Klik "Transformasi"
5. Lihat hasil dan interpretasi

### 🔍 **Eksplorasi Data**
- **Statistik Deskriptif**: Summary statistik untuk variabel numerik
- **Analisis Korelasi**: Hubungan antar variabel
- **Analisis Distribusi**: Histogram dan karakteristik distribusi

**Cara Penggunaan:**
1. Pilih jenis analisis
2. Pilih variabel yang ingin dianalisis
3. Klik "Analisis"
4. Interpretasi hasil akan muncul otomatis

### 📈 **Uji Asumsi Data**
- **Uji Normalitas**: Shapiro-Wilk test
- **Uji Homogenitas**: Levene's test (dalam pengembangan)

**Interpretasi Otomatis:**
- p > 0.05: Data berdistribusi normal
- p ≤ 0.05: Data tidak berdistribusi normal

## 🔧 Troubleshooting

### Masalah Umum dan Solusi:

#### 1. Package tidak terinstall
```r
# Install package yang diperlukan
install.packages(c("shiny", "DT"))
```

#### 2. Data tidak ditemukan
- Pastikan file `sovi_data.csv` ada di direktori yang sama dengan script
- Cek nama file (case-sensitive)

#### 3. Dashboard tidak bisa diakses
- Periksa port yang digunakan (biasanya 3838)
- Coba restart R session
- Gunakan versi minimal jika versi lengkap bermasalah

#### 4. Error saat loading shapefile
- File .shp mungkin korup atau tidak lengkap
- Untuk sementara gunakan versi minimal yang tidak memerlukan shapefile

#### 5. Memory error
```r
# Tingkatkan memory limit
memory.limit(size = 4000)  # Windows
# Atau restart R session
```

## 📊 Contoh Penggunaan

### Analisis Deskriptif
1. Buka tab "🔍 Eksplorasi Data"
2. Pilih "Statistik Deskriptif"
3. Pilih variabel: CHILDREN, ELDERLY, POVERTY
4. Klik "Analisis"
5. Lihat summary statistik dan interpretasi

### Transformasi Data
1. Buka tab "📊 Manajemen Data"
2. Pilih variabel: POVERTY
3. Set jumlah kategori: 3
4. Pilih metode: "Quantiles"
5. Klik "Transformasi"
6. Hasil: Rendah, Sedang, Tinggi

### Uji Normalitas
1. Buka tab "📈 Uji Asumsi"
2. Pilih "Uji Normalitas"
3. Pilih variabel: POPULATION
4. Klik "Jalankan Uji"
5. Interpretasi otomatis akan muncul

## 📈 Tips Penggunaan

### Untuk Analisis yang Efektif:
1. **Mulai dengan Eksplorasi**: Pahami data terlebih dahulu
2. **Transformasi jika Perlu**: Kategorikan variabel untuk analisis tertentu
3. **Cek Asumsi**: Pastikan data memenuhi asumsi statistik
4. **Interpretasi**: Selalu baca interpretasi yang disediakan

### Rekomendasi Variabel untuk Analisis:
- **Demografi**: CHILDREN, FEMALE, ELDERLY
- **Sosial Ekonomi**: POVERTY, ILLITERATE, LOWEDU
- **Infrastruktur**: NOELECTRIC, NOSEWER, TAPWATER
- **Kerentanan**: DPRONE, RENTED

## 🔄 Update dan Pengembangan

Dashboard ini dalam pengembangan berkelanjutan. Fitur yang akan ditambahkan:
- ANOVA satu dan dua arah
- Regresi linear berganda
- Visualisasi peta interaktif
- Export hasil analisis
- Uji statistik lanjutan

## 📞 Bantuan

Jika mengalami masalah:
1. Periksa console R untuk error message
2. Pastikan semua file data ada
3. Coba restart R session
4. Gunakan versi minimal jika versi lengkap bermasalah

---

**SIVANA Dashboard** - Analisis Kerentanan Sosial yang Mudah dan Komprehensif