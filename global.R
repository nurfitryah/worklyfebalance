library(tidyverse) #all u can use package
library(highcharter) #data visualization package
library(splitstackshape) #4 split DF
library(cluster) #clustering package for categorical data type
library(fmsb) #radar chart
library(janitor) #count categories package
library(shiny)
library(shinydashboard)
library(shinythemes)
library(dashboardthemes)

options(shiny.maxRequestSize = 200*1024^2)

#read dataset (general data)
wlb <- read.csv("Working Hours During Pandemic.csv")


#remove `Timestamp` column
wlb <- wlb[-1]


# rename columns (clean general data)
wlb_clean <- wlb %>% 
  rename(Usia = 'Usia..Contoh..17.',
         Field = 'Bidang.pekerjaan.yang.anda.geluti',
         Domisili = 'Domisili..Tempat.tinggal.',
         Pegawai = 'Anda.termasuk.karyawan',
         Kontrak = 'Masa.keterikatan.kontrak.anda.dengan.perusahaan.tempat.anda.bekerja.adalah',
         Sistem_kerja = 'Sistem.kerja.perusahaan.anda.saat.ini.',
         Absensi = 'Bagaimanakah.sistem.absen.di.tempat.anda.bekerja.selama.pandemi.COVID.19.',
         Jam_kerja1 = 'Bila.perusahaan.anda.mengutamakan.WFH..berapa.lamakah.anda.benar.benar.bekerja...Mohon.jawab.dengan.jujur.',
         Lembur1 = 'Bila.anda.bekerja.lebih.dari.8.jam.sehari.ketika.WFH..apakah.anda.mengirim.atau.mengisi.formulir.lembur.',
         Hak_lembur = 'Bila.anda.lembur...bekerja.lebih.dari.8.jam.sehari..apakah.anda.masih.mendapat.hak...co..uang.lembur.',
         Istirahat_wfh = 'Selama.masa.pandemi.dan.anda.WFH..apakah.anda.merasa.waktu.untuk.istirahat.menjadi.lebih.berkurang.',
         Istirahat_wfo = 'Selama.masa.pandemi.dan.anda.WFO..apakah.anda.merasa.waktu.untuk.istirahat.menjadi.lebih.berkurang.',
         Tertular_covid_wfo = 'Selama.masa.pandemi.dan.anda.WFO..apakah.anda.takut.tertular.COVID.19.di.kantor.maupun.saat.perjalanan.pergi...pulang.',
         Tanggap_covid = 'Seberapa.baikkah.perusahaan.anda.bekerja.dalam.menanggapi.pandemi.COVID.19.',
         Fasilitas_covid_wfo = 'Apakah.anda.mendapat.fasilitas.tertentu.saat.anda.WFO.dari.tempat.anda.bekerja..co..multivitamin..masker.kain...kesehatan..dll..',
         Fasilitas_covid_wfh = 'Apakah.anda.mendapat.fasilitas.tertentu.saat.anda.walaupun.anda.WFH.dari.tempat.anda.bekerja..co..multivitamin..masker.kain...kesehatan..dll..',
         Work_life_balance = 'Seberapa.baikkah.Work.Life.Balance.anda.selama.masa.pandemi.',
         Right_to_disconnect = 'Apakah.anda.setuju.bila.diadakannya.undang.undang.yang.mengatur.tentang.jam.kerja.selama.WFH..co..Right.to.Disconnect.',
         Aturan_baru = 'Pilih.beberapa.penyataan.berikut.yang.menurut.anda.baik.untuk.diterapkan.selama.pandemi.COVID.19',
         Resign = 'Apakah.ada.perasaan.anda.ingin.resign.dari.pekerjaan.anda.selama.masa.pandemi.',
         Alasan = 'Alasan.anda.ingin.resign.dari.pekerjaan.anda.selama.masa.pandemi..pilih.yang.paling.mendekati.',
         Khawatir = 'Apakah.anda.khawatir.bila.anda.resign..anda.tidak.mendapatkan.tempat.yang.lebih.baik.dari.tempat.anda.bekerja.sekarang.') %>% 
  mutate_if(is.character, as.factor)


#data cluster of respondent
wlb2 <- wlb_clean %>% 
  mutate(Jam_kerja1 = if_else(Jam_kerja1=="Kurang dari 8 jam sehari", 1,
                              if_else(Jam_kerja1=="Rata-rata 8 jam sehari" & Jam_kerja1=="Saya masih WFO", 2,3)),
         Resign = if_else(Resign =="Tidak", 1, 
                          if_else(Resign == "Ya", 2,3)),
         Pegawai = if_else(Pegawai=="Pegawai Kontrak", "Pegawai Kontrak",
                           if_else(Pegawai=="Pegawai Tetap", "Pegawai Tetap",
                                   "Lainnya"))
  ) %>% 
  select(c(Pegawai,
           Sistem_kerja,
           Jam_kerja1,
           Lembur1,
           Tanggap_covid,
           Work_life_balance,
           Resign)
  ) %>% 
  mutate(Pegawai = as.factor(Pegawai),
         Jam_kerja1 = as.factor(Jam_kerja1),
         Tanggap_covid = as.factor(Tanggap_covid),
         Work_life_balance = as.factor(Work_life_balance),
         Resign = as.factor(Resign)
  )


#ML: Clustering on categorical data using `daisy()` function
gowerdist <- daisy(wlb2[,-c(7)],metric = "gower")


#Cluster after calculating optimum K
pam_fin <- pam(gowerdist, diss = TRUE, k = 3)


#DF `wlb2` after joining with the cluster
wlb3 <- wlb2 %>% 
  mutate(Cluster = pam_fin$clustering) %>% 
  group_by(Cluster)


#Joining `Cluster` from `pam_fin` into `wlb_clean` DF 
wlb4 <- wlb_clean %>% 
  mutate(Cluster = pam_fin$clustering)


#Additional of new regulations during pandemic
#EDA
regulasi <- wlb4 %>% 
  select(c(Aturan_baru,
           Cluster)) %>% 
  cSplit("Aturan_baru", ";") %>% 
  group_by(Cluster) %>% 
  pivot_longer(cols = c("Aturan_baru_1",
                        "Aturan_baru_2",
                        "Aturan_baru_3",
                        "Aturan_baru_4"),
               names_to = "regulasi"
  ) %>% 
  ungroup() %>% 
  rename(aturan = value) %>% 
  group_by(Cluster, 
           aturan) %>% 
  count(aturan) %>% 
  drop_na() %>% 
  rename(total = n)










