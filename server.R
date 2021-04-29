##punya w

function(input, output) {
    
    #tab 1 : Introduction 
    
    output$worklifebalance_img <- renderImage({
        list(src = "work-life-balance.jpg",
             width = "100%")
    }, deleteFile = F)
    
    ##gender respondents
    output$gender <- renderHighchart({
        wlb_clean %>% 
            group_by(Gender) %>% 
            tally() %>% 
            arrange(-n) %>% 
            hchart("column",
                   hcaes(Gender, n)) %>% 
            
            hc_tooltip(crossHairs = TRUE,
                       borderWidth = 3.5,
                       table = TRUE,
                       headerFormat = "<b>Respondent</b>",
                       pointFormat = paste('<br>{point.Gender} : {point.n}')) %>% 
            
            hc_title(text = "Total Respondent Berdasarkan Gender",
                     style = list(fontWeight = "bold"),
                     align = "center") %>% 
            
            hc_yAxis(title = list(text = "")) %>% 
            
            hc_colors("#a29ecd") %>% 
            
            hc_credits(enabled = TRUE,
                       text = "Data Source: Survey during Feb ~ Mar 2021") %>% 
            
            hc_add_theme(hc_theme_ffx())
    })
    
    ##respondent's age
    output$age <- renderHighchart({
        wlb_clean %>% 
            mutate(Usia = as.factor(Usia)) %>% 
            group_by(Gender, Usia) %>% 
            tally() %>%
            rename(total = n) %>%
            ungroup() %>% 
            group_by(Usia) %>% 
            mutate(total_resp = sum(total),
                   percentage = (total/total_resp)*100) %>% 
            arrange(Usia) %>% 
            hchart("column",
                   hcaes(Usia, total,
                         group = Gender),
                   stacking = "percent") %>% 
            
            hc_tooltip(crossHairs = TRUE,
                       borderWidth = 3.5,
                       headerFormat = 'Usia per Gender',
                       pointFormat = paste('<br>Usia : <b>{point.Usia} Tahun</b><br>Gender : <b>{point.Gender}</b><br>Persentase : <b>{point.percentage:.0f}%</b>')
            ) %>% 
            
            hc_title(text = "Usia Respondent Berdasarkan Gender",
                     style = list(fontWeight = "bold"),
                     align = "center") %>% 
            
            hc_subtitle(text = "dalam Persentase",
                        align = "center") %>% 
            
            hc_yAxis(title = list(text = ""),
                     plotLines = list(
                         list(value = 50,
                              color = "red",
                              width = 3,
                              zIndex = 4,
                              dashStyle = "shortdash")
                     )
            ) %>%
            
            hc_colors(c("#0e469a",
                        "#6db6d9")) %>% 
            
            hc_legend(enabled = TRUE) %>% 
            
            hc_credits(enabled = TRUE,
                       text = "Data Source: Survey during Feb ~ Mar 2021") %>% 
            
            hc_add_theme(hc_theme_ffx())
    })
    
    ##respondent's occupation
    output$occup <- renderHighchart({
        wlb_clean %>% 
            dplyr::group_by(Pekerjaan) %>% 
            dplyr::count(Pekerjaan) %>% 
            dplyr::rename(total = n) %>% 
            dplyr::arrange(-total) %>% 
            
            hchart("column",
                   hcaes(Pekerjaan, total)) %>% 
            
            hc_title(text = "Pekerjaan Respondent",
                     style = list(fontWeight = "bold"),
                     align = "center") %>% 
            
            hc_yAxis(title = list(text = "Total Respondent")) %>% 
            
            hc_xAxis(title = list(text = "Pekerjaan Respondent")) %>% 
            
            hc_credits(enabled = TRUE,
                       text = "Data Source: Survey during Feb ~ Mar 2021") %>% 
            
            hc_tooltip(crossHairs = TRUE,
                       borderwidth = 3.5,
                       pointFormat = paste("<br>Total Respondent : <b>{point.total}")) %>% 
            
            hc_colorAxis(minColor = "#fc987d",
                         maxColor = "#c16548") %>% 
            
            hc_add_theme(hc_theme_ffx())
    })
    
    ##field of occupation's respondent
    output$bid_occup <- renderHighchart({
        wlb_clean %>% 
            group_by(Gender, Field) %>% 
            tally() %>%
            rename(total = n) %>% 
            ungroup() %>% 
            group_by(Field) %>% 
            mutate(total_resp = sum(total),
                   percentage = (total/total_resp)*100) %>% 
            arrange(-total) %>% 
            hchart("column",
                   hcaes(Field, total,
                         group = Gender),
                   stacking = "percent") %>% 
            
            hc_tooltip(crossHairs = TRUE,
                       borderWidth = 3.5,
                       headerFormat = 'Profesi per Gender',
                       pointFormat = '<br>Profesi : <b>{point.Field}</b><br>Gender : <b>{point.Gender}</b><br>Persentase : <b>{point.percentage:.0f}%</b>') %>% 
            
            hc_title(text = "Total Respondent Berdasarkan Gender dan Profesi",
                     style = list(fontWeight = "bold"),
                     align = "center") %>% 
            
            hc_subtitle(text = "dalam Persentase",
                        align = "center") %>% 
            
            hc_yAxis(title = list(text = ""),
                     plotLines = list(
                         list(value = 50,
                              color = "red",
                              width = 3,
                              zIndex = 4,
                              dashStyle = "shortdash")
                     )
            ) %>% 
            
            hc_xAxis(title = list(text = "Profesi")) %>% 
            
            hc_colors(c("#0e469a",
                        "#6db6d9")) %>% 
            
            hc_legend(enabled = TRUE) %>% 
            
            hc_credits(enabled = TRUE,
                       text = "Data Source: Survey during Feb ~ Mar 2021") %>% 
            
            hc_add_theme(hc_theme_ffx())
    })
    
    ##respondent's location
    output$location <- renderHighchart({
        wlb_clean %>% 
            dplyr::group_by(Domisili) %>% 
            dplyr::mutate(Domisili = if_else(Domisili == "Depok", "Jawa Barat", 
                                             if_else(Domisili == "Kota bekasi", "Jawa Barat",
                                                     if_else(Domisili == "Tangerang Selatan",
                                                             "Banten", if_else(Domisili == "Banten",
                                                                               "Banten", if_else(Domisili == "DKI Jakarta", 
                                                                                                 "DKI Jakarta", if_else(Domisili == "Jawa Barat", "Jawa Barat",
                                                                                                                        if_else(Domisili == "Jawa Tengah", "Jawa Tengah",
                                                                                                                                "Lainnya")
                                                                                                 )
                                                                               )
                                                             )
                                                     )
                                             )
            )
            ) %>% 
            dplyr::count(Domisili) %>% 
            dplyr::rename(total = n) %>% 
            dplyr::arrange(-total) %>% 
            #viz
            hchart("column",
                   hcaes(Domisili, total)) %>% 
            
            hc_title(text = "Domisili Respondent",
                     style = list(fontWeight = "bold"),
                     align = "center") %>% 
            
            hc_credits(enabled = TRUE,
                       text = "Data Source: Survey during Feb ~ Mar 2021") %>% 
            
            hc_yAxis(title = list(text = "Total Respondent")) %>% 
            
            hc_xAxis(title = list(text = "Domisili Respondent")) %>% 
            
            hc_tooltip(crossHairs = TRUE,
                       borderWidth = 3.5,
                       pointFormat = paste("<br>Total Respondent : <b>{point.total}</b>")) %>% 
            
            hc_colorAxis(minColor = "#e86662",
                         maxColor = "#0e469a") %>% 
            
            hc_add_theme(hc_theme_ffx())
    })
    
    
    # tab 2 : Clusters of respondent
    
    
    ##total respondent per cluster
    output$total_resp <- renderHighchart({
        wlb3 %>% 
            dplyr::group_by(Cluster) %>% 
            dplyr::count(Cluster) %>%  
            hchart("column",
                   hcaes(Cluster, n)
            ) %>% 
            
            hc_tooltip(crossHairs = TRUE,
                       borderWidth = 3.5,
                       table = TRUE,
                       pointFormat = paste('<br> Total respondent : <b>{point.n}</b>')
            ) %>% 
            
            hc_title(text = "Total Respondent per Cluster",
                     style = list(fontWeight = "bold"),
                     align = "center") %>% 
            
            hc_yAxis(title = list(text = "Total Respondent")
            ) %>% 
            
            hc_credits(enabled = TRUE,
                       text = "Data Source: Survey during Feb ~ Mar 2021") %>% 
            
            hc_add_theme(hc_theme_ffx()) %>% 
            
            hc_colorAxis(minColor = "#fec3c3",
                         maxColor = "#d12851")
    })
    
    ##kind of respondent's employement 
    output$employ <- renderHighchart({
        wlb3 %>% 
            dplyr::group_by(Cluster) %>% 
            dplyr::count(Pegawai) %>% 
            dplyr::rename(total = n) %>%
            mutate(total_resp = sum(total),
                   percentage = (total/total_resp)*100
            ) %>% 
            #viz
            hchart("column",
                   hcaes(Cluster, total,
                         group = Pegawai),
                   stacking = "percent"
            ) %>% 
            
            hc_title(text = "Total Jenis Kepegawaian Respondent",
                     style = list(fontWeight = "bold"),
                     align = "center") %>% 
            
            hc_subtitle(text = "Berdasarkan Cluster dalam Persentase",
                        align = "center") %>% 
            
            hc_credits(enabled = TRUE,
                       text = "Data Source: Survey during Feb ~ Mar 2021") %>% 
            
            hc_yAxis(title = list(text = ""),
                     plotLines = list(
                         list(value = 50,
                              color = "red",
                              width = 3,
                              zIndex = 4,
                              dashStyle = "shortdash")
                     )
            ) %>% 
            
            hc_tooltip(crossHairs = TRUE,
                       borderWidth = 3.5,
                       headerFormat = '<b>Jenis Kepegawaian Respondent dan Persentasenya</b>',
                       pointFormat = paste('<br>Jenis kepegawaian : <b>{point.Pegawai}</b>
                                 <br>Persentase : <b>{point.percentage:.0f}%</b>')
            ) %>% 
            
            hc_colors(c("#eacad0",
                        "#cd89ab",
                        "#c8a2cb")
            ) %>% 
            
            hc_add_theme(hc_theme_ffx())
    })
    
    ##work from home or office?
    output$wfh <- renderHighchart({
        #EDA
        polar2 <- wlb3 %>% 
            dplyr::group_by(Cluster) %>% 
            dplyr::count(Sistem_kerja) %>% 
            dplyr::rename(total = n) %>%
            dplyr::mutate(total_res = sum(total))
        
        polar2_1 <- polar2 %>% 
            filter(Cluster == 1) %>% 
            mutate(percent = round((total/total_res)*100))
        
        #cluster 2
        polar2_2 <- polar2 %>% 
            filter(Cluster == 2) %>% 
            mutate(percent = round((total/total_res)*100))
        
        #cluster 3
        polar2_3 <- polar2 %>% 
            filter(Cluster == 3) %>% 
            mutate(percent = round((total/total_res)*100))
        
        #viz
        highchart() %>% 
            
            hc_chart(polar = TRUE) %>% 
            
            hc_title(text = "Sistem Kerja Selama Pandemi",
                     style = list(fontWeight = "bold"),
                     align = "center"
            ) %>% 
            
            hc_subtitle(text = "Berdasarkan Cluster dalam Persentase",
                        align = "center") %>%
            
            hc_xAxis(categories = polar2$Sistem_kerja,
                     style = list(fontWeight = "bold")
            ) %>% 
            
            hc_credits(enabled = TRUE,
                       text = "Data Source: Survey during Feb ~ Mar 2021"
            ) %>% 
            
            hc_series(
                list(name = "Total Respondent Cluster 1 (%)",
                     data = polar2_1$percent,
                     type = "line",
                     color = "steelblue"), 
                list(name = "Total Respondent Cluster 2 (%)",
                     data = polar2_2$percent,
                     type = "line",
                     color = "#a00a4d"),
                list(name = "Total Respondent Cluster 3 (%)",
                     data = polar2_3$percent,
                     type = "line",
                     color = "#fa8742")
            ) %>% 
            
            hc_tooltip(crossHairs = TRUE,
                       borderWidth = 3.5) %>% 
            
            hc_add_theme(hc_theme_ffx())
    })
    
    ##work-hour during pandemic
    output$work_hour <- renderHighchart({
        wlb3 %>% 
            dplyr::group_by(Cluster) %>% 
            dplyr::count(Jam_kerja1) %>% 
            dplyr::rename(total = n) %>% 
            dplyr::mutate(Jam_kerja1 = if_else(Jam_kerja1 == 1, 
                                               "Kurang dari 8h",
                                               if_else(Jam_kerja1 == 2, "8h", "Lebih dari 8h")
            ),
            total_resp = sum(total),
            percentage = (total/total_resp)*100
            ) %>% 
            #visualization 
            hchart("column",
                   hcaes(Cluster, total,
                         group = Jam_kerja1),
                   stacking = "percent"
            ) %>% 
            
            hc_title(text = "Jam Kerja Respondent Selama Pandemi",
                     style = list(fontWeight = "bold"),
                     align = "center") %>% 
            
            hc_subtitle(text = "Berdasarkan Cluster dalam Persentase",
                        align = "center") %>% 
            
            hc_credits(enabled = TRUE,
                       text = "Data Source: Survey during Feb ~ Mar 2021"
            ) %>% 
            
            hc_yAxis(title = list(text = ""),
                     plotLines = list(
                         list(value = 50,
                              color = "red",
                              width = 3,
                              zIndex = 4,
                              dashStyle = "shortdash")
                     )
            ) %>% 
            
            hc_tooltip(crossHairs = TRUE,
                       borderWidth = 3.5,
                       headerFormat = '<b>Jam Kerja Respondent</b>',
                       pointFormat = paste('<br><b>{point.Jam_kerja1}</b>
                                 <br>Persentase : <b>{point.percentage:.0f}%</b>')
            ) %>% 
            
            hc_colors(c("#bcac9a",
                        "#996969")
            ) %>% 
            
            hc_add_theme(hc_theme_ffx())
    })
    
    ##overtime
    output$overtime <- renderHighchart({
        #EDA
        polar3 <- wlb3 %>% 
            dplyr::group_by(Cluster) %>% 
            dplyr::count(Lembur1) %>% 
            dplyr::rename(total = n) %>% 
            dplyr::mutate(total_resp = sum(total))
        
        polar3_1 <- polar3 %>% 
            filter(Cluster == 1) %>% 
            mutate(percentage = round((total/total_resp)*100))
        
        #EDA Cluster 2
        polar3_2 <- polar3 %>% 
            filter(Cluster == 2) %>% 
            mutate(percentage = round((total/total_resp)*100))
        
        #EDA Cluster 3
        polar3_3 <- polar3 %>% 
            filter(Cluster == 3) %>% 
            mutate(percentage = round((total/total_resp)*100))
        
        #viz
        highchart() %>% 
            
            hc_chart(polar = TRUE) %>% 
            
            hc_title(text = "Apakah Para Respondent Mengajukan Lemburan?",
                     style = list(fontWeight = "bold"),
                     align = "center") %>% 
            
            hc_subtitle(text = "Berdasarkan Cluster dalam Persentase",
                        align = "center") %>%
            
            hc_xAxis(categories = polar3$Lembur1,
                     style = list(fontWeight = "bold")
            ) %>% 
            
            hc_credits(enabled = TRUE,
                       text = "Data Source: Survey during Feb ~ Mar 2021"
            ) %>% 
            
            hc_series(
                list(name = "Total Respondent Cluster 1 (%)",
                     data = polar3_1$percentage,
                     type = "line",
                     color = "#0205ab"),
                list(name = "Total Respondent Cluster 2 (%)",
                     data = polar3_2$percentage,
                     type = "line",
                     color = "#ff9300"),
                list(name = "Total Respondent Cluster 3 (%)",
                     data = polar3_3$percentage,
                     type = "line",
                     color = "#ffa7b4")
            ) %>% 
            
            hc_tooltip(crossHairs = TRUE,
                       borderWidth = 3.5) %>% 
            
            hc_add_theme(hc_theme_ffx())
    })
    
    ##interest of resignation 
    output$resign <- renderHighchart({
        #EDA
        coba1 <- wlb3 %>% 
            dplyr::group_by(Cluster) %>% 
            dplyr::count(Resign) %>% 
            dplyr::rename(total = n) %>% 
            mutate(Resign = if_else(Resign == 1, "Tidak",
                                    if_else(Resign == 2, "Ya", "Mungkin")
            ),
            total_res = sum(total)
            )
        
        #EDA: Cluster 1
        coba1_1 <- coba1 %>% 
            filter(Cluster == 1) %>% 
            mutate(percentage = round((total/total_res)*100))
        
        #EDA: Cluster 2
        coba1_2 <- coba1 %>% 
            filter(Cluster == 2) %>% 
            mutate(percentage = round((total/total_res)*100))
        
        #EDA: Cluster 3
        coba1_3 <- coba1 %>% 
            filter(Cluster == 3) %>% 
            mutate(percentage = round((total/total_res)*100))
        
        #viz
        highchart() %>% 
            hc_chart(polar = TRUE) %>%
            
            hc_title(text = "Minat Resign Respondent Selama Pandemi",
                     style = list(fontWeight = "bold"),
                     align = "center"
            ) %>% 
            
            hc_subtitle(text = "Berdasarkan Cluster dalam Persentase",
                        align = "center") %>%
            
            hc_xAxis(categories = coba1$Resign,
                     style = list(fontWeight = "bold")
            ) %>% 
            
            hc_credits(enabled = TRUE,
                       text = "Data Source: Survey during Feb ~ Mar 2021") %>% 
            
            hc_add_theme(hc_theme_ffx()) %>% 
            
            hc_series(
                list(name = "Total Respondent Cluster 1 (%)",
                     data = coba1_1$percentage,
                     type = "line",
                     color = "#f19df8"),
                list(name = "<br>Total Respondent Cluster 2 (%)",
                     data = coba1_2$percentage,
                     type = "line",
                     color = "#58836d"),
                list(name = "<br>Total Respondent Cluster 3 (%)",
                     data = coba1_3$percentage,
                     type = "line",
                     color = "#b3937f")
            ) %>% 
            
            hc_legend(enabled = TRUE) %>% 
            
            hc_tooltip(
                crosshairs = TRUE,
                borderWidth = 3.5,
                headerFormat = "<b>Minat Resign</b><br>"
            )
    })
    
    
    #tab 3 : new regulation during pandemic
    
    ##total new regulation
    output$regul <- renderHighchart({
        regulasi %>% 
            ungroup() %>% 
            group_by(aturan) %>% 
            mutate(total_reg = sum(total)) %>% 
            filter(Cluster == 2) %>% 
            arrange(-total_reg) %>% 
            #viz
            hchart("column",
                   hcaes(aturan, total_reg)
            ) %>%
            
            hc_title(text = "Usulan Regulasi/Aturan Baru Selama Pandemi",
                     style = list(fontWeight = "bold"),
                     align = "center") %>%
            
            hc_credits(enabled = TRUE,
                       text = "Data Source: Survey during Feb ~ Mar 2021"
            ) %>% 
            
            hc_xAxis(title = list(text = "Usulan Regulasi/Aturan Baru")) %>% 
            
            hc_yAxis(title = list(text = "Total Respondent")) %>% 
            
            hc_tooltip(crossHairs = TRUE,
                       borderWidth = 3.5,
                       pointFormat = paste('<br>Total Respondent : <b>{point.total_reg}</b>')
            ) %>% 
            
            hc_colorAxis(minColor = "#cbdcbd",
                         maxColor = "#072a42") %>% 
            
            hc_add_theme(hc_theme_ffx())
    })
    
    ##new regulation per cluster
    output$reg_per_clus <- renderHighchart({
        regulasi %>% 
            ungroup() %>% 
            group_by(aturan) %>% 
            mutate(total_reg = sum(total),
                   percentage = (total/total_reg)*100) %>% 
            arrange(-total_reg) %>% 
            #visualization 
            hchart("column",
                   hcaes(aturan, total,
                         group = Cluster),
                   stacking = "percent"
            ) %>% 
            
            hc_title(text = "Usulan Regulasi/Aturan Baru Selama Pandemi",
                     style = list(fontWeight = "bold"),
                     align = "center") %>% 
            
            hc_subtitle(text = "Berdasarkan per Cluster dalam Persentase",
                        align = "center") %>% 
            
            hc_credits(enabled = TRUE,
                       text = "Data Source: Survey during Feb ~ Mar 2021"
            ) %>% 
            
            hc_yAxis(title = list(text = ""),
                     plotLines = list(
                         list(value = 50,
                              color = "red",
                              width = 3,
                              zIndex = 4,
                              dashStyle = "shortdash")
                     )
            ) %>% 
            
            hc_xAxis(title = list(text = "Usulan Regulasi/Aturan Baru")) %>% 
            
            hc_tooltip(crossHairs = TRUE,
                       borderWidth = 3.5,
                       headerFormat = '<b>Usulan Regulasi/Aturan Baru Selama Pandemi</b>',
                       pointFormat = paste('<br><b>{point.aturan}</b>
                                 <br>Cluster : <b>{point.Cluster}</b>
                                 <br>Persentase : <b>{point.percentage:.0f}%</b>')
            ) %>% 
            
            hc_colors(c("#cab8b0",
                        "#a96467",
                        "#b3937f")
            ) %>% 
            
            hc_add_theme(hc_theme_ffx())
    })
    
    ##UU Right-to-Disconnect
    output$UUD <- renderHighchart({
        wlb4 %>% 
            group_by(Cluster) %>% 
            count(Right_to_disconnect) %>%
            rename(total = n) %>% 
            mutate(total_resp = sum(total),
                   percentage = (total/total_resp)*100) %>% 
            #viz
            hchart("column",
                   hcaes(Cluster, total,
                         group = Right_to_disconnect),
                   stacking = "percent"
            ) %>% 
            
            hc_title(text = "Perlukah UU Right To Disconnect Diadakan/Diterapkan?",
                     style = list(fontWeight = "bold"),
                     align = "center") %>% 
            
            hc_subtitle(text = "Berdasarkan per Cluster dalam Persentase",
                        align = "center") %>% 
            
            hc_credits(enabled = TRUE,
                       text = "Data Source: Survey during Feb ~ Mar 2021"
            ) %>% 
            
            hc_yAxis(title = list(text = ""),
                     plotLines = list(
                         list(value = 50,
                              color = "red",
                              width = 3,
                              zIndex = 4,
                              dashStyle = "shortdash")
                     )
            ) %>%
            
            hc_tooltip(crossHairs = TRUE,
                       borderWidth = 3.5,
                       headerFormat = "<b>Respon Para Respondent</b>",
                       pointFormat = paste("<br>Respon : <b>{point.Right_to_disconnect}</b>
                                 <br>Persentase : <b>{point.percentage:.0f}%</b>")
            ) %>% 
            
            hc_colors(c("#f7b5b5",
                        "#f77777",
                        "#bf4343")) %>% 
            
            hc_add_theme(hc_theme_ffx())
    })
    
}
