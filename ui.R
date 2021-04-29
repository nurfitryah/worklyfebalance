##punya w



dashboardPage(title = "Working Hours",
              
              dashboardHeader(title = "Working Hours"),
              
              dashboardSidebar(
                  sidebarMenu(
                      #tab 1
                      menuItem(
                          text = "Introduction",
                          tabName = "intro",
                          icon = icon("lightbulb")
                      ),
                      
                      #tab 2
                      menuItem(
                          text = "Cluster",
                          tabName = "cluster",
                          icon = icon("users")
                      ),
                      
                      #tab 3
                      menuItem(
                          text = "Solutions",
                          tabName = "solt",
                          icon = icon("pencil-ruler")
                      ),
                      
                      #tab 4
                      menuItem(
                          text = "Information",
                          tabName = "info",
                          icon = icon("question-circle")
                      )
                  )
              ),
              
              dashboardBody(
                
                #change theme
                shinyDashboardThemes(
                  theme = "grey_dark"
                ),
                  
                  
                  #tab 1 "Introduction"
                  tabItems(
                      
                      tabItem(
                          tabName = "intro",
                          #align = "justify",
                          
                          fluidPage(
                              
                              h1(strong("Introduction")
                              ),
                              
                              br(),
                              
                              imageOutput("worklifebalance_img", width = "100%", height = "100%"),
                              
                              br()
                          ),
                          
                          fluidPage(
                            div(style = "text-align:justify",
                              
                              p(h4(em("Work From Home"), "(",strong("WFH"),") marak diterapkan oleh perusahaan dan perkantoran sejak pandemi", strong("COVID-19"), "merebak di Indonesia 
                              demi menekan angka penularan", strong("COVID-19"),", tetapi banyak juga perkantoran yang masih menerapkan", em("Work From Office"), "(",strong("WFO"),
                                   ") dikarenakan alasan tertentu walaupun jumlah karyawan yang hadir dibatasi.
                              Tetapi setelah beberapa perusahaan telah menerapkan sistem WFH, banyak yang merasa jam kerja para karyawan atau pekerja lainnya
                              menjadi lebih panjang dibanding sebelum pandemi. Tidak sedikit karyawan atau pekerja lainnya merasa", em("Work-life-balance"), "mereka tidak seimbang.
                              Bahkan, fenomena ini terjadi di seluruh dunia dan permasalahan ini sudah diatur dan dibatasi dibeberapa negara di wilayah Eropa
                              dengan diadakannya Undang-Undang", strong("Right-to-Disconnect"), ".")
                              ),
                              
                              br(),
                              
                              p(h4("Analisa ini dilakukan survey melalui", strong("Google Form"), tags$a(href = "https://forms.gle/iHKfE3zFYE3Ba5xr5", "(click here)"), "dengan jumlah respondent 70 peserta dari berbagai kalangan dan 
                            berasal dari berbagai daerah.")
                              ),
                              
                              br()
                              )
                            ),
                          
                          fluidPage(
                              tabBox(title = tags$b("Mengenal Para Respondent"),
                                     id = "tabset1",
                                     side = "left",
                                     width = 12,
                                     height = "650px",
                                     tabPanel(
                                         tags$b("Total Respondent per Gender"),
                                         highchartOutput(outputId = "gender", height = "650px")
                                     ),
                                     
                                     tabPanel(
                                         tags$b("Usia Respondent per Gender"),
                                         highchartOutput(outputId = "age", height = "650px")
                                     ),
                                     
                                     tabPanel(
                                         tags$b("Pekerjaan"),
                                         highchartOutput(outputId = "occup", height = "650px")
                                     ),
                                     
                                     tabPanel(
                                         tags$b("Bidang Pekerjaan Respondent"),
                                         highchartOutput(outputId = "bid_occup", height = "650px")
                                     ),
                                     
                                     tabPanel(
                                         tags$b("Domisili"),
                                         highchartOutput(outputId = "location", height = "650px")
                                     )
                              )
                          )
                      ),
                      
                      
                      #tab 2 : "Cluster"
                      tabItem(
                          tabName = "cluster",
                          #align = "justify",
                          
                          fluidPage(
                            div(style = "text-align:justify",
                              h1(strong("The Clusters and The Characteristics")),
                              
                              br(),
                              
                              h2(strong("The Clusters")),
                              
                              br(),
                              
                              p(h4("Setelah di analisa, ternyata respondent terbagi menjadi 3 cluster. Masing-masing
                            cluster tersebut mempunyai beberapa karakteristik, dimana karakteristik tiap cluster
                            dapat digolongkan berdasarkan jenis kepegawaian respondent, sistem kerja selama pandemi 
                            (", strong("WFO/WFH"), "), jam kerja respondent selama pandemi, apakah respondent mengajukan
                            form lembur, dan apakah respondent berminat untuk resign selama pandemi?"
                              )
                              ),
                              
                              br(),
                              
                              highchartOutput(outputId = "total_resp", height = "650px"),
                              
                              br(),
                              
                              h2(strong("The Characteristics")),
                              )
                            ),
                          
                          br(),
                          
                          fluidPage(
                              tabBox(
                                  title = tags$b("Cluster dan Karakteristiknya"),
                                  id = "tabset2",
                                  side = "left",
                                  width = 12,
                                  height = "650px",
                                  tabPanel(tags$b("Kepegawaian"),
                                           highchartOutput(outputId = "employ", height = "650px")
                                  ),
                                  tabPanel(tags$b("WFH"),
                                           highchartOutput(outputId = "wfh", height = "650px")
                                  ),
                                  tabPanel(tags$b("Jam Kerja"),
                                           highchartOutput(outputId = "work_hour", height = "650px")
                                  ),
                                  tabPanel(tags$b("Lemburan"),
                                           highchartOutput(outputId = "overtime", height = "650px")
                                  ),
                                  tabPanel(tags$b("Resign?"),
                                           highchartOutput(outputId = "resign", height = "650px")
                                  )
                              )
                          )
                      ),
                      
                      
                      #tab 3 : "Solutions"
                      tabItem(
                          tabName = "solt",
                          #align = "justify",
                          
                          fluidPage(
                              h1(strong("Solusinya?")),
                              
                              br(),
                              
                              p(h4("Dapat dilakukan pemberlakuan beberapa aturan atau regulasi yang dapat diterapkan
                           selama masa pandemi menurut para respondent:")
                              ),
                              
                              br()
                          ),
                          
                          fluidPage(
                              tabBox(
                                  title = tags$b("Usulan Penambahan Aturan/Regulasi Selama Pandemi"),
                                  id = "tabset3",
                                  side = "left",
                                  width = 12,
                                  height = "650px",
                                  tabPanel(tags$b("Total"),
                                           highchartOutput(outputId = "regul", height = "650px")
                                           ),
                                  tabPanel(tags$b("Per Cluster"),
                                           highchartOutput(outputId = "reg_per_clus", height = "650px")
                                           ),
                                  tabPanel(tags$b("UU Right-to-Disconnect"),
                                           highchartOutput(outputId = "UUD", height = "650px")
                                           )
                              )
                          )
                      ),
                      
                      
                      #tab 4 : "Data & Source"
                      tabItem(
                          tabName = "info",
                          align = "justify",
                          fluidPage(
                              h1(strong("Information")),
                              
                              br(),
                              
                              h4("Google Form :", tags$a(href = "https://forms.gle/iHKfE3zFYE3Ba5xr5", "click here")
                              ),
                              
                              br(),
                              
                              h4("Detail explanation :", tags$a(href = "https://rpubs.com/fitryah_nur/worklyfebalance", "RPubs")
                              ),
                              
                              br(),
                              
                              h4("Code :", tags$a(href = "https://github.com/nurfitryah/worklyfebalance", "GitHub")
                              )
                          )
                      )
                      
                  )
                  
              )
              
)