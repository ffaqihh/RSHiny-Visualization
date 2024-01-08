library(shiny)
library(shinydashboard)

css <- function(){
  tags$head(
    tags$style(HTML("
                      .main-header .logo{
                      font-size:18px;
                      }
                      
                      li {
                        list-style-type: none;
                        margin: 15px 20px;
                        font-size:18px;
                      }
                      
                      .fa{
                      margin-right:3px;
                      width: 22px;
                      }
                      
                      .content-wrapper{
                      background-color:#f5f5f5;
                      }

                      .upload-file{
                      padding-left:20px;
                      padding-right:20px;
                      }
                      
 
                      "))
  )
}



outliers <- function(x) {
  
  Q1 <- quantile(x, probs=.25)
  Q3 <- quantile(x, probs=.75)
  iqr = Q3-Q1
  
  upper_limit = Q3 + (iqr*1.5)
  lower_limit = Q1 - (iqr*1.5)
  
  x > upper_limit | x < lower_limit
}

remove_outliers <- function(df, cols = names(df)) {
  for (col in cols) {
    df <- df[!outliers(df[[col]]),]
  }
  df
}

ui <- dashboardPage(
  skin="purple",
  dashboardHeader(title = "Dashboard MentalHealth"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About Us", tabName = "kami", icon=icon("star"), selected = TRUE),
      menuItem("Pendahuluan", tabName="Data", icon=icon("folder-plus")),
      menuItem("Visualisasi", tabName="Visualisasi", icon=icon("bar-chart-o"),
               menuSubItem("Graph", tabName = "graph", icon=icon("chart-column")),
               menuSubItem("Statistika Deskriptif", tabName = "stats", icon=icon("calculator"))),
      menuItem("Decision Tree", tabName = "decisiontree", icon = icon("tree")),
      menuItem("Interpretasi", tabName="Interpretasi", icon=icon("list-alt"))
      )
    
  ),
  dashboardBody(
    css(),
    tabItems(
      tabItem(
        tabName = "Data",
        navbarPage("PENDAHULUAN",
                   tabPanel("DATA",
                            fluidRow(
                              tags$div(
                                h1("Data"),
                                p("To get data", tags$a(href="https://drive.google.com/file/d/1ZsXC8wFuKolfYTAJSXoR_eAQQPQjlnVc/view?usp=share_link", "click here!")),
                                fileInput("Upload", "choose your CSV file:", accept = ".csv"),
                                uiOutput("files"),
                              ),
                              class = "upload-file",
                            )
                            ),
                   tabPanel("Sumber Data",
                            h3("Sumber data pada penelitian ini berasal dari Data Primer yang diperoleh melalui Survei dengan target sasaran responden yakni Mahasiswa Program Studi Teknologi Sains Data 
                               Universitas Airlangga angkatan 2021. Dari survei tersebut didapatkan responden sebanyak 57 mahasiswa.",style="text-align:justify;")),
                   tabPanel("Latar Belakang",
                            h3("Stress merupakan perubahan reaksi baik secara fisik maupun psikis yang muncul apabila ada perubahan dari lingkungan yang mengharuskan seseorang menyesuaikan diri terhadap ancaman, tekanan ataupun situasi yang baru.
                               Stress merupakan hal yang alami, tetapi apabila dibiarkan dalam waktu yang lama hal tersebut dapat merusak tubuh kita.",br(),br(),
                               "Saat ini marak kasus percobaan bunuh diri yang dilakukan mahasiswa akibat stress karena tugas kuliah, skripsi dan lain lain. Penelitian yang dilakukan oleh Universitas Muhammadiyah Malang menunjukkan persentase yang cukup tinggi terhadap tingkat stress mahasiswanya. 
                               didapati bahwa sebanyak 6,9% mahasiswa S1 dan D3 dari responden mengalami stress tingkat berat, 57,4% stress sedang dan 35,6% responden mengalami stress ringan.", br(),br(),
                               "Pada masa menjelang Ujian Akhir Semester (UAS), tingkat stress mahasiswa biasanya mengalami kenaikan dikarenakan memikirkan hal yang menjadi sumber stress seperti yang dijelaskan sebelumnya. Oleh karena itu, kami melakukan penelitian terkait tingkat stress dan 
                               faktor faktor yang memengaruhi stress pada mahasiswa Teknologi Sains Data"),style="text-align:justify;"),
                   
                   tabPanel("Metodologi",
                            h3("
                               Pada penelitian ini kami menggunakan metode survey secara langsung dalam pengumpulan data. Kemudian pada pengolahan data, kami menggunakan metode statistika deskriptif dengan pendekatan grafik dan metode classification yaitu decision tree. 
                               Terakhir, untuk hasil dari pengolahan data, akan ditampilkan berupa visualisasi. Berikut detail dari metodologi yang diterapkan dalam penelitian kami:",br(),br(),
                               "1.Melakukan survei pada mahasiswa Teknologi Sains Data Universitas Airlangga angkatan 2021 menggunakan google form.",br(),br(),
                               "2.Melakukan input data dari google form ke dalam spreadsheet.",br(),br(),
                               "3.Menggunakan software R studio untuk melakukan  data pre processing, yaitu penanganan outliers, missing values, dan memperbaiki kesalahan input data dari responden.",br(),br(),
                               "4.Membuat Decision Tree untuk mengklasifikasikan data.",br(),br(),
                               "5.Membuat visualisasi menggunakan R studio ,yang kemudian ditampilkan pada R Shiny, agar lebih memudahkan pembaca dalam mengambil highlights, insights, interpretasi serta kesimpulan.

                               "),style="text-align:justify;")
                   )
        
      ),
      
      tabItem(
        tabName = "kami",
        h1(strong("ANALISIS TINGKAT STRESS MAHASISWA TEKNOLOGI SAINS DATA MENJELANG UJIAN AKHIR SEMESTER"), style="text-align:center;"),
        tags$img(src="https://i.postimg.cc/D0047zpT/cobas.png", style="display:block; margin:auto;"),
        h1(strong("Group A"), style = 'text-align :center;'),
        h3(
          "Faqih Syamil (162112133006)",br(),
          "Michael Adi Herryanto (162112133024)", br(),
          "Miftahul Jannah (162112133040)",br(),
          "Vinnie Quartasyeba Tarigan (162112133051)", br(),
          "Muhammad Rowahul Muslim (162112133117)", style = "text-align:center;"
        )
      ),
      
      tabItem(
        tabName = "graph",
        h1(strong("Visualisasi Graph")),

        br(),
        h3("Histogram"),
        fluidRow(
          column(2,radioButtons("withoutlier", "Without Outlier?", choices=c("ya"=TRUE, "tidak"=FALSE), selected = FALSE)),
          column(2,selectInput("pilih_variabel", "variabel", choices = c("movie"="penggunaan_movie_platform", "SKS"="SKS", "tugas"="jumlah_tugas_2minggu"), selected = "SKS")),
          column(2,selectInput("Ubah_Warna1","warna",choices=c("pink"="hot pink", "red"="red", "yellow"="yellow", "green"="green"),selected = "hot pink")),
          column(2,radioButtons("garis","Tampilkan Garis",choices = c("ya"=TRUE, "tidak"=FALSE), selected = FALSE)),
          column(2,radioButtons("count","Tampilkan Angka",choices = c("ya"=TRUE, "tidak"=FALSE), selected = FALSE)),
          column(2,sliderInput(inputId = "binss",
                               label = "number of bins",
                               min=1,
                               max=25,
                               value = 25))),
        uiOutput("hasil"),
        
        
        br(),
        br(),
        h3("Scatter Plot"),
        fluidRow(
          column(2,radioButtons("withoutlier2", "Without Outlier?", choices=c("ya"=TRUE, "tidak"=FALSE), selected = FALSE)),
          column(2,selectInput("pilih_variabel2", "variabel", choices = c("movie"="penggunaan_movie_platform", "SKS"="SKS", "tugas"="jumlah_tugas_2minggu", "tingkat stress"="range_bahagia_stress_angka"), selected = "SKS")),
          column(2,selectInput("pilih_variabel7", "variabel", choices = c("movie"="penggunaan_movie_platform", "SKS"="SKS", "tugas"="jumlah_tugas_2minggu", "tingkat stress"="range_bahagia_stress_angka"), selected = "SKS")),
          column(2,selectInput("Ubah_Warna","warna",choices=c("red"="Reds","blue"="Blues","green"="Greens"),selected = "Greens"))
        ),
        uiOutput("hasil2"),
        
        br(),
        br(),
        h3("Kernell Density Plot"),
        fluidRow(
          column(2,radioButtons("withoutlier3", "Without Outlier?", choices=c("ya"=TRUE, "tidak"=FALSE), selected = FALSE)),
          column(2,selectInput("pilih_variabel3", "variabel", choices = c("movie"="penggunaan_movie_platform", "SKS"="SKS", "tugas"="jumlah_tugas_2minggu"), selected = "SKS")),
          column(2,selectInput("Ubah_Warna5","warna",choices=c("red"="red", "green"="green", "yellow"="yellow"),selected = "red"))
        ),
        uiOutput("hasil3"),
        
        br(),
        br(),
        h3("Box Plot"),
        fluidRow(
          column(2,radioButtons("withoutlier4", "Without Outlier?", choices=c("ya"=TRUE, "tidak"=FALSE), selected = FALSE)),
          column(2,selectInput("pilih_variabel4", "variabel", choices = c("movie"="penggunaan_movie_platform", "SKS"="SKS", "tugas"="jumlah_tugas_2minggu"), selected = "SKS"))
        ),
        uiOutput("hasil4"),
        
        br(),
        br(),
        h3("Bar Chart"),
        fluidRow(
          column(2,selectInput("Ubah_Warna2","warna",choices=c("Royal1"="Royal1", "Royal2"="Royal2", "Rushmore"="Rushmore"),selected = "Royal1")),
          column(2,selectInput("pilih_variabel5", "variabel", choices = c("Movie"="movie_platform","Teman Menonton"="menonton_dengan", "Tingkat Mental"="range_bahagia_stress_kata"), selected ="movie_platform" ))
        ),
        uiOutput("hasil5"),
        
        br(),
        br(),
        h3("Pie Chart"),
        fluidRow(
          column(2,radioButtons("keterangan","Keterangan",choices = c("ya"=TRUE, "tidak"=FALSE), selected = FALSE)),
          column(2,selectInput("pilih_variabel6", "variabel", choices = c("movie"="movie_platform","teman menonton"="menonton_dengan", "Tingkat Mental"="range_bahagia_stress_kata"), selected ="movie_platform" ))
        ),
        uiOutput("hasil6")
      ),
      
      tabItem(
        tabName = "Interpretasi",
        navbarPage("Interretasi dan Kesimpulan",
                   tabPanel(
                     "Interpretasi",
                     br(),
                     h3('Berdasarkan statistika deskriptif, kita dapat melihat bahwa korelasi antara 1 variabel dengan yang lain memiliki nilai rendah,
                          dengan semuanya di bawah 0,3. Dari visualisasi-visualisasi yang ada pun menunjukkan bahwa tingkat stress tidak berhubungan dengan lama menonton,
                        ',em('platform'),'yang digunakan untuk menonton, teman menonton, jumlah SKS yang diambil, dan jumlah tugas selama minggu ke 12 hingga minggu ke 14 semester 3.',style="text-align:justify;"),
                     selectInput("interpretvar","variabel",choices = c("Jenis movie"="movie_platform","teman menonton"="menonton_dengan", "Tingkat Mental"="range_bahagia_stress_kata", "SKS"="SKS", "Durasi Menonton"="penggunaan_movie_platform", "Jumlah Tugas"="jumlah_tugas_2minggu"), selected ="SKS"),
                     p("interpretasi :"),br(),
                     textOutput("interpretasi")
                   ),
                   tabPanel(
                     "Kesimpulan",
                     h3(
                       "Pada bagian akhir ini akan dipaparkan beberapa kesimpulan yang dapat diambil dan saran yang ditemukan dari hasil penelitian ini. Dari hasil analisis pembahasan di atas, secara umum penulis menyimpulkan bahwa tingkat stress mahasiswa Teknologi Sains Data Universitas Airlangga menjelang Ujian Akhir Semester ternyata tidak memiliki korelasi yang kuat terhadap banyaknya SKS, jumlah tugas, durasi menonton film, dan teman menonton. 
                       Akan tetapi, hasil yang didapatkan justru dari banyak variabel yang diduga berkorelasi kuat dengan tingkat stress, ternyata saling berkorelasi lebih baik satu dengan lainnya dibandingkan dengan tingkat stress.",br(),br(),
                       "Adapun saran dari peneliti untuk penelitian selanjutnya dan pembaca antara lain sebagaimana berikut :",br(),br(),
                       "1.Untuk penelitian selanjutnya disarankan dapat menggunakan variabel yang lebih banyak lagi dan dapat berkorelasi dengan range tingkat stress. Selain itu juga menggunakan metode sampel pada seluruh angkatan sehingga dapat merepresentasikan populasi mahasiswa Teknologi Sains Data Universitas Airlangga",br(),br(),
                       "2.Untuk pembaca disarankan lebih cermat dalam memilih dan menggunakan data pada penelitian ini karena masih kurang merepresentasikan populasi mahasiswa Teknologi Sains Data Universitas Airlangga secara keseluruhan."
                     ,style="text-align:justify;")
                   )
                   )
      ),
      tabItem(
        tabName = "stats",
        h1("Statistika Deskriptif"),
        selectInput("vnumerik","Pilihan input : ",
                    c("Durasi movie"="penggunaan_movie_platform", "SKS"="SKS", "tugas"="jumlah_tugas_2minggu", "tingkat stress/bahagia"="range_bahagia_stress_angka")),
        infoBoxOutput("i_min"),
        infoBoxOutput("i_q1"),
        infoBoxOutput("i_median"),
        infoBoxOutput("i_mean"),
        infoBoxOutput("i_q3"),
        infoBoxOutput("i_max"),
        infoBoxOutput("i_IQR"),
        infoBoxOutput("i_range"),
        infoBoxOutput("i_stdev"),
        infoBoxOutput("i_variance"),
        infoBoxOutput("i_corel1"),
        infoBoxOutput("i_corel2"),
        infoBoxOutput("i_corel3"),
        infoBoxOutput("i_corel4"),
        infoBoxOutput("i_cov1"),
        infoBoxOutput("i_cov2"),
        infoBoxOutput("i_cov3"),
        infoBoxOutput("i_cov4")
        
      ),
      tabItem(
        tabName="decisiontree",
        library(rpart),
        library(rpart.plot),
        controller <- rpart.control(minsplit = 4, minbucket = round(4/3), maxdepth = 5),
        tuned_tree1_range_stress_bahagia <- rpart(range_bahagia_stress_angka~ penggunaan_movie_platform + 
                                                    jumlah_tugas_2minggu + SKS + movie_platform_dum_digital_1 +
                                                    menonton_dengan_dum_teman_1 +
                                                    menonton_dengan_dum_sendiri_1 +
                                                    menonton_dengan_dum_pacar_1 +
                                                    menonton_dengan_dum_sahabat_1
                                                  , data = data(), method = 'class', control = controller),
        uiOutput("decisionTREE"),
        h1(strong("Klasifikasi")),
        fluidRow(
          column(2,numericInput("LamaMenonton","Lama Menonton",10, min=1, max=100)),
          column(2,numericInput("JumlahTugas","jumlah Tugas Selama 2 Minggu",10, min=1, max=100)),
          column(2,radioButtons("pacar", "Apakah Menonton Dengan Pacar",choices = c("ya"=TRUE, "tidak"=FALSE),selected = FALSE))
        ),
        p("Tingkat Stress kamu"),
        textOutput("decision")
        )
    ),
  )
)

server <- function(input, output, session){
  data <- reactive({
    req(input$Upload)
    read.csv(file = input$Upload$datapath)
    
  })
  
  output$decision <- renderText({
    if(input$LamaMenonton < 1.5){
      return("stress")}else{
        if(input$JumlahTugas < 3){
          return("stress")}else{
            if(input$pacar){
              if(input$LamaMenonton >= 2.2){
                return("stress")}else{
                  return("tidak stress")}
            }else{
              if(input$LamaMenonton >= 24){
                return("stress")}else{return("tidak stress")}
            }
          }
      }
  })
  
  output$interpretasi <- renderText({
    if(input$interpretvar=="penggunaan_movie_platform"){
      return("pada data terdapat outlier yang tidak terlalu jauh terhadap sekumpulan data. Adapun rata- rata dari durasi menonton movie responden adalah 5-10 jam dalam seminggu atau 7 hari. Lalu, dapat dilihat juga dari garis yang terdapat di histogram tersebut menunjukkan skewness positif atau kanan. 
             Oleh karena itu, dapat diambil kesimpulan bahwa persebaran data dari durasi lama menonton movie banyak berada di sebelah kanan dari mean dan median sehingga mahasiswa TSD Universitas Airlangga menjelang Ujian Akhir Semester banyak yang durasi menonton movienya lebih besar dari rata rata")
    }else if(input$interpretvar=="jumlah_tugas_2minggu"){
      return("Dari gambar histogram variabel Jumlah Tugas bagian kiri, dimana menyatakan banyaknya jumlah tugas mahasiswa Teknologi Sains Data dalam seminggu, dapat dibandingkan dan diinterpretasikan table sebelah kiri dan kanan bahwa terdapat outlier yang tidak terlalu jauh terhadap sekumpulan data. Adapun rata- rata dari Jumlah Tugas mahasiswa adalah 1-4 Tugas dalam seminggu atau 7 hari. Lalu, dapat dilihat juga dari garis yang terdapat di histogram tersebut menunjukkan skewness positif atau kanan. 
             Oleh karena itu, dapat diambil kesimpulan bahwa persebaran data dari Jumlah Tugas banyak berada di sebelah kanan dari mean dan median (12 Tugas) sehingga mahasiswa TSD Universitas Airlangga menjelang Ujian Akhir Semester banyak yang memiliki Tugas untuk dikerjakan. ")
    }else if(input$interpretvar=="SKS"){
      return("Dari Histogram variabel SKS, dimana menyatakan jumlah SKS yang dimiliki oleh setiap mahasiswa di Semester 3 ini, dapat diinterpretasikan bahwa terdapat outlier yang jauh dari kumpulan data. Lalu, dapat dilihat juga pada garis yang terdapat di Histogram menunjukkan skewness negative, dimana persebaran data dari variabel SKS banyak berada di sebelah kiri dari mean dan median. 
             Hal ini wajar dikarenakan maksimal SKS yang dapat diambil oleh mahasiswa adalah sejumlah 24, jika melebihi dari itu, perlu konfirmasi lebih lanjut.")
    }else if(input$interpretvar=="movie_platform"){
      return("Movie platform dari data yang kita peroleh dapat diketahui bahwa lebih banyak mahasiswa Teknologi Sains Data yang menggunakan platform Digital seperti Netflix, Viu, Vidio, Wetv, etc 98,2% lebih banyak daripada platform One Site seperti menonton langsung di bioskop yang hanya 1,8%")
    }else if(input$interpretvar=="menonton_dengan"){
      return("Dari barplot variabel teman menonton pada gambar di samping, dapat dilihat bahwa modus dari jawaban responden adalah menonton sendiri dan jawaban kedua terbanyak setelah menonton sendiri adalah menonton bersama pacar. 
             Dapat diambil kesimpulan bahwa mahasiswa TSD Universitas Airlangga menjelang Ujian Akhir Semester lebih suka menonton film sendiri sebagai bentuk refreshing dari kejenuhan. Hal yang mengejutkan pada barplot juga menunjukkan bahwa ada beberapa responden yang menjawab menonton dengan selingkuhan.")
    }
    else if(input$interpretvar=="range_bahagia_stress_kata"){
      return("Dari bar plot variabel Tingkat Mental, dimana menyatakan tingkat stress mahasiswa dari tingkat sangat bahagia hingga sangat stress dengan skala likert 1 adalah sangat bahagia hingga 7 adalah sangat stress. Dapat disimpulkan bahwa responden dalam hal ini adalah mahasiswa Teknologi Sains data Universitas Airlangga, 
      rata- rata tingkat bahagia mahasiswa adalah 3,702, nilai tengah (median) yaitu 4 (biasa saja), dan modus 5(sedikit stress). Dari modus tersebut menunjukkan bahwa mahasiswa TSD Universitas Airlangga menjelang Ujian Akhir Semester banyak yang merasa sedikit stress")
    }
  })
  
  decisionTREE <- renderPlot({
    rpart.plot(tuned_tree1_range_stress_bahagia, extra = 102)
  })
  
  scatter_plot <- renderPlot({
    library(RColorBrewer)
    data_clean <- remove_outliers(data(), input$pilih_variabel2)
    if(input$withoutlier2){
      plot(data_clean[[input$pilih_variabel2]], data_clean[[input$pilih_variabel7]],xlab = input$pilih_variabel2, ylab = input$pilih_variabel7, col=brewer.pal(n=1, name=input$Ubah_Warna))
    }else{
      plot(data()[[input$pilih_variabel2]], data()[[input$pilih_variabel7]],xlab = input$pilih_variabel2, ylab = input$pilih_variabel7, col=brewer.pal(n=1, name=input$Ubah_Warna))
    }
  })
  
  histogram_avggluc <- renderPlot({
    data_clean <- remove_outliers(data(), input$pilih_variabel)
    if(input$withoutlier){
      avggluc = data_clean[[input$pilih_variabel]]
      bins <-seq(min(avggluc), max(avggluc), length.out=input$binss +1)
      Z= hist(avggluc, main = "HISTOGRAM", col = input$Ubah_Warna1, xlab = "variabel", freq = T, breaks = bins)
      xfit=seq(min(avggluc), max(avggluc))
      yfit=dnorm(xfit, mean = mean(avggluc),sd=sd(avggluc))
      yfit=yfit*diff(Z$mids[1:2])*length(avggluc)
      if(input$garis){
        lines(xfit,yfit)
      }
      if(input$count){
        text(Z$mids,Z$counts,labels=Z$counts, col="black")}
    }else{
      avggluc = data()[[input$pilih_variabel]]
      bins <-seq(min(avggluc), max(avggluc), length.out=input$binss +1)
      Z= hist(avggluc, main = "HISTOGRAM", col = input$Ubah_Warna1, xlab = "variabel", freq = T, breaks = bins)
      xfit=seq(min(avggluc), max(avggluc))
      yfit=dnorm(xfit, mean = mean(avggluc),sd=sd(avggluc))
      yfit=yfit*diff(Z$mids[1:2])*length(avggluc)
      if(input$garis){
        lines(xfit,yfit)
      }
      if(input$count){
        text(Z$mids,Z$counts,labels=Z$counts, col="black")}
    }
  })
  
  kernelDensity <- renderPlot({
    data_clean <- remove_outliers(data(), input$pilih_variabel3)
    if(input$withoutlier3){
      x <- density(data_clean[[input$pilih_variabel3]])
      plot(x, main = "variabel")
      polygon(x, col=input$Ubah_Warna5, border="black")
    }else{
      x <- density(data()[[input$pilih_variabel3]])
      plot(x, main = "variabel")
      polygon(x, col=input$Ubah_Warna5, border="black")
    }
  })
  
  boxxplot <- renderPlot({
    data_clean <- remove_outliers(data(), input$pilih_variabel4)
    if(input$withoutlier4){
      boxplot(data_clean[[input$pilih_variabel4]])
    }else{
      boxplot(data()[[input$pilih_variabel4]])}
  })
  
  barbarchart <- renderPlot({
    library(wesanderson)
    barplot(table(data()[[input$pilih_variabel5]]), col=wes_palette(n=4, name=input$Ubah_Warna2))
  })
  
  piechartt <- renderPlot({
    c <- table(data()[[input$pilih_variabel6]])
    perc <- round(100*c/sum(c),1)
    pie(c, paste0(perc), col=rainbow(length(c)))
    if(input$keterangan){
      if(input$pilih_variabel6 == "movie_platform"){
        legend("topleft",c('Digital (Netflix/Viu/Vidio/Wetv/etc.)', 'On site (Bioskop)'),fill=rainbow(length(c)))
      }else if(input$pilih_variabel6 == 'menonton_dengan'){
        legend("topleft",c('Pacar', 'Sahabat', 'Selingkuhan', 'Sendiri', 'Teman'),fill=rainbow(length(c)))
      }else{
        legend("topleft",c('Bahagia', 'Biasa Saja', 'Sangat Bahagia', 'Sangat Stress', 'Sedikit Bahagia', 'Sedikit Stress', 'Stress'),fill=rainbow(length(c)))}
    }
  })
  
  output$decisionTREE <- renderUI({
    decisionTREE
  })
  
  output$hasil <- renderUI({
    histogram_avggluc
  })
  output$hasil2 <- renderUI({
    scatter_plot
  })
  output$hasil3 <-renderUI({
    kernelDensity
  })
  output$hasil4 <- renderUI({
    boxxplot
  })
  output$hasil5 <- renderUI({
    barbarchart
  })
  output$hasil6 <- renderUI({
    piechartt
  })
  
  output$i_min <- renderInfoBox({
    item <- summary(data()[[input$vnumerik]])
    infoBox("Minimum",paste0(round(item[1],3)))
  })
  output$i_q1 <- renderInfoBox({
    item <- summary(data()[[input$vnumerik]])
    infoBox("Kuartil Bawah",paste0(round(item[2],3)))
  })
  output$i_median <- renderInfoBox({
    item <- summary(data()[[input$vnumerik]])
    infoBox("Median",paste0(round(item[3],3)))
  })
  output$i_mean <- renderInfoBox({
    item <- summary(data()[[input$vnumerik]])
    infoBox("Rata-Rata",paste0(round(item[4],3)))
  })
  output$i_q3 <- renderInfoBox({
    item <- summary(data()[[input$vnumerik]])
    infoBox("Kuartil Atas",paste0(round(item[5],3)))
  })
  output$i_max <- renderInfoBox({
    item <- summary(data()[[input$vnumerik]])
    infoBox("Maksimum",paste0(round(item[6],3)))
  })
  output$i_IQR <- renderInfoBox({
    item <- summary(data()[[input$vnumerik]])
    infoBox("InterQuartileRange",paste0(round(item[5]-item[3],3)))
  })
  output$i_range <- renderInfoBox({
    item <- summary(data()[[input$vnumerik]])
    infoBox("Range",paste0(round(item[6]-item[1],3)))
  })
  output$i_stdev <- renderInfoBox({
    item <- sd(data()[[input$vnumerik]])
    infoBox("Standard Deviation",paste0(round(item,3)))
  })
  output$i_variance <- renderInfoBox({
    item <- var(data()[[input$vnumerik]])
    infoBox("Variance",paste0(round(item,3)))
  })
  output$i_corel1 <- renderInfoBox({
    item <- cor(data()[[input$vnumerik]],data()$range_bahagia_stress_angka)
    infoBox("Corell with range mental",paste0(round(item,3)))
  })
  output$i_corel2 <- renderInfoBox({
    item <- cor(data()[[input$vnumerik]],data()$SKS)
    infoBox("Corell with SKS",paste0(round(item,3)))
  })
  output$i_corel3 <- renderInfoBox({
    item <- cor(data()[[input$vnumerik]],data()$penggunaan_movie_platform)
    infoBox("Corell with Duration watching Movie",paste0(round(item,3)))
  })
  output$i_corel4 <- renderInfoBox({
    item <- cor(data()[[input$vnumerik]],data()$jumlah_tugas_2minggu)
    infoBox("Corell with Jumlah Tugas",paste0(round(item,3)))
  })
  output$i_cov1 <- renderInfoBox({
    item <- cov(data()[[input$vnumerik]],data()$range_bahagia_stress_angka)
    infoBox("Covariance with range mental",paste0(round(item,3)))
  })
  output$i_cov2 <- renderInfoBox({
    item <- cov(data()[[input$vnumerik]],data()$SKS)
    infoBox("Covariance with SKS",paste0(round(item,3)))
  })
  output$i_cov3 <- renderInfoBox({
    item <- cov(data()[[input$vnumerik]],data()$penggunaan_movie_platform)
    infoBox("Covariance with Duration watching Movie",paste0(round(item,3)))
  })
  output$i_cov4 <- renderInfoBox({
    item <- cov(data()[[input$vnumerik]],data()$jumlah_tugas_2minggu)
    infoBox("Covariance with Jumlah Tugas",paste0(round(item,3)))
  })
  
  
  output$files <- renderUI({
    if (is.null(input$Upload)){
      tags$style(HTML('
        #files{
        overflow-x:visible;
    }'
      ))
    } else {
      
      div(
        h3("RINGKASAN DATA"),
        div(
          renderTable(head(data(), n = 10)),
          p(paste("Ukuran Data:", nrow(data()), "x" , ncol(data())), style="font-weight:bold"),
          style = "overflow-x:auto;"
        )
        
      )
      
    }
    
  })
}

shinyApp(ui =ui, server = server)
