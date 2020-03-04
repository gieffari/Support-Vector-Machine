
library(shiny)
library(ggplot2)

#data("AdultUCI")
#data<-AdultUCI

#for(i in c(1,3,5,11,12,13)) {data[i] <- lapply(data[i], as.numeric)}

fluidPage(
  titlePanel("KLASIFIKASI TWEET", tags$head()),
  
  h6("Developed by Satriagiva@gmail.com (Gieffari Satria Abdillah)"),
  h6("Mahasiswa Teknik Informatika Unisbank Semarang"),
  
  # Copy the line below to make a text input box
  
  sidebarPanel(
    
    sliderInput('prosentase', "Besar prosentase", min = 0.1, max = 0.5, value = 0.2, step = 0.01)
    
  ),
  
  
  
  
  mainPanel(
    tabsetPanel(id = 'mytab',
                tabPanel('Tabel Data', value = 'datatable', tableOutput("dataasli")), 
                tabPanel('Tabel Data Test', value = 'datatext', tableOutput("datatest")),  
                tabPanel('Histogram', value = 'graph',plotOutput('histoplot')),
                tabPanel('Wordcloud', value = 'wordcloud',plotOutput('wordcloudplot')),
                tabPanel('Confusion Matrix', value = 'datamatix', textOutput("matrix"))
                #tabPanel('Accuracy', value = 'akurasi',textOutput("akurasi")) ,
                #tabPanel('Confusion Matrix SVM', value = 'datatable', textOutput("matrixsvm"))            
    )
  )
)