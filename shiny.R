#R packages
library(rms)
library(mice)
library(pROC)
library(ggsci)
#library(ggDCA)
library(shiny)
library(purrr)
library(readxl)
library(impute)
library(ggpubr)
library(cowplot)
library(ggplot2)
library(showtext)
library(patchwork)
library(data.table)
library(missForest)
library(randomForest)
library(RColorBrewer)
library(shinydashboard)
library(shinycssloaders)


#model data
load("model.RData")


#function
source("ggDCA.R")

#app
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Single sample", tabName = "single", icon = icon("user")),
    menuItem("Multiple samples", tabName = "multi",icon = icon("users")),
    menuItem("Interpretability", 
             tabName = "interpretability", 
             icon = icon("chart-bar"))))

body <- dashboardBody(
  tabItems(tabItem(tabName = "single",
           h2("Single sample prediction"),
           
  fluidRow(
    column(width = 3,
           box(numericInput(inputId="AGE",label="unit: year",value=0),
               width = NULL, height = 150,title ="Age",
               status = "primary",solidHeader = TRUE,
               collapsible = TRUE,),
           box(numericInput(inputId="LDH_L",
                            label="Reference value: 109~245U/L",
                            value=0),
               width = NULL, height = 150,title ="Lactate Dehydrogenase (LDHL)",
               status = "primary",solidHeader = TRUE,
               collapsible = TRUE,),
           box(numericInput(inputId="ALB",
                            label="Reference value: 35~50 g/L",
                            value=0),
               width = NULL, height = 150,title ="Albumin (ALB)",
               status = "primary",solidHeader = TRUE,
               collapsible = TRUE,),
           box(numericInput(inputId="ALB_CR",
                            label="Reference value: <0.30",
                            value=0),
               width = NULL, height = 150,title ="Albumin/Creatinine (ALB/CR)",
               status = "primary",solidHeader = TRUE,
               collapsible = TRUE,)
          ),
    
    column(width = 3,
           box(numericInput(inputId="BMI",
                            label="Reference value: 18.5~23.9 kg/㎡",
                            value=0),
               width = NULL, height = 150,title ="Body Mass Index (BMI)",
               status = "primary",solidHeader = TRUE,
               collapsible = TRUE),
           box(numericInput(inputId="AST",
                            label="Reference value: 0~40 U/L",
                            value=0),
               width = NULL, height = 150,title ="Aspartate Transaminase (AST)",
               status = "primary",solidHeader = TRUE,
               collapsible = TRUE,),
           box(numericInput(inputId="SCR",
                            label="Reference value: 53~106umol/L",
                            value=0),
               width = NULL, height = 150,title ="Serum Creatinine (SCR)",
               status = "primary",solidHeader = TRUE,
               collapsible = TRUE,),
           box(numericInput(inputId="BU",
                            label="Reference value: 1.7~8.3mmol/L",
                            value=0),
               width = NULL, height = 150,title ="Blood Urea (BU)",
               status = "primary",solidHeader = TRUE,
               collapsible = TRUE,),
           
           
           ),
    column(width = 3,
           box(numericInput(inputId="BP_LOW",label="Reference value: 60~90 mmHg",value=0),
               width = NULL, height = 150,title ="Diastolic Pressure (BP_LOW)",
               status = "primary",solidHeader = TRUE,
               collapsible = TRUE,),
           box(numericInput(inputId="GGT",
                            label="Reference value: 11~50U/L",
                            value=0),
               width = NULL, height = 150,title ="Gamma-Glutamyltransferase (GGT)",
               status = "primary",solidHeader = TRUE,
               collapsible = TRUE,),
           box(numericInput(inputId="FBG",
                            label="Reference value: 2~4g/L",
                            value=0),
               width = NULL, height = 150,title ="Fibrinogen (FBG)",
               status = "primary",solidHeader = TRUE,
               collapsible = TRUE,),
           box(numericInput(inputId="TC",
                            label="Reference value: 2.83~5.20mmol/L",value=0),
               width = NULL, height = 150,title ="Total Cholesterol (TC)",
               status = "primary",solidHeader = TRUE,
               collapsible = TRUE),
           ),
    column(width = 3,
           align = "left",
           box(numericInput(inputId="BP_HIGH",label="Reference value: 90~139 mmHg",value=0),
               width = NULL, height = 150,title ="Systolic Pressure (BP_HIGH)",
               status = "primary",solidHeader = TRUE,
               collapsible = TRUE,),
           box(numericInput(inputId="LPS",
                            label="Reference value: 0~79U/L",
                            value=0),
               width = NULL, height = 150,title ="Lipase (LPS)",
               status = "primary",solidHeader = TRUE,
               collapsible = TRUE,),
           box(numericInput(inputId="DBILI",
                            label="Reference value: 0~6.8umol/L",
                            value=0),
               width = NULL, height = 150,title ="Direct Bilirubin (DBILI)",
               status = "primary",solidHeader = TRUE,
               collapsible = TRUE,),
           div(
             style = "display: flex; 
                               flex-direction: column; 
                               justify-content: center; 
                               align-items: center; 
                               height: 150px;",
             
             actionButton(
               "go1",
               "Submit",
               width = 150,
               height = 50,
               icon("refresh"),
               class = "btn btn-primary",
               style = "font-size: 16px; 
                                 padding: 15px 25px;"
             )
           ))
    ),
  fluidRow(
    box(
      width = 12, 
      height = 480,
      title ="Result",
      status = "warning",
      solidHeader = TRUE,
      collapsible = TRUE, 
      withSpinner(plotOutput("gplot_color"))
    )
  ),
  
  fluidRow(
    box(
      width = 12, 
      height = 480,
      title ="Notes",
      status = "warning",
      solidHeader = TRUE,
      collapsible = TRUE, 
      tags$span(
        style = "color: black; font-size: 18px;",
        HTML("<p>When using this platform, please take notes of the following:
              </p><p>1.The model results are intended for reference and to assist in decision-making, and should not be used as the sole basis for clinical diagnosis.
              </p><p>2.If there are any suspicions, further confirmatory tests are recommended.
              </p><p>3.While this model shows relatively reliable results in both training data and external validation, diagnostic errors may still occur due to variations in detection errors from different instruments and individual differences. As the results approach 50%, the probability of prediction errors gradually increases, as shown in the following figure.
              </p><p>4.The platform build code can be found on the <a href='https://github.com/JJM-labs/DN-platform'>GitHub</a> homepage,
                       and the data used in this study can be obtained from the following databases:  
                       <a href='http://www.ncmi.cn'>NPHDC</a>, 
                       <a href='https://wwwn.cdc.gov/nchs/nhanes'>NHANES</a>, and 
                       <a href='https://pubmed.ncbi.nlm.nih.gov/18370851/'>TWBB</a>.
              </p><p>5.If you are using this website, please cite our paper: <a href='https://link.springer.com/article/10.1007/s12020-024-03735-1'>Ma, J., An, S., Cao, M. et al. Integrated machine learning and deep learning for predicting diabetic nephropathy model construction, validation, and interpretability. Endocrine (2024). https://doi.org/10.1007/s12020-024-03735-1.</a>
              "
             )
        ),
      withSpinner(plotOutput("gplot_error")),
      style = "max-height: 435px; 
               max-width: auto; 
               overflow: auto;"
    )
  )
  ),

    tabItem(tabName = "multi",
            h2("Multiple sample prediction"),
    fluidRow(
      box(fileInput(inputId="multi",
                    accept=c(".txt",".csv",".xlsx"),
                    label="file formats: txt, csv, xlsx"),
          title ="Please upload your file",width = 4,
          status = "primary",solidHeader = TRUE,
          collapsible = TRUE,
          
          selectInput("imp", 
                      "Missing values",
                      list("No" = "no",
                           "Yes(by Random Forest)" = "imput_rf",
                           "Yes(by K-Nearest Neighbors)" = "imput_knn",
                           "Yes(by Multiple Imputation)" = "imput_mice"
                           )
                      ),
          selectInput("outliers", 
                      "outliers",
                      list("Not processed" = "no",
                           "Processed (by SD)"="sd",
                           "Processed (by IQR)"="box")),
          
          actionButton("go_mul","Submit",width = 100,
                       icon("refresh"),class = "btn btn-primary")
          ),
      box(tableOutput("example_out"),
          title ="Reference File (please ensure that the name of the detection indicator matches the sample file)",
          status = "primary",solidHeader = TRUE,
          collapsible = TRUE,width = 8,height=355,
          downloadButton("down1","Example File"),
          downloadButton("label","Data Dictionary")
          ),
      
    ),
         
    fluidRow(
      box(title ="Predicted Result",status = "warning",
          solidHeader = TRUE,collapsible = TRUE, 
          width = 4, height=460,
          withSpinner(tableOutput("muti_out")),
          downloadButton("down2","Result File"),
          ), 
      
      box(withSpinner(plotOutput("gplot_num")), 
          title ="Predicted Number",
          status = "warning",solidHeader = TRUE,height=460,
          collapsible = TRUE, width = 8),
       )
    ),
  tabItem(tabName = "interpretability",
          h2("Interpretability analysis"),
          fluidRow(
            box(width = 4,
                title ="Please upload your file",
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                fileInput(inputId="interpretability",
                          accept=c(".txt",".csv",".xlsx"),
                          label="file formats: txt, csv, xlsx"),
                selectInput("inter_type",
                            "Type",
                            list("ROC" = "roc",
                                 "DCA" = "dca",
                                 "Difference" = "diff",
                                 "Correlation" = "cor"
                            )
                ),
                
                actionButton("go_inter",
                             "Submit",
                             width = 100,
                             icon("refresh"),
                             class = "btn btn-primary")
            ),
            
            box(width = 8,
                tableOutput("inter_example_out"),
                title ="Reference File (the first column is the DN type and the second column is the DN probability)",
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                downloadButton("down_inter","Download"),
            ),
          ),
          
          fluidRow(
            box(height=500,
                width = 12,
                withSpinner(plotOutput("gplot_inter")), 
                title ="Result",
                status = "warning",
                solidHeader = TRUE,
                collapsible = TRUE, 
                downloadButton("down_gplot_inter","Download"))
          )
  )
  )
)


ui <- dashboardPage(
  dashboardHeader(title = "DN Risk Prediction"),
  sidebar,
  body
)

server <- function(input, output) {
  single_pred_data <- eventReactive(input$go1,{
    data=data.frame(ALB_CR=as.data.frame(input$ALB_CR)[,1,drop=T],
               SCR=as.data.frame(input$SCR)[,1,drop=T],
               BU=as.data.frame(input$BU)[,1,drop=T],
               ALB=as.data.frame(input$ALB)[,1,drop=T],
               BMI=as.data.frame(input$BMI)[,1,drop=T],
               LDH_L=as.data.frame(input$LDH_L)[,1,drop=T],
               BP_LOW=as.data.frame(input$BP_LOW)[,1,drop=T],
               AST=as.data.frame(input$AST)[,1,drop=T],
               FBG=as.data.frame(input$FBG)[,1,drop=T],
               DBILI=as.data.frame(input$DBILI)[,1,drop=T],
               BP_HIGH=as.data.frame(input$BP_HIGH)[,1,drop=T],
               TC=as.data.frame(input$TC)[,1,drop=T],
               LPS=as.data.frame(input$LPS)[,1,drop=T],
               GGT=as.data.frame(input$GGT)[,1,drop=T],
               AGE=as.data.frame(input$AGE)[,1,drop=T])
    data=scale(data ,center = min, scale = range)
    data
  })
  
  output$out_DN <- renderPrint({
    cat("Warning: This result is not a diagnostic gold standard, and it is recommended to conduct further diagnostic tests for suspected patients")
  })
  
  
  output$gplot_color=renderPlot({
    data1=single_pred_data()
    DN=data.frame(predict(rf.biop,data1,type="p"))[1,2,drop=T]
    DN_p=paste0(" \n DN probability: ",as.numeric(round(DN*100,2)),"% \n ")
    
    values=seq(0, 0.5, length.out = 600)
    values2=seq(0.5, 1, length.out = 600)
    data=data.frame(x = "x", y=c(values,values2) ,value = c(values,values2))
    showtext_auto()
    ggplot(data, aes(x = y, y = 1, fill = value)) +
      geom_tile() + geom_vline(xintercept = DN,lty="dashed")+
      scale_fill_gradientn(colours = c('blue','cyan','green','orange','red'))+
      scale_y_continuous(expand = c(0,0))+
      scale_x_continuous(expand = c(0,0))+
      theme_bw()+labs(x="Probability",y=NULL,title =DN_p)+
      theme(legend.position = "none",
            panel.grid = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            plot.title = element_text(size=25),
            axis.title.x = element_text(size=20),
            axis.text.x = element_text(size=15,color="black")
      )
  })
  
  output$gplot_error=renderPlot({
    
    data$Cohorts=factor(data$Cohorts,levels = unique(data$Cohorts))
    data$prob=factor(data$prob,level=unique(data$prob))
    
    ggplot(data,aes(x=prob,y=radio,fill=Cohorts))+
      geom_col(position = position_dodge(0.9))+
      geom_text(aes(y=radio+0.5,label=round(label,2)),position = position_dodge(0.9))+
      #scale_fill_d3("category20c")+
      scale_fill_manual(values = c("#626262","#306CA8","#BB482E"))+
      theme_bw()+
      scale_y_continuous(expand = c(0.01,0.03),limits = c(0,17))+
      labs(x="DN probability",y="Error %")+
      theme(legend.position = c(0.99,0.99),
            legend.justification = c(0.99,0.99),
            panel.grid = element_blank(),
            plot.margin = unit(c(1, 1, 1, 1), "cm"),
            plot.title = element_text(size=25),
            axis.title = element_text(size=20),
            axis.text = element_text(size=15,color="black")
      )
  })
  
  ##multi
  read.d1 <- eventReactive(input$go_mul,{
    inFile_multi = input$multi
    if (is.null(inFile_multi)) {
      return(NULL)
    }
    
    file_extension = tools::file_ext(inFile_multi$name)
    
    if (file_extension %in% c("csv", "txt")) {
      data=fread(inFile_multi$datapath)
    } 
    else if (file_extension %in% c("xlsx", "xls")) {
      data=read_excel(inFile_multi$datapath)
    }
    else {
      return("Unsupported file type")
    }
    return(data)
   }
  )
  
  muti_data_imp=eventReactive(input$go_mul,{
    muti_data=read.d1()
    muti_data=as.matrix(muti_data)
    rownames(muti_data)=muti_data[,1,drop=T]
    muti_data=muti_data[,-1]
    muti_data=muti_data[,colnames(example_data)[-1]]
    
    if(input$imp=="no"){
      muti_data=muti_data
    }
    else if(input$imp=="imput_knn"){
      set.seed(2023)
      muti_data=as.matrix(muti_data)
      imp_knn=impute.knn(muti_data)
      muti_data=imp_knn$data
    }
    else if(input$imp=="imput_rf"){
      set.seed(2023)
      imp_rf=missForest(muti_data)
      muti_data=imp_rf$ximp
    }
    else if(input$imp=="imput_mice"){
      set.seed(2023)
      imp=mice(muti_data,method = "pmm",m=5)
      muti_data=complete(imp)
    }
    
    #outliers
    if(input$outliers=="no"){
      muti_data=muti_data
    }
    else if(input$outliers=="sd"){
      
      sd=apply(muti_data,2,sd)
      threshold=2*sd
      
      for (i in colnames(muti_data)) {
        muti_data[which(abs(muti_data[,i])>threshold[i]),i]=threshold[i]
      }
    }
    else if(input$outliers=="box"){
      
      quant_data=sapply(colnames(muti_data),
                        function(x){
                          data=muti_data[,x]
                          quant=as.data.frame(quantile(data,na.rm = T))
                          colnames(quant)=x
                          
                          quant["IQR",]=(quant[4,1]-quant[2,1])*1.5
                          quant["low",]=quant[2,1]-quant["IQR",]
                          quant["up",]=quant[4,1]+quant["IQR",]
                          
                          return(quant)
                        },simplify = F)
      quant_data=do.call("cbind",quant_data)
      
      for (i in colnames(muti_data)) {
        muti_data[which(muti_data[,i]>quant_data["up",i]),i]=quant_data["up",i]
        muti_data[which(muti_data[,i]<quant_data["low",i]),i]=quant_data["low",i]
      }
    }
    
    return(muti_data)
  })
  
  
  muti_data_imp_scale=reactive({
    muti_data_scale=scale(muti_data_imp() ,center = min, scale = range)
    return(muti_data_scale)
  })
  
  muti_pred_data <- reactive({
    muti_data=muti_data_imp_scale()
    muti_pred=data.frame(predict(rf.biop,muti_data,type="p"))
    muti_pred$Type=round(muti_pred$X1)
    colnames(muti_pred)=c("nDN_probability","DN_probability","Type")
    muti_pred$Type=ifelse(muti_pred$Type==0,"nDN","DN")
    muti_pred$ID=rownames(muti_data)
    muti_pred=muti_pred[,c("ID","nDN_probability","DN_probability","Type")]
    
    return(muti_pred)
  })
  
  muti_pred_down=reactive({
    muti_data_imp=muti_data_imp() 
    muti_data_imp_scale=muti_data_imp_scale()
    colnames(muti_data_imp_scale)=paste0(colnames(muti_data_imp_scale),"_scale")
    muti_pred=muti_pred_data()
    muti_pred_down=cbind(muti_pred,muti_data_imp,muti_data_imp_scale)
    return(muti_pred_down)
  })
  
  output$muti_out=renderTable({
    muti_pred_data()[1:10,]
  })
  
  output$example_out=renderTable({
    example_data[1:6,]
  }
  )
  
  output$down1 <- downloadHandler(
    filename = "Reference-file_multi.csv",
    content = function(file) {
      fwrite(example_data[1:20,], file,col.names = T)
    }
  )
  
  output$label <- downloadHandler(
    filename = "Data dictionary.txt",
    content = function(file) {
      fwrite(label, file,col.names = T, sep = "\t")
    }
  )
  
  output$down2 <- downloadHandler(
    filename = function() {
      paste0("DN_prediction.csv")
    },
    content = function(file) {
      fwrite(muti_pred_down(), file,col.names = T)
    }
  )
  
  muti_pred_num=reactive({
    data=muti_pred_data()
    data=data.frame(table(data$Type))
    colnames(data)=c("Type","num")
    return(data)
  }
  )
  
  muti_pred_num2=reactive({
    data=muti_pred_data()
    data=data[order(data$DN_probability),]
    data$ID=rep(1:length(data$ID))
    return(data)
  })
  
  color=c("#D4B982","#518BB0")
  
  output$gplot_num <- renderPlot({
    showtext_auto()
    p1=ggplot(data = muti_pred_num()) + 
      geom_col(aes(x=Type,y=num,fill=Type)) +
      geom_text(aes(x=Type,y=num+num*0.04,label=num),size=6)+
      scale_fill_manual(values = color)+
      labs(x = NULL, y = "Number")+ 
      theme_classic() + 
      scale_y_continuous(expand = c(0.03,0.03))+
      theme(legend.position = "none",
            axis.title = element_text(size = 15,hjust=0.5),
            plot.title = element_text(hjust = 0.5, size = 15),
            axis.text.x = element_text(colour = "black", size = 15),
            axis.text.y = element_text(colour = "black", size = 15))
    
    p2=ggplot(data = muti_pred_num2()) + 
      theme_bw()+ 
      geom_col(aes(x = ID,y = DN_probability,fill = Type))+ 
      labs(x="Patient ID (Increasing possibility)",y="DN probability")+
      scale_fill_manual(values = color)+
      theme(legend.position=c(0.05,0.95),
            panel.grid = element_blank(),
            axis.title = element_text(size=15),
            legend.text = element_text(size=15),
            legend.title = element_text(size=15),
            legend.justification = c("left", "top"),
            axis.text = element_text(size=12,color="black"))
    
    p3=ggplot(muti_pred_data(),
              aes(x=Type,y=DN_probability,
                  color=Type,fill=Type))+
      geom_violin(alpha = 0.4, width = 1.2,
                  size = 0.8, color="black") +
      geom_boxplot(notch = TRUE, 
                   outlier.size = -1, 
                   width=0.3,lwd=0.8,
                   color="black",alpha = 0.7) +
      geom_jitter(shape = 21, size=2, 
                  position = position_dodge(0.3), 
                  color="black", alpha = 1) +
      scale_color_manual(values = color)+
      scale_fill_manual(values = color)+
      labs(x = NULL, y = "DN probability")+ 
      theme_classic() + 
      theme(legend.position = "none",
            axis.title = element_text(size = 15,hjust=0.5),
            plot.title = element_text(hjust = 0.5, size = 15),
            axis.text.x = element_text(colour = "black", size = 15),
            axis.text.y = element_text(colour = "black", size = 15))
    
    p2 + p3 + p1 + plot_layout(widths = c(3,1.5,2))
  }
  )
  
  ##inter
  output$inter_example_out=renderTable({
    inter_file[1:4,]
  }
  )
  
  output$down_inter <- downloadHandler(
    filename = "Reference-file_Interpretability.csv",
    content = function(file) {
      fwrite(inter_file, file,col.names = T)
    }
  )
  
  read_inter=eventReactive(input$go_inter,{
    inFile_inter = input$interpretability
    if (is.null(inFile_inter)) {
      return(NULL)
    }
    
    file_extension = tools::file_ext(inFile_inter$name)
    
    if (file_extension %in% c("csv", "txt")) {
      data=fread(inFile_inter$datapath)
    } 
    else if (file_extension %in% c("xlsx", "xls")) {
      data=read_excel(inFile_inter$datapath)
    }
    else {
      return("不支持的文件类型")
    }
    return(data)
  }
  )
  
  inter_data=eventReactive(input$go_inter,{
    data=read_inter()
    data=as.data.frame(data)
    colnames(data)=c("Type","prob",colnames(data)[-c(1:2)])
    data$Type=as.factor(data$Type)
    return(data)
  }
  )
  
  gplot_inter_list=eventReactive(input$go_inter,{
    
    if(input$inter_type=="roc"){
      
      plot_data=inter_data()
      plot_data=plot_data[,-2]
      
      roc_list=sapply(colnames(plot_data)[-1],
                      function(x){
                        roc_data=plot_data[,c("Type",x)]
                        roc_curve=roc(roc_data$Type,roc_data[,2,drop=T], quiet = TRUE)
                        return(roc_curve)
                      },
                      simplify = F)
      
      roc_plot_data=sapply(names(roc_list),
                           function(x){
                             roc_obj=roc_list[[x]]
                             roc_plot=data.frame(
                               sensitivity=roc_obj$sensitivities,
                               specificity=roc_obj$specificities)
                             roc_plot$features=rep(x,nrow(roc_plot))
                             roc_plot$AUC=rep(round(roc_obj$auc,3))
                             return(roc_plot)
                           },
                           simplify = F)
      roc_plot_data=do.call("rbind",roc_plot_data)
      
      plot_list=sapply(unique(roc_plot_data$features),
                       function(x){
                         data=roc_plot_data[roc_plot_data$feature==x,]
                         
                         p=ggplot(data,aes(
                           y=sensitivity,x=1-specificity))+
                           geom_line(lwd=1,color="tomato")+
                           theme_bw()+
                           labs(color=NULL,title = paste0(
                             x,"\nAUC=",unique(data$AUC)))+
                           geom_abline(intercept = 0, 
                                       slope = 1,lty="dashed")+
                           guides(color=guide_legend(
                             ncol=1,override.aes = list(lwd=1)))+
                           theme(legend.position = c(0.99,0.01),
                                 panel.grid = element_blank(),
                                 legend.justification = c(0.99,0.01), 
                                 axis.title = element_text(size=18),
                                 legend.text = element_text(size=10),
                                 plot.title = element_text(size=15,hjust=0.5),
                                 axis.text = element_text(size=12,color="black"))
                         return(p)
                       },
                       simplify = F)
    }
    else if(input$inter_type=="dca"){
      plot_data=inter_data()
      plot_data=plot_data[,-2]
      plot_data=as.data.frame(plot_data)
      plot_data[,1]=as.numeric(plot_data[,1])
      dca_data<<-plot_data
      
      dca_model=sapply(colnames(dca_data)[-1],
                       function(x){
                         formula=as.formula(paste("Type ~", x))
                         dca_lr=rms::lrm(formula,dca_data)
                         return(dca_lr)
                       },
                       simplify = F)
      
      plot_list=sapply(names(dca_model),
                       function(x){
                         model=dca_model[[x]]
                         dca=dca(model,model.names=x)
                         p=ggplot(dca,
                                  linetype =F,
                                  lwd = 1)+
                           theme_classic()+ 
                           labs(color=NULL,title=x)+ 
                           scale_color_d3("category20c")+
                           guides(color=guide_legend(override.aes = list(lwd=1)))+
                           theme(legend.position = "top",
                                 panel.grid = element_blank(),
                                 #legend.justification = c(0.99,0.01), 
                                 axis.title = element_text(size=18),
                                 legend.text = element_text(size=15),
                                 plot.title = element_text(size=18,hjust=0.5),
                                 axis.text = element_text(size=12,color="black"))
                         return(p)
                       },
                       simplify = F)
    }
    else if(input$inter_type=="cor"){
      plot_data=inter_data()
      plot_data=plot_data[,-1]
      
      plot_list=sapply(colnames(plot_data)[-1],
                       function(x){
                         data=plot_data[,c("prob",x)]
                         colnames(data)=c("prob","feature")
                         
                         p=ggplot(data,aes(
                           y=feature,x=prob))+
                           geom_point()+
                           stat_cor(color="red",
                                    size = 5,
                                    method = "spearman")+
                           geom_smooth(method = "lm")+
                           theme_bw()+
                           labs(y=x,x="DN probability")+
                           theme(legend.position = c(0.99,0.01),
                                 panel.grid = element_blank(),
                                 legend.justification = c(0.99,0.01), 
                                 axis.title = element_text(size=18),
                                 legend.text = element_text(size=10),
                                 plot.title = element_text(size=15,hjust=0.5),
                                 axis.text = element_text(size=12,color="black"))
                         return(p)
                       },
                       simplify = F)
    }
    else if(input$inter_type=="diff"){
      plot_data=inter_data()
      plot_data=plot_data[,-2]
      
      plot_list=sapply(colnames(plot_data)[-1],
                       function(x){
                         data=plot_data[,c("Type",x)]
                         colnames(data)=c("Type","feature")
                         
                         p=ggplot(data,aes(
                           y=feature,x=Type))+
                           geom_violin(aes(fill=Type))+
                           geom_boxplot(fill="white",
                                        width=0.2,alpha=0.5,outlier.alpha = 0)+
                           #geom_jitter(aes(color=Type),width=0.3)+
                           theme_bw()+
                           scale_fill_d3("category20c")+
                           labs(y=x,x="Type")+
                           theme(legend.position = "none",
                                 panel.grid = element_blank(),
                                 axis.title = element_text(size=18),
                                 legend.text = element_text(size=10),
                                 plot.title = element_text(size=12,hjust=0.5),
                                 axis.text = element_text(size=12,color="black"))+
                           stat_compare_means(vjust = 0.6,
                                              tip.length = 0,
                                              bracket.size=0.5,
                                              method = "wilcox",
                                              label = "p.signif",
                                              comparisons = list(levels(data$Type)))
                         return(p)
                       },
                       simplify = F)
    }
    return(plot_list)
  }
  )
  
  output$gplot_inter <- renderPlot({
    plot_list=gplot_inter_list()
    
    plot=cowplot::plot_grid(plotlist = plot_list,ncol = 5,align = 'hv')
    return(plot)
  })
  
  output$down_gplot_inter <- downloadHandler(
    
    filename = function() {
      if(input$inter_type=="roc"){
        "ROC-Results.zip" 
      }
      else if(input$inter_type=="dca"){
        "DCA-Results.zip" 
      }
      else if(input$inter_type=="cor"){
        "Correlation-Results.zip" 
      }
      else if(input$inter_type=="diff"){
        "Difference-Results.zip" 
      }
    },
    
    content = function(file) {
      
      plot_list=gplot_inter_list()
      save_path=paste0("./",input$inter_type)
      dir.create(save_path)
      
      if(input$inter_type=="roc"){
        width=5
        heigh=5
      }
      else if(input$inter_type=="dca"){
        width=4.5
        heigh=5
      }
      else{
        width=5
        heigh=4.6
      }
      
      for (i in names(plot_list)) {
        gplot_file=plot_list[[i]]
        ggsave(file.path(save_path, 
                         paste0(i, ".pdf")), 
               gplot_file,
               w=width,h=heigh)
      }
      
      zip(file, files = list.files(
        save_path, full.names = TRUE))
      
      unlink(save_path, recursive = TRUE)
    }
  )
  output$down_paper <- downloadHandler(
    zip(file, files = list.files(
      save_path, full.names = TRUE))
  )
}

#run
shinyApp(ui, server)

