rm(list=ls())
library(shiny)
library(shinyWidgets)
library(car)
library(tidyverse)
library(ggplot2)
library(leaflet)

load('./npcpdat.rData') #siteTable, seriesMetaData, wqdata

varPerSite = wqdata%>%group_by(LawaSiteID)%>%summarise(nVar = length(unique(Measurement)))%>%ungroup
siteTable$varPerSite = varPerSite$nVar
rm(varPerSite)


ui <- fluidPage(
  fluidRow(h1("DairyNZ::NPCP"),align='center'),
  #-----------------------
  fluidRow(
    column(width=6,div(style="height:50px")),
    column(width=6,htmlOutput("curSelect"),align='center')
  ),
  #----------------------- Map and Chart
  fluidRow(
    column(width=6,leafletOutput('nationMap',height = 600)),
    column(width=6,plotOutput('NPCPplot',click='NPCPplot_click',height = 600))
  ),
  #----------------------- Spacer
  
  fluidRow(div(style="height:10px")),
  #----------------------- (available variables, selection, y toggle) and legend
  fluidRow(div(style="height:50px;"),
           column(width=6,
                  fluidRow(htmlOutput("varsAvail",align='center')),
                  fluidRow(div(style="height:25px")),
                  fluidRow(align='center',selectInput(inputId = "waVariable",
                                       label = "Select a site on the map above",
                                       choices = NULL,selected = NULL,multiple = F)),
                  fluidRow(align='center',checkboxInput(inputId = 'logYaxis',
                                                     label="Log Y axis",
                                                     value = FALSE)),
                  fluidRow(align='center',sliderInput(inputId = 'ADpercentile',
                                                    label='Percentile of absolute deviances (50% - 80%)',
                                                    min=0.5,max=0.8,value=0.5,width='80%'))
           ),
           column(width=6,
                  fluidRow(htmlOutput('Results',align='center')),
                  fluidRow(plotOutput('legend'),offset=1))
  ),
  fluidRow(width=12,
    column(width=1),
    column(width=10,includeHTML('method.html')),
    column(width=1)
  )
)

addResourcePath('tmpuser',getwd())

pad <- function(xin,pin=0.5){
  quantile(x=abs(xin-median(xin,na.rm=T)),p=as.numeric(pin),na.rm=T)
}






server <- function(input, output,session) {
  
  uniData <- reactiveValues(siteID=NULL,subDat=NULL,longth=NULL,
                            fiveYearSkips=NULL,yearSkips=NULL,
                            interval=NULL,ADpercentile=0.5,
                            informantDat=NULL,nInformant=NULL)
  chp <- reactiveValues(chp=NULL)
  NPCPplot.dat <- reactiveValues(main=NULL,FiveYearEpochs=NULL,chpLine=NULL,
                                 tooOldPts=NULL,beforePts=NULL,includePts=NULL,afterPts=NULL,
                                 hiMedian=NULL,medianMedian=NULL,loMedian=NULL,
                                 layer8=NULL,layer9=NULL,
                                 informantMedians=NULL,compliant5yrMedians=NULL,violant5yrMedians=NULL)
 
  
  
  
  output$nationMap <- renderLeaflet({
    leaflet()%>%
      addProviderTiles('OpenStreetMap',options = providerTileOptions(minZoom=5,maxZoom=12))%>%
      setView(lng = 173.8654,lat = -40.91811,zoom = 5)
  })
  
  observe({
    leafletProxy("nationMap")%>%
      clearShapes()%>%
      addCircleMarkers(layerId =  siteTable$SiteID,
                       data=siteTable,
                       lng = siteTable$Long,lat = siteTable$Lat,
                       radius = (siteTable$varPerSite*(as.numeric(18-input$nationMap_zoom)/13)),
                       stroke=FALSE,
                       fillOpacity =  ((input$nationMap_zoom-4)/13)*siteTable$varPerSite/max(siteTable$varPerSite),
                       label=as.character(siteTable$SiteID),
                       fillColor=c("#03F","#D2D")[as.numeric(tolower(siteTable$SiteID)==tolower(uniData$siteID))+1])
  })
  

  #Select a site ####
  observeEvent(input$nationMap_marker_click,{
    chp$chp<<-NULL
    NPCPplot.dat$main=NULL
    NPCPplot.dat$FiveYearEpochs=NULL
    NPCPplot.dat$chpLine=NULL
    NPCPplot.dat$tooOldPts=NULL
    NPCPplot.dat$beforePts=NULL
    NPCPplot.dat$includePts=NULL
    NPCPplot.dat$afterPts=NULL
    NPCPplot.dat$hiMedian=NULL
    NPCPplot.dat$medianMedian=NULL
    NPCPplot.dat$loMedian=NULL
    NPCPplot.dat$layer8=NULL
    NPCPplot.dat$layer9=NULL
    NPCPplot.dat$informant5yrMedians=NULL
    NPCPplot.dat$compliant5yrMedians=NULL
    NPCPplot.dat$violant5yrMedians=NULL
    output$NPCPplot <- renderPlot({
      NPCPplot.dat$main
    })
    output$Results = renderText('')
    
    nP = siteTable[which(siteTable$Long == input$nationMap_marker_click$lng &
                           siteTable$Lat == input$nationMap_marker_click$lat),]
    # nP <- nearPoints(siteTable,input$nationMap_click,addDist=TRUE)[1,]
    
    if(dim(nP)[1]>0){
      uniData$siteID <- nP$SiteID
      uniData$subDat <- wqdata%>%filter(LawaSiteID==nP$LawaSiteID)
      availableMeasurements<-uniData$subDat%>%
        dplyr::select(Measurement)%>%table%>%sort%>%names
      
      if(length(availableMeasurements)>1){
        updateSelectInput(session=getDefaultReactiveDomain(),
                          inputId = 'waVariable',label='Select measurement',
                          choices=availableMeasurements,selected=availableMeasurements)
      }else{
        updateSelectInput(session=getDefaultReactiveDomain(),
                          inputId='waVariable',label='Please select a different site',
                          choices=NULL,selected=NULL)
      }
      
      output$varsAvail <- renderPrint({
        knitr::kable(format = 'html',
                     rbind(nP$SiteID,
                           availableMeasurements%>%
                             paste(collapse=', ')))
      })
      output$curSelect <- renderText({
        paste0(nP$SiteID,'<br>',availableMeasurements[1])
      })

     uniData$longth <<- uniData$subDat%>%
         dplyr::filter(Measurement==availableMeasurements[1])%>%
         dplyr::mutate(Date=lubridate::dmy(Date))%>%
         dplyr::arrange(Date)
      
    }else{
      uniData$subDat <- NULL
      uniData$longth <- NULL
      uniData$siteID <- NULL
      updateSelectInput(session=getDefaultReactiveDomain(),
                        inputId = 'waVariable',label='Please select a different site',
                        choices=NULL,selected=NULL)
    }
  },ignoreInit = T)
  
  
  #Select a variable ####
  observeEvent(input$waVariable,{
    if(is.null(input$waVariable)|is.na(input$waVariable)|is.null(uniData$subDat)){
      # reset the plot
      NPCPplot.dat$main=NULL
      NPCPplot.dat$FiveYearEpochs=NULL
      NPCPplot.dat$chpLine=NULL
      NPCPplot.dat$tooOldPts=NULL
      NPCPplot.dat$beforePts=NULL
      NPCPplot.dat$includePts=NULL
      NPCPplot.dat$afterPts=NULL
      NPCPplot.dat$hiMedian=NULL
      NPCPplot.dat$medianMedian=NULL
      NPCPplot.dat$loMedian=NULL
      NPCPplot.dat$layer8=NULL
      NPCPplot.dat$layer9=NULL
      NPCPplot.dat$informant5yrMedians=NULL
      NPCPplot.dat$compliant5yrMedians=NULL
      NPCPplot.dat$violant5yrMedians=NULL
      
      output$NPCPplot <- renderPlot({
        NPCPplot.dat$main
      })
      
      output$Results = renderText('')
      uniData$interval=NULL
    }else{
      output$curSelect <- renderText({
        paste0(uniData$siteID,'<br>',input$waVariable)
      })
      output$Results = renderText('')
      uniData$interval=NULL
      subDat=uniData$subDat
      longth <- subDat%>%filter(Measurement==input$waVariable)%>%
        mutate(Date=lubridate::dmy(Date))%>%
        arrange(Date)
      if(dim(longth)[1]>0){
        #DealCensor ####
      if(any(longth$Censored)){
        if(all(longth$CenType[longth$Censored]=="Left")){
          maxCens = max(longth$Value[longth$Censored],na.rm=T)
          longth$Value[longth$Value<=maxCens] = 0.5*maxCens
          rm(maxCens)
        }
        if(all(longth$CenType[longth$Censored]=='Right')){
          minCens = min(longth$Value[longth$Censored],na.rm=T)
          longth$Value[longth$Value>=minCens] = 1.1*minCens
          rm(minCens)
        }
        if(all(c("Left","Right")%in%unique(longth$CenType))){
          maxCens = max(longth$Value[longth$CenType=="Left"],na.rm=T)
          longth$Value[longth$Value<=maxCens] = 0.5*maxCens
          minCens = min(longth$Value[longth$CenType=="Right"],na.rm=T)
          longth$Value[longth$Value>=minCens] = 1.1*minCens
          rm(minCens,maxCens)
        }
      }
      
        
      #PerMonth ####
      
      monSeq = data.frame(Date=as.Date(seq.POSIXt(round.POSIXt(min(longth$Date),units = 'month'),
                          round.POSIXt(max(longth$Date),units = 'month'),by = 'month')),
                          Value=NA,Measurement=unique(longth$Measurement))
      for(ms in seq_along(monSeq$Date[-1])){
        these = which(longth$Date>monSeq$Date[ms]&longth$Date<=monSeq$Date[ms+1]&longth$Value>0)
        if(length(these)>0){
          monSeq$Value[ms]= exp(mean(log(longth$Value[these]),na.rm=T))
        }
      }

      longth=monSeq%>%
        mutate(year=lubridate::year(Date),
               month=lubridate::month(Date),
               day=lubridate::day(Date),
               dateNum=year+month/12)
      years = unique(longth$year)
      
      #Caculate 5yr stats ####
      longth$med5yr=NA
      longth$PAD5=NA
      longth$med1yr=NA
      longth$PAD1=NA
      for(r in seq_along(longth$med5yr)){
        within5yr = which(longth$dateNum >= (longth$dateNum[r] - 5) &
                            longth$dateNum <= (longth$dateNum[r]))
        within1yr = which(longth$dateNum >= (longth$dateNum[r] - 1) &
                            longth$dateNum <= (longth$dateNum[r]))
        pass=min(longth$dateNum,na.rm=T)<(longth$dateNum[r] - 5)
        if (pass) {
          longth$med5yr[r]=median(longth$Value[within5yr], na.rm = T)
          longth$PAD5[r]  =pad(xin = longth$Value[within5yr],pin = input$ADpercentile)
        }
        pass=min(longth$dateNum,na.rm=T)<(longth$dateNum[r] - 1)
        
        if (pass) {
          longth$med1yr[r]=median(longth$Value[within1yr], na.rm = T)
          longth$PAD1[r]  =pad(xin = longth$Value[within1yr],pin = input$ADpercentile)
        }
      }

      #Update measurement plot ####
      NPCPchart <-  ggplot(data = longth, mapping = aes(x = dateNum,y = Value))+
        labs(x = "Date",y=input$waVariable)+
        geom_point(size=1.375,shape=16)+
        geom_line(data = longth%>%filter(!is.na(med5yr)),
                  mapping = aes(x = dateNum,y = med5yr),size=0.75)+
        geom_line(data=longth%>%filter(!is.na(med1yr)),
                  mapping=aes(x=dateNum,y=med1yr),size=0.25,linetype='dashed')+
        scale_x_continuous(breaks=seq(1980,2020,1),
                           labels = paste0('`',stringr::str_pad(string = c(80:99,0:20),
                                                                width = 2,side='left',pad='0')))+
        theme(panel.grid.minor=element_blank())
      
      if(input$logYaxis){
        NPCPchart <- NPCPchart + scale_y_log10()
      }
      
      NPCPplot.dat$main = NPCPchart 
      
      NPCPplot.dat$FiveYearEpochs = NULL
      NPCPplot.dat$chpLine = NULL
      NPCPplot.dat$tooOldPts=NULL
      NPCPplot.dat$beforePts=NULL
      NPCPplot.dat$includePts=NULL
      NPCPplot.dat$afterPts=NULL
      NPCPplot.dat$hiMedian=NULL
      NPCPplot.dat$medianMedian=NULL
      NPCPplot.dat$loMedian=NULL
      NPCPplot.dat$layer8=NULL
      NPCPplot.dat$layer9=NULL
      NPCPplot.dat$informant5yrMedians=NULL
      NPCPplot.dat$compliant5yrMedians=NULL
      NPCPplot.dat$violant5yrMedians=NULL
      
      output$NPCPplot <- renderPlot({
        NPCPplot.dat$main
      })
      
      #Export this variable
      uniData$longth<<-longth
      }
    }
  },ignoreInit = T)
  
  #Toggle y axis logging
  observeEvent(input$logYaxis,{
    if(!is.null(uniData$longth)){
      #Update measurement plot ####
      NPCPchart <-  ggplot(data = uniData$longth,
                           mapping = aes(x = dateNum,y = Value))+
        labs(x = "Date",y=input$waVariable)+
        geom_point(size=1.375,shape=16)+
        geom_line(data = uniData$longth%>%filter(!is.na(med5yr)),
                  mapping = aes(x = dateNum,y = med5yr),size=0.75)+
        geom_line(data=uniData$longth%>%filter(!is.na(med1yr)),
                  mapping=aes(x=dateNum,y=med1yr),size=0.25,linetype='dashed')+
        scale_x_continuous(breaks=seq(1980,2020,1),
                           labels = paste0('`',stringr::str_pad(string = c(80:99,0:20),
                                                                width = 2,side='left',pad='0')))+
        theme(panel.grid.minor=element_blank())
      
      if(input$logYaxis){
        NPCPchart <- NPCPchart + scale_y_log10()
      }
      
      NPCPplot.dat$main <<- NPCPchart 
      
      
      output$NPCPplot <- renderPlot({
        NPCPplot.dat$main + NPCPplot.dat$FiveYearEpochs + NPCPplot.dat$chpLine + NPCPplot.dat$tooOldPts +
          NPCPplot.dat$beforePts + NPCPplot.dat$includePts + NPCPplot.dat$afterPts +
          NPCPplot.dat$hiMedian + NPCPplot.dat$medianMedian + NPCPplot.dat$loMedian +
          NPCPplot.dat$layer8 + NPCPplot.dat$layer9 + NPCPplot.dat$informant5yrMedians +
          NPCPplot.dat$compliant5yrMedians+ NPCPplot.dat$violant5yrMedians
      })
    }
  },ignoreInit=TRUE)
  
  #Change scale of PAD interval ####
  observeEvent(input$ADpercentile,{
    inputADpercentile <- as.numeric(input$ADpercentile)
    if(!is.na(inputADpercentile) & !is.null(uniData$interval)){
      if(inputADpercentile>40 & inputADpercentile<90){
        inputADpercentile=inputADpercentile/100
      }
      if(inputADpercentile>=0.45 & inputADpercentile<=0.85){
        uniData$ADpercentile = inputADpercentile
        rm(inputADpercentile)
        
        if("Hi"%in%names(uniData$interval)){
          uniData$interval = data.frame(median = median(uniData$informantDat$med5yr,na.rm=T),
                                        PAD = pad(uniData$informantDat$med5yr,pin = uniData$ADpercentile))
          uniData$interval$Hi = uniData$interval$median+uniData$interval$PAD
          uniData$interval$Lo = uniData$interval$median-uniData$interval$PAD

        if(!is.na(uniData$interval$Hi)){
          output$Results = renderText({
            paste("Selected timepoint:",round.POSIXt(lubridate::date_decimal(chp$chp),units = 'days'),'<br>',
                  "Number of five-year medians before selected time:",
                  uniData$nInformant,'<br>',
                  "Median of five-year medians before selected time:",round(uniData$interval$median,3),'<br>',
                  "Upper bound of these five-year medians:",round(uniData$interval$Hi,3),'<br>',
                  "Lower bound of these five-year medians:",round(uniData$interval$Lo,3),'<br>',
                  uniData$ADpercentile*100,"Percentile absolute deviation:",round(uniData$interval$PAD,3))
          })
          NPCPplot.dat$hiMedian <<-
            geom_hline(data=uniData$interval,mapping=aes(yintercept=Hi),linetype='dashed',colour='green4')
          NPCPplot.dat$medianMedian <<-
            geom_hline(data=uniData$interval,mapping=aes(yintercept=median),size=0.5,colour='green4')
          NPCPplot.dat$loMedian <<-
            geom_hline(data=uniData$interval,mapping=aes(yintercept=Lo),linetype='dashed',colour='green4')
          
        }else{
          output$Results = renderText({
            "No interval prior to the selected time point can be calculated, probably because there is too little data available before this timepoint."})
        }
        
        l10Dat=uniData$longth%>%filter(!is.na(med5yr),dateNum>=chp$chp)
        if(dim(l10Dat)[1]>1){
          if(l10Dat$Measurement[1]%in%c("TON","TN","NO3N","NH4","DRP","TP","ECOLI","TURB","DIN", "TURBFNU")){
            l10Dat$viol = (l10Dat$med5yr > uniData$interval$Hi)
          }else if(l10Dat$Measurement[1]=="BDISC"){
            l10Dat$viol =  (l10Dat$med5yr < uniData$interval$Lo)
          }else if(l10Dat$Measurement[1]=="PH"){
            l10Dat$viol = (l10Dat$med5yr > uniData$interval$Hi) | (l10Dat$med5yr < uniData$interval$Lo)
          }
          NPCPplot.dat$violant5yrMedians <<-
            geom_point(data=l10Dat%>%filter(viol,ForEval),mapping=aes(x=dateNum,y=med5yr),
                       col='red',size=3.5,shape=16)
          NPCPplot.dat$compliant5yrMedians <<-
            geom_point(data=l10Dat%>%filter(!viol,ForEval),mapping=aes(x=dateNum,y=med5yr),
                       col='green',size=3.5,shape=16)
        }else{
          NPCPplot.dat$informant5yrMedians=NULL
          NPCPplot.dat$violant5yrMedians=NULL
          NPCPplot.dat$compliant5yrMedians=NULL
        }
        
        output$NPCPplot <- renderPlot({
          NPCPplot.dat$main + NPCPplot.dat$FiveYearEpochs + NPCPplot.dat$chpLine + NPCPplot.dat$tooOldPts +
            NPCPplot.dat$beforePts + NPCPplot.dat$includePts + NPCPplot.dat$afterPts +
            NPCPplot.dat$hiMedian + NPCPplot.dat$medianMedian + NPCPplot.dat$loMedian +
            NPCPplot.dat$layer8 + NPCPplot.dat$layer9 + NPCPplot.dat$informant5yrMedians +
            NPCPplot.dat$compliant5yrMedians+ NPCPplot.dat$violant5yrMedians
        })
        }

      }
    }
  },ignoreInit = T)
  
  #Choose a changepoint time ####
  observeEvent(input$NPCPplot_click,{  
    NPCPplot.dat$FiveYearEpochs=NULL
    NPCPplot.dat$chpLine=NULL
    NPCPplot.dat$tooOldPts=NULL
    NPCPplot.dat$beforePts=NULL
    NPCPplot.dat$includePts=NULL
    NPCPplot.dat$afterPts=NULL
    NPCPplot.dat$hiMedian=NULL
    NPCPplot.dat$medianMedian=NULL
    NPCPplot.dat$loMedian=NULL
    NPCPplot.dat$layer8=NULL
    NPCPplot.dat$layer9=NULL
    NPCPplot.dat$informant5yrMedians=NULL
    NPCPplot.dat$compliant5yrMedians=NULL
    NPCPplot.dat$violant5yrMedians=NULL
    
    
    chp$chp <- input$NPCPplot_click$x[1]
    if(chp$chp>min(uniData$longth$dateNum,na.rm=T)){
      dists = abs(chp$chp-uniData$longth$dateNum)
      chp$chp = uniData$longth$dateNum[which.min(dists)]
      rm(dists)
      uniData$fiveYearSkips = seq(chp$chp, min(uniData$longth$dateNum,na.rm=T),by=-5)
      uniData$yearSkips = unique(c(seq(chp$chp, min(uniData$longth$dateNum,na.rm=T),by=-1),
                    seq(chp$chp, max(uniData$longth$dateNum,na.rm=T),by=1)))
      
      uniData$longth$ForEval = uniData$longth$dateNum%in%uniData$yearSkips & uniData$longth$dateNum>=chp$chp

      plotDat=data.frame(fYS = uniData$fiveYearSkips,chp=chp$chp)
      NPCPplot.dat$FiveYearEpochs <<- geom_vline(data=plotDat,mapping=aes(xintercept = fYS),linetype='dashed')
      NPCPplot.dat$chpLine <<- geom_vline(data=plotDat,mapping=aes(xintercept=chp))
      rm(plotDat)
      
      #Here, find five-year medians every five years before selected changepoint,
      #not necessarily the earliest in each fiveyear period
      
      
    }else{
      uniData$fiveYearSkips=NULL
      uniData$yearSkips=NULL
    }
    beforeChp=NULL
    
    if(length(uniData$fiveYearSkips)>1){
      stopnomore=F
      for(epochBoundary in seq_along(uniData$fiveYearSkips)){
        if(stopnomore){next}
        firstSet = sort(which(uniData$longth$dateNum>(uniData$fiveYearSkips[epochBoundary]-10) &      #ten years before
                                uniData$longth$dateNum<=(uniData$fiveYearSkips[epochBoundary]-5)))
        secondSet = sort(which(uniData$longth$dateNum>uniData$fiveYearSkips[epochBoundary]-5 &
                                 uniData$longth$dateNum<=(uniData$fiveYearSkips[epochBoundary]))) #five years before
        if(length(secondSet)>1){
          #Is there change within an epoch
          oneoftwo = secondSet[1:floor(length(secondSet)/2)]
          twooftwo = secondSet[(1+floor(length(secondSet)/2)):length(secondSet)]
          beforeChp = c(beforeChp,twooftwo)
          # wttwo = try({wilcox.test(x=uniData$longth$Value[oneoftwo],
          #                                       y=uniData$longth$Value[twooftwo],alternative='two',conf.int=T)},silent = T)
          # if('try-error'%in%attr(wttwo,'class')){
          #   stopnomore=T
          #   break
          # }else{
          #   wttwosig = ((3-as.numeric(cut(wttwo$p.value,breaks = c(-0.01, 0.01, 0.05, 0.1, 1.1)))))
          #   if(wttwosig>=2){
          #     l8Dat = data.frame(x=median(uniData$longth$dateNum[oneoftwo],na.rm=T),
          #                        xend=median(uniData$longth$dateNum[twooftwo],na.rm=T),
          #                        y=median(uniData$longth$Value[oneoftwo],na.rm=T),
          #                        yend=median(uniData$longth$Value[twooftwo],na.rm=T))
          #     NPCPplot.dat$layer8 <<-
          #       geom_segment(data=l8Dat,mapping=aes(x=x,xend=xend,y=y,yend=yend),
          #                    size=wttwosig/5,
          #                    colour='magenta')
          #   }else{
          #     NPCPplot.dat$layer8<<-NULL
          #   }
          #   if(wttwo$p.value<0.01){
          #     rm(wttwo,oneoftwo,twooftwo)
          #     stopnomore=T
          #     break
          #   }else{
             beforeChp=c(beforeChp,oneoftwo)
              rm(wttwo,oneoftwo,twooftwo)
            # }
          # }
        }
        if(length(firstSet)>0){
          #Between Epochs
          #Test location difference
          wt=try({wilcox.test(x=uniData$longth$Value[firstSet],
                                           y=uniData$longth$Value[secondSet],alternative='two',conf.int=T)},silent = T)
        if('try-error'%in%attr(wt,'class')){
          stopnomore=T
          break
        }else{
          wtsig=((3-as.numeric(cut(wt$p.value,breaks = c(-0.01, 0.01, 0.05, 0.1, 1.1)))))
          if(wtsig>=2){
            l9Dat=data.frame(x = median(uniData$longth$dateNum[firstSet],na.rm=T),
                             xend = median(uniData$longth$dateNum[secondSet],na.rm=T),
                             y = median(uniData$longth$Value[firstSet],na.rm=T),
                             yend = median(uniData$longth$Value[secondSet],na.rm=T))
            NPCPplot.dat$layer9 <<- 
              geom_segment(data=l9Dat,mapping=aes(x=x,xend=xend,y=y,yend=yend),
                           size = wtsig/5,
                           colour='magenta')
          }else{
            NPCPplot.dat$layer9<<-NULL
          }
          if(wt$p.value<0.01){
            stopnomore=T
            break
          }
          }
          rm(wt)
        }
      }
      
      
      beforeChp=sort(unique(beforeChp))
      includingChp = which(uniData$longth$dateNum-chp$chp<=5 & uniData$longth$dateNum-chp$chp>0)
      afterChp = which(uniData$longth$dateNum>(chp$chp+5))
      tooOld = seq_along(uniData$longth$dateNum)[-c(beforeChp,includingChp,afterChp)]
      
      plotDatBef=data.frame(dateNum=uniData$longth$dateNum[beforeChp],Value=uniData$longth$Value[beforeChp])
      plotDatInc=data.frame(dateNum=uniData$longth$dateNum[includingChp],Value=uniData$longth$Value[includingChp])
      plotDatAft=data.frame(dateNum=uniData$longth$dateNum[afterChp],Value=uniData$longth$Value[afterChp])
      plotDatTooOld=data.frame(dateNum=uniData$longth$dateNum[tooOld],Value=uniData$longth$Value[tooOld])
      
      NPCPplot.dat$beforePts <<- 
        geom_point(data=plotDatBef,mapping=aes(x=dateNum,y=Value),colour='steelblue',size=1.375)
      NPCPplot.dat$includePts <<-
        geom_point(data=plotDatInc,mapping=aes(x=dateNum,y=Value),colour='cyan',size=1.375)
      NPCPplot.dat$afterPts <<-
        geom_point(data=plotDatAft,mapping=aes(x=dateNum,y=Value),colour='salmon',size=1.375)
      NPCPplot.dat$tooOldPts <<- 
        geom_point(data=plotDatTooOld,mapping=aes(x=dateNum,y=Value),colour='thistle',size=1.375)
     
      firstFiveYearMedianDate = min(uniData$longth$dateNum[beforeChp],na.rm=T)+5.0
      
      rm(plotDatTooOld,plotDatBef,plotDatInc,plotDatAft,beforeChp,includingChp,afterChp,tooOld)
      
      
      if(firstFiveYearMedianDate<chp$chp){
        informantDat=uniData$longth%>%filter(dateNum>=firstFiveYearMedianDate,
                                             dateNum%in%uniData$yearSkips,
                                             dateNum<chp$chp)
        NPCPplot.dat$informant5yrMedians <<- 
          geom_point(data=informantDat,mapping=aes(x=dateNum,y=med5yr),col='blue',size=3.5,shape=16)
        
        uniData$interval = data.frame(median = median(informantDat$med5yr,na.rm=T),
                                      PAD = pad(xin = informantDat$med5yr,pin = uniData$ADpercentile))
        uniData$interval$Hi = uniData$interval$median+uniData$interval$PAD
        uniData$interval$Lo = uniData$interval$median-uniData$interval$PAD
        uniData$nInformant=dim(informantDat)[1]
        
        uniData$informantDat <<- informantDat
        rm(informantDat)
        
        output$Results = renderText({
          paste("Selected timepoint:",round.POSIXt(lubridate::date_decimal(chp$chp),units = 'days'),'<br>',
                "Number of five-year medians before selected time:",
                uniData$nInformant,'<br>',
                "Median of five-year medians before selected time:",round(uniData$interval$median,3),'<br>',
                "Upper bound of these five-year medians:",round(uniData$interval$Hi,3),'<br>',
                "Lower bound of these five-year medians:",round(uniData$interval$Lo,3),'<br>',
                uniData$ADpercentile*100,"Percentile absolute deviation:",round(uniData$interval$PAD,3))
        })
        NPCPplot.dat$hiMedian <<-
          geom_hline(data=uniData$interval,mapping=aes(yintercept=Hi),linetype='dashed',colour='green4')
        NPCPplot.dat$medianMedian <<-
          geom_hline(data=uniData$interval,mapping=aes(yintercept=median),size=0.5,colour='green4')
        NPCPplot.dat$loMedian <<-
          geom_hline(data=uniData$interval,mapping=aes(yintercept=Lo),linetype='dashed',colour='green4')
        
        l10Dat=uniData$longth%>%filter(!is.na(med5yr),dateNum>=chp$chp)
        if(dim(l10Dat)[1]>1){
          if(l10Dat$Measurement[1]%in%c("TON","TN","NO3N","NH4","DRP","TP","ECOLI","TURB","DIN", "TURBFNU")){
            l10Dat$viol = (l10Dat$med5yr > uniData$interval$Hi)
          }else if(l10Dat$Measurement[1]=="BDISC"){
            l10Dat$viol =  (l10Dat$med5yr < uniData$interval$Lo)
          }else if(l10Dat$Measurement[1]=="PH"){
            l10Dat$viol = (l10Dat$med5yr > uniData$interval$Hi) | (l10Dat$med5yr < uniData$interval$Lo)
          }
          NPCPplot.dat$violant5yrMedians <<-
            geom_point(data=l10Dat%>%filter(viol,ForEval),mapping=aes(x=dateNum,y=med5yr),
                       col='red',size=3.5,shape=16)
          NPCPplot.dat$compliant5yrMedians <<-
            geom_point(data=l10Dat%>%filter(!viol,ForEval),mapping=aes(x=dateNum,y=med5yr),
                       col='green',size=3.5,shape=16)
        }else{
          uniData$interval=NULL
          NPCPplot.dat$informant5yrMedians=NULL
          NPCPplot.dat$violant5yrMedians=NULL
          NPCPplot.dat$compliant5yrMedians=NULL
        }
        
      }else{
        NPCPplot.dat$informant5yrMedians <<- NULL 
        uniData$interval=NULL
        output$Results = renderText({
          paste("No interval prior to the selected time point",
                round.POSIXt(lubridate::date_decimal(chp$chp),units = 'days'),
                             "can be calculated, probably because there is too little data available before this timepoint, or because there is a significant shift, indicated by a magenta line, within five years before this timepoint.")})
        NPCPplot.dat$hiMedian <<- NULL
        NPCPplot.dat$medianMedian <<- NULL
        NPCPplot.dat$loMedian <<- NULL
        uniData$interval=NULL
        NPCPplot.dat$informant5yrMedians=NULL
        NPCPplot.dat$violant5yrMedians=NULL
        NPCPplot.dat$compliant5yrMedians=NULL
      }
      
      
    }else{ #length of fiveYearSkips <=1
      output$Results = renderText({
        paste("No interval prior to the selected time point",
              round.POSIXt(lubridate::date_decimal(chp$chp),units = 'days'),
              "can be calculated, probably because there is too little data available before this timepoint.")})
    }
    

    
    
    output$NPCPplot <- renderPlot({
        NPCPplot.dat$main + NPCPplot.dat$FiveYearEpochs + NPCPplot.dat$chpLine + NPCPplot.dat$tooOldPts +
        NPCPplot.dat$beforePts + NPCPplot.dat$includePts + NPCPplot.dat$afterPts +
        NPCPplot.dat$hiMedian + NPCPplot.dat$medianMedian + NPCPplot.dat$loMedian +
        NPCPplot.dat$layer8 + NPCPplot.dat$layer9 +
        NPCPplot.dat$informant5yrMedians+
        NPCPplot.dat$compliant5yrMedians+ NPCPplot.dat$violant5yrMedians
    })
    
    output$legend <- renderPlot({
      plot(0,0,type='n',xlab='',ylab='',xaxt='n',yaxt='n')
      legend('topleft',c('Raw monthly data',
                         "Monthly data informing range",
                         "Within 5yr after selected timepoint",
                         "Excluded due to significant shift",
                         "Monthly data isolated from timepoint", #5
                         
                         '5-year median','In-range median','Out-of-range median', #3
                         
                         'Significant shift',                      #1
                         '1-year median','5-year median',          #2
                         '5-year epoch boundary',                  #1
                         'Upper bound of 5-yr med','Median of 5-year medians',"Lower bound of 5-yr med"),
             col=c('black','steelblue','cyan','thistle2','salmon',
                   'blue','green','red',
                  'magenta',
                  'black','black','black','green4','green4','green4'),
             lty=c(0,0,0,0,0,
                   0,0,0,
                   1,  2,1,  0,
                   2,1,2),
             lwd=list(0,0,0,0,0,
                   0,0,0,
                   1,  1,2,  0,
                   1,1,1),
             pch=c(16,16,16,16,16,
                   16,16,16,
                   NA,
                   NA,NA,
                   73,
                   NA,NA,NA),
             bty = 'n')
    })
    
  },ignoreInit=T)
  
}#end of server definition



shinyApp(ui, server)

