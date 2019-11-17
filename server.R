# SERVER

library(shiny)
library(mlr)
library(rpart)
library(rpart.plot)
library(e1071)
library(caret)
library(smotefamily)
library(FSelectorRcpp)
library(randomForest)
library(data.table)
library(DT)
library(ggplot2)
library(plotly)

# chemin d'acces C:\Users\nahny\Documents\GitHub

#DT=read.csv("C:/Users/nahny/Documents/GitHub/creditcard.csv", stringsAsFactors = FALSE )

DT=readRDS("templatePDF/creditcard.rds")

#DT=read.csv("C:/Users/33668/Documents/MASTER 2 ESA/projet svm/app_0611/creditcard.csv", stringsAsFactors = FALSE )

set.seed(123234)
DT$Amount=as.vector(scale(DT$Amount))
DT$Time=as.vector(scale(DT$Time))
DT$Class=as.factor(DT$Class)
levels(DT$Class)[1] <- "nonFraud"
levels(DT$Class)[2] <- "Fraud"


table_prop<-rbind(table(DT$Class),prop.table(table(DT$Class)))
row.names(table_prop)<-c("Number","Proportion (in %)")
table_prop[2,]<-round(table_prop[2,],4)*100
tableex=DT[1:100,c(1,2,3,4,5,6,30,31)]
stat=summary(DT[,-31])


tab1=prop.table(table(DT$Class))
tab=table(DT$Class)
mat=rbind(tab,tab1)
row.names(mat)<-c("Number","Proportion (in %)")
mat[2,]<-round(mat[2,],4)*100
 


# data splitting
intrain=createDataPartition(DT$Class, p = 0.7,list=FALSE)
DT.train=as.data.frame(DT[intrain,])
data.test=as.data.frame(DT[-intrain,])

shinyServer(function(input, output) {
    
    ## 
    output$tableDT2<-renderDT({
         DT::datatable(table_prop,list(dom = 't'))
    })

    output$tableDTt<-renderDT({
        DT::datatable(tableex)
    })

    output$tableDTs<-renderDT({
        DT::datatable(stat)
    })
    
    # table DT
    output$tabletrain1 <- renderDT({ DT::datatable(mat,list(dom = 't')) })
  
    # creation of smote data train 
    data.train <- reactive({
        data.train=smotefamily::SMOTE(DT.train[,-31], DT.train[,31], K = input$kn, 
                                    dup_size = input$prop)$data
        names(data.train)[31]="Class"
        
        set.seed(12345)
        s=sample(1:nrow(data.train),input$size)
        data.train=data.train[s,]
        return(data.train)
    }) 
    
    output$smote <- renderDT({
        
        ab=table(data.train()$Class)
        ab1= prop.table(table(data.train()$Class))
        at=rbind(ab,ab1)
        row.names(at)<-c("Number","Proportion (in %)")
        at[2,]<-round(at[2,],4)*100
        
        DT::datatable(at,list(dom = 't'))
    })
    
    
   #########################################################################
    # MODELISATION
    ###############################################################################
    
    train.task <- reactive({
        makeClassifTask(data=data.train(),target="Class",positive = "Fraud")
    })
    
    test.task <- reactive({
        makeClassifTask(data=data.test,target="Class",positive = "Fraud")
    })
    
    filter <- reactive({
        mlr::generateFilterValuesData(task=train.task(),
                                      method="FSelectorRcpp_information.gain")
    })
    
    output$filtre <- renderPlotly({ 
        des=mlr::plotFilterValues(filter(),n.show=10)
        plotly_build(des)
    }) 
    
    
    
    #logistic regression 
    train.lg <- reactive({ 
        
        learner.lg <- makeLearner(cl="classif.logreg",predict.type="prob")
        
        
        train.lg <- mlr::train(learner.lg,train.task())
        
        return(train.lg)
    }) 
    
    
    pred.lg <- reactive({ 
        predict(train.lg(),test.task())
    }) 
    cm <- reactive({
      cm=calculateConfusionMatrix(pred.lg(),relative = TRUE)
    })
    
    output$matrix.lg <- renderPlot({
        
        cmd=as.data.frame(cm()[["result"]]) 
        
        predicted <- factor(c("fraud", "fraud", "fraud", "non fraud", "non fraud", "non fraud","error","error","error"))
        true <- factor(c("fraud", "non fraud","error", "fraud", "non fraud","error","fraud", "non fraud","error"))
        b=cmd[,2]
        a=cmd[,1]
        c=cmd[,3]
        Y =c(a,b,c)
        
        df <- data.frame(predicted, true, Y)
        
        ggplot(data =  df, mapping = aes(x = predicted, y =true )) +
          geom_tile(aes(fill = Y), colour = "white") +
          geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
          scale_fill_gradient(low = "light blue", high = "green") +
          theme_bw() + theme(legend.position = "none")
  
    })
    
    # output$rmatrix.lg <- renderPlot({
    #   
    #   lg.cm=as.data.frame(cm()[["relative.col"]]) 
    #   
    #   predicted <- factor(c("fraud", "fraud","fraud", "non fraud","non fraud","non fraud"))
    #   true <- factor(c("fraud", "non fraud","error","fraud", "non fraud","error"))
    #   lg.a=lg.cm[,1]
    #   lg.b=lg.cm[,2]
    #   #c=cmd[,3]
    #   lg.Y=c(lg.a,lg.b)
    #   
    #   lg.df <- data.frame(predicted, true, lg.Y)
    #   
    #   ggplot(data =  lg.df, mapping = aes(x = predicted, y =true )) +
    #     geom_tile(aes(fill = lg.Y), colour = "white") +
    #     geom_text(aes(label = sprintf("%1.0f", lg.Y)), vjust = 1) +
    #     scale_fill_gradient2(mid="light blue", high = "blue") +
    #     theme_bw() + theme(legend.position = "none")
    #   
    # })
    # 
    output$roc.lg <- renderPrint({
        calculateROCMeasures(pred.lg())
    })
    
    
    df.lg <- reactive({ generateThreshVsPerfData(pred.lg(),
                                                 measures = list(fpr, tpr, ppv, tnr,mmce))
    })
    
    
    output$graph1.lg <- renderPlotly({
        #plotROCCurves(df.lg(), measures = list(tpr, ppv), diagonal = FALSE)
        pli1=ggplot(df.lg()$data)+geom_line(aes(x=tpr,y=ppv),color="orange")+theme_bw()+
            xlab("True positive rate")+ylab("positive predict rate")
        plotly_build(pli1)
        
    })
    
    output$graph2.lg <- renderPlotly({
        #plotROCCurves(df.lg(), measures = list(tnr, tpr), diagonal = FALSE)
        pli2=ggplot(df.lg()$data)+geom_line(aes(x=tnr,y=tpr),color="orange")+theme_bw()+
            xlab("True negative rate")+ylab("True positive rate")
        plotly_build(pli2)
        
    })
    
    output$graph3.lg <- renderPlotly({
        #plotROCCurves(df.lg(), measures = list(fpr, tpr), diagonal = TRUE)
        pli3=ggplot(df.lg()$data)+geom_line(aes(x=fpr,y=tpr),color="orange")+theme_bw()+
            xlab("False positive rate")+ylab("True positive rate")
        plotly_build(pli3)
        
    })
    
    
    
    
    
    #arbre de decision 
    train.tree <- reactive({ 
        
        learner.tree <- makeLearner(cl="classif.rpart",predict.type="prob")
        
        train.tree <- mlr::train(learner.tree,train.task())
        
        return(train.tree)
    }) 
    
    
    output$tree <- renderPlot({
        
        rpart.plot(getLearnerModel(train.tree()),roundint = F)
        
    })
    
    
    pred.tree <- reactive({ 
        predict(train.tree(),test.task())
    }) 
    
    tcm <- reactive({ calculateConfusionMatrix(pred.tree(),relative = TRUE)
    })
    
    
    output$amatrix.tree <- renderPlot({
      
      #acmd=as.data.frame(tcm()[["result"]]) 
      
      atcm=as.data.frame(tcm()[["result"]]) 
      
      predicted <- factor(c("fraud", "fraud", "fraud", "non fraud", "non fraud", "non fraud","error","error","error"))
      true <- factor(c("fraud", "non fraud","error", "fraud", "non fraud","error","fraud", "non fraud","error"))
      atb=atcm[,2]
      ata=atcm[,1]
      atc=atcm[,3]
      atY =c(ata,atb,atc)
      
      atdf <- data.frame(predicted, true, atY)
      
      ggplot(data =  atdf, mapping = aes(x = predicted, y =true )) +
        geom_tile(aes(fill = atY), colour = "white") +
        geom_text(aes(label = sprintf("%1.0f", atY)), vjust = 1) +
        scale_fill_gradient(low = "light blue", high = "green") +
        theme_bw() + theme(legend.position = "none")
      
      
    })
    
    # output$rmatrix.tree <- renderPlot({
    #   
    #   cmd=as.data.frame(tcm()[["relative.col"]]) 
    #   
    #   predicted <- factor(c("fraud", "fraud","fraud", "non fraud","non fraud","non fraud"))
    #   true <- factor(c("fraud", "non fraud","error","fraud", "non fraud","error"))
    #   ar=cmd[,1]
    #   br=cmd[,2]
    #   #c=cmd[,3]
    #   Yr =c(ar,br)
    #  
    #   dfr <- data.frame(predicted, true, Yr)
    #   
    #   ggplot(data =  dfr, mapping = aes(x = predicted, y =true )) +
    #     geom_tile(aes(fill = Yr), colour = "white") +
    #     geom_text(aes(label = sprintf("%1.0f", Yr)), vjust = 1) +
    #     scale_fill_gradient2(mid="light blue", high = "blue") +
    #     theme_bw() + theme(legend.position = "none")
    # })
    # 
    output$roc.tree <- renderPrint({
        calculateROCMeasures(pred.tree())
    })
    
    
    df.tree <- reactive({ generateThreshVsPerfData(pred.tree(),
                                                 measures = list(fpr, tpr, ppv, tnr,mmce))
    })
    
    
    output$graph1.tree <- renderPlotly({
        # plotROCCurves(df.tree(), measures = list(tpr, ppv), diagonal = FALSE)
        pp=ggplot(df.tree()$data)+geom_line(aes(x=tpr,y=ppv),color="blue")+theme_bw()
        plotly_build(pp)
        
    })
    
    output$graph2.tree <- renderPlotly({
        # plotROCCurves(df.tree(), measures = list(tnr, tpr), diagonal = FALSE)
        bbp= ggplot(df.tree()$data)+geom_line(aes(x=tnr,y=tpr),color="blue")+theme_bw()
        plotly_build(bbp)
    })
    
    output$graph3.tree <- renderPlotly({
        # plotROCCurves(df.tree()$data, measures = list(fpr, tpr), diagonal = TRUE)
        tt=ggplot(df.tree()$data)+geom_line(aes(x=fpr,y=tpr),color="blue")+theme_bw()
        plotly_build(tt)
    })
    
    
    
    
    
    #svm 
    train.svm <- reactive({ 
        
        learner.svm <- makeLearner(cl="classif.svm",predict.type="prob",
                                   par.vals = list(cost = input$cost.param,
                                                   kernel = input$kernel.param))
        
        train.svm <- mlr::train(learner.svm,train.task())
        
        return(train.svm)
    }) 
    
    
    pred.svm <- reactive({ 
        predict(train.svm(),test.task())
    }) 
    
    svm.cm <- reactive({
      calculateConfusionMatrix(pred.svm(),relative = TRUE)
    })
    
    output$amatrix.svm <- renderPlot({
      svm.cmd=as.data.frame(svm.cm()[["result"]]) 
      
      predicted <- factor(c("fraud", "fraud", "fraud", "non fraud", "non fraud", "non fraud","error","error","error"))
      true <- factor(c("fraud", "non fraud","error", "fraud", "non fraud","error","fraud", "non fraud","error"))
      svm.b=svm.cmd[,2]
      svm.a=svm.cmd[,1]
      svm.c=svm.cmd[,3]
      svm.Y =c(svm.a,svm.b,svm.c)
      
      svm.df <- data.frame(predicted, true, svm.Y)
      
      ggplot(data =  svm.df, mapping = aes(x = predicted, y =true )) +
        geom_tile(aes(fill = svm.Y), colour = "white") +
        geom_text(aes(label = sprintf("%1.0f", svm.Y)), vjust = 1) +
        scale_fill_gradient(low = "light blue", high = "green") +
        theme_bw() + theme(legend.position = "none")

      })
    
    
    # output$rmatrix.svm <- renderPlot({
    #   
    #   asvm.cm=as.data.frame(svm.cm()[["relative.col"]]) 
    #   
    #   predicted <- factor(c("fraud", "fraud","fraud", "non fraud","non fraud","non fraud"))
    #   true <- factor(c("fraud", "non fraud","error","fraud", "non fraud","error"))
    #   asvm.a=asvm.cm[,1]
    #   asvm.b=asvm.cm[,2]
    #   #c=cmd[,3]
    #   asvm.Y =c(asvm.a,asvm.b)
    #   
    #   asvm.df <- data.frame(predicted, true, asvm.Y)
    #   
    #   ggplot(data =  asvm.df, mapping = aes(x = predicted, y =true )) +
    #     geom_tile(aes(fill = asvm.Y), colour = "white") +
    #     geom_text(aes(label = sprintf("%1.0f", asvm.Y)), vjust = 1) +
    #     scale_fill_gradient2(mid="light blue", high = "blue") +
    #     theme_bw() + theme(legend.position = "none")
    # })
    
    output$roc.svm <- renderPrint({
        calculateROCMeasures(pred.svm())
    })
    
    
    df <- reactive({ generateThreshVsPerfData(pred.svm(), measures = list(fpr, tpr, ppv, tnr,mmce))
    })
    
    
    output$graph1.svm <- renderPlotly({
         #plotROCCurves(df(), measures = list(tpr, ppv), diagonal = FALSE)
         plo1=ggplot(df()$data)+geom_line(aes(x=tpr,y=ppv),color="dark green")+theme_bw()+
        xlab("True positive rate")+ylab("Positive predict rate")
         plotly_build(plo1)
        
     })
    
  
    output$graph2.svm <- renderPlotly({
        # plotROCCurves(df(), measures = list(tnr, tpr), diagonal = FALSE)
        plo2=ggplot(df()$data)+geom_line(aes(x=tnr,y=tpr),color="dark green")+theme_bw()+
            xlab("True negative rate")+ylab("True positive rate")
        plotly_build(plo2)
    
    })
    
     output$graph3.svm <- renderPlotly({
    #     plotROCCurves(df(), measures = list(fpr, tpr), diagonal = TRUE)
         plo3=ggplot(df()$data)+geom_line(aes(x=fpr,y=tpr),color="dark green")+theme_bw()+
             xlab("false positive rate")+ylab("True positive rate")
         plotly_build(plo3)
    })
    
    
    
    #random forest
    train.rf <- reactive({ 
        
        learner.rf <- makeLearner(cl="classif.randomForest",predict.type="prob",
                                  par.vals = list(ntree = input$ntree.param,
                                                  mtry = input$mtry.param))
        
        train.rf <- mlr::train(learner.rf,train.task())
        
        return(train.rf)
    }) 
    
    
    pred.rf <- reactive({ 
        predict(train.rf(),test.task())
    }) 
    
    rf.cm <- reactive ({
      calculateConfusionMatrix(pred.rf(),relative = TRUE)
    })
    
    output$amatrix.rf <- renderPlot({
      
      rf.cm=as.data.frame(rf.cm()[["result"]]) 
      
      predicted <- factor(c("fraud", "fraud", "fraud", "non fraud", "non fraud", "non fraud","error","error","error"))
      true <- factor(c("fraud", "non fraud","error", "fraud", "non fraud","error","fraud", "non fraud","error"))
      rf.b=rf.cm[,2]
      rf.a=rf.cm[,1]
      rf.c=rf.cm[,3]
      rf.Y =c(rf.a,rf.b,rf.c)
      
      rf.df <- data.frame(predicted, true, rf.Y)
      
      ggplot(data =  rf.df, mapping = aes(x = predicted, y =true )) +
        geom_tile(aes(fill = rf.Y), colour = "white") +
        geom_text(aes(label = sprintf("%1.0f", rf.Y)), vjust = 1) +
        scale_fill_gradient(low = "light blue", high = "green") +
        theme_bw() + theme(legend.position = "none")
      
    })
    # 
    # output$ramatrix.rf <- renderPlot ({
    #   rrf.cm=as.data.frame(rf.cm()[["relative.col"]]) 
    #   
    #   predicted <- factor(c("fraud", "fraud","fraud", "non fraud","non fraud","non fraud"))
    #   true <- factor(c("fraud", "non fraud","error","fraud", "non fraud","error"))
    #   rrf.a=rrf.cm[,1]
    #   rrf.b=rrf.cm[,2]
    #   #c=cmd[,3]
    #   rrf.Y =c(rrf.a,rrf.b)
    #   
    #   rrf.df <- data.frame(predicted, true, rrf.Y)
    #   
    #   ggplot(data =  rrf.df, mapping = aes(x = predicted, y =true )) +
    #     geom_tile(aes(fill = rrf.Y), colour = "white") +
    #     geom_text(aes(label = sprintf("%1.0f", rrf.Y)), vjust = 1) +
    #     scale_fill_gradient2(mid="light blue", high = "blue") +
    #     theme_bw() + theme(legend.position = "none")
    #   
    # })
    # 
    output$roc.rf <- renderPrint({
        calculateROCMeasures(pred.rf())
    })
    
    
    df.rf <- reactive({ generateThreshVsPerfData(pred.rf(),
                                                 measures = list(fpr, tpr, ppv, tnr,mmce))
    })
    
    
    output$graph1.rf <- renderPlotly({
       # plotROCCurves(df.rf(), measures = list(tpr, ppv), diagonal = FALSE)
        pp=ggplot(df.rf()$data)+geom_line(aes(x=tpr,y=ppv),color="blue")+theme_bw()
        plotly_build(pp)
        
    })
    
    output$graph2.rf <- renderPlotly({
       # plotROCCurves(df.rf(), measures = list(tnr, tpr), diagonal = FALSE)
       bbp= ggplot(df.rf()$data)+geom_line(aes(x=tnr,y=tpr),color="blue")+theme_bw()
        plotly_build(bbp)
    })
    
    output$graph3.rf <- renderPlotly({
       # plotROCCurves(df.rf()$data, measures = list(fpr, tpr), diagonal = TRUE)
        tt=ggplot(df.rf()$data)+geom_line(aes(x=fpr,y=tpr),color="blue")+theme_bw()
        plotly_build(tt)
    })
    
    
    # benchmarking on smote data sets 
    rdesc=reactive({ makeResampleDesc("CV", iters = input$k, stratify = TRUE)
    })
    
    bmr=reactive({ 
        ms = list(mlr::auc, mmce)
        lrns = list(makeLearner("classif.svm", predict.type = "prob"),
                    makeLearner("classif.rpart", predict.type = "prob"),
                    makeLearner("classif.randomForest", predict.type = "prob"),
                    makeLearner("classif.logreg", predict.type = "prob")
        )
        bmr = benchmark(lrns, train.task(), rdesc(), measures = ms, models = TRUE)
        return(bmr)
    })
    
    output$bmr1 <- renderPrint({
        bmr()
    })
    
    df.bmr <- reactive({ generateThreshVsPerfData(bmr(),
                                                  measures = list(fpr, tpr, ppv, tnr,mmce))
    })
    
    
    output$graph1.bmr <- renderPlotly({
       ta= plotROCCurves(df.bmr(), measures = list(tpr, ppv), diagonal = FALSE)
       plotly_build(ta)
        #ggplot(df.bmr()$data)+geom_line(aes(x=tpr,y=ppv))+theme_bw()
    })
    
    output$graph2.bmr <- renderPlotly({
        dd=plotROCCurves(df.bmr(), measures = list(tnr, tpr), diagonal = FALSE)
        plotly_build(dd)
        #ggplot(df.bmr()$data)+geom_line(aes(x=tnr,y=tpr))+theme_bw()
    })
    
    output$graph3.bmr <- renderPlotly({
        ti=plotROCCurves(df.bmr(), measures = list(fpr, tpr), diagonal = TRUE)
        plotly_build(ti)
        #ggplot(df.bmr()$data)+geom_line(aes(x=fpr,y=tpr))+theme_bw()
    })
    
    output$bmrplot <- renderPlot({
        plotBMRBoxplots(bmr(), measure = mmce, order.lrn = getBMRLearnerIds(bmr()))
        #plot_ly(bmr()$data, type = "box")
    })
    # 
    # output$bmri <- renderPlot({
    #     plot(bmri())
    #     
    #     
    # })
    
    
    compar = reactive({  generateThreshVsPerfData(list(
      
      logreg = pred.lg(),
      tree = pred.tree(),
      randomForest = pred.rf(),
      svm = pred.svm()), 
      measures = list(fpr, tpr, ppv, tnr,mmce))
    })
    
    output$graph1.compar <- renderPlotly({
      pc1= plotROCCurves(compar(), measures = list(tpr, ppv), diagonal = FALSE)
      plotly_build(pc1)

    })
    output$graph2.compar <- renderPlotly({
      pc2=plotROCCurves(df.bmr(), measures = list(tnr, tpr), diagonal = FALSE)
      plotly_build(pc2)
      
    })
    output$graph3.compar <- renderPlotly({
      pc3=plotROCCurves(df.bmr(), measures = list(fpr, tpr), diagonal = TRUE)
      plotly_build(pc3)
    
    })
    
    
    
    
})
