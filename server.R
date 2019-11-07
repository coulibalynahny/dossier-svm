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

# chemin d'acces

#DT=read.csv("C:/Users/nahny/Documents/GitHub/dossier-svm/creditcard.csv", stringsAsFactors = FALSE )

# DT=read.csv("C:/Users/pierr/Desktop/svm/dossier-svm/creditcard.csv", stringsAsFactors = FALSE )

# DT=read.csv("C:/Users/33668/Documents/MASTER 2 ESA/projet svm/app_0611/creditcard.csv", stringsAsFactors = FALSE )


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
 

shinyServer(function(input, output) {
    
    ## PARTIR NAHNY
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
  
    # creation of smote database 
    DT.smote <- reactive({
        DT.smote=smotefamily::SMOTE(DT[,-31], DT[,31], K = input$kn, 
                                    dup_size = input$prop)$data
        names(DT.smote)[31]="Class"
        s=sample(1:nrow(DT.smote),input$size)
        DT.smote=DT.smote[s,]
        return(DT.smote)
    }) 
    
    output$tableSmote <- renderDT({
        
        ab=table(DT.smote()$Class)
        ab1= prop.table(table(DT.smote()$Class))
        at=rbind(ab,ab1)
        row.names(at)<-c("Number","Proportion (in %)")
        at[2,]<-round(at[2,],4)*100
        
        DT::datatable(at,list(dom = 't'))
    })
    
    
    
    # table data.train
    output$smote <- renderPrint({
        table(DT.smote()$Class)
    })
    output$smote.prop <- renderPrint({
        prop.table(table(DT.smote()$Class))
    })
    
    
    # splitting train (70%) / test (30%)
    inTrain <- reactive({ createDataPartition(DT.smote()$Class, p = 0.7,list=FALSE) }) 
    data.train <- reactive({ DT.smote()[inTrain(),]  }) 
    data.test <- reactive({ DT.smote()[-inTrain(),]  }) 
    
    
    ###############################################################################
    # MODELISATION
    ###############################################################################
    
    
    
    # dans le package mlr on definit une tache qui permet de specifier une fois pour toute le probleme que nous traitons
    #cest a dire la base de donee utilisee, la variable cible et la modalite cible
    train.task <- reactive({
        makeClassifTask(data=data.test(),target="Class",positive = "Fraud")
    })
    
    test.task <- reactive({
        makeClassifTask(data=data.test(),target="Class",positive = "Fraud")
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
    
    
    output$matrix.lg <- renderPrint({
        calculateConfusionMatrix(pred.lg(),relative = TRUE)
    })
    
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
    
    
    output$matrix.tree <- renderPrint({
        calculateConfusionMatrix(pred.tree(),relative = TRUE)
    })
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
    
    
    output$matrix.svm <- renderPrint({
        calculateConfusionMatrix(pred.svm(),relative = TRUE)
    })
    
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
    
#     output$graph1.svm <- renderPlot({
#        print(df()$data[,c("tpr")])
#        print(base::rep(df()$data[,c("tpr")],times=2))
#         data_2<-data.frame(tpr=base::rep(df()$data[,c("tpr")],times=2),
#                                  values=c(df()$data[,c("tnr")],df()$data[,c("fpr")]),
#                                  group=c(base::rep("graph 1",dim(df()$data[,c("tpr")])[1]),base::rep("graph 2",dim(df()$data[,c("tpr")])[1])))
# print(data_2)
# print(df()$data[,c("tpr")])
#         plotROCCurves(df(), measures = list(tpr, ppv), diagonal = FALSE)
#         ggplot(data_2)+geom_line(aes(x=tpr,y=values,group=group),color="blue",size=3)+theme_minimal()+
#              xlab("True positive rate")+ylab("Positive predict rate")
#     })
    
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
    
    
    output$matrix.rf <- renderPrint({
        calculateConfusionMatrix(pred.rf(),relative = TRUE)
    })
    
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
    
    
    # benchmarking
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
    
})
