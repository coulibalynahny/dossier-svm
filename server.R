# SERVER

library(shiny)
library(mlr)
library(rpart)
library(rpart.plot)
library(e1071)
library(caret)
library(DMwR)
library(FSelectorRcpp)
library(randomForest)
library(data.table)
library(DT)
library(ggplot2)
library(plotly)

<<<<<<< HEAD
<<<<<<< HEAD
DT=read.csv("C:/Users/nahny/Documents/GitHub/dossier-svm/creditcard.csv", stringsAsFactors = FALSE )
=======
DT=read.csv("C:/Users/pierr/Desktop/svm/dossier-svm/creditcard.csv", stringsAsFactors = FALSE )
>>>>>>> ef10eb7d104b21b358202e7748ce73a6d92ad55a
=======

DT=read.csv("C:/Users/33668/Documents/MASTER 2 ESA/projet svm/app_0611/creditcard.csv", stringsAsFactors = FALSE )

#DT=read.csv("C:/Users/pierr/Desktop/svm/dossier-svm/creditcard.csv", stringsAsFactors = FALSE )

>>>>>>> 321309851f07192a13fa9553f5d631958377a61a


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


set.seed(1234)
inTrain = createDataPartition(DT$Class, p = 0.7,list=FALSE)
data.train = as.data.frame(DT[inTrain,])  
data.test = as.data.frame(DT[-inTrain,])


shinyServer(function(input, output) {
    
    # on reequilibre l'echantillon train avec smote, on definit un echantillon nomé data.smote
    data.smote <- reactive({ 
        as.data.frame(SMOTE(Class ~., data.train, perc.over = input$over, k = input$ksmote,
                            perc.under = input$under))
        
    }) 
    # table dt  
    #output$tableDT1 <- renderPrint({
       # table(DT$Class)
    #})
    #output$tableDT <- renderPrint({
        #prop.table(table(DT$Class))
    #})
    

    output$tableDT2<-renderDT({
         DT::datatable(table_prop,list(dom = 't'))
    })
    
    output$tableDTt<-renderDT({
        DT::datatable(tableex)
    })
    
    output$tableDTs<-renderDT({
        DT::datatable(stat)
    })
    
    # table data.train
    output$tabletrain1 <- renderPrint({
        table(data.train$Class)
    })
    output$tabletrain <- renderPrint({
        prop.table(table(data.train$Class))
    })
    # table data.smote
    output$tableSmote1 <- renderPrint({
        table(data.smote()$Class)
    })
    output$tableSmote <- renderPrint({
        prop.table(table(data.smote()$Class))
    })
    
    ###############################################################################
    # MODELISATION
    ###############################################################################
    
    
    
    # dans le package mlr on definit une tache qui permet de specifier une fois pour toute le probleme que nous traitons
    #cest a dire la base de donee utilisee, la variable cible et la modalite cible
    train.task <- reactive({
        makeClassifTask(data=data.smote(),target="Class",positive = "Fraud")
    })
    
    test.task <- reactive({
        makeClassifTask(data=data.test,target="Class",positive = "Fraud")
    })
    
    filter.kruskal <- reactive({
        mlr::generateFilterValuesData(task=train.task(),method="FSelectorRcpp_information.gain")
    })
    
    output$filtre <- renderPlot({ mlr::plotFilterValues(filter.kruskal(),n.show=10)
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
    
    
    output$graph1.lg <- renderPlot({
        plotROCCurves(df.lg(), measures = list(tpr, ppv), diagonal = FALSE)
    })
    
    output$graph2.lg <- renderPlot({
        plotROCCurves(df.lg(), measures = list(tnr, tpr), diagonal = FALSE)
    })
    
    output$graph3.lg <- renderPlot({
        plotROCCurves(df.lg(), measures = list(fpr, tpr), diagonal = TRUE)
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
    
    
    output$graph1.svm <- renderPlot({
         #plotROCCurves(df(), measures = list(tpr, ppv), diagonal = FALSE)
         ggplot(df()$data)+geom_line(aes(x=tpr,y=ppv),color="blue")+theme_bw()+
        xlab("True positive rate")+ylab("Positive predict rate")
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
    
    output$graph2.svm <- renderPlot({
        # plotROCCurves(df(), measures = list(tnr, tpr), diagonal = FALSE)
        ggplot(df()$data)+geom_line(aes(x=tnr,y=tpr),color="orange")+theme_dark()+
            xlab("True negative rate")+ylab("True positive rate")
    
    })
    
     output$graph3.svm <- renderPlot({
    #     plotROCCurves(df(), measures = list(fpr, tpr), diagonal = TRUE)
         ggplot(df()$data)+geom_line(aes(x=fpr,y=tpr),color="green",size=0.6)+theme_bw()+
             xlab("false positive rate")+ylab("True positive rate")
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
    
    
    output$graph1.rf <- renderPlot({
        plotROCCurves(df.rf(), measures = list(tpr, ppv), diagonal = FALSE)
    })
    
    output$graph2.rf <- renderPlot({
        plotROCCurves(df.rf(), measures = list(tnr, tpr), diagonal = FALSE)
    })
    
    output$graph3.rf <- renderPlot({
        plotROCCurves(df.rf(), measures = list(fpr, tpr), diagonal = TRUE)
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
    
    
    output$graph1.bmr <- renderPlot({
        plotROCCurves(df.bmr(), measures = list(tpr, ppv), diagonal = FALSE)
    })
    
    output$graph2.bmr <- renderPlot({
        plotROCCurves(df.bmr(), measures = list(tnr, tpr), diagonal = FALSE)
    })
    
    output$graph3.bmr <- renderPlot({
        plotROCCurves(df.bmr(), measures = list(fpr, tpr), diagonal = TRUE)
    })
    
    output$bmrplot <- renderPlot({
        plotBMRBoxplots(bmr(), measure = mmce, order.lrn = getBMRLearnerIds(bmr()))
    })
    
})
