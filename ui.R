# UI

library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    # shinythemes::themeSelector(),
    #missmap
    theme=shinytheme(theme = "united"),
    
    titlePanel("SVM project on R Shiny"),
    actionButton("download","download"),
    
        h2("I.data visualisation",
           h4("our database contains the list of clients of a bank. 
              Customers can be categorized into two groups, which are customers in fraud situations and non-fraud customers."),
           h3("1.first view of the database"),
           h4("the database contains 31 variables. the variable time, the variable amounts, 
              the variable class and the 28 other variables are main components.
              We present in the following table a small preview of our database. 
              In this table we can see the variables time, amount, class and the first five main components"),
           br(),
           dataTableOutput("tableDTt"),
           h3("2.some descriptive statistics"),
           dataTableOutput("tableDTs")
       
        ),
        
        h2("II.rebalance database",
                 
                 h3("Class distribution in the Original database"),
                 #verbatimTextOutput("tableDT1"),
                 #verbatimTextOutput("tableDT"),
                 dataTableOutput("tableDT2"),
                 h4("our database contains a total of 284808 observations including 
              492 observations for individuals in a fraud situation and the rest for individuals in non-fraud situations. Which allows us to notice that our database is not balanced. 
              We have 99.83% of non fraud and 0.17% of fraud. before any modeling, we will rebalance the database to implement the calculations more easily"),
                 
                 h3("Distribution of Class in the training database"),
                 verbatimTextOutput("tabletrain1"),
                 verbatimTextOutput("tabletrain"),
                 
                 h3("Rebalancing training database using Smote"),
                 
                 sliderInput("ksmote","number of nearest neighbours that are used 
                             to generate the new examples of fraud",
                             3,10,value=5),
                 
                 sliderInput("over","oversampling fraud case",
                             100,1000,value=500,round = T),
                 
                 sliderInput("under","undersampling non fraud case",
                             50,100,value=80,round = T),
                 
                 h4("Distribution of Class in the Smote data sets"),
                 verbatimTextOutput("tableSmote1"),
                 verbatimTextOutput("tableSmote"),
                 plotOutput("filtre")
                 
                 
        ),
                h2("III.the models"),
    
    navlistPanel(
        "list of models to test",
        
        
        #logistic regression
        tabPanel("logistic regression",
                 h2("logistic regression model"),
                 
                 
                 h4("confusion matrix"),
                 verbatimTextOutput("matrix.lg"),
                 
                 h4("ROC measures"),
                 verbatimTextOutput("roc.lg"),
                 
                 h4("Precision/recall graph"),
                 plotOutput("graph1.lg"),
                 
                 h4("Sensitivity/specificity graph"),
                 plotOutput("graph2.lg"),
                 
                 h4("roc curve"),
                 plotOutput("graph3.lg")
                 
        ),
        
        
        
        
        #svm
        tabPanel("SVM",
                 h1("SVM model"),
                 
                 h3("hyperparameters's choise"),
                 
                 sliderInput("cost.param","choice of cost parameters",
                             0,10,value=1,round = T),
                 
                 selectInput("kernel.param", "choice of kernel", 
                             choices= c("linear","sigmoid","polynomial","radial")),
                 
                 h4("confusion matrix"),
                 verbatimTextOutput("matrix.svm"),
                 
                 h4("ROC measures"),
                 verbatimTextOutput("roc.svm"),
                 
                 h4("Precision/recall graph"),
                 plotOutput("graph1.svm"),
                 
                 h4("Sensitivity/specificity graph"),
                 plotOutput("graph2.svm"),
                 
                 h4("roc curve"),
                 plotOutput("graph3.svm")
                 
        ),
        
        # decision tree
        tabPanel("decision tree",
                 h1("decision tree"),
                 h5("construction of the decision tree"),
                 plotOutput("tree"),
                 h3("the result analysis"),
                 
                 h4("confusion matrix"),
                 verbatimTextOutput("matrix.tree")
                 
                 h4("ROC measures"),
                 verbatimTextOutput("roc.tree"),
                 
                 h4("Precision/recall graph"),
                 plotOutput("graph1.tree"),
                 
                 h4("Sensitivity/specificity graph"),
                 plotOutput("graph2.tree"),
                 
                 h4("roc curve"),
                 plotOutput("graph3.tree")
                 
        ),
        
        
        
        
        #random forest
        tabPanel("random forest",
                 h1("random forest model"),
                 
                 h3("choice of hyperparameters"),
                 
                 sliderInput("ntree.param","Number of tree",
                             100,500,value=200,round = T),
                 
                 sliderInput("mtry.param", "Number of variables randomly sampled as candidates at each split", 
                             3,8,value=5),
                 
                 h4("confusion matrix"),
                 verbatimTextOutput("matrix.rf"),
                 
                 h4("ROC measures"),
                 verbatimTextOutput("roc.rf"),
                 
                 h4("Precision/recall graph"),
                 plotOutput("graph1.rf"),
                 
                 h4("Sensitivity/specificity graph"),
                 plotOutput("graph2.rf"),
                 
                 h4("roc curve"),
                 plotOutput("graph3.rf")
                 
        ),
      
        # model comparision
        tabPanel("model comparison",
                 h1("comparison of different models"),
                 h5("We chose to re-sample the model in two ways that are: k-fold and holdout. "),
                 h4("resampling using k-fold method"),
                 sliderInput("k","chose your K ",0,10,value=5,round = T),
                 verbatimTextOutput("bmr1"),
                 
                 h4("Precision/recall graph"),
                 plotOutput("graph1.bmr"),
                 
                 h4("Sensitivity/specificity graph"),
                 plotOutput("graph2.bmr"),
                 
                 h4("roc curve"),
                 plotOutput("graph3.bmr"),
                 
                 h4("distribution of performance values across resampling iterations for
                    one performance measure and for all learners"),
                 plotOutput("bmrplot")
                 
        )
        
        
        
    )))
