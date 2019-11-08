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
    
    navlistPanel(
    
    tabPanel("I.data visualisation",
             
        h1("I.data visualisation"),
           br(),
           br(),
           h4("our database contains the list of clients of a bank. 
              Customers can be categorized into two groups, which are customers in fraud situations and non-fraud customers."),
           br(),
           h3("1.first view of the database"),
           br(),
           h4("the database contains 31 variables. the variable time, the variable amounts, 
              the variable class and the 28 other variables are main components.
              We present in the following table a small preview of our database. 
              In this table we can see the variables time, amount, class and the first five main components"),
           br(),
           dataTableOutput("tableDTt"),
           br(),
           h3("2.some descriptive statistics"),
           br(),
           h4("We look at some basic descriptive statistics to analyze the variables that make up our models. On the first page of the table we see the statistics for the first two variables. 
              To see the static of the other variables, go to the following pages"),
           br(),
           dataTableOutput("tableDTs")
       
        ),
        
    tabPanel("II.rebalancing",
             
        h1("II.rebalance database"),
                 
                 h3("1.Class distribution in the Original database"),
           
               br(),
           
                 h4("our database contains a total of 284808 observations including 
              492 observations for individuals in a fraud situation and the rest for individuals in non-fraud situations. Which allows us to notice that our database is not balanced. 
              We have 99.83% of non fraud and 0.17% of fraud. before any modeling, we will rebalance the database to implement the calculations more easily"),
                 h4("Because when a database is unbalanced than pushing the model to predict the most prevalent modality. 
                    To rebalance the database we use the smote function"),
           
                 br(),
                 dataTableOutput("tableDT2"),
           ###################################################################
           # debut modifications !!!!
           br(),
           h3("2.Distribution of Class in the SMOTE database"),
           br(),
           h4("Machine learning algorithms have trouble learning when one class heavily
                dominates the other.
                So we create a new rebalanced datasets using SMOTE 
                    :Synthetic Minority Over-sampling Technique."),    
           "SMOTE synthesises new minority instances between existing (real) minority instances.",
           
           br(),
           sliderInput("size","Choose Smote sample size",
                       1000,80000,value=40000,round = T),
           
           sliderInput("kn","Number of the closest neighbours are considered for synthesis
                             new cases of fraud",
                       3,10,value=4),
           sliderInput("prop","Number of times existing data points get reused for 
                             synthesis new cases of fraud",
                       50,100,value=90,round = T),
           h4("Proportion of Fraud/NonFraud"),
           verbatimTextOutput("smote.prop"),
           
           h4("Number of Fraud/NonFraud"),
           verbatimTextOutput("smote"),
           
           br(),
           
           h4("After rebalancing the database we will split it into two parts. 
              A test sample that made up 30 percent of our samples and a learning sample for the other 70 percent"),
           br(),
           
           h4("Features selection"),
           "Entropy-based Filters: Algorithms that find ranks of importance of 
           discrete attributes, basing on their entropy with a continous class attribute",
           plotlyOutput("filtre")
           
           
        ),
    ###################
    # fin des modifications !!!!!
    
         #       h2("III.the models"),

        
        
        #svm
        tabPanel("III.SVM",
            
                 h1("III.SVM model"),
                 
                 br(),
                 h2("Theoretical Explanation of SVM "),
                 h3("Introduction"),
                 h4("Support vector machines (SVM), introduced by Vladimir Vapnik (Vapnik (1995, 1998)), are a set of supervised learning techniques designed to solve classification or regression problems.
In the case of a classification problem, we are reffering to SVM (support vector machine) and in the case of a regression, we are reffering to SVR (support vector regression).
For the presentation of the project, we will expose the classification problem (Fraud or Non-Fraud)
SVMs are based on two key ideas:"),
                 h3("1. The notion of maximum margin"),
                 h3("2.The notion of kernel function"),
                
                   br(),
                 h3("1. The notion of maximum margin"),
                 br(),
                 h4("In the SVM, the optimal separation boundary is chosen as the one that maximizes the margin, with the margin being the distance between the separation (hyperplane) boundary and the nearest observations.
The problem is to find this optimal dividing boundary from a learning set and the solution is to state the problem as a quadratic optimization problem."),
                 h3("2.The notion of kernel function"),
                 h4("In the case where the data is not linearly separable, the second key idea of the SVM is to transform the representation space of the input data into a larger dimension space (possibly of infinite dimension), in which it is probable that there is a linear separation.
The trick is to use a kernel function that does not require the explicit knowledge of the transformation to be applied for the space change.
The kernel functions make it possible to transform a scalar product (expensive computation in a large space) into a simple point evaluation of a function."),
                 h3("SVM'S Intuition "),
                 br(),
                 h4("We are interested in a phenomenon f (deterministic or stochastic) which, from a certain set of inputs (predictors, variables) x, produces an output (label) y = f (x).
The goal is to find f (unknown) from a training sample {xi, yi}, i = 1, ..., n.
We are looking for a function (classifier) g that minimizes the classification error probability, but rather than building g directly, we usually build a decision function h that is associated to the classifier.
The classifier takes the value 1 for a decision function greater or equal to 0 and -1 for a value inferior to 0 because in continuous variables, the probability of being at a point is zero. 
We introduce the notion of separating hyperplane H which separates the space of the input data X into two half-spaces corresponding to the two classes provided for y. It is defined by the equation of the decision function that equals 0.
The probability of making a classification error is the probablity of having an y different than the classifier g"),
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
                 plotlyOutput("graph1.svm"),
                 
                 h4("Sensitivity/specificity graph"),
                 plotlyOutput("graph2.svm"),
                 
                 h4("roc curve"),
                 plotlyOutput("graph3.svm")
                 
        ),
        
        #logistic regression
        tabPanel("IV.logistic regression",
                 h2("IV.logistic regression model"),
                 
                 
                 h4("confusion matrix"),
                 verbatimTextOutput("matrix.lg"),
                 
                 h4("ROC measures"),
                 verbatimTextOutput("roc.lg"),
                 
                 h4("Precision/recall graph"),
                 plotlyOutput("graph1.lg"),
                 
                 h4("Sensitivity/specificity graph"),
                 plotlyOutput("graph2.lg"),
                 
                 h4("roc curve"),
                 plotlyOutput("graph3.lg")
                 
        ),
        # decision tree
        tabPanel("V.decision tree",
                 
                 h1("V.decision tree"),
                 
                 h5("construction of the decision tree"),
                 
                 plotOutput("tree"),
                 
                 h3("the result analysis"),
                 
                 h4("confusion matrix"),
                 verbatimTextOutput("matrix.tree"),
                 
                 h4("ROC measures"),
                 verbatimTextOutput("roc.tree"),
                 
                 h4("Precision/recall graph"),
                 plotlyOutput("graph1.tree"),
                 
                 h4("Sensitivity/specificity graph"),
                 plotlyOutput("graph2.tree"),
                 
                 h4("roc curve"),
                 plotlyOutput("graph3.tree")
                 
        ),
        
        
        
        
        #random forest
        tabPanel("VI.random forest",
                 
                 h1("VI.random forest model"),
                 
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
                 plotlyOutput("graph1.rf"),
                 
                 h4("Sensitivity/specificity graph"),
                 plotlyOutput("graph2.rf"),
                 
                 h4("roc curve"),
                 plotlyOutput("graph3.rf")
                 
        ),
      
        # model comparision
        tabPanel("VII.model comparison",
                 
                 h1("VII.comparison of different models"),
                 h5("We chose to re-sample the model in two ways that are: k-fold and holdout. "),
                 h4("resampling using k-fold method"),
                 sliderInput("k","chose your K ",0,10,value=5,round = T),
                 verbatimTextOutput("bmr1"),
                 
                 h4("Precision/recall graph"),
                 plotlyOutput("graph1.bmr"),
                 
                 h4("Sensitivity/specificity graph"),
                 plotlyOutput("graph2.bmr"),
                 
                 h4("roc curve"),
                 plotlyOutput("graph3.bmr"),
                 # 
                 # plotOutput("bmri"),
                 
                 h4("distribution of performance values across resampling iterations for
                    one performance measure and for all learners"),
                 plotOutput("bmrplot")
                 
        )
        
        
        
  )  ))
