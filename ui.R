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
    
    tabPanel("I.Data Visualisation",
             
        h1("I.Data Visualisation"),
           br(),
           br(),
           h4("Our database contains the list of clients of a well-known bank. These customers are classed as being fraudulent(1) or non-fraudulent(0).In the following presentation we will implement new techniques
in order to ameliorate the fraud system model."),
           br(),
           h3("1.First view of the database"),
           br(),
           h4("The database includes 31 variables:the variable time, the variable amounts, the variable class and the 28 other variables are main components.
              We will present in the following table a preview of a part of our database variables: time, amount, class and the first five main components"),
           br(),
           dataTableOutput("tableDTt"),
           br(),
           h3("2.Some descriptive statistics"),
           br(),
           h4("We are looking now at some basic descriptive statistics to analyze the variables that make up our model. On the first page of the table we see the statistics for the first two variables. To see the statistics of the other variables, go to the following pages.As you can see, the variables 
Amount and time have been centered and reduced(mean equals 0) for all variables.The main composants are centered and reduced from the beginning."),
           br(),
           dataTableOutput("tableDTs")
       
        ),
        
    tabPanel("II.Rebalancing",
             
        h1("II.Rebalance the database"),
                 
                 h3("1.Class distribution in the Original database"),
           
               br(),
           
                 h4("Our database contains a total of 284808 observations including 492 observations for individuals in a fraud situation and the rest for individuals in non-fraud situations,which allows us to notice that our database is unbalanced. We have a 99.83% proportion of non-fraud and 0.17% proportion of fraud. 
                    Before issuing any Data Modeling Technique,we will rebalance the database in order to have accurate results."),
                 h4("When a database is unbalanced the model is conducive to predict the most prevailing modality."),
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
           "SMOTE is a oversampling technique which synthesizes a new minority instance between a pair of one minority instance and one of its K nearest neighbor.",
           br(),
           sliderInput("size","Choose Smote sample size",
                       1000,10000,value=5000,round = T),
           
           sliderInput("kn","The value of parameter K for nearest neighbor process used for generating data",
                       3,10,value=4),
           sliderInput("prop","The maximum times of synthetic minority instances over original majority in-stances in the oversampling",
                       50,100,value=90,round = T),
           
           h4("Distribution of the database after rebalancing"),
           dataTableOutput("smote"),
           
           br(),
           
           h4(" We will split our Database into two parts: a test sample that makes up 30 percent of our sample and a training sample for the other 70 percent and then we're rebalancing the learning sample using the Smote Function"),
           br(),
           
           h4("Features selection"),
           "Entropy-based Filters: Algorithms that find ranks of importance of 
           discrete attributes, basing on their entropy with a continous class attribute",
           plotlyOutput("filtre")
           
           
        ),
    ###################
    # fin des modifications !!!!!
    
         #       h2("III.The models"),

        
        
        #svm
        tabPanel("III.SVM",
            
                 h1("III.SVM model"),
                 
                 br(),
                 
                 h2("Theoretical Explanation of SVM "),
                 
                 h3("Introduction"),
                 
                 h4("Support vector machines (SVM), introduced by Vladimir Vapnik (Vapnik (1995, 1998)), are a set of supervised learning techniques designed to solve classification or regression problems.
In the case of a classification problem, we are reffering to SVM (support vector machine) and in the case of a regression, we are reffering to SVR (support vector regression).
For the presentation of the project, we will expose the classification problem (Fraud or Non-Fraud)
SVMs are based on two key ideas:
                 The notion of maximum margin
                 The notion of kernel function"),
                
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
                 h3("Linearly Separable Sample"),
                 br(),
                 h4("A sample is linearly separable if it there is a linear classifier that correctly classifies all the observations in the sample.If all classifications g (x) = 1 are located in the area above the separating hyperplane and all the classifications g (x) = -1 are below the separator hyperplane, there is no classification error.
For a linearly separable sample, there may be several linear classifiers ( several pairs (w, b)) whose learning performance is identical(without misclassification) .
                    To select the optimal hyperplane: it is the criterion of optimal margins. 
                    In the case of a linearly separable sample, the support vector machine (SVM) is the linear classifier ( w*, b *) that perfectly classifies all observations on the training sample and is associated with the largest margin (The margin is twice the distance d from the closest point(support vector) to the hyperplane"),
                 h3("It is now assumed that the training sample is not linearly separable.
Two cases:"),
                 br(),
                 h4("1. The sample is almost linearly separable: the 'optimal' separation is
linear, but some observations can not be correctly classified."),
                 h4("2.The sample is not linearly separable: the 'optimal' separation is
                    non-linear."),
                 h4("1.In the case of an almost linearly separable sample, we introduce n relaxation variables of the classification constraints yi h (xi)> 1, which are called slack variables and which are well
classified but below the margin if they are between 0 and 1, or even poorly classified, if they exceed 1.
                    The penalization parameter C controls the arbitration between the size of the margin and the error rate. If C is small, classification errors are less penalized and the focus is on maximizing the margin. There is a risk of under-fitting(poor classification rate on the sample)
                    If it is large, the emphasis is on the absence of misclassification at the cost of a lower margin. There is a risk of over-fitting."),
                 br(),
                 h4("In practice, the performances of the SVM are very sensitive to the choice of the parameter of penalization (cost parameter) C. How to select an optimal hyper-parameter C ?"),
                 h3("First solution: cross validation"),
                 br(),
                 h4("1. The sample is partitioned into 3 learning samples (training), validation (validation) and test (test) by random undersampling. The test sample is used to evaluate the out - of - sample performance of the classifier."),
                 h4("2. On the training sample, the SVM is applied for different values of C."),
                 h4("3.The next step is to determine the value of C that minimizes the probability of misclassification measured on the validation sample."),
                 br(),
                 h3("Second solution: k fold cross validation"),
                 br(),
                 h4("The original sample is divided into k samples, then one of the k samples is selected as the validation set and the other k-1 samples are the training set. 
                    The performance score is calculated,the probability of making a classification error is evaluated. We are looking for the value of C which minimizes the average of the probabilities of errors. and the operation is repeated by selecting another validation sample from the k-1 samples that have not yet been used for model validation. 
                    The operation is repeated k times so that ultimately each sub-sample has been used exactly once as a validation set."),
                 br(),
                 h3("Most classification problems are non-linear separations. But the data space can always be plunged into a larger space in which the data can be linearly separated "),
                 h2("Kernel trick"),
                 br(),
                 h4("In practice, the kernel trick consists in choosing a kernel function, which represents the scalar product associated with the intermediate representation space without necessarily characterizing the space and the transformation function."),
                 h3("Hyperparameters's choice"),
                 
                 sliderInput("cost.param","Choice of cost parameters",
                             0,10,value=1,round = T),
                 
                 selectInput("kernel.param", "Choice of kernel", 
                             choices= c("linear","sigmoid","polynomial","radial")),
                 
                 h3("Absolute confusion matrix"),
                 plotOutput("amatrix.svm"),
                 # 
                 # h3("Relative confusion matrix"),
                 # plotOutput("rmatrix.svm"),
                 
                 h4("ROC measures"),
                 verbatimTextOutput("roc.svm"),
                 
                 h4("Precision/Recall graph"),
                 plotlyOutput("graph1.svm"),
                 
                 h4("Sensitivity/Specificity graph"),
                 plotlyOutput("graph2.svm"),
                 
                 h4("Roc curve"),
                 plotlyOutput("graph3.svm")
                 
        ),
        
        #logistic regression
        tabPanel("IV.Logistic regression",
                 h2("IV.Logistic regression model"),
                 
                 
                 h3("Absolute Confusion Matrix"),
                 plotOutput("matrix.lg"),
                 
                 # 
                 # h3("Relative Confusion Matrix"),
                 # plotOutput("rmatrix.lg"),
                 
                 
                 h4("ROC measures"),
                 verbatimTextOutput("roc.lg"),
                 
                 h4("Precision/Recall graph"),
                 plotlyOutput("graph1.lg"),
                 
                 h4("Sensitivity/Specificity graph"),
                 plotlyOutput("graph2.lg"),
                 
                 h4("Roc curve"),
                 plotlyOutput("graph3.lg")
                 
        ),
        # decision tree
        tabPanel("V.Decision Tree",
                 
                 h1("V.Decision Tree"),
                 
                 h5("Construction of the decision tree"),
                 
                 plotOutput("tree"),
                 
                 h3("Result analysis"),
                 
                 h3("Absolute Confusion Matrix"),
                 plotOutput("amatrix.tree"),
                 # 
                 # h3("Relative Confusion Matrix"),
                 # plotOutput("rmatrix.tree"),
                 
                 h4("ROC measures"),
                 verbatimTextOutput("roc.tree"),
                 
                 h4("Precision/Recall graph"),
                 plotlyOutput("graph1.tree"),
                 
                 h4("Sensitivity/Specificity graph"),
                 plotlyOutput("graph2.tree"),
                 
                 h4("Roc curve"),
                 plotlyOutput("graph3.tree")
                 
        ),
        
        
        
        
        #random forest
        tabPanel("VI.Random Forest",
                 
                 h1("VI.Random Forest Model"),
                 
                 h3("Choice of hyperparameters"),
                 
                 sliderInput("ntree.param","Number of trees",
                             100,500,value=200,round = T),
                 
                 sliderInput("mtry.param", "Number of variables randomly sampled as candidates at each split", 
                             3,8,value=5),
                 
                 h4("Absolute Confusion Matrix"),
                 plotOutput("amatrix.rf"),
                 # 
                 # h4("Relative Confusion Matrix"),
                 # plotOutput("ramatrix.rf"),
                 # 
                 
                 h4("ROC measures"),
                 verbatimTextOutput("roc.rf"),
                 
                 h4("Precision/Recall graph"),
                 plotlyOutput("graph1.rf"),
                 
                 h4("Sensitivity/Specificity graph"),
                 plotlyOutput("graph2.rf"),
                 
                 h4("Roc curve"),
                 plotlyOutput("graph3.rf")
                 
        ),
      
        # model comparision
        tabPanel("VII.Model Comparison using cross validation in training set",
                 
                 h1("VII.Comparison of different models"),
                 
                 h4("Resampling using the k-fold method"),
                 sliderInput("k","Choose your K ",0,10,value=5,round = T),
                 verbatimTextOutput("bmr1"),
                 
                 h4("Precision/Recall graph"),
                 plotlyOutput("graph1.bmr"),
                 
                 h4("Sensitivity/Specificity graph"),
                 plotlyOutput("graph2.bmr"),
                 
                 h4("Roc curve"),
                 plotlyOutput("graph3.bmr"),
              
                 
                 h4("Distribution of performance values across resampling iterations for
                    one performance measure and for all learners"),
                 plotOutput("bmrplot")
                 
        ),
    # model comparision
    tabPanel("VIII.Model Comparison on test set",
             
             h4("We compare the performance of our model on testing set 
                which is not used for model implementation"),
        
             
             h4("Precision/Recall graph"),
             plotlyOutput("graph1.compar"),
             
             h4("Sensitivity/Specificity graph"),
             plotlyOutput("graph2.compar"),
             
             h4("Roc curve"),
             plotlyOutput("graph3.compar")
    )
          
             
        
        
        
  )  ))
