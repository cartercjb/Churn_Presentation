#Ensure all needed libraries are installed
list.of.packages <- c("plyr", "dplyr","ROCR","caret","randomForest",
                      "kernlab","magrittr","rpart","ggplot2","rpart.plot",
                      "pROC","ada", "tidyr", "data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages))
  install.packages(new.packages)

# Load an R data frame.
setwd("~/Desktop")
CRI_Workbook<- read.csv("2CRI_Workbook.csv")
MYdataset<- CRI_Workbook
str(MYdataset,width=80,strict.width="wrap")
library(plyr)
library(dplyr)
library(data.table)
library(tidyr)

summary(MYdataset)

#WHAT PROPORTION OF STAFF ARE LEAVING?
StatusCount<-with(MYdataset,table(STATUS_YEAR,STATUS))
StatusCount<-spread(data.frame(StatusCount),STATUS,Freq)
StatusCount$PREVIOUSACTIVE<- shift(StatusCount$Active,1L, type = "lag")
StatusCount

        #Remove Column
        StatusCount<- subset(StatusCount, select=-2)

    #Add Percentage Terminated
        StatusCount$PercentTerminated <- shift(StatusCount$ACTIVE, 1L, type = "lag")
        StatusCount$PercentTerminated <- StatusCount$Terminated / StatusCount$PREVIOUSACTIVE*100
        StatusCount
    
    #You will notice that by doing that, we get NA for the first year. To calculate the average we need to ignore
        mean(StatusCount$PercentTerminated,na.rm = TRUE)
    
    #Export to CSV
    write.csv(StatusCount,file="Export_StatusCount.csv")
          
    #Plot percentages
    library(ggplot2)
    ggplot() + geom_point(aes(x = as.factor(STATUS_YEAR), y = PercentTerminated), data = StatusCount) + geom_smooth(method = "lm")
    
#WHERE ARE THE TERMINATIONS OCCURING
    
    #BY BUSINESS UNIT
    ggplot() + geom_bar(aes(y = ..count..,x =as.factor(BUSINESS_UNIT),
                            fill = as.factor(STATUS)),
                        data=MYdataset,position = position_stack())
    # BY TERMINATION TYPE AND STATUS YEAR
    TerminatesData<- as.data.frame(MYdataset %>% filter(STATUS=="Terminated"))
    ggplot() + geom_bar(aes(y = ..count..,x =as.factor(STATUS_YEAR),
                            fill = as.factor(termtype_desc)),
                        data=TerminatesData,position = position_stack())+
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
    
    #BY STATUS YEAR AND TERMINATION REASON
    ggplot() + geom_bar(aes(y = ..count..,x =as.factor(STATUS_YEAR),
                            fill = as.factor(termreason_desc)),
                        data=TerminatesData, position = position_stack())+
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
    
    #BY TERMINATION REASON AND DEPARTMNET
    ggplot() +geom_bar(aes(y=..count..,x=as.factor(department_name),
                            fill=as.factor(termreason_desc)),
                         data=TerminatesData,position=position_stack())+
        theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))


    
#How DOES AGE AND LENGTH OF SERVICE AFFECT TERMINATION?
library(caret)
featurePlot(x=MYdataset[,6:7],y=MYdataset$STATUS,plot="density",auto.key = list(columns = 2))


#AGE AND LEGNTH OF SERVICE DISTRIBUTION BY STATUS
featurePlot(x=MYdataset[,6:7],y=MYdataset$STATUS,plot="box",auto.key = list(columns = 2))


#BUILD A MODEL

  #PARTITION DATA 
  #library(rattle)
  library(magrittr) # For the %>% and %<>% operators.

    building <- TRUE
    scoring <- ! building
    # A pre-defined value is used to reset the random seed so that results are repeatable.
    MYseed <- 42
    # Load an R data frame.
    
    #Create training and testing datasets
    set.seed(MYseed)
    MYnobs <- nrow(MYdataset)
    MYsample <- MYtrain <- subset(MYdataset,STATUS_YEAR<=2016)
    MYvalidate <- NULL
    MYtest <- subset(MYdataset,STATUS_YEAR== 2017)
    # The following variable selections have been noted.
    MYinput <- c("age", "length_of_service", "gender_full",
                 "STATUS_YEAR", "BUSINESS_UNIT")
    MYnumeric <- c("age", "length_of_service", "STATUS_YEAR")
    MYcategoric <- c(
      "gender_full", "BUSINESS_UNIT")
    MYtarget <- "STATUS"
    MYrisk <- NULL
    MYident <- "EmployeeID"
    MYignore <- c("recorddate_key", "birthdate_key", "orighiredate_key",
                  "terminationdate_key",
                  "city_name", "gender_short", "termreason_desc",
                  "termtype_desc","department_name",
                  "job_title", "store_name")
    MYweights <- NULL
    MYTrainingData<-MYtrain[c(MYinput, MYtarget)]
    MYTestingData<-MYtest[c(MYinput, MYtarget)]
    
#Decision Tree
    #library(rattle)
    library(rpart,quietly=TRUE)
    # Reset the random number seed to obtain the same results each time.
    set.seed(MYseed)
    # Build the Decision Tree model.
    MYrpart <- rpart(STATUS ~ .,
                     data=MYtrain[, c(MYinput, MYtarget)],
                     method="class",
                     parms=list(split="information"),
                     control=rpart.control(usesurrogate=0,
                                           maxsurrogate=0))

    # Generate a textual view of the Decision Tree model.
    print(MYrpart)
    
    printcp(MYrpart)
    #cat("\n")
    # Time taken: 0.63 secs
    #============================================================
    # Rattle timestamp: 2016-03-25 09:45:25 x86_64-w64-mingw32
    # Plot the resulting Decision Tree.
    # We use the rpart.plot package.
    #fancyRpartPlot(MYrpart, main="Decision Tree MFG10YearTerminationData $ STATUS")
    library(rpart.plot)
    rpart.plot(MYrpart, type = 3)
  
#Random Forests 
    #============================================================
    # Rattle timestamp: 2016-03-25 18:21:29 x86_64-w64-mingw32
    # Random Forest
    # The !randomForest! package provides the !randomForest! function.
    library(randomForest, quietly=TRUE)
    # Build the Random Forest model.
    set.seed(MYseed)
    MYrf <- randomForest::randomForest(STATUS ~ .,data=MYtrain[c(MYinput, MYtarget)],ntree=500,mtry=2,importance=TRUE,na.action=randomForest::na.roughfix,replace=FALSE)
    # To Generate textual output of !Random Forest! model uncomment next line.
    #MYrf
    # The "pROC! package implements various AUC functions.
    # To Calculate the Area Under the Curve (AUC) uncomment next line.
    pROC::roc(MYrf$y, as.numeric(MYrf$predicted))
    
    # Calculate the AUC Confidence Interval.
   
     pROC::ci.auc(MYrf$y, as.numeric(MYrf$predicted))

    # List the importance of the variables.
    rn <- round(randomForest::importance(MYrf), 2)
    rn[order(rn[,3], decreasing=TRUE),]
    

    
#ADA Boost 
    # The "ada! package implements the boost algorithm.
    # Build the Ada Boost model.
    set.seed(MYseed)
    MYada <- ada::ada(STATUS ~ .,
                      data=MYtrain[c(MYinput, MYtarget)],
                      control=rpart::rpart.control(maxdepth=30,
                                                   cp=0.010000,
                                                   minsplit=20,
                                                   xval=10),
                      iter=50)
    # To Print the results of the modeling uncomment next 2 lines.
    print(MYada)
    
    round(MYada$model$errs[MYada$iter,], 2)
    
    #cat(!Variables actually used in tree construction:\n!)
    #print(sort(names(listAdaVarsUsed(MYada))))
    #cat(!\nFrequency of variables actually used:\n!)
    #print(listAdaVarsUsed(MYada))
    
#Support Vector Machine
    #============================================================
    # Rattle timestamp: 2016-03-25 18:22:56 x86_64-w64-mingw32
    # Support vector machine.
    # The !kernlab! package provides the !ksvm! function.
    library(kernlab, quietly=TRUE)
    # Build a Support Vector Machine model.
    set.seed(MYseed)
    MYksvm <- ksvm(as.factor(STATUS) ~ .,
                   data=MYtrain[c(MYinput, MYtarget)],
                   kernel="rbfdot",
                   prob.model=TRUE)
    # To Generate a textual view of the SVM model uncomment next line.
    MYksvm
    
    
#Linear Models
    #============================================================
    # Rattle timestamp: 2016-03-25 18:23:56 x86_64-w64-mingw32
    # Regression model
    # Build a Regression model.
    MYglm <- glm(STATUS ~ .,
                 data=MYtrain[c(MYinput, MYtarget)],
                 family=binomial(link="logit"))
    # Generate a textual view of the Linear model.
    print(summary(MYglm))

    cat(sprintf("Log likelihood: %.3f (%d df)\n",
                logLik(MYglm)[1],
                attr(logLik(MYglm), "df")))    

    cat(sprintf("Null/Residual deviance difference: %.3f (%d df)\n",
                MYglm$null.deviance-MYglm$deviance,
                MYglm$df.null-MYglm$df.residual))    

    cat(sprintf("Chi-square p-value: %.8f\n",
                dchisq(MYglm$null.deviance-MYglm$deviance,
                       MYglm$df.null-MYglm$df.residual)))    

    cat(sprintf("Pseudo R-Square (optimistic): %.8f\n",
                cor(MYglm$y, MYglm$fitted.values)))  
    
    cat('\n==== ANOVA ====\n\n')

    print(anova(MYglm, test="Chisq"))    

    cat("\n")    

    
#EVALUATE AND CRITIQUE MODELS
    
    #DECISION TREES
    #============================================================
      # Rattle timestamp: 2016-03-25 18:50:22 x86_64-w64-mingw32
      # Evaluate model performance.
      # Generate an Error Matrix for the Decision Tree model.
      # Obtain the response from the Decision Tree model.
      MYpr <- predict(MYrpart, newdata=MYtest[c(MYinput, MYtarget)], type="class")
      # Generate the confusion matrix showing counts.
      table(MYtest[c(MYinput, MYtarget)]$STATUS, MYpr,
            dnn=c("Actual", "Predicted"))
      
      # Generate the confusion matrix showing proportions.
      pcme <- function(actual, cl)
      {
        x <- table(actual, cl)
        nc <- nrow(x)
        tbl <- cbind(x/length(actual),
                     Error=sapply(1:nc,
                                  function(r) round(sum(x[r,-r])/sum(x[r,]), 2)))
        names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
        return(tbl)
      }
      per <- pcme(MYtest[c(MYinput, MYtarget)]$STATUS, MYpr)
      round(per, 2)
      
      # Calculate the overall error percentage.
      cat(100*round(1-sum(diag(per), na.rm=TRUE), 2))
      
      # Calculate the averaged class error percentage.
      cat(100*round(mean(per[,"Error"], na.rm=TRUE), 2))

    #ADABOOST
      # Generate an Error Matrix for the Ada Boost model.
      # Obtain the response from the Ada Boost model.
      MYpr <- predict(MYada, newdata=MYtest[c(MYinput, MYtarget)])
      # Generate the confusion matrix showing counts.
      table(MYtest[c(MYinput, MYtarget)]$STATUS, MYpr,
            dnn=c("Actual", "Predicted"))
      ## Predicted
      ## Actual ACTIVE TERMINATED
      ## ACTIVE 4799 0
      ## TERMINATED 99 63
      # Generate the confusion matrix showing proportions.
      pcme <- function(actual, cl)
      {
        x <- table(actual, cl)
        nc <- nrow(x)
        tbl <- cbind(x/length(actual),
                     Error=sapply(1:nc,
                                  function(r) round(sum(x[r,-r])/sum(x[r,]), 2)))
        names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
        return(tbl)
      }
      per <- pcme(MYtest[c(MYinput, MYtarget)]$STATUS, MYpr)
      round(per, 2)
      
      # Calculate the overall error percentage.
      cat(100*round(1-sum(diag(per), na.rm=TRUE), 2))
      
      # Calculate the averaged class error percentage.
      cat(100*round(mean(per[,"Error"], na.rm=TRUE), 2))
      
    #RANDOM FOREST
      # Generate an Error Matrix for the Random Forest model.
      # Obtain the response from the Random Forest model.
      MYpr <- predict(MYrf, newdata=na.omit(MYtest[c(MYinput, MYtarget)]))
      # Generate the confusion matrix showing counts.
      table(na.omit(MYtest[c(MYinput, MYtarget)])$STATUS, MYpr,
            dnn=c("Actual", "Predicted"))
      # Generate the confusion matrix showing proportions.
      pcme <- function(actual, cl)
      {
        x <- table(actual, cl)
        nc <- nrow(x)
        tbl <- cbind(x/length(actual),
                     Error=sapply(1:nc,
                                  function(r) round(sum(x[r,-r])/sum(x[r,]), 2)))
        names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
        return(tbl)
      }
      per <- pcme(na.omit(MYtest[c(MYinput, MYtarget)])$STATUS, MYpr)
      round(per, 2)
      
      # Calculate the overall error percentage.
      cat(100*round(1-sum(diag(per), na.rm=TRUE), 2))
      
      # Calculate the averaged class error percentage.
      cat(100*round(mean(per[,"Error"], na.rm=TRUE), 2))
      
    #SVM
      
      # Generate an Error Matrix for the SVM model.
      # Obtain the response from the SVM model.
      MYpr <- kernlab::predict(MYksvm, newdata=na.omit(MYtest[c(MYinput, MYtarget)]))
      # Generate the confusion matrix showing counts.
      table(na.omit(MYtest[c(MYinput, MYtarget)])$STATUS, MYpr,
            dnn=c("Actual", "Predicted"))
      
      # Generate the confusion matrix showing proportions.
      pcme <- function(actual, cl)
      {
        x <- table(actual, cl)
        nc <- nrow(x)
        tbl <- cbind(x/length(actual),
                     Error=sapply(1:nc,
                                  function(r) round(sum(x[r,-r])/sum(x[r,]), 2)))
        names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
        return(tbl)
      }
      per <- pcme(na.omit(MYtest[c(MYinput, MYtarget)])$STATUS, MYpr)
      round(per, 2)
      
      #Calculate the overall error percentage.
      cat(100*round(1-sum(diag(per), na.rm=TRUE), 2))
      
      # Calculate the averaged class error percentage.
      cat(100*round(mean(per[,"Error"], na.rm=TRUE), 2))
      
    #LINEAR MODEL
      #Generate an Error Matrix for the Linear model.
      # Obtain the response from the Linear model.
      MYpr <- as.vector(ifelse(predict(MYglm, type="response",
                                       newdata=MYtest[c(MYinput, MYtarget)]) > 0.5,
                               "Terminated", "Active"))
      # Generate the confusion matrix showing counts.
      table(MYtest[c(MYinput, MYtarget)]$STATUS, MYpr,
            dnn=c("Actual", "Predicted"))
      
      # Generate the confusion matrix showing proportions.
      pcme <- function(actual, cl)
      {
        x <- table(actual, cl)
        nc <- nrow(x)
        tbl <- cbind(x/length(actual),
                     Error=sapply(1:nc,
                                  function(r) round(sum(x[r,-r])/sum(x[r,]), 2)))
        names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
        return(tbl)
      }
      per <- pcme(MYtest[c(MYinput, MYtarget)]$STATUS, MYpr)
      round(per, 2)
      # Calculate the overall error percentage.
      cat(100*round(1-sum(diag(per), na.rm=TRUE), 2))
      # Calculate the averaged class error percentage.
      cat(100*round(mean(per[,"Error"], na.rm=TRUE), 2))
      

      
#AUC- Decision Tree       
      # Evaluate model performance.
      # ROC Curve: requires the ROCR package.
      library(ROCR)
      
      # ROC Curve: requires the ggplot2 package.
      library(ggplot2, quietly=TRUE)
      # Generate an ROC Curve for the rpart model on MFG10YearTerminationData [test].
      MYpr <- predict(MYrpart, newdata=MYtest[c(MYinput, MYtarget)])[,2]
      # Remove observations with missing target.
      no.miss <- na.omit(MYtest[c(MYinput, MYtarget)]$STATUS)
      miss.list <- attr(no.miss, "na.action")
      attributes(no.miss) <- NULL
      if (length(miss.list))
      {
        pred <- prediction(MYpr[-miss.list], no.miss)
      } else
      {
        pred <- prediction(MYpr, no.miss)
      }
      pe <- performance(pred, "tpr", "fpr")
      au <- performance(pred, "auc")@y.values[[1]]
      pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
      p <- ggplot(pd, aes(x=fpr, y=tpr))
      p <- p + geom_line(colour="red")
      p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
      p <- p + ggtitle("ROC Curve Decision Tree")
      p <- p + theme(plot.title=element_text(size=10))
      p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
      p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                        label=paste("AUC =", round(au, 2)))
      print(p)
      
      # Calculate the area under the curve for the plot.
      # Remove observations with missing target.
      no.miss <- na.omit(MYtest[c(MYinput, MYtarget)]$STATUS)
      miss.list <- attr(no.miss, "na.action")
      attributes(no.miss) <- NULL
      if (length(miss.list))
      {
        pred <- prediction(MYpr[-miss.list], no.miss)
      } else
      {
        pred <- prediction(MYpr, no.miss)
      }
      performance(pred, "auc")
      
  #AUC - ADA MODEL
      # ROC Curve: requires the ROCR package.
      library(ROCR)
      # ROC Curve: requires the ggplot2 package.
      library(ggplot2, quietly=TRUE)
      # Generate an ROC Curve for the ada model on MFG10YearTerminationData [test].
      MYpr <- predict(MYada, newdata=MYtest[c(MYinput, MYtarget)], type="prob")[,2]
      # Remove observations with missing target.
      no.miss <- na.omit(MYtest[c(MYinput, MYtarget)]$STATUS)
      miss.list <- attr(no.miss, "na.action")
      attributes(no.miss) <- NULL
      if (length(miss.list))
      {
        pred <- prediction(MYpr[-miss.list], no.miss)
      } else
      {
        pred <- prediction(MYpr, no.miss)
      }
      pe <- performance(pred, "tpr", "fpr")
      au <- performance(pred, "auc")@y.values[[1]]
      pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
      p <- ggplot(pd, aes(x=fpr, y=tpr))
      p <- p + geom_line(colour="red")
      p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
      p <- p + ggtitle("ROC Curve Ada Boost")
      p <- p + theme(plot.title=element_text(size=10))
      p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
      p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                        label=paste("AUC =", round(au, 2)))
      print(p)
      
      # Calculate the area under the curve for the plot.
      # Remove observations with missing target.
      no.miss <- na.omit(MYtest[c(MYinput, MYtarget)]$STATUS)
      miss.list <- attr(no.miss, "na.action")
      attributes(no.miss) <- NULL
      if (length(miss.list))
      {
        pred <- prediction(MYpr[-miss.list], no.miss)
      } else
      {
        pred <- prediction(MYpr, no.miss)
      }
      performance(pred, "auc")

      
#RANDOM FOREST
      # ROC Curve: requires the ROCR package.
      library(ROCR)
      # ROC Curve: requires the ggplot2 package.
      library(ggplot2, quietly=TRUE)
      # Generate an ROC Curve for the rf model on MFG10YearTerminationData [test].
      MYpr <- predict(MYrf, newdata=na.omit(MYtest[c(MYinput, MYtarget)]), type="prob")[,2]
      # Remove observations with missing target.
      no.miss <- na.omit(na.omit(MYtest[c(MYinput, MYtarget)])$STATUS)
      miss.list <- attr(no.miss, "na.action")
      attributes(no.miss) <- NULL
      if (length(miss.list))
      {
        pred <- prediction(MYpr[-miss.list], no.miss)
      } else
      {
        pred <- prediction(MYpr, no.miss)
      }
      pe <- performance(pred, "tpr", "fpr")
      au <- performance(pred, "auc")@y.values[[1]]
      pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
      p <- ggplot(pd, aes(x=fpr, y=tpr))
      p <- p + geom_line(colour="red")
      p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
      p <- p + ggtitle("ROC Curve Random Forest")
      p <- p + theme(plot.title=element_text(size=10))
      p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
      p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                        label=paste("AUC =", round(au, 2)))
      print(p)
      
      # Calculate the area under the curve for the plot.
      # Remove observations with missing target.
      no.miss <- na.omit(na.omit(MYtest[c(MYinput, MYtarget)])$STATUS)
      miss.list <- attr(no.miss, "na.action")
      attributes(no.miss) <- NULL
      if (length(miss.list))
      {
        pred <- prediction(MYpr[-miss.list], no.miss)
      } else
      {
        pred <- prediction(MYpr, no.miss)
      }
      performance(pred, "auc")
      
      
      
#AUC- SVM
      
      # ROC Curve: requires the ROCR package.
      library(ROCR)
      # ROC Curve: requires the ggplot2 package.
      library(ggplot2, quietly=TRUE)
      # Generate an ROC Curve for the ksvm model on MFG10YearTerminationData [test].
      MYpr <- kernlab::predict(MYksvm, newdata=na.omit(MYtest[c(MYinput, MYtarget)]),
                               type="probabilities")[,2]
      # Remove observations with missing target.
      no.miss <- na.omit(na.omit(MYtest[c(MYinput, MYtarget)])$STATUS)
      miss.list <- attr(no.miss, "na.action")
      attributes(no.miss) <- NULL
      if (length(miss.list))
      {
        pred <- prediction(MYpr[-miss.list], no.miss)
      } else
      {
        pred <- prediction(MYpr, no.miss)
      }
      pe <- performance(pred, "tpr", "fpr")
      au <- performance(pred, "auc")@y.values[[1]]
      pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
      p <- ggplot(pd, aes(x=fpr, y=tpr))
      p <- p + geom_line(colour="red")
      p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
      p <- p + ggtitle("ROC Curve SVM")
      p <- p + theme(plot.title=element_text(size=10))
      p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
      p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                        label=paste("AUC =", round(au, 2)))
      print(p)
      
      # Calculate the area under the curve for the plot.
      # Remove observations with missing target.
      no.miss <- na.omit(na.omit(MYtest[c(MYinput, MYtarget)])$STATUS)
      miss.list <- attr(no.miss, "na.action")
      attributes(no.miss) <- NULL
      if (length(miss.list))
      {
        pred <- prediction(MYpr[-miss.list], no.miss)
      } else
      {
        pred <- prediction(MYpr, no.miss)
      }
      performance(pred, "auc")
      
      
      
#AUC - GLM
      # ROC Curve: requires the ROCR package.
      library(ROCR)
      # ROC Curve: requires the ggplot2 package.
      library(ggplot2, quietly=TRUE)
      # Generate an ROC Curve for the glm model on MFG10YearTerminationData [test].
      MYpr <- predict(MYglm, type="response", newdata=MYtest[c(MYinput, MYtarget)])
      # Remove observations with missing target.
      no.miss <- na.omit(MYtest[c(MYinput, MYtarget)]$STATUS)
      miss.list <- attr(no.miss, "na.action")
      attributes(no.miss) <- NULL
      if (length(miss.list))
      {
        pred <- prediction(MYpr[-miss.list], no.miss)
      } else
      {
        pred <- prediction(MYpr, no.miss)
      }
      pe <- performance(pred, "tpr", "fpr")
      au <- performance(pred, "auc")@y.values[[1]]
      pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
      p <- ggplot(pd, aes(x=fpr, y=tpr))
      p <- p + geom_line(colour="red")
      p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
      p <- p + ggtitle("ROC Curve Linear Model")
      p <- p + theme(plot.title=element_text(size=10))
      p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
      p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                        label=paste("AUC =", round(au, 2)))
      print(p)
      
      # Calculate the area under the curve for the plot.
      # Remove observations with missing target.
      no.miss <- na.omit(MYtest[c(MYinput, MYtarget)]$STATUS)
      miss.list <- attr(no.miss, "na.action")
      attributes(no.miss) <- NULL
      if (length(miss.list))
      {
        pred <- prediction(MYpr[-miss.list], no.miss)
      } else
      {
        pred <- prediction(MYpr, no.miss)
      }
      performance(pred, "auc")
      
      
      

#Deploy THE MODDEL
      
  #Apply model: predicting next year's terminates
    Employees2018<-MYtest
    ActiveEmployees2018<-subset(Employees2018,STATUS=="Active")
    ActiveEmployees2018$age<-ActiveEmployees2018$age+1
    ActiveEmployees2018$length_of_service<-ActiveEmployees2018$length_of_service+1
      
  #Apply the model with highest AUC
    
    #AUC Model
      ActiveEmployees2018$PredictedSTATUS2018<-predict(MYada,ActiveEmployees2018)
      PredictedTerminatedEmployees2018<-subset(ActiveEmployees2018,
                                               PredictedSTATUS2018=="Terminated")
      #show records for first 5 predictions
      head(PredictedTerminatedEmployees2018$EmployeeID)
      