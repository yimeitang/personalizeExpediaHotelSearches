# -*- coding: utf-8 -*-
"""
Created on Mon May 29 16:47:50 2017

@author: Yimei Tang
"""
##Import Libraries
import sklearn
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.mlab as mlab


from sklearn.preprocessing import MinMaxScaler
from sklearn.cross_validation import train_test_split,cross_val_score
from sklearn.metrics import classification_report, precision_score,recall_score,roc_curve,auc,confusion_matrix
from sklearn.neighbors import KNeighborsClassifier
from sklearn.ensemble import RandomForestClassifier
from sklearn.tree import DecisionTreeClassifier
from sklearn.grid_search import GridSearchCV
from sklearn.grid_search import RandomizedSearchCV
from sklearn.ensemble import ExtraTreesClassifier
from sklearn import cross_validation,metrics
from sklearn.feature_selection import SelectFromModel


#Load Data
train_df=pd.read_csv('C:\\Users\\Yimei Tang\\Desktop\\DePaul\\CSC 424 Advanced Data Analysis\\Homework\\Homework 3\\train_prePCA.csv',
                     index_col=0)

row,column=train_df.shape
##Preview Data
train_df.columns
row,column=train_df.shape
row,column


##Convert categorical varialbes to numerical by using OneHotEncoder
train_df.dtypes
train_df.info()
train_df=pd.get_dummies(train_df, columns=['Month','TofD','TraMon','RG','Condition','Type'])

##Use MinMaxScaler
mm=MinMaxScaler()
train_df['price_usd']=mm.fit_transform(train_df['price_usd'].reshape(-1,1))


##Seperate Click_bool as Y1 and booking_bool as Y2
Y1=train_df['click_bool'].as_matrix()
Y2=train_df['booking_bool'].as_matrix()
train_df=train_df.drop(['click_bool','booking_bool'], 1)
X=train_df.as_matrix()


##Check any null values
np.isnan(np.sum(X))

###Seperate Train and Test Set
x_train,x_test,y_train,y_test = train_test_split(X,Y1,test_size=0.2,random_state=1)




#####Decision Tree
parameters={'class_weight':['balanced'],
            'max_depth':list(range(3, 15)),
            'min_samples_split' :list(range(3, 15)),
            'max_features': ['auto','sqrt','log2',None]
            }
clf=DecisionTreeClassifier()
gs=GridSearchCV(clf,param_grid=parameters,scoring='f1')
gs.fit(x_train,y_train)
gs.best_score_
gs.best_estimator_
gs.scorer_


###Cross Validation
cvs=cross_val_score(est,x_train,y_train,cv=10,scoring='f1')
cvs


predictions=est.predict(x_test)
confusion_matrix(y_test,predictions)
print(classification_report(y_test,predictions))
dtpfsf1='%0.2f' % sklearn.metrics.f1_score(y_test,predictions)
dtpfsrc='%0.2f' % sklearn.metrics.recall_score(y_test,predictions)


##Feature Importance
imp = est.feature_importances_ 
names=train_df.columns
imp,names=zip(*sorted(zip(imp,names)))
plt.figure(figsize=(20,20))
plt.barh(range(len(names)),imp,align='center')
plt.yticks(range(len(names)),names)

###ExtraTreesClassifier
forest= ExtraTreesClassifier(n_estimators=500, class_weight='balanced')
forest.fit(x_train,y_train)
imp2 = forest.feature_importances_
imp2,names=zip(*sorted(zip(imp2,names)))
plt.figure(figsize=(20,20))
plt.barh(range(len(names)),imp2,align='center')
plt.yticks(range(len(names)),names)

###Use the important features
sel = SelectFromModel(forest, prefit=True)
x_new_train = sel.transform(x_train)
x_new_test = sel.transform(x_test)

feature_idx = sel.get_support()
feature_name = train_df.columns[feature_idx]
feature_name


###After selecting the important features:
###Results are much better
gs.fit(x_new_train,y_train)
gs.best_score_
gs.best_estimator_
gs.scorer_
est=gs.best_estimator_

cvs=cross_val_score(est,x_new_train,y_train,cv=10,scoring='f1')


predictions=est.predict(x_new_test)
confusion_matrix(y_test,predictions)
print(classification_report(y_test,predictions))
dtafsf1='%0.2f' % sklearn.metrics.f1_score(y_test,predictions)
dtafsrc='%0.2f' % sklearn.metrics.recall_score(y_test,predictions)



###Random Forest
parameters={'max_depth':[10,20],
            'min_samples_split' :[3,7],
            'class_weight':['balanced'],
            'n_estimators':[300,310]}
clf=RandomForestClassifier()
rs=RandomizedSearchCV(clf,param_distributions=parameters,scoring='f1',n_iter=4)
rs.fit(x_train,y_train)

rs.best_score_
rs.best_estimator_
rs.scorer_
est=rs.best_estimator_
 
###Cross Validation
cvs=cross_val_score(est,x_train,y_train,cv=10,scoring='f1')
cvs


predictions=est.predict(x_test)
confusion_matrix(y_test,predictions)
print(classification_report(y_test,predictions))
rfbfsf1='%0.2f' % sklearn.metrics.f1_score(y_test,predictions)
rfbfsrc='%0.2f' % sklearn.metrics.recall_score(y_test,predictions)


###After selecting the important features:
rs.fit(x_new_train,y_train)
rs.best_score_
rs.best_estimator_
rs.scorer_
est=rs.best_estimator_

predictions=est.predict(x_new_test)
confusion_matrix(y_test,predictions)
print(classification_report(y_test,predictions))

rfafsf1='%0.2f' % sklearn.metrics.f1_score(y_test,predictions)
rfafsrc='%0.2f' % sklearn.metrics.recall_score(y_test,predictions)



###KNN
parameters={'n_neighbors':list(range(2, 5)),
            'weights':['uniform','distance'],
            'algorithm' :['auto', 'ball_tree', 'kd_tree', 'brute'],
            }
clf=KNeighborsClassifier()
gs=GridSearchCV(clf,param_grid=parameters,scoring='f1')
gs.fit(x_train,y_train)
gs.best_score_
gs.best_estimator_
gs.scorer_
est=gs.best_estimator_

###Cross Validation
cvs=cross_val_score(est,x_train,y_train,cv=10,scoring='f1')
cvs

predictions=est.predict(x_test)
confusion_matrix(y_test,predictions)
print(classification_report(y_test,predictions))
knnppcaf1='%0.2f' % sklearn.metrics.f1_score(y_test,predictions)
knnppcarc='%0.2f' % sklearn.metrics.recall_score(y_test,predictions)



###Use AfterPCA
#Load Data
train_df2=pd.read_csv('C:\\Users\\Yimei Tang\\Desktop\\DePaul\\CSC 424 Advanced Data Analysis\\Homework\\Homework 3\\train_afterPCA.csv',
                     index_col=0)

##Convert categorical varialbes to numerical by using OneHotEncoder
train_df2=pd.get_dummies(train_df2, columns=['Month','TofD','TraMon','RG','Condition','Type'])


##Seperate Click_bool as Y1 and booking_bool as Y2
Y1=train_df2['click_bool'].as_matrix()
Y2=train_df2['booking_bool'].as_matrix()
train_df2=train_df2.drop(['click_bool','booking_bool'], 1)
X=train_df2.as_matrix()

###Seperate Train and Test Set
x_train,x_test,y_train,y_test = train_test_split(X,Y1,test_size=0.2,random_state=1)



clf=KNeighborsClassifier(algorithm='auto', leaf_size=30, metric='minkowski',
           metric_params=None, n_jobs=1, n_neighbors=2, p=2,
           weights='distance')
 
 

###Cross Validation          
clf.fit(x_train,y_train)          
cvs=cross_val_score(clf,x_train,y_train,cv=10,scoring='f1')
cvs


predictions=clf.predict(x_test)
confusion_matrix(y_test,predictions)
print(classification_report(y_test,predictions))
knnapcaf1='%0.2f' % sklearn.metrics.f1_score(y_test,predictions)
knnapcarc='%0.2f' % sklearn.metrics.recall_score(y_test,predictions)

#####Conclusion
threemodels = pd.DataFrame({
        'Model': ['Decision Tree Before Feature Selection',
                  'Decison Tree After Feature Selection',
                  'Random Forest Before Feature Selection',
                  'Random Forest After Feature Selection',
                  'KNN Beore PCA Feature Selection',
                  'KNN After PCA Feature Selection'],
        'F1_score': [dtpfsf1,dtafsf1,rfbfsf1,rfafsf1,knnppcaf1,knnapcaf1],
        'Recall_score': [dtpfsrc,dtafsrc,rfbfsrc,rfafsrc,knnppcarc,knnapcarc],
        'Numbers of Features': [30,20,30,20,30,20]})

pd.set_option('display.height', 1000)
pd.set_option('display.max_rows', 500)
pd.set_option('display.max_columns', 500)
pd.set_option('display.width', 1000)


threemodels.sort_values(by='F1_score',ascending=False)

