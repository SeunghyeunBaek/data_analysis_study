
# coding: utf-8

# # 전처리 / 차원축소 / 샘플링 함수 라이브러리

# In[5]:


import numpy as np
import pandas as pd
from sklearn.preprocessing import scale, robust_scale, minmax_scale, maxabs_scale
from sklearn.preprocessing import normalize
from sklearn.preprocessing import StandardScaler
from sklearn.preprocessing import RobustScaler
from scipy.stats import iqr
from sklearn.model_selection import StratifiedShuffleSplit
from sklearn.model_selection import train_test_split
from sklearn.manifold import TSNE
from sklearn.decomposition import PCA
from sklearn.decomposition import TruncatedSVD
from imblearn.under_sampling import RandomUnderSampler
from imblearn.over_sampling import RandomOverSampler
from imblearn.over_sampling import SMOTE


# In[15]:


if __name__ == "__main__":     
    print('******************************')
    print('There are some descriptions')
    print('HelpDataMunging(), HelpReduction(), HelpSampling()')
    print('******************************')


# In[7]:


### Preprocessing - Normalization, Standardization, Outlier


# In[1]:


def HelpDataMunging():
    print('**** code by LJH, Description by JJH ****')
    print('** List of DataMunging Function **')
    print('standardize(data)')
    print('minmaxScale(data)')
    print('maxabsScale(data)') 
    print('robustScale(data)')
    print('** example **')
    print('X = standardize(X)')
    
def standardize(data):
    return scale(data)
    
def minmaxScale(data):
    return minmax_scale(data)

def maxabsScale(data):
    return maxabs_scale(data)
    
def robustScale(data):
    return robust_scale(data)


# In[9]:


### Dimensionality Reduction - PCA, TSNE, TruncatedSVD, Auto encoder


# In[10]:


def HelpReduction():
    print('**** code by BSH , Description by JJH ****')
    print('** List of Reduction Function **')
    print('reduction_pca(input_df,n_comp=2,random_state=123)')
    print('eduction_tsne(input_df,n_comp=2,random_state=123)')
    print('reduction_tsvd(input_df,n_comp=2,random_state=123)')
    print('reduction_auto(input_df,n_comp=2,random_state=123,learning_rate=0.001,n_epoch=100,cnt = 10)')
    print('** params **')
    print('n_comp : Dimension of the embedded space')
    print('cnt : print mse each cnt of n_epoch')
    print('** example **')
    print('x_pca = reduction_pca(x,n_comp=2,random_state=123)')
    print('x_auto = reduction_auto(x,n_comp=2,random_state=123,learning_rate=0.001,n_epoch=100, cnt = 10)')

def reduction_pca(input_df,n_comp=2,random_state=123):
    return pd.DataFrame(PCA(n_components=n_comp, random_state=random_state).fit_transform(input_df.values))

def reduction_tsne(input_df,n_comp=2,random_state=123):
    return pd.DataFrame(TSNE(n_components=n_comp, random_state=random_state).fit_transform(input_df.values))

def reduction_tsvd(input_df,n_comp=2,random_state=123):
    return pd.DataFrame(TruncatedSVD(n_components=n_comp, random_state=random_state).fit_transform(input_df.values))

def reduction_auto(input_df,n_comp=2,random_state=123,learning_rate=0.001,n_epoch=100,cnt = 10):
    import tensorflow as tf
    from tensorflow.contrib.layers import fully_connected
    n_input = input_df.shape[1]
    n_hidden = n_comp
    n_output = n_input

    #set encoder
    x_ph = tf.placeholder(tf.float32,shape = [None,n_input])
    hidden_layer = fully_connected(x_ph,n_hidden,activation_fn=None)
    y_model = fully_connected(hidden_layer,n_output,activation_fn=None)

    #set loss function, optimizer
    loss = tf.reduce_mean(tf.square(x_ph-y_model)) # MSE
    optimizer = tf.train.AdamOptimizer(learning_rate = learning_rate) # Adam
    train = optimizer.minimize(loss)

    #Initiate variables
    init = tf.global_variables_initializer()

    #run session
    with tf.Session() as sess:
        sess.run(init)
        for i in range(n_epoch):
            my_feed = {x_ph:input_df.values}
            sess.run(train, feed_dict = my_feed)
            if i % cnt == 0: 
                mse = sess.run(loss, feed_dict = my_feed)
                print(i, "\tMSE loss:", mse)
        result = sess.run(hidden_layer, feed_dict = my_feed)
    return pd.DataFrame(result)


# In[11]:


### Sampling - Stratfied, Random_Under, Random_Over, SMOTE_Over


# In[12]:


def HelpSampling():
    print('**** code by JJH , Description by JJH ****')
    print('** List of Sampling Function **')
    print('StratifiedSampling(data, label, ratio=0.3)')
    print('UnderSampling(data, label, under=1.0, ratio=0.3)')
    print('OverSampling(data, label, over=1.0, ratio=0.3)')
    print('SMOTESampling(data, label, over=1.0, ratio=0.3)')
    print('** params **')
    print('ratio = test data / total data')
    print('over, under = minor class / major class')
    print('** example **')
    print('X_train, X_test, y_train, y_test = StratifiedSampling(X,y)')
    print('X_train, X_test, y_train, y_test=UnderSampling(X,y, under=0.5,ratio=0.3)')
    
def StratifiedSampling(data, label, ratio=0.3):
    X_train, X_test, y_train, y_test = train_test_split(
        data, label, test_size=ratio, stratify=y )
    return X_train, X_test, y_train, y_test    

def UnderSampling(data, label, under=1.0, ratio=0.3,random_state=123):
    # under = Minor class / Major class
    rus = RandomUnderSampler(sampling_strategy=under,random_state=random_state) 
    X_resampled, y_resampled = rus.fit_resample(data, label)
    X_train, X_test, y_train, y_test = train_test_split(
        X_resampled, y_resampled, test_size=ratio ,random_state=123)
    return X_train, X_test, y_train, y_test

def OverSampling(data, label, over=1.0, ratio=0.3, random_state =123):

    rus = RandomOverSampler(sampling_strategy=0.3,random_state=123)

    X_resampled, y_resampled = rus.fit_resample(data, label)
    X_train, X_test, y_train, y_test = train_test_split(
        X_resampled, y_resampled, test_size=ratio, random_state = rs)
    return X_train, X_test, y_train, y_test

def SMOTESampling(data, label, over=1.0, ratio=0.3,random_state=123):
    # over = Minor class / Major class
    rus = SMOTE(sampling_strategy=over,random) 
    X_resampled, y_resampled = rus.fit_resample(data, label)
    X_train, X_test, y_train, y_test = train_test_split(
        X_resampled, y_resampled, test_size=ratio,random_state = random_state)
    return X_train, X_test, y_train, y_test


