"""
******************R software output**********************************
the contents of nomogram
Points per unit of linear predictor: 46.38193 
Linear predictor units per point   : 0.02156012 
Age Points
   0   0   
  10   9   
  20  18   
  30  27   
  40  36   
  50  45   
  60  55   
  70  64   
  80  73   
  90  82   
 100  91   
 110 100   
Marital  Points
 Divorced 19    
 Married  10    
 Single   16    
 Unkonwn   0    
 Widowed  21    
Race  Points
 Black 7     
 Other 0     
 White 2     
Histology       Points
 Chromophobe RCC  0    
 Clear cell RCC  20    
 Other           49    
 Papillary RCC   28    
 RCC             31    
 Sarcomatoid RCC 57    
Grade         Points
 Grade I        0    
 Grade II       6    
 Grade III     23    
 Grade IV      41    
 Grade unknown 21    
TumorSize Points
  0         0    
  2         4    
  4         8    
  6        12    
  8        16    
 10        20    
 12        23    
 14        27    
 16        31    
 18        35    
 20        39    
 22        43    
 24        47    
 26        51    
NodeInvolvement Points
 No               0    
 Yes             24    
Metastasis Points
 Bone       44    
 Brain      48    
 Liver      56    
 Lung       46    
 Multiple   66    
 No          0    
 Other      37    
Surgery              Points
 Nephroureterectomy    0    
 No sugery            52    
 Partial ureterectomy 14    
 Radical nephrectomy  11    
Radiation Points
 No        0
 Yes       9
Chemotherapy Points
 No/Unknown   7     
 Yes          0     
Total Points 1 year survival
          294             0.1
          278             0.2
          264             0.3
          252             0.4
          239             0.5
          225             0.6
          208             0.7
          186             0.8
          151             0.9
Total Points 3 year survival
          250             0.1
          233             0.2
          219             0.3
          207             0.4
          194             0.5
          180             0.6
          163             0.7
          141             0.8
          106             0.9
******************R software output**********************************
"""
import numpy as np
import pandas as pd


VARNUM = 11
DIC_Marital = {'Divorced':19,'Married':10,'Single':16,'Unkonwn':0,'Widowed':21}
DIC_Race = {'Black':7,'Other': 0,'White':2}
DIC_Histology = {'Chromophobe RCC':0,'Clear cell RCC':20, 'Other': 49,'Papillary RCC':28,'RCC':31,'Sarcomatoid RCC':57}
DIC_Grade = {'Grade I':0,'Grade II':6,'Grade III':23,'Grade IV':41,'Grade unknown':21}
DIC_Surgery = {'Nephroureterectomy':0,'No sugery':52,'Partial ureterectomy':14,'Radical nephrectomy':11}
DIC_NodeInvolvement = {'No':0,'Yes':24}
DIC_Metastasis = { 'Bone':44,'Brain':48,'Liver':56,'Lung':46,'Multiple':66,'No':0,'Other':37}
DIC_Radiation = { 'No':0,'Yes':9 }
DIC_Chemotherapy = {'No/Unknown':7,'Yes':0}

DIC = {'Marital':DIC_Marital,'Race':DIC_Race,'Histology':DIC_Histology,
       'Grade':DIC_Grade,'Surgery':DIC_Surgery,'NodeInvolvement':DIC_NodeInvolvement,
       'Metastasis':DIC_Metastasis,'Radiation':DIC_Radiation,'Chemotherapy':DIC_Chemotherapy}

VAR_ANALYSIS= ['Age','Marital','Race', 'Histology', 'Grade','Surgery',
       'TumorSize', 'NodeInvolvement', 'Metastasis',
       'Radiation','Chemotherapy','survival','dead']
VAR_CALCULATE= ['Age','Marital','Race', 'Histology', 'Grade','Surgery',
       'TumorSize', 'NodeInvolvement', 'Metastasis',
       'Radiation','Chemotherapy']
PURPOSE = [VAR_ANALYSIS,VAR_CALCULATE]
OS_COEFFIENT_1year = {"beta1":-0.030177,"beta0":7.87909}
OS_COEFFIENT_3year = {"beta1":-0.030094,"beta0":5.71608}
##根据目的为分析还是计算，提取原数据的需要的变量
def extract(raw_data,purpose = PURPOSE[1]):
    data = raw_data.loc[:,purpose]
    return data

##计算总分
def total_score(index,data):
    total = 0
    for i in range(VARNUM):
        patient = data.iloc[index]
        return total

##处理分类变量
def catgorical_handle(data):
    for var,dic in DIC.items():
        data.loc[:,var] = data.loc[:,var].map(dic)
    return data
AGE_COEFFIENTS = {'beta1':0.9126,'beta0':-0.1923}
TUMERSIZE_COEEFIENTS = {'beta1':1.9473,'beta0':0.1143}
##处理连续变量
def numeric_handle_lr(data,type,beta0,beta1):
    data.loc[:,type] = data.loc[:,type] * beta1 + beta0
    return data
##检查数据的完整性
def check_var_inte(data,purpose = PURPOSE[1]):
    VAR_INTE = True
    col = data.columns
    for i in purpose:
        if(i not in col):
            VAR_INTE = False
    return VAR_INTE

def OS_Calc(data,coeffient):
    if coeffient == OS_COEFFIENT_1year:
        data["1_year_survival"] = (np.exp(coeffient['beta0']+coeffient['beta1']*data['total_score']))\
                                     /(1+np.exp(coeffient['beta0']+coeffient['beta1']*data['total_score']))
    elif coeffient == OS_COEFFIENT_3year:
        data["3_year_survival"] = (np.exp(coeffient['beta0'] + coeffient['beta1'] * data['total_score'])) \
                                         / (1 + np.exp(coeffient['beta0'] + coeffient['beta1'] * data['total_score']))

##走流程
def calculate(data_path,purpose = PURPOSE[1]):
    data = pd.read_csv(data_path)
    if not check_var_inte(data,purpose):
        return "err"
    data_df = extract(data)
    data_df = catgorical_handle(data_df)
    data_df = numeric_handle_lr(data_df, 'Age', AGE_COEFFIENTS['beta0'],
                                AGE_COEFFIENTS['beta1'])
    data_df = numeric_handle_lr(data_df, 'TumorSize', TUMERSIZE_COEEFIENTS['beta0'],
                                TUMERSIZE_COEEFIENTS['beta1'])
    #data_df['total_score'] = data_df.apply(lambda x: x.sum(), axis=1)
    data_df['total_score'] = data_df['Age'] + data_df['Marital'] + data_df['Race'] + \
                             data_df['Histology'] + data_df['Grade'] + data_df['Surgery'] + \
                             data_df['TumorSize'] + data_df['NodeInvolvement'] + \
                             data_df['Metastasis'] + data_df['Radiation'] + data_df['Chemotherapy']

    data_df['survival'] = data['survival']
    data_df['dead'] = data['dead']
    data_df['id'] = data['id']
    OS_Calc(data_df,OS_COEFFIENT_1year)
    OS_Calc(data_df,OS_COEFFIENT_3year)
    data_df.to_stata('OS'+data_path.split('/')[-1])





