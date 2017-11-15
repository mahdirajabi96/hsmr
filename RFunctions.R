# Import & Info: -------------------------------------------------------------------------------------------
#Scope: SCALL,NCALL,SCSEL,NCSEL
#MType: 'Base','Covar','Full','All'
#CrashAssig: '250','Var'
#Type:'R2U','R4D','R4U','U2U','U3T','U4U','U4D','U5T','AllSeg'
#     'R3ST'  ,'R4ST'  ,'R4SG'  ,'RM3ST'  ,'RM4ST'  ,'RM4SG'  ,'U3ST'  ,'U4ST'  ,'U3SG'  ,'U4SG'  ,'AllInt'
#CFDef: 'HSM','LSE','Fitted'
library(rgl)   #plot3d
library(MASS)  #NB Regression
library(plotrix) #color scale
library(ape) #Moran I

# General Functions: -------------------------------------------------------------------------------------------
SaveImage = function(Scope,FileName,Format){
  FN = paste('Outputs/',Scope,'/',FileName,sep='')
  if(Format=='wmf'){win.metafile(paste(FN,'.wmf',sep=''))}
  if(Format=='jpg'){jpg(filename = paste(FN,'.jpg',sep=''), width = 1500, height = 1000, units = "px", pointsize = 12,quality = 100, res =200,bg='transparent' )}
  if(Format=='png'){png(filename = paste(FN,'.png',sep=''), width = 10   , height = 5   , units = "in", pointsize = 12              , res =200,bg='transparent' )}
  if(Format=='bmp'){bmp(paste(FN,'.bmp',sep=''))}
}
FileNames = function(Scope = NULL,Facility = NULL,Year = '2014',CrashAssign = 'Var'){
  #Scopes: SCALL,NCALL,SCSEL,NCSEL
  #Facility: Crash,Seg,Int
  #CrashAssign: Var,250
  O = NULL
  if(Scope == 'SCSEL'){
    if(Facility == 'Int'){
      if(CrashAssign == 'Var'){O = paste('Inputs/Int_',toString(Year),    '.csv',sep = '')}
      if(CrashAssign == '250'){O = paste('Inputs/Int_',toString(Year),'_250.csv',sep = '')}
    }
    if(Facility == 'Seg'){
      if(CrashAssign == 'Var'){O = paste('Inputs/Seg_',toString(Year),    '.csv',sep = '')}
      if(CrashAssign == '250'){O = paste('Inputs/Seg_',toString(Year),'_250.csv',sep = '')}
    }
    if(Facility == 'Crash'){
      if(CrashAssign == 'Var'){O = paste('Inputs/SC_Crash_',toString(Year),    '.csv',sep = '')}
      if(CrashAssign == '250'){O = paste('Inputs/SC_Crash_',toString(Year),'_250.csv',sep = '')}
    }
  }
  if(Scope == 'SCALL'){
    if(Facility == 'Int'){
      if(CrashAssign == 'Var'){O = paste('Inputs/SC_Int_',toString(Year),    '.csv',sep = '')}
      if(CrashAssign == '250'){O = paste('Inputs/SC_Int_',toString(Year),'_250.csv',sep = '')}
    }
    if(Facility == 'Seg'){
      if(CrashAssign == 'Var'){O = paste('Inputs/SC_Seg_',toString(Year),    '.csv',sep = '')}
      if(CrashAssign == '250'){O = paste('Inputs/SC_Seg_',toString(Year),'_250.csv',sep = '')}
    }
    if(Facility == 'Crash'){
      if(CrashAssign == 'Var'){O = paste('Inputs/SC_Crash_',toString(Year),    '.csv',sep = '')}
      if(CrashAssign == '250'){O = paste('Inputs/SC_Crash_',toString(Year),'_250.csv',sep = '')}
    }
  }  
  if(Scope == 'NCSEL'){
    if(Facility == 'Int'){
      if(CrashAssign == 'Var'){O = paste('Inputs/NCSEL_Int_',toString(Year),    '.csv',sep = '')}
      if(CrashAssign == '250'){O = paste('Inputs/NCSEL_Int_',toString(Year),'_250.csv',sep = '')}
    }
    if(Facility == 'Seg'){
      if(CrashAssign == 'Var'){O = paste('Inputs/NCSEL_Seg_',toString(Year),    '.csv',sep = '')}
      if(CrashAssign == '250'){O = paste('Inputs/NCSEL_Seg_',toString(Year),'_250.csv',sep = '')}
    }
    if(Facility == 'Crash'){
      if(CrashAssign == 'Var'){O = paste('Inputs/NC_Crash_',toString(Year),    '.csv',sep = '')}
      if(CrashAssign == '250'){O = paste('Inputs/NC_Crash_',toString(Year),'_250.csv',sep = '')}
    }
  }
  if(Scope == 'NCALL'){
    if(Facility == 'Int'){
      if(CrashAssign == 'Var'){O = paste('Inputs/NC_Int_',toString(Year),    '.csv',sep = '')}
    }
    if(Facility == 'Seg'){
      if(CrashAssign == 'Var'){O = paste('Inputs/NC_Seg_',toString(Year),    '.csv',sep = '')}
    }
    if(Facility == 'Crash'){
      if(CrashAssign == 'Var'){O = paste('Inputs/NC_Crash_',toString(Year),    '.csv',sep = '')}
    }
  }  
  return(O)
}
ConvertNCSEL = function(){
  MergeDF = function(DL){
    n = NULL
    for(d in DL){
      n = c(n,names(d))
    }
    O = NULL
    n = unique(n)
    for(d in DL){
      for(nn in n){
        if(!(nn %in% names(d))){
          d[nn]=NA
        }
      }
      O = rbind(O,d)
    }
    return(O)
  }
  
  USeg2009 = read.csv('Inputs/NCSEL_USeg_2009.csv')
  USeg2008 = read.csv('Inputs/NCSEL_USeg_2008.csv')
  USeg2007 = read.csv('Inputs/NCSEL_USeg_2007.csv')
  USeg2009['Year'] = 2009;USeg2009$County = substr(USeg2009$cntryrte,1,2)
  USeg2008['Year'] = 2008;USeg2008$County = substr(USeg2008$cntryrte,1,2)
  USeg2007['Year'] = 2007;USeg2007$County = substr(USeg2007$cntrynbr,1,2)
  USeg = MergeDF(list(USeg2009,USeg2008,USeg2007))
  USeg$FType = paste('U',USeg$Type,sep='')
  
  RMSeg2009 = read.csv('Inputs/NCSEL_R4D_2009.csv')
  RMSeg2008 = read.csv('Inputs/NCSEL_R4D_2008.csv')
  RMSeg2007 = read.csv('Inputs/NCSEL_R4D_2007.csv')
  RMSeg2009['Year'] = 2009;RMSeg2009$County = substr(RMSeg2009$cntyrte,1,2)
  RMSeg2008['Year'] = 2008;RMSeg2008$County = substr(RMSeg2008$cntyrte,1,2)
  RMSeg2007['Year'] = 2007;RMSeg2007$County = substr(RMSeg2007$cntyrte,1,2)
  RMSeg = MergeDF(list(RMSeg2009,RMSeg2008,RMSeg2007))
  RMSeg$FType = 'R4D'
  
  RMInt2009 = read.csv('Inputs/NCSEL_R4Int_2009.csv')
  RMInt2008 = read.csv('Inputs/NCSEL_R4Int_2008.csv')
  RMInt2007 = read.csv('Inputs/NCSEL_R4Int_2007.csv')
  RMInt2009['Year'] = 2009
  RMInt2008['Year'] = 2008
  RMInt2007['Year'] = 2007
  RMInt = MergeDF(list(RMInt2009,RMInt2008,RMInt2007))
  RMInt$FType = paste('RM',RMInt$Type,sep='')
  
  
  R2Int2009 = read.csv('Inputs/NCSEL_R2Int_2009.csv')
  R2Int2008 = read.csv('Inputs/NCSEL_R2Int_2008.csv')
  R2Int2007 = read.csv('Inputs/NCSEL_R2Int_2007.csv')
  R2Int2009['Year'] = 2009
  R2Int2008['Year'] = 2008
  R2Int2007['Year'] = 2007
  R2Int = MergeDF(list(R2Int2009,R2Int2008,R2Int2007))
  R2Int$FType = paste('R',R2Int$Type,sep='')
  
  UInt2009 = read.csv('Inputs/NCSEL_UInt_2009.csv')
  UInt2008 = read.csv('Inputs/NCSEL_UInt_2008.csv')
  UInt2007 = read.csv('Inputs/NCSEL_UInt_2007.csv')
  UInt2009['Year'] = 2009
  UInt2008['Year'] = 2008
  UInt2007['Year'] = 2007
  UInt = MergeDF(list(UInt2009,UInt2008,UInt2007))
  UInt$FType = paste('UM',UInt$Type,sep='')
  
  Int = MergeDF(list(UInt,R2Int,RMInt))
  Seg = MergeDF(list(USeg,RMSeg))
  if ('AADT.Major' %in% names(Int)){Int['AADT_Major'] = Int['AADT.Major']}
  if ('AADT.Minor' %in% names(Int)){Int['AADT_Minor'] = Int['AADT.Minor']}
  Seg$Length = Seg$Length*5280

  Int$FI_OC = Int$TOT_OC*.2
  Seg$FI_OC = Seg$TOT_OC*.2
  
  
  IntHeaders = c('ID','AADT_Major','AADT_Minor','TOT_OC','TOT_PC','CCMF','Year','FType','FI_OC')
  IntAreaHeaders = c('AADT_Major','AADT_Minor','TOT_OC','TOT_PC','CCMF','FType')
  SegHeaders = c('ID','AADT','Length','TOT_OC','TOT_PC','CCMF','FType','Year','FI_OC','County')
  Int = subset(Int,select = IntHeaders[IntHeaders %in% names(Int)])
  Seg = subset(Seg,select = SegHeaders[SegHeaders %in% names(Seg)])
  Int=Int[order(Int$FType),]
  Seg=Seg[order(Seg$FType),]
  
  I2007 = Int[which(Int$Year==2007),]
  I2008 = Int[which(Int$Year==2008),]
  I2009 = Int[which(Int$Year==2009),]
  AreaInt = read.csv('Inputs/NCSEL_Int_Area.csv')
  AreaInt$ID = AreaInt$NewID
  Head = names(AreaInt)[!(names(AreaInt) %in% IntAreaHeaders)]
  AreaInt = subset(AreaInt,select=names(AreaInt)[!(names(AreaInt) %in% IntAreaHeaders)])
  I2007 = merge(I2007,AreaInt,by='ID')
  I2008 = merge(I2008,AreaInt,by='ID')
  I2009 = merge(I2009,AreaInt,by='ID')
  
  write.csv(I2007,paste('Inputs/NCSEL_Int_2007_250.csv',sep=''),row.names=F)
  write.csv(I2008,paste('Inputs/NCSEL_Int_2008_250.csv',sep=''),row.names=F)
  write.csv(I2009,paste('Inputs/NCSEL_Int_2009_250.csv',sep=''),row.names=F)
  
  AreaSeg = read.csv('Inputs/NCSEL_Seg_Area.csv')
  AreaSeg$County = AreaSeg$DOT_COUNTY
  AreaSeg = subset(AreaSeg,select=c('County','Area1','Area2'))
  
  Seg = merge(Seg,AreaSeg,by='County') 
  S2007 = Seg[which(Seg$Year==2007),]
  S2008 = Seg[which(Seg$Year==2008),]
  S2009 = Seg[which(Seg$Year==2009),]
  
  write.csv(S2007,paste('Inputs/NCSEL_Seg_2007_250.csv',sep=''),row.names=F)
  write.csv(S2008,paste('Inputs/NCSEL_Seg_2008_250.csv',sep=''),row.names=F)
  write.csv(S2009,paste('Inputs/NCSEL_Seg_2009_250.csv',sep=''),row.names=F)
}
Round = function(Number,decimals){
  return(round(Number*10^decimals)/10^decimals)
}

# Reading Data: -------------------------------------------------------------------------------------------
Text2Dat        = function(Scope,Type,Year,CrashAssign = 'Var',Headers = NULL){
  D = NULL
  IntTypes = c('R3ST','R4ST','R4SG','RM3ST','RM4ST','RM4SG','U3ST','U4ST','U3SG','U4SG')
  SegTypes = c('R2U','R4D','R4U','U2U','U3T','U4U','U4D','U5T','R4F','U4F','U6F')
  AllInt   = c('AllInt','AllIntSig','AllIntStp')
  AllSeg   = c('AllSeg')

  for (y in Year){
    
    #Read Data
    if (Type %in% c(AllInt,IntTypes)){FileName = FileNames(Scope,'Int',y,CrashAssign)}
    if (Type %in% c(AllSeg,SegTypes)){FileName = FileNames(Scope,'Seg',y,CrashAssign)}
    Tab = read.csv(FileName,header = TRUE)
    Tab[is.na(Tab)] = 0
    
    
    #Split Data
    if('ONEWAY' %in% names(Tab)){
      Tab = Tab[which(Tab$ONEWAY==0),]
    }
    if(Type %in% c(IntTypes,SegTypes) & 'FType' %in% names(Tab)){
      Tab = split(Tab,Tab['FType'])
      Tab = Tab[Type]
      Tab = Tab[[1]]
    }
    if(is.null(Tab)){stop(paste('No sites of',Type,'where found in:',FileName))}
    Tab['FID']=seq(0,length(Tab[,1])-1)
    if(Type %in% c(SegTypes,AllSeg) && all(c('dMjC','dMnC','dMjI','dMnI','dMjR','dMnR','dO','Length') %in% names(Tab))){
      Tab['DrwDens']=(Tab['dMjC']+Tab['dMnC']+Tab['dMjI']+Tab['dMnI']+Tab['dMjR']+Tab['dMnR']+Tab['dO'])/Tab['Length']*5280
    }
    
    #Rename
    if ('AREA1' %in% names(Tab)){Tab['Area1'] = Tab['AREA1']}
    if ('AREA2' %in% names(Tab)){Tab['Area2'] = Tab['AREA2']}
    if ('TOTALLANES' %in% names(Tab)){Tab['TotalLanes'] = Tab['TOTALLANES']}
    if ('no_lanes'   %in% names(Tab)){Tab['TotalLanes'] = Tab['no_lanes'  ]}
    if ('med_type'   %in% names(Tab)){Tab['MedianID'  ] = Tab['med_type'  ]}
    if ('Median_ID'  %in% names(Tab)){Tab['MedianID'  ] = Tab['Median_ID' ]}
    if ('Pop_dens_p' %in% names(Tab)){Tab['PopDens'   ] = Tab['Pop_dens_p']}
    if ('aadt'       %in% names(Tab)){Tab['AADT'      ] = Tab['aadt'      ]}
    if ('Shape_Leng' %in% names(Tab) & !('Length' %in% names(Tab))){Tab['Length'] = Tab['Shape_Leng']}
    if(!('AADT' %in% names(Tab))){
      if(y==2014 & 'AADT14' %in% names(Tab)){Tab['AADT'] = (Tab['AADT14'])}
      if(y==2013 & 'AADT13' %in% names(Tab)){Tab['AADT'] = (Tab['AADT13'])}
      if(y==2012 & 'AADT12' %in% names(Tab)){Tab['AADT'] = (Tab['AADT12'])}
      if(y==2011 & 'AADT11' %in% names(Tab)){Tab['AADT'] = (Tab['AADT11'])}
    }      
    
    if ('AADT_Major' %in% names(Tab)){
      Tab = Tab[which(Tab$AADT_Major > 0),]
      Tab['AADT_Major'] = log(Tab['AADT_Major'])
    }
    if ('AADT_Minor' %in% names(Tab)){
      Tab = Tab[which(Tab$AADT_Minor > 0),]
      Tab['AADT_Minor'] = log(Tab['AADT_Minor'])
    }
    if ('AADT' %in% names(Tab)){
      Tab = Tab[which(Tab$AADT > 0),]
      Tab['AADT'] = log(Tab['AADT'])
    }
    if ('LENGTH' %in% names(Tab)){
      Tab['Length'] = Tab['LENGTH']
    }
    if ('Length' %in% names(Tab)){
      Tab = Tab[which(Tab$Length > 0),]
      Tab['Length'] = log(Tab['Length']/5280)
    }
    if(Scope %in% c('SCSEL','SCALL') & 'MedianID' %in% names(Tab)){
      Tab['MedianID'] = (Tab['MedianID'] == 3)*1 + 
        (Tab['MedianID'] == 1)*2 + (Tab['MedianID'] == 2)*2 + 
        (Tab['MedianID'] == 4)*2 + (Tab['MedianID'] == 5)*2 + (Tab['MedianID'] == 6)*2 
      Tab$MedianID = Tab$MedianID[,1]
    }
    if(Scope %in% c('NCSEL','NCALL') & 'MedianID' %in% names(Tab)){
      Tab['MedianID'] = (Tab['MedianID'] == 2)*1 + 
        (Tab['MedianID'] == 1)*2 + (Tab['MedianID'] == 3)*2 + 
        (Tab['MedianID'] == 4)*2 + (Tab['MedianID'] == 5)*2 + (Tab['MedianID'] == 6)*2 +
        (Tab['MedianID'] == 7)*2 + (Tab['MedianID'] == 8)*2 + (Tab['MedianID'] == 9)*2 +
        (Tab['MedianID'] == 10)*2 + (Tab['MedianID'] == 11)*2
      Tab$MedianID = Tab$MedianID[,1]
    }
    
    if ('Lane_Width' %in% names(Tab)){Tab['Lane_Width'] = Tab['Lane_Width'] - 12}
    if ('PopDens'    %in% names(Tab)){Tab['PopDens'   ] = log(Tab['PopDens'] + 5)}
    
    if(Type=='R2U'){
      if('Shuold_Wid' %in% names(Tab)){Tab['Shuold_Wid']=Tab['Shuold_Wid'] - 6}
      if('RHR'        %in% names(Tab)){Tab['RHR'       ]=Tab['RHR'       ] - 3}
      if('DrwDens'    %in% names(Tab)){Tab['DrwDens'   ]=Tab['DrwDens'   ] - 5}
    }
    if(Type=='R4U'){
      if('Shuold_Wid' %in% names(Tab)){Tab['Shuold_Wid']=Tab['Shuold_Wid'] - 6}
    }
    if(Type=='R4D'){
      if('Shuold_Wid' %in% names(Tab)){Tab['Shuold_Wid']=Tab['Shuold_Wid'] - 8}
      if('Median_Wid' %in% names(Tab)){Tab['Median_Wid']=Tab['Median_Wid'] - 30}
    }
    if(Type=='U4D'){
      if('Median_Wid' %in% names(Tab)){Tab['Median_Wid']=Tab['Median_Wid'] - 15}
    }

    if(Type %in% AllInt & 'FType' %in% names(Tab)){
      Tab = Tab[which(Tab$FType %in% IntTypes),]
      if('TOTALLANES' %in% names(Tab)){
        Tab = Tab[which(Tab$TOTALLANES %in% c(2,4)),]
      }
      if('RCT_Major' %in% names(Tab) & 'RCT_Minor' %in% names(Tab)){
        Tab = Tab[which(Tab$RCT_Major %in% c(2,4,7) & Tab$RCT_Minor %in% c(2,4,7)),]
      }
    }
    if(Type %in% AllSeg){
      Tab = Tab[which(Tab$FType %in% SegTypes),]
      if('Route_Type' %in% names(Tab)){
        Tab = Tab[which(Tab$Route_Type %in% c(2,4,7)),]
      }
    }
    if(Type == 'AllIntSig' & 'SG' %in% names(Tab)){Tab = Tab[which(Tab$SG == 1),]}
    if(Type == 'AllIntStp' & 'SG' %in% names(Tab)){Tab = Tab[which(Tab$SG == 0),]}
    if(!is.null(Headers)){
      #Headers = ListHeaders(Type)
      Tab = subset(Tab,select = Headers[Headers %in% names(Tab)])
    }
    CommNames = names(Tab)
    if(!is.null(D)){CommNames = Reduce(intersect,list(names(D),names(Tab)))}
    D = rbind(D[,CommNames],Tab[,CommNames])
  }
  return (D)
}
ListHeaders     = function(Type){
  Out = NULL
  IntTypes = c('R3ST','R4ST','R4SG','RM3ST','RM4ST','RM4SG','U3ST','U4ST','U3SG','U4SG','AllInt','AllIntSig','AllIntStp')
  SegTypes = c('R2U','R4D','R4U','U2U','U3T','U4U','U4D','U5T','AllSeg')
  AllInt   = c('AllInt','AllIntSig','AllIntStp')
  AllSeg   = c('AllSeg')
  
  AllIntL = c('Area1','Area2','Area3','FitDif','GiZScore','GiPValue','FID','TOT_OC','FI_OC','TOT_PC','AADT_Major','AADT_Minor','FType','CCMF','PopDens','TotalLanes','SG','LEGS','RCT_Major','RCT_Minor','Lane_Major','Lane_Minor','URBAN')
  AllSegL = c('Area1','Area2','Area3','FitDif','GiZScore','GiPValue','FID','TOT_OC','FI_OC','TOT_PC','AADT'      ,'Length'    ,'FType','CCMF','PopDens','TotalLanes','MedianID','Route_Type')
  R3ST  = c(AllIntL,c('LIGHTING','LTL','RTL','SKEW1'))
  R4ST  = c(AllIntL,c('LIGHTING','LTL','RTL','SKEW1','SKEW2'))
  R4SG  = c(AllIntL,c('LIGHTING','LTL','RTL'))
  RM3ST = c(AllIntL,c('LIGHTING','LTL','RTL','SKEW1'))
  RM4ST = c(AllIntL,c('LIGHTING','LTL','RTL','SKEW1','SKEW2'))
  RM4SG = c(AllIntL)
  U3ST = c(AllIntL,c('LIGHTING','LTL','RTL'))
  U4ST = c(AllIntL,c('LIGHTING','LTL','RTL'))
  U3SG = c(AllIntL,c('LIGHTING','LTL','RTL','LTP1','LTP2','No_RTOR'))
  U4SG = c(AllIntL,c('LIGHTING','LTL','RTL','LTP1','LTP2','LTP3','LTP4','No_RTOR'))
  
  R2U   = c(AllSegL,c(           'HorCur','Grade','Lane_Width','Shuold_Wid','Shuold_Typ','RHR','DrwDens','URBAN'))
  R4U   = c(AllSegL,c('LIGHTING','Lane_Width','Shuold_Wid','Grade','FI_PC'))
  R4D   = c(AllSegL,c('LIGHTING','Lane_Width','Shuold_Wid','Median_Wid','FI_PC'))
  U2U   = c(AllSegL,c('LIGHTING','OSPType','OSPProp','FODensity','FOOffset','dMjC','dMnC','dMjI','dMnI','dMjR','dMnR','dO','DrwDens','MV'))
  U3T   = c(AllSegL,c('LIGHTING'                    ,'FODensity','FOOffset','dMjC','dMnC','dMjI','dMnI','dMjR','dMnR','dO','DrwDens'))
  U4D   = c(AllSegL,c('LIGHTING','OSPType','OSPProp','FODensity','FOOffset','dMjC','dMnC','dMjI','dMnI','dMjR','dMnR','dO','Median_Wid','DrwDens'))
  U4U   = c(AllSegL,c('LIGHTING','OSPType','OSPProp','FODensity','FOOffset','dMjC','dMnC','dMjI','dMnI','dMjR','dMnR','dO','DrwDens'))
  U5T   = c(AllSegL,c('LIGHTING'                    ,'FODensity','FOOffset','dMjC','dMnC','dMjI','dMnI','dMjR','dMnR','dO','DrwDens'))
  
  Out = list('R3ST'=R3ST,'R4ST'=R4ST,'R4SG'=R4SG,'RM3ST'=RM3ST,'RM4ST'=RM4ST,'RM4SG'=RM4SG,
             'U3ST'=U3ST,'U4ST'=U4ST,'U3SG'=U3SG,'U4SG'=U4SG,
             'R2U'=R2U,'R4D'=R4D,'R4U'=R4U,'U2U'=U2U,'U3T'=U3T,'U4D'=U4D,'U4U'=U4U,'U5T'=U5T)
  
  if(Type %in% c(AllInt)){return(AllIntL)}
  if(Type %in% c(AllSeg)){return(AllSegL)}
  if(Type %in% c(IntTypes)){return(Out[Type][[1]])}
  if(Type %in% c(SegTypes)){return(Out[Type][[1]])}
}
FilterData      = function(Scope,Type,Years,Field){
  # c('Cook','JR','Lev','AADT Major','AADT Minor')
  S1 = 'SCSEL'
  Y1 = toString(2011:2014)
  R3ST  = c(c(4,0,1),c(16000,7500 ))
  R4ST  = c(c(2,0,2),c(13000,3500 ))
  R4SG  = c(c(1,0,0),c(15000,10000))
  RM3ST = c(c(1,0,5),c(30000,8000 ))
  RM4ST = c(c(0,0,0),c(30000,10000 ))
  RM4SG = c(c(1,1,0),c(35000,8500 ))
  U3ST = c(c(2,0,0),c(45000,20000 ))
  U4ST = c(c(1,1,0),c(40000,8000 ))
  U3SG = c(c(3,0,1),c(60000,50000))
  U4SG = c(c(4,0,0),c(50000,30000))
  AllInt = c(c(4,0,0),c(50000,30000))
  AllIntSig = c(c(1,0,0),c(60000,50000))
  AllIntStp = c(c(1,0,0),c(50000,20000))
  R2U = c(c(1,0,0),c(16000,0.1))
  R4U = c(c(1,0,0),c(13000,0.1))
  R4D = c(c(1,0,0),c(60000,0.1))
  U2U = c(c(1,0,0),c(35000,0.1))
  U3T = c(c(1,0,0),c(26000,0.1))
  U4U = c(c(1,0,0),c(35000,0.1))
  U4D = c(c(1,0,0),c(80000,0.1))
  U5T = c(c(1,0,0),c(50000,0.1))
  AllSeg = c(c(1,0,0),c(80000,0.1))
  T1        = list(R3ST  ,R4ST  ,R4SG  ,RM3ST  ,RM4ST  ,RM4SG  ,U3ST  ,U4ST  ,U3SG  ,U4SG  ,R2U  ,R4D  ,R4U  ,U2U  ,U3T  ,U4U  ,U4D  ,U5T , AllInt , AllIntSig , AllIntStp , AllSeg )
  names(T1) = c(  'R3ST','R4ST','R4SG','RM3ST','RM4ST','RM4SG','U3ST','U4ST','U3SG','U4SG','R2U','R4D','R4U','U2U','U3T','U4U','U4D','U5T','AllInt','AllIntSig','AllIntStp','AllSeg')
  
  Y2 = toString(2013:2014)
  R3ST  = c(c(1,0,0),c(16000,7500 ))
  R4ST  = c(c(4,0,0),c(13000,3500 ))
  R4SG  = c(c(1,0,0),c(15000,10000))
  RM3ST = c(c(1,0,2),c(30000,8000 ))
  RM4ST = c(c(3,2,0),c(20000,2000 ))
  RM4SG = c(c(1,1,0),c(35000,9000 ))
  U3ST = c(c(1,0,0),c(45000,8000 ))
  U4ST = c(c(1,1,2),c(40000,8000 ))
  U3SG = c(c(3,0,0),c(40000,15000))
  U4SG = c(c(2,0,0),c(40000,20000))
  AllInt = c(c(4,0,0),c(50000,30000))
  AllIntSig = c(c(1,0,0),c(60000,50000))
  AllIntStp = c(c(1,0,0),c(50000,20000))
  R2U = c(c(1,1,0),c(16000,0.1))
  R4U = c(c(1,0,1),c(13000,0.1))
  R4D = c(c(1,0,2),c(32000,0.1))
  U2U = c(c(1,1,0),c(15000,0.1))
  U3T = c(c(1,1,2),c(27000,0.1))
  U4U = c(c(2,1,2),c(35000,0.1))
  U4D = c(c(1,0,2),c(55000,0.1))
  U5T = c(c(1,1,0),c(38000,0.1))
  R4F = c(c(1,1,0),c(80000,0.1))
  U4F = c(c(1,1,0),c(90000,0.1))
  U6F = c(c(1,1,0),c(120000,0.1))
  AllSeg = c(c(1,0,0),c(80000,0.1))
  T2        = list(R3ST  ,R4ST  ,R4SG  ,RM3ST  ,RM4ST  ,RM4SG  ,U3ST  ,U4ST  ,U3SG  ,U4SG  ,R2U  ,R4D  ,R4U  ,U2U  ,U3T  ,U4U  ,U4D  ,U5T ,R4F  ,U4F  ,U6F  , AllInt , AllIntSig , AllIntStp , AllSeg )
  names(T2) = c(  'R3ST','R4ST','R4SG','RM3ST','RM4ST','RM4SG','U3ST','U4ST','U3SG','U4SG','R2U','R4D','R4U','U2U','U3T','U4U','U4D','U5T','R4F','U4F','U6F','AllInt','AllIntSig','AllIntStp','AllSeg')
  
  Y3 = toString(2014:2014)
  R3ST  = c(c(1,0,0),c(16000,7500 ))
  R4ST  = c(c(1,0,0),c(13000,3500 ))
  R4SG  = c(c(1,0,0),c(15000,10000))
  RM3ST = c(c(1,0,0),c(30000,8000 ))
  RM4ST = c(c(1,0,0),c(20000,2000 ))
  RM4SG = c(c(1,0,0),c(35000,9000 ))
  U3ST = c(c(1,0,0),c(45000,8000 ))
  U4ST = c(c(1,0,0),c(40000,8000 ))
  U3SG = c(c(1,0,0),c(40000,15000))
  U4SG = c(c(1,0,0),c(40000,20000))
  AllInt = c(c(4,0,0),c(50000,30000))
  AllIntSig = c(c(1,0,0),c(60000,50000))
  AllIntStp = c(c(1,0,0),c(50000,20000))
  R2U = c(c(1,0,0),c(16000,0.1))
  R4U = c(c(1,0,0),c(13000,0.1))
  R4D = c(c(1,0,0),c(32000,0.1))
  U2U = c(c(1,0,0),c(15000,0.1))
  U3T = c(c(1,0,0),c(27000,0.1))
  U4U = c(c(1,0,0),c(35000,0.1))
  U4D = c(c(1,0,0),c(55000,0.1))
  U5T = c(c(1,0,0),c(38000,0.1))
  AllSeg = c(c(1,0,0),c(80000,0.1))
  T3        = list(R3ST  ,R4ST  ,R4SG  ,RM3ST  ,RM4ST  ,RM4SG  ,U3ST  ,U4ST  ,U3SG  ,U4SG  ,R2U  ,R4D  ,R4U  ,U2U  ,U3T  ,U4U  ,U4D  ,U5T , AllInt , AllIntSig , AllIntStp , AllSeg )
  names(T3) = c(  'R3ST','R4ST','R4SG','RM3ST','RM4ST','RM4SG','U3ST','U4ST','U3SG','U4SG','R2U','R4D','R4U','U2U','U3T','U4U','U4D','U5T','AllInt','AllIntSig','AllIntStp','AllSeg')
  
  Lib = list(T1,T2,T3,T3,T3,T3,T3,T3,T2)
  names(Lib) = c(Y1,Y2,Y3,'2013','2012','2011','2010','2015',toString(2013:2015))
  Bin1 = list(Lib)
  names(Bin1) = S1
  
  S2 = 'SCALL'
  Y4 = toString(2011:2014)
  R3ST  = c(c(4,0,1),c(16000,7500 ))
  R4ST  = c(c(2,0,2),c(13000,3500 ))
  R4SG  = c(c(1,0,0),c(15000,10000))
  RM3ST = c(c(1,0,5),c(30000,8000 ))
  RM4ST = c(c(0,0,0),c(30000,10000 ))
  RM4SG = c(c(1,1,0),c(35000,8500 ))
  U3ST = c(c(2,0,0),c(45000,20000 ))
  U4ST = c(c(1,1,0),c(40000,8000 ))
  U3SG = c(c(3,0,1),c(60000,50000))
  U4SG = c(c(4,0,0),c(50000,30000))
  AllInt = c(c(4,0,0),c(50000,30000))
  AllIntSig = c(c(1,0,0),c(60000,50000))
  AllIntStp = c(c(1,0,0),c(50000,20000))
  R2U = c(c(1,0,0),c(16000,0.1))
  R4U = c(c(1,0,0),c(13000,0.1))
  R4D = c(c(1,0,0),c(60000,0.1))
  U2U = c(c(1,0,0),c(35000,0.1))
  U3T = c(c(1,0,0),c(26000,0.1))
  U4U = c(c(1,0,0),c(35000,0.1))
  U4D = c(c(1,0,0),c(80000,0.1))
  U5T = c(c(1,0,0),c(50000,0.1))
  AllSeg = c(c(1,0,0),c(80000,0.1))
  T4        = list(R3ST  ,R4ST  ,R4SG  ,RM3ST  ,RM4ST  ,RM4SG  ,U3ST  ,U4ST  ,U3SG  ,U4SG  ,R2U  ,R4D  ,R4U  ,U2U  ,U3T  ,U4U  ,U4D  ,U5T , AllInt , AllIntSig , AllIntStp , AllSeg )
  names(T4) = c(  'R3ST','R4ST','R4SG','RM3ST','RM4ST','RM4SG','U3ST','U4ST','U3SG','U4SG','R2U','R4D','R4U','U2U','U3T','U4U','U4D','U5T','AllInt','AllIntSig','AllIntStp','AllSeg')
  
  Y5 = toString(2013:2014)
  Y6 = toString(2014:2014)
  Lib = list(T4,T4,T4,T4,T4,T4,T4)
  names(Lib) = c(Y4,Y5,Y6,'2013','2012','2011')
  Bin2 = list(Lib)
  names(Bin2) = S2
  
  S3 = 'NCALL'
  Y7 = toString(2011:2013)
  R3ST  = c(c(4,0,1),c(22000,7500 ))
  R4ST  = c(c(2,0,2),c(13000,3500 ))
  R4SG  = c(c(1,0,0),c(15000,10000))
  RM3ST = c(c(1,0,5),c(30000,8000 ))
  RM4ST = c(c(0,0,0),c(30000,10000 ))
  RM4SG = c(c(1,1,0),c(35000,8500 ))
  U3ST = c(c(2,0,0),c(45000,20000 ))
  U4ST = c(c(1,1,0),c(40000,8000 ))
  U3SG = c(c(3,0,1),c(60000,50000))
  U4SG = c(c(4,0,0),c(50000,30000))
  AllInt = c(c(4,0,0),c(50000,30000))
  AllIntSig = c(c(1,0,0),c(60000,50000))
  AllIntStp = c(c(1,0,0),c(50000,20000))
  R2U = c(c(1,0,0),c(21000,0.1))
  R4U = c(c(1,0,0),c(40000,0.1))
  R4D = c(c(1,0,0),c(51000,0.1))
  U2U = c(c(1,0,0),c(40000,0.1))
  U3T = c(c(1,0,0),c(27000,0.1))
  U4U = c(c(1,0,0),c(80000,0.1))
  U4D = c(c(1,0,0),c(120000,0.1))
  U5T = c(c(1,0,0),c(60000,0.1))
  AllSeg = c(c(1,0,0),c(80000,0.1))
  T5        = list(R3ST  ,R4ST  ,R4SG  ,RM3ST  ,RM4ST  ,RM4SG  ,U3ST  ,U4ST  ,U3SG  ,U4SG  ,R2U  ,R4D  ,R4U  ,U2U  ,U3T  ,U4U  ,U4D  ,U5T , AllInt , AllIntSig , AllIntStp , AllSeg )
  names(T5) = c(  'R3ST','R4ST','R4SG','RM3ST','RM4ST','RM4SG','U3ST','U4ST','U3SG','U4SG','R2U','R4D','R4U','U2U','U3T','U4U','U4D','U5T','AllInt','AllIntSig','AllIntStp','AllSeg')
  
  Y8 = toString(2013:2013)
  Lib = list(T5,T5,T5,T5)
  names(Lib) = c(Y7,Y8,'2012','2011')
  Bin3 = list(Lib)
  names(Bin3) = S3

  S4 = 'NCSEL'
  Y9 = toString(2007:2009)
  R3ST  = c(c(0,0,0),c(17000,6000 ))
  R4ST  = c(c(0,0,0),c(25000,6000 ))
  R4SG  = c(c(0,0,0),c(30000,25000))
  RM3ST = c(c(0,0,0),c(15000,8000 ))
  RM4ST = c(c(0,0,0),c(10000,2500 ))
  RM4SG = c(c(0,0,0),c(50000,15000 ))
  U3ST = c(c(0,0,0),c(40000,18000 ))
  U4ST = c(c(0,0,0),c(30000,15000 ))
  U3SG = c(c(0,0,0),c(40000,17000))
  U4SG = c(c(0,0,0),c(41000,30000))
  AllInt = c(c(0,0,0),c(52000,30000))
  AllIntSig = c(c(0,0,0),c(60000,50000))
  AllIntStp = c(c(0,0,0),c(50000,20000))
  R2U = c(c(0,0,0),c(21000,0))
  R4U = c(c(0,0,0),c(40000,0))
  R4D = c(c(0,0,0),c(75000,0))
  U2U = c(c(0,0,0),c(40000,0))
  U3T = c(c(0,0,0),c(33000,0))
  U4U = c(c(0,0,0),c(46000,0))
  U4D = c(c(0,0,0),c(46000,0))
  U5T = c(c(0,0,0),c(46000,0))
  AllSeg = c(c(0,0,0),c(72000,0))
  T6        = list(R3ST  ,R4ST  ,R4SG  ,RM3ST  ,RM4ST  ,RM4SG  ,U3ST  ,U4ST  ,U3SG  ,U4SG  ,R2U  ,R4D  ,R4U  ,U2U  ,U3T  ,U4U  ,U4D  ,U5T , AllInt , AllIntSig , AllIntStp , AllSeg )
  names(T6) = c(  'R3ST','R4ST','R4SG','RM3ST','RM4ST','RM4SG','U3ST','U4ST','U3SG','U4SG','R2U','R4D','R4U','U2U','U3T','U4U','U4D','U5T','AllInt','AllIntSig','AllIntStp','AllSeg')

  Y10 = toString(2011:2013)
  R3ST  = c(c(0,0,0),c(17000,12000 ))
  R4ST  = c(c(0,0,0),c(25000,6000 ))
  R4SG  = c(c(0,0,0),c(30000,25000))
  RM3ST = c(c(0,0,0),c(40000,8000 ))
  RM4ST = c(c(0,0,0),c(40000,8000 ))
  RM4SG = c(c(0,0,0),c(65000,15000 ))
  U3ST = c(c(0,0,0),c(40000,18000 ))
  U4ST = c(c(0,0,0),c(30000,15000 ))
  U3SG = c(c(0,0,0),c(50000,20000))
  U4SG = c(c(0,0,0),c(60000,30000))
  AllInt = c(c(0,0,0),c(65000,30000))
  AllIntSig = c(c(0,0,0),c(60000,50000))
  AllIntStp = c(c(0,0,0),c(65000,20000))
  R2U = c(c(0,0,0),c(21000,0))
  R4U = c(c(0,0,0),c(40000,0))
  R4D = c(c(0,0,0),c(75000,0))
  U2U = c(c(0,0,0),c(40000,0))
  U3T = c(c(0,0,0),c(33000,0))
  U4U = c(c(0,0,0),c(46000,0))
  U4D = c(c(0,0,0),c(46000,0))
  U5T = c(c(0,0,0),c(46000,0))
  AllSeg = c(c(0,0,0),c(72000,0))
  T7        = list(R3ST  ,R4ST  ,R4SG  ,RM3ST  ,RM4ST  ,RM4SG  ,U3ST  ,U4ST  ,U3SG  ,U4SG  ,R2U  ,R4D  ,R4U  ,U2U  ,U3T  ,U4U  ,U4D  ,U5T , AllInt , AllIntSig , AllIntStp , AllSeg )
  names(T7) = c(  'R3ST','R4ST','R4SG','RM3ST','RM4ST','RM4SG','U3ST','U4ST','U3SG','U4SG','R2U','R4D','R4U','U2U','U3T','U4U','U4D','U5T','AllInt','AllIntSig','AllIntStp','AllSeg')
  
  Lib = list(T6,T6,T6,T6,T7)
  names(Lib) = c(Y9,'2007','2008','2009',Y10)
  Bin4 = list(Lib)
  names(Bin4) = S4
  
  
  
  Lib = list(Bin1,Bin2,Bin3,Bin4)
  names(Lib) = c(S1,S2,S3,S4)
  Out = NULL
  if(Field=='Outliers'){Out = Lib[Scope][[1]][Scope][[1]][toString(Years)][[1]][Type][[1]][1:3]}
  if(Field=='Volumes' ){Out = Lib[Scope][[1]][Scope][[1]][toString(Years)][[1]][Type][[1]][4:5]}
  return(Out)
}
BaseData        = function(D){
  O = NULL  
  Type = toString(unique(D$FType)[1])
  if(Type=='R3ST' ){O = D[which(D$LTL==0 & D$RTL==0 & D$SKEW1<20),]}
  if(Type=='R4ST' ){O = D[which(D$LTL==0 & D$RTL==0 & D$SKEW1<20 & D$SKEW2<20),]}
  if(Type=='R4SG' ){O = D[which(D$LTL==0 & D$RTL==0),]}
  if(Type=='RM3ST'){O = D[which(D$LTL==0 & D$RTL==0 & D$SKEW1<20),]}
  if(Type=='RM4ST'){O = D[which(D$LTL==0 & D$RTL==0 & D$SKEW1<20 & D$SKEW2<20),]}
  if(Type=='RM4SG'){O = D}
  if(Type=='U3ST'){O = D[which(D$LTL==0 & D$RTL==0),]}
  if(Type=='U4ST'){O = D[which(D$LTL==0 & D$RTL==0),]}
  if(Type=='U3SG'){O = D[which(D$LTL==0 & D$RTL==0 & D$LTP1==0 & D$LTP2==0 & D$No_RTOR==0),]}
  if(Type=='U4SG'){O = D[which(D$LTL==0 & D$RTL==0 & D$LTP1==0 & D$LTP2==0 & D$LTP3==0 & D$LTP4==0 & D$No_RTOR==0),]}
  if(Type=='R2U'  ){O = D[which(abs(D$Lane_Width)<=1 & abs(D$Shuold_Wid)<=3  & abs(D$RHR)<=2 & abs(D$DrwDens)<=3 & D$HorCur<=0.001 & abs(D$Grade)<=0.05),]}
  if(Type=='R4U'  ){O = D[which(abs(D$Lane_Width)<=1 & abs(D$Shuold_Wid)<=3 & abs(D$Grade)<=0.05),]}
  if(Type=='R4D'  ){O = D[which(abs(D$Lane_Width)<=1 & abs(D$Shuold_Wid)<=4 & abs(D$Median_Wid)<=15),]}
  if(Type=='U2U'  ){O = D[which(D$OSPProp<0.1 & D$FODensity<0.1 & abs(D$DrwDens)<=3),]}
  if(Type=='U3T'  ){O = D[which(D$OSPProp<0.1 & D$FODensity<0.1 & abs(D$DrwDens)<=3),]}
  if(Type=='U4U'  ){O = D[which(D$OSPProp<0.1 & D$FODensity<0.1 & abs(D$DrwDens)<=3),]}
  if(Type=='U4D'  ){O = D[which(D$OSPProp<0.1 & D$FODensity<0.1 & abs(D$Median_Wid)<=10 & abs(D$DrwDens)<=3),]}
  if(Type=='U5T'  ){O = D[which(D$OSPProp<0.1 & D$FODensity<0.1 & abs(D$DrwDens)<=3),]}
  return(O)  
}
ListBaseValues  = function(Type){
  D = data.frame('SG'=0,'TotalLanes'=2,'LEGS'=4,'PopDens'=log(300),'MedianID'=0,'RCT_Major'=7,'RCT_Minor'=7,'Route_Type'=7,'Lane_Major'=2,'Lane_Minor'=2)
  if(Type=='R3ST' ){D$LIGHTING=0;D$LTL=0;D$RTL=0;D$SKEW1=0}
  if(Type=='R4ST' ){D$LIGHTING=0;D$LTL=0;D$RTL=0;D$SKEW1=0;D$SKEW2=0}
  if(Type=='R4SG' ){D$LIGHTING=0;D$LTL=0;D$RTL=0}
  if(Type=='RM3ST'){D$LIGHTING=0;D$LTL=0;D$RTL=0;D$SKEW1=0}
  if(Type=='RM4ST'){D$LIGHTING=0;D$LTL=0;D$RTL=0;D$SKEW1=0;D$SKEW2=0}
  if(Type=='RM4SG'){}
  if(Type=='U3ST'){D$LIGHTING=0;D$LTL=0;D$RTL=0}
  if(Type=='U4ST'){D$LIGHTING=0;D$LTL=0;D$RTL=0}
  if(Type=='U3SG'){D$LIGHTING=0;D$LTL=0;D$RTL=0;D$LTP1=0;D$LTP2=0;D$No_RTOR=0}
  if(Type=='U4SG'){D$LIGHTING=0;D$LTL=0;D$RTL=0;D$LTP1=0;D$LTP2=0;D$LTP3=0;D$LTP4=0;D$No_RTOR=0}
  if(Type=='R2U'  ){             D$Lane_Width=0;D$Shuold_Wid=0;D$RHR=0;D$DrwDens=0;D$HorCur=0;D$Grade=0}
  if(Type=='R4U'  ){D$LIGHTING=0;D$Lane_Width=0;D$Shuold_Wid=0;D$Grade=0}
  if(Type=='R4D'  ){D$LIGHTING=0;D$Lane_Width=0;D$Shuold_Wid=0;D$Median_Wid=0}
  if(Type=='U2U'  ){D$LIGHTING=0;D$OSPProp=0;D$FODensity=0;D$DrwDens=0}
  if(Type=='U3T'  ){D$LIGHTING=0;D$OSPProp=0;D$FODensity=0;D$DrwDens=0}
  if(Type=='U4U'  ){D$LIGHTING=0;D$OSPProp=0;D$FODensity=0;D$DrwDens=0}
  if(Type=='U4D'  ){D$LIGHTING=0;D$OSPProp=0;D$FODensity=0;D$Median_Wid=0;D$DrwDens=0}
  if(Type=='U5T'  ){D$LIGHTING=0;D$OSPProp=0;D$FODensity=0;D$DrwDens=0}
  return(D)
  
}
ListPredictors  = function(Type = NULL){
  AllInt= c('AADT_Major','AADT_Minor')
  R3ST  = c(AllInt)
  R4ST  = c(AllInt,c('LIGHTING'))
  R4SG  = c(AllInt)
  RM3ST = c(AllInt,c('LIGHTING'))
  RM4ST = c(AllInt)
  RM4SG = c(AllInt)
  U3ST = c(AllInt)
  U4ST = c(AllInt)
  U3SG = c(AllInt,c('LIGHTING'))
  U4SG = c(AllInt)
  
  AllSeg = c('AADT','Length') 
  R2U = c(AllSeg)
  R4U = c(AllSeg)
  R4D = c(AllSeg,c('Shuold_Wid','Median_Wid'))
  U2U = c(AllSeg)
  U3T = c(AllSeg)
  U4U = c(AllSeg,c('LIGHTING','DrwDens'))
  U4D = c(AllSeg,c('Median_Wid'))
  U5T = c(AllSeg,c('LIGHTING'          ,'DrwDens'))
  
  
  Out = list('R3ST'=R3ST,'R4ST'=R4ST,'R4SG'=R4SG,'RM3ST'=RM3ST,'RM4ST'=RM4ST,'RM4SG'=RM4SG,
             'U3ST'=U3ST,'U4ST'=U4ST,'U3SG'=U3SG,'U4SG'=U4SG,'AllInt'=AllInt,
             'R2U'=R2U,'R4D'=R4D,'R4U'=R4U,'U2U'=U2U,'U3T'=U3T,'U4D'=U4D,'U4U'=U4U,'U5T'=U5T,'AllSeg'=AllSeg)
  if(is.null(Type)){return(Out)}
  else(if(Type %in%  names(Out)){return(Out[Type][[1]])})
}
ListPredictors_All = function(Type = NULL){
  AllInt= c('AADT_Major','AADT_Minor')
  R3ST  = c(AllInt,c('LIGHTING','LTL','RTL','SKEW1'))
  R4ST  = c(AllInt,c('LIGHTING','LTL','RTL','SKEW1','SKEW2'))
  R4SG  = c(AllInt,c('LIGHTING','LTL','RTL'))
  RM3ST = c(AllInt,c('LIGHTING','LTL','RTL','SKEW1'))
  RM4ST = c(AllInt,c('LIGHTING','LTL','RTL','SKEW1','SKEW2'))
  RM4SG = c(AllInt)
  U3ST = c(AllInt,c('LIGHTING','LTL','RTL'))
  U4ST = c(AllInt,c('LIGHTING','LTL','RTL'))
  U3SG = c(AllInt,c('LIGHTING','LTL','RTL','LTP1','LTP2','No_RTOR'))
  U4SG = c(AllInt,c('LIGHTING','LTL','RTL','LTP1','LTP2','LTP3','LTP4','No_RTOR'))
  
  AllSeg = c('AADT','Length') 
  R2U = c(AllSeg,c(           'Lane_Width','Shuold_Wid','RHR','DrwDens','HorCur','Grade'))
  R4U = c(AllSeg,c('LIGHTING','Lane_Width','Shuold_Wid','Grade'))
  R4D = c(AllSeg,c('LIGHTING','Lane_Width','Shuold_Wid','Median_Wid'))
  U2U = c(AllSeg,c('LIGHTING','OSPProp','FODensity','DrwDens'))
  U3T = c(AllSeg,c('LIGHTING'          ,'FODensity','DrwDens'))
  U4U = c(AllSeg,c('LIGHTING','OSPProp','FODensity','DrwDens'))
  U4D = c(AllSeg,c('LIGHTING'          ,'FODensity','Median_Wid','DrwDens'))
  U5T = c(AllSeg,c('LIGHTING'          ,'FODensity','DrwDens'))
  R4F = c(AllSeg,c(           'Lane_Width'))
  U4F = c(AllSeg,c(           'Lane_Width'))
  U6F = c(AllSeg,c(           'Lane_Width'))
  
  
  Out = list('R3ST'=R3ST,'R4ST'=R4ST,'R4SG'=R4SG,'RM3ST'=RM3ST,'RM4ST'=RM4ST,'RM4SG'=RM4SG,
             'U3ST'=U3ST,'U4ST'=U4ST,'U3SG'=U3SG,'U4SG'=U4SG,'AllInt'=AllInt,
             'R2U'=R2U,'R4D'=R4D,'R4U'=R4U,'U2U'=U2U,'U3T'=U3T,'U4D'=U4D,'U4U'=U4U,'U5T'=U5T,'R4F'=R4F,'U4F'=U4F,'U6F'=U6F,'AllSeg'=AllSeg)
  if(is.null(Type)){return(Out)}
  else(if(Type %in%  names(Out)){return(Out[Type][[1]])})
}

# Regression Models -------------------------------------------------------------------------------------------
GLMModels     = function(D, MType = 'Base',FType = NULL){
  IntModels = function(D, MType, FType){
    HSMBaseModel      = function(D,Theta, FType){
      M = glm(TOT_OC ~ AADT_Major + AADT_Minor, data = D, family= negative.binomial(theta = Theta,link= "log"))
      return(list('Model' = M, 'Data' = D))
    }
    HSMCovariateModel = function(D,Theta, FType){
      Pred = ListPredictors(FType)
      Pred = Pred[Pred %in% names(D)]
      Rem = NULL;for(p in Pred){if(length(unique(D[,p]))>1){Rem = c(Rem,p)}};Pred = Rem
      M = glm(as.formula(paste('TOT_OC ~ ',paste(Pred,collapse=' + '))), data = D,family=negative.binomial(theta = Theta,link= "log"))
      return(list('Model' = M, 'Data' = D))
    }
    AllIntModel       = function(D,Theta, FType){
      Pred = ListPredictors(FType)
      Pred = c(Pred,'PopDens')
      Pred = Pred[Pred %in% names(D)]
      Rem = NULL;for(p in Pred){if(length(unique(D[,p]))>1){Rem = c(Rem,p)}};Pred = Rem
      M = glm(as.formula(paste('TOT_OC ~ ',paste(Pred,collapse=' + '))) , data = D,family=negative.binomial(theta = Theta,link= "log"))
      return(list('Model' = M, 'Data' = D))
    }
    FullIntModel      = function(D,Theta, FType){
      Pred = ListPredictors(FType)
      Pred = c(Pred,'PopDens','TotalLanes','SG','LEGS')
      Pred = Pred[Pred %in% names(D)]
      Rem = NULL;for(p in Pred){if(length(unique(D[,p]))>1){Rem = c(Rem,p)}};Pred = Rem
      M = glm(as.formula(paste('TOT_OC ~ ',paste(Pred,collapse=' * '))) , data = D,family=negative.binomial(theta = Theta,link= "log"))
      return(list('Model' = M, 'Data' = D))
    }
    m = mean(D$TOT_OC)
    v = var(D$TOT_OC)
    Theta = m^2 / (v-m)
    if(Theta<0){Theta = 100000}
    if (MType == 'Base' ){return (HSMBaseModel     (D,Theta, FType))}
    if (MType == 'Covar'){return (HSMCovariateModel(D,Theta, FType))}
    if (MType == 'Full' ){return (FullIntModel     (D,Theta, FType))}
    if (MType == 'All'  ){return (AllIntModel      (D,Theta, FType))}
  }
  SegModels = function(D, MType, FType){
    HSMBaseModel      = function(D,Theta, FType){
      #M = glm(TOT_OC/exp(Length)/exp(AADT)/10^(-6)/365~1 , data = D, family= negative.binomial(theta = Theta,link= "log"))
      M = glm(TOT_OC ~ AADT + offset(Length), data = D, family= negative.binomial(theta = Theta,link= "log"))
      return(list('Model' = M, 'Data' = D))
    }
    HSMCovariateModel = function(D,Theta, FType){
      #M = glm(as.formula(paste('TOT_OC/exp(Length)/exp(AADT)/10^(-6)/365 ~ ',paste(Pred,collapse=' + '))) , data = D, family= negative.binomial(theta = Theta,link= "log"))
      Pred = NULL
      for(p in ListPredictors(FType)){
        if(!(p %in% c('Length'))){Pred = c(Pred,p)}
      }
      Pred = Pred[Pred %in% names(D)]
      Rem = NULL;for(p in Pred){if(length(unique(D[,p]))>1){Rem = c(Rem,p)}};Pred = Rem
      M = glm(as.formula(paste('TOT_OC ~ offset(Length) + ',paste(Pred,collapse=' + '))) , data = D, family= negative.binomial(theta = Theta,link= "log"))
      return(list('Model' = M, 'Data' = D))
    }
    AllSegModel       = function(D,Theta, FType){
      Pred = NULL
      for(p in ListPredictors(FType)){
        if(!(p %in% c('Length'))){Pred = c(Pred,p)}
      }
      Pred = c(Pred,'PopDens')
      Pred = Pred[Pred %in% names(D)]
      Rem = NULL;for(p in Pred){if(length(unique(D[,p]))>1){Rem = c(Rem,p)}};Pred = Rem
      M = glm(as.formula(paste('TOT_OC ~ offset(Length) + ',paste(Pred,collapse=' + '))) , data = D,family=negative.binomial(theta = Theta,link= "log"))
      return(list('Model' = M, 'Data' = D))
    }
    FullSegModel      = function(D,Theta, FType){
      Pred = ListPredictors(FType)
      Pred = c(Pred,'PopDens','TotalLanes','MedianID','Route_Type')
      Pred = Pred[Pred %in% names(D)]
      Rem = NULL;for(p in Pred){if(length(unique(D[,p]))>1){Rem = c(Rem,p)}};Pred = Rem
      M = glm(as.formula(paste('TOT_OC ~ offset(Length) * ',paste(Pred,collapse=' * '))) , data = D,family=negative.binomial(theta = Theta,link= "log"))
      return(list('Model' = M, 'Data' = D))
    }
    m = mean(D$TOT_OC)
    v = var(D$TOT_OC)
    Theta = m^2 / (v-m)
    if(Theta<0){Theta = 100000}
    if (MType == 'Base' ){return (HSMBaseModel     (D,Theta, FType))}
    if (MType == 'Covar'){return (HSMCovariateModel(D,Theta, FType))}
    if (MType == 'Full' ){return (FullSegModel     (D,Theta, FType))}
    if (MType == 'All'  ){return (AllSegModel      (D,Theta, FType))}
  }
  if(is.null(FType)){FType = toString(unique(D$FType)[1])}
  AllInt = c('R3ST'  ,'R4ST'  ,'R4SG'  ,'RM3ST'  ,'RM4ST'  ,'RM4SG'  ,'U3ST'  ,'U4ST'  ,'U3SG'  ,'U4SG','AllInt','AllIntStp','AllIntSig'  )
  AllSeg = c('R2U','R4D','R4U','U2U','U3T','U4U','U4D','U5T','R4F','U4F','U6F','AllSeg')
  MDOut = NULL
  if(FType %in% AllInt){MDOut = IntModels(D,MType,FType)}
  if(FType %in% AllSeg){MDOut = SegModels(D,MType,FType)}
  return(MDOut)
}
GLMPredict    = function(M,D,Type = NULL){
  Out = NULL
  if(is.null(Type)){Type = toString(unique(M$data$FType)[1])}
  P = ListHeaders(Type)
  V = ListBaseValues(Type)
  for(p in P){if(!(p %in% names(D)) & p %in% names(V)){D[p] = V[p]}}
  
  if(Type %in% c('R3ST','R4ST','R4SG','RM3ST','RM4ST','RM4SG','U3ST','U4ST','U3SG','U4SG','AllInt')){
    Out = exp(predict.glm(M,D)) 
  }
  if(Type %in% c('R2U','R4D','R4U','U2U','U3T','U4U','U4D','U5T','R4F','U4F','U6F','AllSeg')){
    Out = exp(predict.glm(M,D))
  }
  return(Out)
}
HSMPredict    = function(M,D,CF){
  Out = NULL
  AllInt = c('R3ST','R4ST','R4SG','RM3ST','RM4ST','RM4SG','U3ST','U4ST','U3SG','U4SG')
  AllSeg = c('R2U','R4D','R4U','U2U','U3T','U4U','U4D','U5T')
  HSMC = NULL;for(t in c(AllInt,AllSeg)){HSMC=rbind(HSMC,HSMValues(t,CF))}
  for(i in 1:length(D[,1])){
    d = D[i,]
    Type = toString(d$FType)
    if(Type %in% c('R3ST','R4ST','R4SG','RM3ST','RM4ST','RM4SG','U3ST','U4ST','U3SG','U4SG')){
      Out = c(Out,HSMC[Type,4][[1]]*exp(HSMC[Type,1][[1]] + HSMC[Type,2][[1]]*d$AADT_Major + HSMC[Type,3][[1]]*d$AADT_Minor))
    }
    if(Type == 'R2U'){
      Out = c(Out,HSMC[Type,4][[1]]*(exp(d$AADT)*exp(d$Length)*365*10^(-6)*exp(HSMC[Type,1])[[1]]))  
    }
    if(Type %in% c('R4D','R4U')){
      Out = c(Out,HSMC[Type,4][[1]]*exp(d$Length)*exp(HSMC[Type,1][[1]]+HSMC[Type,2][[1]]*d$AADT))
    }
    if(Type %in% c('U2U','U3T','U4U','U4D','U5T')){
      Out = c(Out,HSMC[Type,4][[1]]*exp(d$Length)*exp(HSMC[Type,1][[1]]+HSMC[Type,2][[1]]*d$AADT)
              +HSMC[Type,4][[1]]*exp(d$Length)*exp(HSMC[Type,9][[1]]+HSMC[Type,10][[1]]*d$AADT))
    }
  }
  return(Out)
}

# Plots: -------------------------------------------------------------------------------------------
PlotXX        = function(Scope,D2,D1,VolRange,HSMDefaults,Title,LogScale = FALSE){
  PlotXXI  = function(D2,D1,VolRange,HSMDefaults,Title){
    X1  = exp(D1$AADT_Major)
    X2  = exp(D1$AADT_Minor)
    Xo1 = exp(D1[!(rownames(D1) %in% rownames(D2)),]$AADT_Major)
    Xo2 = exp(D1[!(rownames(D1) %in% rownames(D2)),]$AADT_Minor)
    Xi1 = exp(D2$AADT_Major)
    Xi2 = exp(D2$AADT_Minor)
    HSMX1 = HSMDefaults[Title,]$AADT_Major_UB
    HSMX2 = HSMDefaults[Title,]$AADT_Minor_UB
    x1r = range(0,X1,HSMX1,VolRange[1])
    x2r = range(0,X2,HSMX2,VolRange[2])
    x2r[2] = x2r[2] * 1.4
    layout(1)
    plot.new()
    layout(matrix(c(2,1,4,3),2,2),width=c(6,2),height=c(3,6))
    
    par(mar=c(6.1,6.1,0,0))
    plot(x=X1,y=X2,xlab='AADT Major',ylab='AADT Minor',col=color.scale(D1$TOT_OC,c(0,1,1),0,1),xlim=x1r,ylim=x2r)
    par(new=TRUE)
    plot(x=Xo1,y=Xo2,xlab='',ylab='',col='red',xlim=x1r,ylim=x2r,pch=4)
    grid()
    segments(c(VolRange[1],x1r[1]),c(x2r[1],VolRange[2]),c(VolRange[1],VolRange[1]),c(VolRange[2],VolRange[2]),col='brown',lty=2)
    l1 = paste('Sample Size: ',length(X1), ', Reduce to: ',length(Xi1),sep='')
    l2 = paste('AADT Major Max, HSM: ',round((HSMX1)),', State: ',round((max(Xi1))))
    l3 = paste('AADT Minor Max, HSM: ',round((HSMX2)),', State: ',round((max(Xi2))))
    legend(legend=c(l1,l2,l3),'topright')
    legend(legend='Domain of Applicability',lty=2,col='brown','topleft')
    
    par(mar=c(0,6.1,6.1,0))
    boxplot(X1,xaxt='n',horizontal=TRUE,ylim=x1r,xlim=c(0,3),col='lightblue',border='blue')
    grid(nx=NULL,ny=0)
    segments(0,2,HSMX1,2,col='red')
    par(new=TRUE);plot(y=c(2,2),x=c(0,HSMX1),ylim=c(0,3),xlim=x1r,pch=10,xaxt='n',yaxt='n',ylab='',col='red',xlab='')
    axis(3)
    axis(2,at=c(1,2),label=c('State Data','HSM Range'),las=2)
    par(new=TRUE);plot(y=rep(x1r[2]/3,length(Xo1[Xo1>VolRange[1]])),x=3/(x1r[2]-x1r[1])*(Xo1[Xo1>VolRange[1]]-x1r[1]),xlab='',ylab='',col='red',xlim=c(0,3),ylim=x1r,pch=4,xaxt='n',yaxt='n')
    
    par(mar=c(6.1,0,0,4.1))
    boxplot(X2,yaxt='n',ylim=x2r,xlim=c(0,3),col='lightblue',border='blue')
    grid(ny=NULL,nx=0)
    segments(2,0,2,HSMX2,col='red')
    par(new=TRUE);plot(x=c(2,2),y=c(0,HSMX2),xlim=c(0,3),ylim=x2r,pch=10,xaxt='n',yaxt='n',ylab='',col='red',xlab='')
    axis(4)
    axis(1,at=c(1,2),label=c('State Data','HSM Range'),las=2)
    par(new=TRUE);plot(x=rep(1,length(Xo2[Xo2>VolRange[2]])),y=Xo2[Xo2>VolRange[2]],xlab='',ylab='',col='red',xlim=c(0,3),ylim=x2r,pch=4,xaxt='n',yaxt='n')
    
    title(paste(Title,'Predictor vs Predictor Plot Compared with HSM Ranges'),outer=TRUE,line=-2)
    layout(1)
    par(mar=c(5,4,4,2)+0.1)
  }
  PlotXXIL = function(D2,D1,VolRange,HSMDefaults,Title){
    X1  = (D1$AADT_Major)
    X2  = (D1$AADT_Minor)
    Xo1 = (D1[!(rownames(D1) %in% rownames(D2)),]$AADT_Major)
    Xo2 = (D1[!(rownames(D1) %in% rownames(D2)),]$AADT_Minor)
    Xi1 = (D2$AADT_Major)
    Xi2 = (D2$AADT_Minor)
    HSMX1 = log(HSMDefaults[Title,]$AADT_Major_UB)
    HSMX2 = log(HSMDefaults[Title,]$AADT_Minor_UB)
    x1r = range(X1,HSMX1,0.5,log(VolRange[1]))
    x2r = range(X2,HSMX2,0.5,log(VolRange[2]))
    x2r[2] = x2r[2] * 1.4
    layout(1)
    plot.new()
    layout(matrix(c(2,1,4,3),2,2),width=c(6,2),height=c(3,6))
    
    par(mar=c(6.1,6.1,0,0))
    plot(x=X1,y=X2,xlab='AADT Major (Log Scale)',ylab='AADT Minor (Log Scale)',col=color.scale(D1$TOT_OC,c(0,1,1),0,1),xlim=x1r,ylim=x2r)
    par(new=TRUE)
    plot(x=Xo1,y=Xo2,xlab='',ylab='',col='red',xlim=x1r,ylim=x2r,pch=4)
    grid()
    l1 = paste('Sample Size: ',length(X1), ', Reduce to: ',length(Xi1),sep='')
    l2 = paste('AADT Major Max, HSM: ',round(exp(HSMX1)),', State: ',round(exp(max(X1))))
    l3 = paste('AADT Minor Max, HSM: ',round(exp(HSMX2)),', State: ',round(exp(max(X2))))
    legend(legend=c(l1,l2,l3),'topleft')
    legend(legend='Domain of Applicability',lty=2,col='brown','topleft')
    
    par(mar=c(0,6.1,6.1,0))
    boxplot(X1,xaxt='n',horizontal=TRUE,ylim=x1r,xlim=c(0,3),col='lightblue',border='blue')
    grid(nx=NULL,ny=0)
    segments(1,2,HSMX1,2,col='red')
    par(new=TRUE);plot(y=c(2,2),x=c(1,HSMX1),ylim=c(0,3),xlim=x1r,pch=10,xaxt='n',yaxt='n',ylab='',col='red',xlab='')
    axis(3)
    axis(2,at=c(1,2),label=c('State Data','HSM Range'),las=2)
    par(new=TRUE);plot(y=rep(x1r[2]/3,length(Xo1[Xo1>log(VolRange[1])])),x=3/(x1r[2]-x1r[1])*(Xo1[Xo1>log(VolRange[1])]-x1r[1]),xlab='',ylab='',col='red',xlim=c(0,3),ylim=x1r,pch=4,xaxt='n',yaxt='n')
    
    par(mar=c(6.1,0,0,4.1))
    boxplot(X2,yaxt='n',ylim=x2r,xlim=c(0,3),col='lightblue',border='blue')
    grid(ny=NULL,nx=0)
    segments(2,1,2,HSMX2,col='red')
    par(new=TRUE);plot(x=c(2,2),y=c(1,HSMX2),xlim=c(0,3),ylim=x2r,pch=10,xaxt='n',yaxt='n',ylab='',col='red',xlab='')
    axis(4)
    axis(1,at=c(1,2),label=c('State Data','HSM Range'),las=2)
    par(new=TRUE);plot(x=rep(1,length(Xo2[Xo2>log(VolRange[2])])),y=Xo2[Xo2>log(VolRange[2])],xlab='',ylab='',col='red',xlim=c(0,3),ylim=x2r,pch=4,xaxt='n',yaxt='n')
    
    title(paste(Title,'Logarithmic Predictor vs Predictor Plot Compared with HSM Ranges'),outer=TRUE,line=-2)
    layout(1)
    par(mar=c(5,4,4,2)+.1)
  }
  PlotXXS  = function(D2,D1,VolRange,HSMDefaults,Title){
    X1  = exp(D1$AADT)
    X2  = exp(D1$Length)
    Xo1 = exp(D1[!(rownames(D1) %in% rownames(D2)),]$AADT)
    Xo2 = exp(D1[!(rownames(D1) %in% rownames(D2)),]$Length)
    Xi1 = exp(D2$AADT)
    Xi2 = exp(D2$Length)
    HSMX1 = HSMDefaults[Title,]$AADT_Major_UB
    HSMX2 = HSMDefaults[Title,]$Length_LB
    x1r = range(0,X1,HSMX1,(VolRange[1]))
    x2r = range(0,X2,HSMX2,(VolRange[2]))
    x2r[2] = x2r[2] * 1.4
    layout(1)
    plot.new()
    layout(matrix(c(2,1,4,3),2,2),width=c(6,2),height=c(3,6))
    
    par(mar=c(6.1,6.1,0,0))
    plot(x=X1,y=X2,xlab='AADT',ylab='Length',col=color.scale(D1$TOT_OC,c(0,1,1),0,1),xlim=x1r,ylim=x2r)
    par(new=TRUE)
    plot(x=Xo1,y=Xo2,xlab='',ylab='',col='red',xlim=x1r,ylim=x2r,pch=4)
    grid()
    segments(c(VolRange[1],x1r[1]),c(x2r[2],VolRange[2]),c(VolRange[1],VolRange[1]),c(VolRange[2],VolRange[2]),col='brown',lty=2)
    l1 = paste('Sample Size: ',length(X1), ', Reduce to: ',length(Xi1),sep='')
    l2 = paste('AADT Max, HSM: ',round((HSMX1)),', State: ',round((max(Xi1))))
    l3 = paste('Length Min, HSM: ',round((HSMX2)*100)/100,', State: ',round((min(Xi2))*100)/100)
    legend(legend=c(l1,l2,l3),'topright')
    legend(legend='Domain of Applicability',lty=2,col='brown','topleft')
    
    par(mar=c(0,6.1,6.1,0))
    boxplot(X1,xaxt='n',horizontal=TRUE,ylim=x1r,xlim=c(0,3),col='lightblue',border='blue')
    grid(nx=NULL,ny=0)
    segments(0,2,HSMX1,2,col='red')
    par(new=TRUE);plot(y=c(2,2),x=c(0,HSMX1),ylim=c(0,3),xlim=x1r,pch=10,xaxt='n',yaxt='n',ylab='',col='red',xlab='')
    axis(3)
    axis(2,at=c(1,2),label=c('State Data','HSM Range'),las=2)
    par(new=TRUE);plot(y=rep(x1r[2]/3,length(Xo1[Xo1>VolRange[1]])),x=3/(x1r[2]-x1r[1])*(Xo1[Xo1>VolRange[1]]-x1r[1]),xlab='',ylab='',col='red',xlim=c(0,3),ylim=x1r,pch=4,xaxt='n',yaxt='n')
    
    par(mar=c(6.1,0,0,4.1))
    boxplot(X2,yaxt='n',ylim=x2r,xlim=c(0,3),col='lightblue',border='blue')
    grid(ny=NULL,nx=0)
    segments(2,HSMX2,2,x2r[2],col='red')
    par(new=TRUE);plot(x=c(2,2),y=c(HSMX2,x2r[2]),xlim=c(0,3),ylim=x2r,pch=c(10,24),xaxt='n',yaxt='n',ylab='',col='red',xlab='')
    axis(4)
    axis(1,at=c(1,2),label=c('State Data','HSM Range'),las=2)
    par(new=TRUE);plot(x=rep(1,length(Xo2[Xo2<VolRange[2]])),y=Xo2[Xo2<VolRange[2]],xlab='',ylab='',col='red',xlim=c(0,3),ylim=x2r,pch=4,xaxt='n',yaxt='n')
    
    title(paste(Title,'Predictor vs Predictor Plot Compared with HSM Ranges'),outer=TRUE,line=-2)
    layout(1)
    par(mar=c(5,4,4,2)+0.1)
  }
  PlotXXSL = function(D2,D1,VolRange,HSMDefaults,Title){
    X1  = (D1$AADT)
    X2  = (D1$Length)
    Xo1 = (D1[!(rownames(D1) %in% rownames(D2)),]$AADT)
    Xo2 = (D1[!(rownames(D1) %in% rownames(D2)),]$Length)
    Xi1 = (D2$AADT)
    Xi2 = (D2$Length)
    HSMX1 = log(HSMDefaults[Title,]$AADT_Major_UB)
    HSMX2 = log(HSMDefaults[Title,]$Length_LB)
    x1r = range(X1,HSMX1,0.5,log(VolRange[1]))
    x2r = range(X2,HSMX2,0.5)
    x2r[2] = x2r[2] * 1.4
    layout(1)
    plot.new()
    layout(matrix(c(2,1,4,3),2,2),width=c(6,2),height=c(3,6))
    
    par(mar=c(6.1,6.1,0,0))
    plot(x=X1,y=X2,xlab='AADT Major (Log Scale)',ylab='AADT Minor (Log Scale)',col=color.scale(D1$TOT_OC,c(0,1,1),0,1),xlim=x1r,ylim=x2r)
    par(new=TRUE)
    plot(x=Xo1,y=Xo2,xlab='',ylab='',col='red',xlim=x1r,ylim=x2r,pch=4)
    grid()
    l1 = paste('Sample Size: ',length(X1), ', Reduce to: ',length(Xi1),sep='')
    l2 = paste('AADT Max, HSM: ',round(exp(HSMX1)),', State: ',round(exp(max(X1))))
    l3 = paste('Length in, HSM: ',round(exp(HSMX2)),', State: ',round(exp(min(X2))))
    legend(legend=c(l1,l2,l3),'topleft')
    legend(legend='Domain of Applicability',lty=2,col='brown','topleft')
    
    par(mar=c(0,6.1,6.1,0))
    boxplot(X1,xaxt='n',horizontal=TRUE,ylim=x1r,xlim=c(0,3),col='lightblue',border='blue')
    grid(nx=NULL,ny=0)
    segments(1,2,HSMX1,2,col='red')
    par(new=TRUE);plot(y=c(2,2),x=c(1,HSMX1),ylim=c(0,3),xlim=x1r,pch=10,xaxt='n',yaxt='n',ylab='',col='red',xlab='')
    axis(3)
    axis(2,at=c(1,2),label=c('State Data','HSM Range'),las=2)
    par(new=TRUE);plot(y=rep(x1r[2]/3,length(Xo1[Xo1>log(VolRange[1])])),x=3/(x1r[2]-x1r[1])*(Xo1[Xo1>log(VolRange[1])]-x1r[1]),xlab='',ylab='',col='red',xlim=c(0,3),ylim=x1r,pch=4,xaxt='n',yaxt='n')
    
    par(mar=c(6.1,0,0,4.1))
    boxplot(X2,yaxt='n',ylim=x2r,xlim=c(0,3),col='lightblue',border='blue')
    grid(ny=NULL,nx=0)
    segments(2,1,2,HSMX2,col='red')
    par(new=TRUE);plot(x=c(2,2),y=c(1,HSMX2),xlim=c(0,3),ylim=x2r,pch=10,xaxt='n',yaxt='n',ylab='',col='red',xlab='')
    axis(4)
    axis(1,at=c(1,2),label=c('State Data','HSM Range'),las=2)
    par(new=TRUE);plot(x=rep(1,length(Xo2[Xo2>log(VolRange[2])])),y=Xo2[Xo2>log(VolRange[2])],xlab='',ylab='',col='red',xlim=c(0,3),ylim=x2r,pch=4,xaxt='n',yaxt='n')
    
    title(paste(Title,'Logarithmic Predictor vs Predictor Plot Compared with HSM Ranges'),outer=TRUE,line=-2)
    layout(1)
    par(mar=c(5,4,4,2)+.1)
  }
  FType = toString(unique(D2$FType)[1])
  AllInt = c('R3ST'  ,'R4ST'  ,'R4SG'  ,'RM3ST'  ,'RM4ST'  ,'RM4SG'  ,'U3ST'  ,'U4ST'  ,'U3SG'  ,'U4SG'  )
  AllSeg = c('R2U','R4D','R4U','U2U','U3T','U4U','U4D','U5T')
  if( LogScale){SaveImage(Scope,paste(Title,'1 XXL'),'png')}
  if(!LogScale){SaveImage(Scope,paste(Title,'1 XX') ,'png')}
  
  if( LogScale & FType %in% AllInt){PlotXXIL(D2,D1,VolRange,HSMDefaults,Title)}
  if(!LogScale & FType %in% AllInt){PlotXXI (D2,D1,VolRange,HSMDefaults,Title)}
  if( LogScale & FType %in% AllSeg){PlotXXSL(D2,D1,VolRange,HSMDefaults,Title)}
  if(!LogScale & FType %in% AllSeg){PlotXXS (D2,D1,VolRange,HSMDefaults,Title)}
  dev.off()
  if( LogScale & FType %in% AllInt){PlotXXIL(D2,D1,VolRange,HSMDefaults,Title)}
  if(!LogScale & FType %in% AllInt){PlotXXI (D2,D1,VolRange,HSMDefaults,Title)}
  if( LogScale & FType %in% AllSeg){PlotXXSL(D2,D1,VolRange,HSMDefaults,Title)}
  if(!LogScale & FType %in% AllSeg){PlotXXS (D2,D1,VolRange,HSMDefaults,Title)}
}
Outliers      = function(Scope,MD, Plot = TRUE, Title = '', Max_Out = c(0,0,0)){
  OutliersInt   = function(MD, Plot, Title, Max_Out){
    FindIndex = function(Vector,Names){
      Out_Index = which(names(Vector) %in% Names)
      names(Out_Index) = Names
      return(Out_Index)
    }
    MaxOut_Cooks = 5
    MaxOut_JRes  = 5
    MaxOut_Lev   = 5
    if(length(Max_Out)>=1){MaxOut_Cooks = Max_Out[1]} 
    if(length(Max_Out)>=2){MaxOut_JRes  = Max_Out[2]} 
    if(length(Max_Out)>=3){MaxOut_Lev   = Max_Out[3]} 
    
    M = MD[[1]]
    D = MD[[2]]
    
    # Cooks Distance
    Cooks      = cooks.distance(M)
    names(Cooks) = rownames(D)
    #Threshold  = CooksRange[1] + (CooksRange[2]-CooksRange[1])*2/3
    #Out        = Cooks[Cooks>Threshold]
    #if(length(Out)>MaxOut_Cooks){
    #  Out_Order = Out[order(-Out)]
    #  Out = Out[names(Out_Order[1:MaxOut])]
    #}
    Cooks_Order = Cooks[order(-Cooks)]
    Out = Cooks_Order[1:MaxOut_Cooks]  
    if(MaxOut_Cooks<1){Out = NULL}
    Out_Index = FindIndex(Cooks,names(Out))
    Out_Cook    = strtoi(names(Out_Index))
    names(Out_Cook) = rep('Cooks Distance',length(Out_Cook))
    
    # Jackknife Residuals
    JRes = rstudent(M)
    names(JRes) = rownames(D)
    JRes_Order = JRes[order(-abs(JRes))]
    Out = JRes_Order[1:MaxOut_JRes]
    if(MaxOut_JRes<1){Out = NULL}
    Out_Index = FindIndex(JRes,names(Out))
    Out_JRes    = strtoi(names(Out_Index))
    names(Out_JRes) = rep('Jackknife Residuals',length(Out_JRes))
    
    # Leverage
    X = model.matrix(M)
    Lev = hat(X)
    names(Lev) = rownames(D)
    Lev_Order = Lev[order(-abs(Lev))]
    Out = Lev_Order[1:MaxOut_Lev]
    if(MaxOut_Lev<1){Out = NULL}
    Out_Index = FindIndex(Lev,names(Out))
    Out_Lev    = strtoi(names(Out_Index))
    names(Out_Lev) = rep('X Leverage',length(Out_Lev))
    
    Out_All = c(Out_Cook, Out_JRes, Out_Lev)
    
    # Plot
    PlotOutVec   = function(Vector,Names,Label='',Title = '',Color='red'){
      Out_Index = FindIndex(Vector,Names)
      Outliers = Vector[names(Vector) %in% Names]
      xr = c(1,length(Vector))
      yr = range(Vector)
      plot(Vector,xlab = 'Index', ylab=Label,xlim=xr,ylim=yr,main = Title,col='blue')
      #segments(1:length(Vector),0,1:length(Vector),Vector)
      par(new='TRUE')
      plot(x=Out_Index,y=Outliers,col=Color,xlim=xr,ylim=yr,xlab = 'Index', ylab=Label,pch=4)
      par(new='FALSE')
    }
    PlotOutXX    = function(D,Names,Title = '',Color){
      xr = range(D$AADT_Major)
      yr = range(D$AADT_Minor)
      d = D[rownames(D) %in% Names,]
      plot(D$AADT_Minor~D$AADT_Major,xlab = 'AADT Major (LogScale)', ylab='AADT Minor (LogScale)',xlim=xr,ylim=yr,main = Title,col='blue')
      #segments(1:length(Vector),0,1:length(Vector),Vector)
      par(new='TRUE')
      plot(d$AADT_Minor~d$AADT_Major,col=Color,xlim=xr,ylim=yr,xlab = 'AADT Major (LogScale)', ylab='AADT Minor (LogScale)',pch=4)
      par(new='FALSE')
    }
    PlotOutliers = function(D,Out_Names,Title='',Color){
      xr = range(D$AADT_Major)
      yr = range(D$TOT_OC)
      plot(D$TOT_OC ~ D$AADT_Major, xlim=xr, ylim=yr,col=Color[1], xlab='AADT Major (Log Scale)', ylab='TOT Observed Crashes',,main = Title)
      l = paste('Observations (n=',length(D[,1]),')',sep='')
      Series = names(Out_Names)[!duplicated(names(Out_Names))]
      for(i in 1:length(Series)){
        par(new='TRUE')
        d = D[paste(Out_Names[names(Out_Names)==Series[i]]),]
        plot(d$TOT_OC ~ d$AADT_Major, xlim=xr, ylim=yr, col=Color[i+1], xlab='', ylab='',pch=4,cex=1.5)
        l = c(l,paste('Outliers by ',Series[i],' (n=',length(d[,1]),')',sep=''))
      }
      legend(legend = l, 'topleft',text.col=Color,pch=c(1,4,4,4),col=Color)
      par(new='FALSE')
    }
    Color = c('blue','darkorange3','green4','red')
    SaveImage(Scope,paste(Title,'2 Outliers'),'png');
    layout(matrix(c(1,1,1,2,2,2,5,5,5,5,3,3,5,5,5,5,4,4),3,6,byrow=TRUE))
    PlotOutVec  (Cooks,Out_Cook,'Cooks Dist','',Color[2]);title('Cooks Distance'              ,line=1,font.main=1)
    PlotOutVec  (JRes ,Out_JRes,'J Res'     ,'',Color[3]);title('Jackknife Residuals'         ,line=1,font.main=1)  
    PlotOutXX   (D    ,Out_Lev ,''             ,Color[4]);title('Predictor vs Predictor Plot' ,line=1,font.main=1)
    PlotOutVec  (Lev  ,Out_Lev ,'X Lev'     ,'',Color[4]);title('X Leverages'                 ,line=1,font.main=1)  
    PlotOutliers(D    ,Out_All ,''             ,Color   );title('Predictor vs Prediction Plot'  ,line=1,font.main=1)
    title(paste(Title,'Outliers'),outer=TRUE,line=-2)
    dev.off()    
    
    if(Plot){
      layout(matrix(c(1,1,1,2,2,2,5,5,5,5,3,3,5,5,5,5,4,4),3,6,byrow=TRUE))
      PlotOutVec  (Cooks,Out_Cook,'Cooks Dist','',Color[2]);title('Cooks Distance'      ,line=1,font.main=1)
      PlotOutVec  (JRes ,Out_JRes,'J Res'     ,'',Color[3]);title('Jackknife Residuals' ,line=1,font.main=1)  
      PlotOutXX   (D    ,Out_Lev ,''             ,Color[4]);title('Predictor vs Predictor Plot'             ,line=1,font.main=1)
      PlotOutVec  (Lev  ,Out_Lev ,'X Lev'     ,'',Color[4]);title('X Leverages'         ,line=1,font.main=1)  
      PlotOutliers(D    ,Out_All ,''             ,Color   );title('Predictor vs Prediction Plot'             ,line=1,font.main=1)
      title(paste(Title,'Outliers'),outer=TRUE,line=-2)
    }
    
    D_Out = D[paste(Out_All),]
    D_Out['Outlier'] = names(c(Out_Cook, Out_JRes, Out_Lev))
    #print(D_Out)
    #layout(1)
    return(D_Out)
  }
  OutliersSeg   = function(MD, Plot, Title, Max_Out){
    FindIndex = function(Vector,Names){
      Out_Index = which(names(Vector) %in% Names)
      names(Out_Index) = Names
      return(Out_Index)
    }
    MaxOut_Cooks = 5
    MaxOut_JRes  = 5
    MaxOut_Lev   = 5
    if(length(Max_Out)>=1){MaxOut_Cooks = Max_Out[1]} 
    if(length(Max_Out)>=2){MaxOut_JRes  = Max_Out[2]} 
    if(length(Max_Out)>=3){MaxOut_Lev   = Max_Out[3]} 
    
    M = MD[[1]]
    D = MD[[2]]
    
    # Cooks Distance
    Cooks      = cooks.distance(M)
    names(Cooks) = rownames(D)
    Cooks_Order = Cooks[order(-Cooks)]
    Out = Cooks_Order[1:MaxOut_Cooks]  
    if(MaxOut_Cooks<1){Out = NULL}
    Out_Index = FindIndex(Cooks,names(Out))
    Out_Cook    = strtoi(names(Out_Index))
    names(Out_Cook) = rep('Cooks Distance',length(Out_Cook))
    
    # Jackknife Residuals
    JRes = rstudent(M)
    names(JRes) = rownames(D)
    JRes_Order = JRes[order(-abs(JRes))]
    Out = JRes_Order[1:MaxOut_JRes]
    if(MaxOut_JRes<1){Out = NULL}
    Out_Index = FindIndex(JRes,names(Out))
    Out_JRes    = strtoi(names(Out_Index))
    names(Out_JRes) = rep('Jackknife Residuals',length(Out_JRes))
    
    # Leverage
    X = model.matrix(M)
    Lev = hat(X)
    names(Lev) = rownames(D)
    Lev_Order = Lev[order(-abs(Lev))]
    Out = Lev_Order[1:MaxOut_Lev]
    if(MaxOut_Lev<1){Out = NULL}
    Out_Index = FindIndex(Lev,names(Out))
    Out_Lev    = strtoi(names(Out_Index))
    names(Out_Lev) = rep('X Leverage',length(Out_Lev))
    
    Out_All = c(Out_Cook, Out_JRes, Out_Lev)
    
    # Plot
    PlotOutVec   = function(Vector,Names,Label='',Title = '',Color='red'){
      Out_Index = FindIndex(Vector,Names)
      Outliers = Vector[names(Vector) %in% Names]
      xr = c(1,length(Vector))
      yr = range(Vector)
      if(yr[1]==yr[2]){yr[2]=1.5*yr[1]}
      plot(Vector,xlab = 'Index', ylab=Label,xlim=xr,ylim=yr,main = Title,col='blue')
      #segments(1:length(Vector),0,1:length(Vector),Vector)
      par(new='TRUE')
      plot(x=Out_Index,y=Outliers,col=Color,xlim=xr,ylim=yr,xlab = 'Index', ylab=Label,pch=4)
      par(new='FALSE')
    }
    PlotOutXX    = function(D,Names,Title = '',Color){
      xr = range(D$AADT)
      yr = range(exp(D$Length))
      d = D[rownames(D) %in% Names,]
      plot(exp(D$Length)~D$AADT,xlab = 'AADT (LogScale)', ylab='Length (LogScale)',xlim=xr,ylim=yr,main = Title,col='blue')
      #segments(1:length(Vector),0,1:length(Vector),Vector)
      par(new='TRUE')
      plot(d$Length~d$AADT,col=Color,xlim=xr,ylim=yr,xlab = 'AADT (LogScale)', ylab='Length (LogScale)',pch=4)
      par(new='FALSE')
    }
    PlotOutliers = function(D,Out_Names,Title='',Color){
      xr = range(D$AADT)
      yr = range(D$TOT_OC)
      plot(D$TOT_OC ~ D$AADT, xlim=xr, ylim=yr,col=Color[1], xlab='AADT (Log Scale)', ylab='Total Observed Crashes',,main = Title)
      l = paste('Observations (n=',length(D[,1]),')',sep='')
      Series = names(Out_Names)[!duplicated(names(Out_Names))]
      for(i in 1:length(Series)){
        par(new='TRUE')
        d = D[paste(Out_Names[names(Out_Names)==Series[i]]),]
        plot(d$TOT_OC ~ d$AADT, xlim=xr, ylim=yr, col=Color[i+1], xlab='', ylab='',pch=4,cex=1.5)
        l = c(l,paste('Outliers by ',Series[i],' (n=',length(d[,1]),')',sep=''))
      }
      legend(legend = l, 'topleft',text.col=Color,pch=c(1,4,4,4),col=Color)
      par(new='FALSE')
    }
    Color = c('blue','darkorange3','green4','red')
    SaveImage(Scope,paste(Title,'2 Outliers'),'png');
    layout(matrix(c(1,1,1,2,2,2,5,5,5,5,3,3,5,5,5,5,4,4),3,6,byrow=TRUE))
    PlotOutVec  (Cooks,Out_Cook,'Cooks Dist','',Color[2]);title('Cooks Distance'              ,line=1,font.main=1)
    PlotOutVec  (JRes ,Out_JRes,'J Res'     ,'',Color[3]);title('Jackknife Residuals'         ,line=1,font.main=1)  
    PlotOutXX   (D    ,Out_Lev ,''             ,Color[4]);title('Predictor vs Predictor Plot' ,line=1,font.main=1)
    PlotOutVec  (Lev  ,Out_Lev ,'X Lev'     ,'',Color[4]);title('X Leverages'                 ,line=1,font.main=1)  
    PlotOutliers(D    ,Out_All ,''             ,Color   );title('Predictor vs Variable Plot'  ,line=1,font.main=1)
    title(paste(Title,'Outliers'),outer=TRUE,line=-2)
    dev.off()    
    
    if(Plot==1){
      layout(matrix(c(1,1,1,2,2,2,5,5,5,5,3,3,5,5,5,5,4,4),3,6,byrow=TRUE))
      PlotOutVec  (Cooks,Out_Cook,'Cooks Dist','',Color[2]);title('Cooks Distance'      ,line=1,font.main=1)
      PlotOutVec  (JRes ,Out_JRes,'J Res'     ,'',Color[3]);title('Jackknife Residuals' ,line=1,font.main=1)  
      PlotOutXX   (D    ,Out_Lev ,''             ,Color[4]);title('Predictor vs Predictor Plot'             ,line=1,font.main=1)
      PlotOutVec  (Lev  ,Out_Lev ,'X Lev'     ,'',Color[4]);title('X Leverages'         ,line=1,font.main=1)  
      PlotOutliers(D    ,Out_All ,''             ,Color   );title('Predictor vs Prediction Plot'             ,line=1,font.main=1)
      title(paste(Title,'Outliers'),outer=TRUE,line=-2)
    }
    
    D_Out = D[paste(Out_All),]
    D_Out['Outlier'] = names(c(Out_Cook, Out_JRes, Out_Lev))
    #print(D_Out)
    #layout(1)
    return(D_Out)
  }
  FType = toString(unique(MD[[2]]$FType)[1])
  if(Title == ''){Title = FType}
  AllInt = c('R3ST'  ,'R4ST'  ,'R4SG'  ,'RM3ST'  ,'RM4ST'  ,'RM4SG'  ,'U3ST'  ,'U4ST'  ,'U3SG'  ,'U4SG'  )
  AllSeg = c('R2U','R4D','R4U','U2U','U3T','U4U','U4D','U5T')
  MDOut = NULL
  if(FType %in% AllInt){MDOut = OutliersInt(MD,Plot,Title,Max_Out)}
  if(FType %in% AllSeg){MDOut = OutliersSeg(MD,Plot,Title,Max_Out)}
  return(MDOut)
}
PlotCF        = function(Scope,D,Title,Years,ConfLevel = 0.95,CFDef){
  PlotCF1 = function(D,Title, ConfLevel,Years,CFDef){
    CF_All = CFactor(D,c(0,0),10000,ConfLevel,CFDef)
    CF_Cos = CFactor(D,c(1,0),10000,ConfLevel,CFDef)
    CF_Mid = CFactor(D,c(2,0),10000,ConfLevel,CFDef)
    CF_Up  = CFactor(D,c(3,0),10000,ConfLevel,CFDef)
    CF_Den = CFactor(D,c(0,1),10000,ConfLevel,CFDef)
    CF_Spa = CFactor(D,c(0,2),10000,ConfLevel,CFDef)
    
    cf    = c(CF_Cos$CFactor, CF_Mid$CFactor, CF_Up$CFactor, CF_Den$CFactor, CF_Spa$CFactor, CF_All$CFactor)
    cf_UB = c(CF_Cos$C.UB   ,CF_Mid$C.UB    ,CF_Up$C.UB    ,CF_Den$C.UB    ,CF_Spa$C.UB    ,CF_All$C.UB    )
    cf_LB = c(CF_Cos$C.LB   ,CF_Mid$C.LB    ,CF_Up$C.LB    ,CF_Den$C.LB    ,CF_Spa$C.LB    ,CF_All$C.LB    )
    Size  = c(CF_Cos$Size   ,CF_Mid$Size    ,CF_Up$Size    ,CF_Den$Size    ,CF_Spa$Size    ,CF_All$Size    )
    OC    = c(CF_Cos$OC     ,CF_Mid$OC      ,CF_Up$OC      ,CF_Den$OC      ,CF_Spa$OC      ,CF_All$OC      )
    CV    = c(CF_Cos$CV     ,CF_Mid$CV      ,CF_Up$CV      ,CF_Den$CV      ,CF_Spa$CV      ,CF_All$CV      )
    cr    = range(cf_UB[!is.na(cf_UB)],cf_LB[!is.na(cf_LB)])
    cr[2] = cr[2] * 1.25
    
    Color1 = NULL
    for(i in 1:6){
      Color1[i] = 'green4'
      if(CV[i]>0.15 | CV[i] == 0){Color1[i]='red'}
    }
    Color2 = NULL
    for(i in 1:6){
      Color2[i] = 'green4'
      if(Size[i]<50){Color2[i]='red'}
    }
    Color3 = NULL
    for(i in 1:6){
      Color3[i] = 'green4'
      if(OC[i]<100){Color3[i]='red'}
    }
    Color4 = NULL
    for(i in 1:6){
      Color4[i] = 'green4'
      if(CV[i]>0.15 | CV[i] == 0){Color4[i]='red'}
    }
    layout(1)
    plot.new()
    layout(matrix(c(2,3,1,4),2,2),widths=c(1,7),heights=c(7,3))
    for(i in 1:6){
      if(i != 1){par(new=TRUE)}
      if(cf[i]!=0){
        plot(x=i,y=cf[i],ylim=cr,xlim=c(1,6),xaxt='n',xlab='',ylab='',col=Color1[i],pch=19)
        segments(i,cf_LB[i],i,cf_UB[i],col=Color1[i])
        par(new=TRUE);plot(x=i,y=cf_LB[i],ylim=cr,xlim=c(1,6),pch=25,xaxt='n',yaxt='n',xlab='',ylab='',col=Color1[i])
        par(new=TRUE);plot(x=i,y=cf_UB[i],ylim=cr,xlim=c(1,6),pch=24,xaxt='n',yaxt='n',xlab='',ylab='',col=Color1[i])
      }
    }
    m=cf_UB[6];n=cf_LB[6]
    if(m>n){polygon(x=c(1,1,6,6),y=c(n,m,m,n),col=rgb(0,0.1,1,.3),border='darkblue')}
    m = min(cf_UB[!is.na(cf_UB)]);n = max(cf_LB[!is.na(cf_LB)])
    if(m>n){polygon(x=c(1,1,6,6),y=c(n,m,m,n),col=rgb(0.5,1,1,.3),border='darkblue')}
    axis(1,1:6,c('Coastal','Midstate','Upstate','Dense Counties','Sparse Counties','Entire State'),line=0)
    
    
    for(i in 1:6){
      axis(1,i,round(cf[i]*1000)/1000,line=3,col.axis=Color1[i])
      axis(1,i,Size[i]               ,line=6,col.axis=Color2[i])
      axis(1,i,OC[i]                 ,line=9,col.axis=Color3[i])
      axis(1,i,round(CV[i]*1000)/1000,line=12,col.axis=Color4[i])
    }
    axis(1,1:6,rep('',6),line=3)
    axis(1,1:6,rep('',6),line=6)
    axis(1,1:6,rep('',6),line=9)
    axis(1,1:6,rep('',6),line=12)
    mtext('Calibration Factors (CF):',1,line=3 ,at=0.15)
    mtext('Sample Size > 50:'        ,1,line=6 ,at=0.32)
    mtext('Observed Crashes > 100:'  ,1,line=9 ,at=0.18)
    mtext('CF C.V. < 0.15:'          ,1,line=12,at=0.33)
    
    grid(nx=0,ny=NULL)
    title(c(paste(Title,' Calibration Factors ', ConfLevel * 100,'% CI By Areas',sep=''),paste('Year(s):',paste(Years,collapse=', '))),outer=TRUE,line=-1.8)
    if(m<=n){legend(legend=c('Entire State'),fill=rgb(0.5,1,1,.3),'topright',border='darkblue')}
    if(m> n){legend(legend=c('Entire State','Min Upperbound to Max Lowerbound'),fill=c(rgb(0,0.1,1,.3),rgb(0.5,1,1,.3)),'topright',border='darkblue')}
    layout(1)
  }
  SaveImage(Scope,paste(Title,'3 CF'),'png')
  PlotCF1(D, Title, ConfLevel,Years,CFDef)
  dev.off()
  PlotCF1(D, Title, ConfLevel,Years,CFDef)
}
Plot2D        = function(Scope,Type,MD,LogScale = FALSE, Title = NULL,HSMCoef = NULL,Plot = TRUE){
  Plot2DInt = function(Scope,Type,MD,LogScale,Title,HSMCoef,Plot){
    P1Int = function(MD,LogScale,Title,HSMCoef){
      NumOfLines = 3
      M = MD[[1]]
      D = MD[[2]]
      Color = c('green4','blue','red')
      l = NULL
      
      x1rL = range(    D$AADT_Major )
      x1r  = range(exp(D$AADT_Major))
      x1L  = seq(x1rL[1],x1rL[2], by=(x1rL[2]-x1rL[1])/500)  
      x1   = seq(x1r [1],x1r [2], by=(x1r [2]-x1r [1])/500)  
      
      x2rL = range(    D$AADT_Minor )
      x2r  = range(exp(D$AADT_Minor))
      x2L  = seq(x2rL[1],x2rL[2],by=(x2rL[2]-x2rL[1])/NumOfLines)
      x2   = seq(x2r [1],x2r [2],by=(x2r [2]-x2r [1])/NumOfLines)
      
      yr = range(D$TOT_OC)
      for (i in 1:(length(x2)-1)){
        d = D[which(D$AADT_Minor >=x2L[i] & D$AADT_Minor <x2L[i+1]),]
        
        if( LogScale){plot(d$TOT_OC~    d$AADT_Major ,col=Color[i],xlab='AADT Major (Log Scale)',ylab='Total Observed Crashes',xlim=x1rL,ylim=yr,main=Title,pch=19)}
        if(!LogScale){plot(d$TOT_OC~exp(d$AADT_Major),col=Color[i],xlab='AADT Major'            ,ylab='Total Observed Crashes',xlim=x1r ,ylim=yr,main=Title,pch=19)}
        
        x2mL = (x2L[i]+x2L[i+1])/2
        x2m  = (x2 [i]+x2 [i+1])/2
        
        if( LogScale){lines(x1L,GLMPredict(M,data.frame('AADT_Major'=x1L    ,'AADT_Minor'=x2mL)),col=Color[i])}
        if(!LogScale){lines(x1 ,GLMPredict(M,data.frame('AADT_Major'=log(x1),'AADT_Minor'=x2mL)),col=Color[i])}
        
        if(!sum(HSMCoef[1:3])==0){
          if( LogScale){lines(x1L,HSMPredict(M,data.frame('AADT_Major'=x1L    ,'AADT_Minor'=x2mL,'FType'=toString(unique(D$FType))),HSMCoef[4][[1]]),col=Color[i],lty=2)}
          if(!LogScale){lines(x1 ,HSMPredict(M,data.frame('AADT_Major'=log(x1),'AADT_Minor'=x2mL,'FType'=toString(unique(D$FType))),HSMCoef[4][[1]]),col=Color[i],lty=2)}
        }
        l = c(l,paste('AADT Minor =~',round(x2m)))
        par(new='TRUE')
      }
      n = length(D[,1])
      legend(legend = l, 'topleft',text.col=Color)
      legend(legend=c(paste('Observed Data (n = ',n,')',sep=''),paste('State Specific SPF (k = ',round(summary(MD[[1]])$dispers*1000)/1000,')',sep=''), paste('HSM Calibrated SPF (k = ',HSMCoef[8],')',sep=''),paste('HSM Calibration Factor = ',round(HSMCoef[4]*100)/100,sep='')),'topright',col='black',lty=c(NA,1,2,NA),pch=c(19,NA,NA,NA))
    }
    P2Int = function(MD,HSMCoef){
      Table = CoefSummary(MD)
      
      yr = range(Table$X.Intercept..UB,Table$X.Intercept..LB,HSMCoef[1])
      if(length(MD[[1]]$coef)==3){
        xl = substitute(atop(A == C,HSM(A==D)),list(C=round(Table$X.Intercept..Es*1000)/1000,D=HSMCoef[1]))
        yl = expression(N[spf] == exp(A + B ~ ln(AADT[Major]) + C ~ ln(AADT[Minor])))
        yl2 = ''
      }
      else{
        xl = substitute(atop(A + beta[i] * X[0][i] == B,HSM(A == D)),list(B=round(Table$X.Intercept..Es*1000)/1000,D=HSMCoef[1]))
        yl = expression(N[spf] == exp(A + B ~ ln(AADT[Major]) + C ~ ln(AADT[Minor]) + beta[i] * X[i]))
        Pred = NULL
        for(p in rownames(data.frame(summary(MD[[1]])$coef))){
          if(!(p %in% c('(Intercept)','AADT_Major','AADT_Minor'))){Pred = c(Pred,p)}}
        yl2 = substitute(X[i] ~ ~ A,list(A=paste(':',paste(Pred,collapse=' ,'))))
      }
      plot(Table$X.Intercept..Es,xlab=xl,ylab=yl,xaxt='n',col='blue',pch=20,cex=2,ylim=yr)
      segments(1,Table$X.Intercept..LB,1,Table$X.Intercept..UB,col='blue')
      par(new=TRUE);plot(Table$X.Intercept..LB,xlab='',ylab='',xaxt='n',col='blue',pch=25,cex=2,ylim=yr)
      par(new=TRUE);plot(Table$X.Intercept..UB,xlab='',ylab='',xaxt='n',col='blue',pch=24,cex=2,ylim=yr)
      par(new=TRUE);plot(HSMCoef[1],xlab='',ylab='',xaxt='n',col='red' ,pch=1 ,cex=2,ylim=yr)
      
      par(new=FALSE)
      yr = range(Table$AADT_Major.UB,Table$AADT_Major.LB,HSMCoef[2])
      plot(Table$AADT_Major.Es,xlab=substitute(atop(B == C,HSM(B==D)),list(C=round(Table$AADT_Major.Es*1000)/1000,D=HSMCoef[2])),ylab=yl2,xaxt='n',col='blue',pch=20,cex=2,ylim=yr);title('Coeff 95% CIs',line=1,font.main=1)
      segments(1,Table$AADT_Major.LB,1,Table$AADT_Major.UB,col='blue')
      par(new=TRUE);plot(Table$AADT_Major.LB,xlab='',ylab='',xaxt='n',col='blue',pch=25,cex=2,ylim=yr)
      par(new=TRUE);plot(Table$AADT_Major.UB,xlab='',ylab='',xaxt='n',col='blue',pch=24,cex=2,ylim=yr)
      par(new=TRUE);plot(HSMCoef[2],xlab='',ylab='',xaxt='n',col='red' ,pch=1 ,cex=2,ylim=yr)
      
      par(new=FALSE)
      yr = range(Table$AADT_Minor.UB,Table$AADT_Minor.LB,HSMCoef[3])
      plot(Table$AADT_Minor.Es,xlab=substitute(atop(C == D,HSM(B==E)),list(D=round(Table$AADT_Minor.Es*1000)/1000,E=HSMCoef[3])),ylab='',xaxt='n',col='blue',pch=20,cex=2,ylim=yr)
      segments(1,Table$AADT_Minor.LB,1,Table$AADT_Minor.UB,col='blue')
      par(new=TRUE);plot(Table$AADT_Minor.LB,xlab='',ylab='',xaxt='n',col='blue',pch=25,cex=2,ylim=yr)
      par(new=TRUE);plot(Table$AADT_Minor.UB,xlab='',ylab='',xaxt='n',col='blue',pch=24,cex=2,ylim=yr)
      par(new=TRUE);plot(HSMCoef[3],xlab='',ylab='',xaxt='n',col='red' ,pch=1 ,cex=2,ylim=yr)
      
      title(paste(Title,'State Specific SPF Compared with Calibrated HSM SPF'),outer= TRUE,line=-1.5)
    }
    SaveImage(Scope,paste(Title,'5 Plot2D'),'png')
    layout(matrix(c(1,2,3,4),1,4),width=c(5,1,1,1))
    P1Int(MD,LogScale,'',HSMCoef);title('Data and SPF Plot',line=1,font.main=1)
    P2Int(MD,HSMCoef)    
    dev.off()    
    if(Plot){
      layout(matrix(c(1,2,3,4),1,),width=c(5,1,1,1))
      P1Int(MD,LogScale,'',HSMCoef);title('Data and SPF Plot',line=1,font.main=1)
      P2Int(MD,HSMCoef)    
    }
    layout(1)
    par(new='FALSE')
  }
  Plot2DR2U = function(Scope,Type,MD,LogScale,Title,HSMCoef,Plot){
    P1R2U = function(MD,LogScale,Title,HSMCoef){
      NumOfLines = 3
      M = MD[[1]]
      D = MD[[2]]
      Color = c('green4','blue','red')
      l = NULL
      
      x1r  = range(exp(D$AADT))
      x1   = seq(x1r [1],x1r [2], by=(x1r [2]-x1r [1])/500)  
      
      x2r  = range(exp(D$Length))
      x2   = seq(x2r [1],x2r [2],by=(x2r [2]-x2r [1])/NumOfLines)
      
      yr = range(D$TOT_OC)
      for (i in 1:(length(x2)-1)){
        d = D[which(exp(D$Length) >=x2[i] & exp(D$Length) <x2[i+1]),]
        
        plot(d$TOT_OC~exp(d$AADT),col=Color[i],xlab='AADT'            ,ylab='Total Observed Crashes',xlim=x1r ,ylim=yr,main=Title,pch=19)
        
        x2m  = (x2 [i]+x2 [i+1])/2
        
        lines(x1 ,GLMPredict(M,data.frame('AADT'=log(x1),'Length'=log(x2m))),col=Color[i])
        
        if(length(HSMCoef)>3){
          lines(x1 ,HSMPredict(MD,log(x1),log(x2m),HSMCoef),col=Color[i],lty=2)
        }
        l = c(l,paste('Length =~',round(x2m*100)/100))
        par(new='TRUE')
      }
      n = length(D[,1])
      legend(legend = l, 'topleft',text.col=Color)
      legend(legend=c(paste('Observed Data (n = ',n,')',sep=''),paste('State Specific SPF (k = ',round(summary(MD[[1]])$dispers*1000)/1000,'/L)',sep=''), paste('HSM Calibrated SPF (k = ',HSMCoef[8],'/L)',sep=''),paste('HSM Calibration Factor = ',round(HSMCoef[4]*100)/100,sep='')),'topright',col='black',lty=c(NA,1,2,NA),pch=c(19,NA,NA,NA))
    }
    P2R2U = function(MD,HSMCoeg){
      Table = CoefSummary(MD)
      yr = range(Table$X.Intercept..UB,Table$X.Intercept..LB,HSMCoef[1])
      if(length(MD[[1]]$coef)==1){
        xl = c(paste('A = ',round(Table$X.Intercept..Es*1000)/1000),paste('HSM(A=',HSMCoef[1],')',sep='')) 
        yl = expression(N[spf] == AADT ~ Length ~ 365 ~  10^-6 ~ exp(A))
      }
      else{
        xl = substitute(atop(A + beta[i] * X[0][i] == B,HSM(A==C)),list(B=round(Table$X.Intercept..Es*1000)/1000,C=HSMCoef[1])) 
        Pred = NULL
        for(p in rownames(data.frame(summary(MD[[1]])$coef))){
          if(!(p %in% c('(Intercept)','AADT','Length'))){Pred = c(Pred,p)}}
        #yl = substitute(N[spf] == AADT ~ Length ~ 365 ~  10^-6 ~ exp(A + beta[i] * X[i]) ~ " , " ~ X[i] ~ ~ A,list(A=paste(':',paste(Pred,collapse=' ,'))))
        yl = substitute(N[spf] == AADT ~ Length ~ 365 ~  10^-6 ~ exp(A + beta[i] * X[i]))
      }
      plot(Table$X.Intercept..Es,xlab=xl,ylab=yl,xaxt='n',col='blue',pch=20,cex=2,cex.lab=.7,ylim=yr);title('Coeff 95% CIs',line=1,font.main=1)
      segments(1,Table$X.Intercept..LB,1,Table$X.Intercept..UB,col='blue')
      par(new=TRUE);plot(Table$X.Intercept..LB,xlab='',ylab='',xaxt='n',col='blue',pch=25,cex=2,ylim=yr)
      par(new=TRUE);plot(Table$X.Intercept..UB,xlab='',ylab='',xaxt='n',col='blue',pch=24,cex=2,ylim=yr)
      par(new=TRUE);plot(HSMCoef[1],xlab='',ylab='',xaxt='n',col='red' ,pch=1 ,cex=2,ylim=yr)
      title(paste(Title,'State Specific SPF Compared with Calibrated HSM SPF'),outer= TRUE,line=-1.5)
    }
    SaveImage(paste(Title,'5 Plot2D'),'png')
    layout(matrix(c(1,2),1,2),width=c(4,1))
    P1R2U(MD,LogScale,'',HSMCoef);title('Data and SPF Plot',line=1,font.main=1)
    P2R2U(MD,HSMCoef)
    dev.off()    
    if(Plot){
      layout(matrix(c(1,2),1,2),width=c(4,1))
      P1R2U(MD,LogScale,'',HSMCoef);title('Data and SPF Plot',line=1,font.main=1)
      P2R2U(MD,HSMCoef)
    }
    layout(1)
    par(new='FALSE')
  }
  Plot2DSeg = function(Scope,Type,MD,LogScale,Title,HSMCoef,Plot){
    P1Seg = function(MD,LogScale,Title,HSMCoef){
      NumOfLines = 3
      M = MD[[1]]
      D = MD[[2]]
      Color = c('green4','blue','red')
      l = NULL
      
      x1r  = range(exp(D$AADT))
      x1   = seq(x1r [1],x1r [2], by=(x1r [2]-x1r [1])/500)  
      
      x2r  = range(exp(D$Length))
      x2   = seq(x2r [1],x2r [2],by=(x2r [2]-x2r [1])/NumOfLines)
      
      yr = range(D$TOT_OC)
      for (i in 1:(length(x2)-1)){
        d = D[which(exp(D$Length) >=x2[i] & exp(D$Length) <x2[i+1]),]
        
        plot(d$TOT_OC~exp(d$AADT),col=Color[i],xlab='AADT'            ,ylab='Total Observed Crashes',xlim=x1r ,ylim=yr,main=Title,pch=19)
        
        x2m  = (x2 [i]+x2 [i+1])/2
        
        lines(x1 ,GLMPredict(M,data.frame('AADT'=log(x1),'Length'=log(x2m))),col=Color[i])
        
        if(!sum(HSMCoef[1:3])==0){
          lines(x1 ,HSMPredict(M,data.frame('AADT'=log(x1),'Length'=log(x2m),'FType'=toString(unique(D$FType))),HSMCoef[4][[1]]),col=Color[i],lty=2)
        }
        l = c(l,paste('Length =~',round(x2m*100)/100))
        par(new='TRUE')
      }
      n = length(D[,1])
      legend(legend = l, 'topleft',text.col=Color)
      legend(legend=c(paste('Observed Data (n = ',n,')',sep=''),paste('State Specific SPF (k = 1/exp(',round(summary(MD[[1]])$dispers*1000)/1000,'+ln(L))',sep=''), paste('HSM Calibrated SPF (k = 1/exp(',HSMCoef[8],'+ln(L))',sep=''),paste('HSM Calibration Factor = ',round(HSMCoef[4]*100)/100,sep='')),'topright',col='black',lty=c(NA,1,2,NA),pch=c(19,NA,NA,NA))
    }
    P2Seg = function(MD,HSMCoef){
      FType = toString(unique(MD[[2]]$FType)[1])
      Table = CoefSummary(MD)
      if(length(MD[[1]]$coef)==2){
        yl2 = ''
        yl  = expression(N[spf] == exp(A + B ~ ln(AADT) + Length))
      }
      else{
        Pred = NULL
        for(p in rownames(data.frame(summary(MD[[1]])$coef))){
          if(!(p %in% c('(Intercept)','AADT','Length'))){Pred = c(Pred,p)}}
        yl2 = substitute(X[i] ~ ~ A,list(A=paste(':',paste(Pred,collapse=' ,'))))
        yl  = substitute(N[spf] == exp(A + B ~ ln(AADT) + Length + beta[i] * X[i]))
      }
      if(FType %in% c('R2U','U2U','U3T','U4U','U4D','U5T')){
        yr1 = range(Table$X.Intercept..UB,Table$X.Intercept..LB)
        yr2 = range(Table$AADT.UB,Table$AADT.LB)
        if(length(MD[[1]]$coef)==2){xl1 = paste('A = ',round(Table$X.Intercept..Es*1000)/1000)}
        if(length(MD[[1]]$coef) >2){xl1 = substitute(A + beta[i] * X[0][i] == B,list(B=round(Table$X.Intercept..Es*1000)/1000))}
        xl2 = paste('B = ',round(Table$AADT.Es*1000)/1000)
      }
      if(FType %in% c('R4U','R4D')){
        yr1 = range(Table$X.Intercept..UB,Table$X.Intercept..LB,HSMCoef[1])
        yr2 = range(Table$AADT.UB,Table$AADT.LB,HSMCoef[2])
        if(length(MD[[1]]$coef)==2){xl1 = substitute(atop(A == B,HSM(A == C)),list(B=round(Table$X.Intercept..Es*1000)/1000,C=HSMCoef[1]))}
        if(length(MD[[1]]$coef) >2){xl1 = substitute(atop(A + beta[i] * X[0][i] == B,HSM (A == D)),list(B=round(Table$X.Intercept..Es*1000)/1000,D=HSMCoef[1]))}
        xl2 = substitute(atop(B == C,HSM(B == D)),list(C=round(Table$AADT.Es*1000)/1000,D=HSMCoef[2]))
      }
      plot(Table$X.Intercept..Es,xlab=xl1,ylab=yl,xaxt='n',col='blue',pch=20,cex=2,ylim=yr1)
      segments(1,Table$X.Intercept..LB,1,Table$X.Intercept..UB,col='blue')
      par(new=TRUE);plot(Table$X.Intercept..LB,xlab='',ylab='',xaxt='n',col='blue',pch=25,cex=2,ylim=yr1)
      par(new=TRUE);plot(Table$X.Intercept..UB,xlab='',ylab='',xaxt='n',col='blue',pch=24,cex=2,ylim=yr1)
      if(!HSMCoef[1]==0){par(new=TRUE);plot(HSMCoef[1],xlab='',ylab='',xaxt='n',col='red' ,pch=1 ,cex=2,ylim=yr1)}

      par(new=FALSE)
      plot(Table$AADT.Es,xlab=xl2,ylab=yl2,xaxt='n',col='blue',pch=20,cex=2,ylim=yr2);title('Coeff 95% CIs',line=1,font.main=1)
      segments(1,Table$AADT.LB,1,Table$AADT.UB,col='blue')
      par(new=TRUE);plot(Table$AADT.LB,xlab='',ylab='',xaxt='n',col='blue',pch=25,cex=2,ylim=yr2)
      par(new=TRUE);plot(Table$AADT.UB,xlab='',ylab='',xaxt='n',col='blue',pch=24,cex=2,ylim=yr2)
      if(!HSMCoef[1]==0){par(new=TRUE);plot(HSMCoef[2],xlab='',ylab='',xaxt='n',col='red' ,pch=1 ,cex=2,ylim=yr2)}
      title(paste(Title,'State Specific SPF Compared with Calibrated HSM SPF'),outer= TRUE,line=-1.5)
    }
    SaveImage(Scope,paste(Title,'5 Plot2D'),'png')
    layout(matrix(c(1,2,3),1,3),width=c(4,1,1))
    P1Seg(MD,LogScale,'',HSMCoef);title('Data and SPF Plot',line=1,font.main=1)
    P2Seg(MD,HSMCoef)
    dev.off()    
    if(Plot){
      layout(matrix(c(1,2,3),1,3),width=c(4,1,1))
      P1Seg(MD,LogScale,'',HSMCoef);title('Data and SPF Plot',line=1,font.main=1)
      P2Seg(MD,HSMCoef)      
    }
    layout(1)
    par(new='FALSE')
  }
  #FType = toString(unique(MD[[2]]$FType)[1])
  AllInt = c('R3ST','R4ST','R4SG','RM3ST','RM4ST','RM4SG','U3ST','U4ST','U3SG','U4SG','AllInt')
  AllSeg = c('R2U','R4U','R4D','U2U','U3T','U4U','U4D','U5T','AllSeg')
  if(Type %in% AllInt){Plot2DInt(Scope,Type,MD,LogScale,Title,HSMCoef,Plot)}
  if(Type %in% AllSeg){Plot2DSeg(Scope,Type,MD,LogScale,Title,HSMCoef,Plot)}
}
PlotAllHist   = function(Scope,MD,CF,Title = NULL,Plot = TRUE){
  AllHist1 = function(Vector,Title,MaxOC,XLab,Color){
    Vector_Mean  = mean(Vector)
    Vector_Var   = var(Vector)
    k_Factor     = (Vector_Var-Vector_Mean) / Vector_Mean^2
    Vector_Range = c(0,MaxOC)
    p  = 1 - Vector_Mean/Vector_Var
    r  = Vector_Mean*(1-p) / p
    y  = seq(Vector_Range[1],Vector_Range[2]-1)
    yr = c(0,1)
    NB = rep(0,MaxOC+1)
    if(k_Factor>0){NB = dnbinom(y,r,p)}
    hist(Vector, freq = F, breaks = seq(Vector_Range[1],Vector_Range[2]),
         xlim = Vector_Range,ylim = yr,
         main = Title, xlab = XLab, ylab = 'Probability',border=Color[1])
    if(k_Factor>0){
      par(new = TRUE)
      plot(x = y + 0.5, y = NB,
           xlim = Vector_Range, ylim = yr, type = 'b',
           main = '', xlab = '', ylab = '',col=Color[2],bty='n')
    }
    l = c(paste('Sample Size =',length(Vector)),
          paste('Mean ='       ,round(Vector_Mean*1000)/1000),
          paste('Variance ='   ,round(Vector_Var *1000)/1000))
    if(k_Factor>0){l = c(l,paste('k Factor ='   ,round(k_Factor   *1000)/1000),'Equivalent NB Distr')}
    legend(legend = l, 'topright',bty='n',cex=0.8,lty=c(NA,NA,NA,NA,1),pch=c(NA,NA,NA,NA,1),col=c('black','black','black','black','green4'))
    par(new = FALSE)
  }
  V1 = MD[[3]]$'TOT_OC'
  V2 = MD[[2]]$'TOT_OC'
  V3 = MD[[3]]$StateFitted
  V4 = MD[[3]]$HSMFitted
  MaxOC = round(max(V1,V2,V3,V4))+1
  SaveImage(Scope,paste(Title,'6 Cr Hist'),'png')
  if(!length(V1)==length(V2)){layout(matrix(c(1,2,3,4),2,2))}
  if( length(V1)==length(V2)){layout(matrix(c(1,1,2,3),2,2))}
  AllHist1(V1,'',MaxOC,'Total Observerd Crshes',c('blue','green4'));title('Observed - All Data'  ,line=1,font.main=1,cex=0.9)
  if(!length(V1)==length(V2)){AllHist1(V2,'',MaxOC,'Total Observerd Crshes',c('blue','green4'));title('Observed - Base Data' ,line=1,font.main=1,cex=0.9)}
  #AllHist1(V3,'',MaxOC,'Total Predicted Crshes',c('blue','green4'));title(paste('Fitted - State SPF (CF = ',round(mean(V1)/mean(V3)*1000)/1000,')',sep=''),line=1,font.main=1,cex=0.9)
  AllHist1(V3,'',MaxOC,'Total Predicted Crshes',c('blue','green4'));title(paste('Fitted - State SPF'),line=1,font.main=1,cex=0.9)
  AllHist1(V4,'',MaxOC,'Total Predicted Crshes',c('blue','green4'));title(paste('Fitted - Calibrated HSM SPF (CF = ',round(CF$CFactor*1000)/1000,')',sep=''),line=1,font.main=1,cex=0.9)
  title(paste(Title,'Crash Distributions'),outer=TRUE,line=-2)
  dev.off()
  if(Plot){
    if(!length(V1)==length(V2)){layout(matrix(c(1,2,3,4),2,2))}
    if( length(V1)==length(V2)){layout(matrix(c(1,1,2,3),2,2))}
    AllHist1(V1,'',MaxOC,'Total Observerd Crshes',c('blue','green4'));title('Observed - All Data'  ,line=1,font.main=1,cex=0.9)
    if(!length(V1)==length(V2)){AllHist1(V2,'',MaxOC,'Total Observerd Crshes',c('blue','green4'));title('Observed - Base Data' ,line=1,font.main=1,cex=0.9)}
    AllHist1(V3,'',MaxOC,'Total Predicted Crshes',c('blue','green4'));title('Predicted - State SPF',line=1,font.main=1,cex=0.9)
    AllHist1(V4,'',MaxOC,'Total Predicted Crshes',c('blue','green4'));title(paste('Predicted - Calibrated HSM SPF (',round(CF$CFactor*1000)/1000,')',sep=''),line=1,font.main=1,cex=0.9)
    title(paste(Title,'Crash Distributions'),outer=TRUE,line=-2)
  }
  layout(1)
}
CUREPlot      = function(Scope,MD,CF,Title = ''){
  CUREPlot1 = function(DF1,DF2,CF,Var = ''){
    COL = c('black','green4','green4','blue','blue','black')
    LTY = c(NA,1,2,1,2,NA)
    PCH = c(NA,20,NA,1,NA,NA)
    yr = range(DF1$LIM,DF1$CUMRES,-DF1$LIM,DF2$LIM,DF2$CUMRES,-DF2$LIM)
    yr[2] = yr[2]*2
    plot(x=DF1$X,y=DF1$CUMRES,xlab=Var,ylab='Cumulative Residuals',col=COL[2],ylim=yr,pch=PCH[2])
    par(new=TRUE);plot(x=DF1$X,y=DF1$CUMRES,xlab='',ylab='',col=COL[2],ylim=yr,type='l',lty=LTY[2],xaxt='n',yaxt='n')
    par(new=TRUE);plot(y= DF1$LIM,x=DF1$X,xlab='',ylab='',ylim=yr,type='l',xaxt='n',yaxt='n',col=COL[3],lty=LTY[3])
    par(new=TRUE);plot(y=-DF1$LIM,x=DF1$X,xlab='',ylab='',ylim=yr,type='l',xaxt='n',yaxt='n',col=COL[3],lty=LTY[3])

    par(new=TRUE);plot(x=DF2$X,y=DF2$CUMRES,xlab='',ylab='',col=COL[4],ylim=yr,pch=PCH[4],xaxt='n',yaxt='n')
    par(new=TRUE);plot(x=DF2$X,y=DF2$CUMRES,xlab='',ylab='',col=COL[4],ylim=yr,type='l',lty=LTY[4],xaxt='n',yaxt='n')
    par(new=TRUE);plot(y= DF2$LIM,x=DF2$X,xlab='',ylab='',ylim=yr,type='l',xaxt='n',yaxt='n',lty=LTY[5],col=COL[5])
    par(new=TRUE);plot(y=-DF2$LIM,x=DF2$X,xlab='',ylab='',ylim=yr,type='l',xaxt='n',yaxt='n',lty=LTY[5],col=COL[5])
    grid()
    abline(h=0)
    #title(paste('CURE Plot for',Var))
  }
  CUREPlot2 = function(DF11,DF12,DF21,DF22,CF,Var1,Var2,Title){
    COL = c('green4','green4','blue','blue')
    LTY = c(1,2,1,2)
    PCH = c(20,NA,1,NA)
    l1 = c(paste('Sample Size: ',length(DF11$X)),
          paste('Calibration Factor:',round(CF$CFactor*10000)/10000))
    l2 = c('State SPF Fitted',expression(paste("±2",sigma,"* State SPF Limits")),
          'Calibrated HSM SPF Fitted',expression(paste("±2",sigma,"* Calibrated HSM SPF Limits")))
    
    layout(c(1,2),height=c(1.5,1))
    par(mar=c(4,4,3,1))
    CUREPlot1(DF11,DF12,CF,Var1)    
    legend(legend=l1,'topleft')
    legend(legend=l2,lty=LTY,pch=PCH,'topright',text.col=COL,col=COL)
    par(mar=c(4,4,1,1))
    CUREPlot1(DF21,DF22,CF,Var2)
    title(paste(Title,'CURE Plots'),outer = TRUE,line = -1.8)
    layout(1)
    par(mar=c(5,4,4,2)+.1)
  }
  DFGen = function(X,Y,Yhat){
    RES  = Y-Yhat
    DF = data.frame(X,RES)
    DF = DF[with(DF,order(X)),]
    CUMRES = NULL
    CUMRES2 = NULL
    CUMRES[1] = DF$RES[1]
    CUMRES2[1] = DF$RES[1]^2
    for(i in 2:length(DF$RES)){
      CUMRES[i] = CUMRES[i-1] + DF$RES[i]
      CUMRES2[i] = CUMRES2[i-1] + DF$RES[i]^2
    }
    DF['CUMRES'] = CUMRES
    DF['CUMRES2'] = CUMRES2
    VAR = NULL
    for(i in 1:length(DF$RES)){
      VAR[i] = CUMRES2[i] *(1-CUMRES2[i]/max(CUMRES2))
    }
    DF['LIM'] = 2 * sqrt(VAR)
    return (DF)
  }
  FType = toString(unique(MD[[2]]$FType)[1])
  AllInt = c('R3ST'  ,'R4ST'  ,'R4SG'  ,'RM3ST'  ,'RM4ST'  ,'RM4SG'  ,'U3ST'  ,'U4ST'  ,'U3SG'  ,'U4SG'  )
  if(FType %in% AllInt){
    DF11 = DFGen(exp(MD[[3]]$AADT_Major),MD[[3]]$TOT_OC,MD[[3]]$StateFitted)
    DF12 = DFGen(exp(MD[[3]]$AADT_Major),MD[[3]]$TOT_OC,MD[[3]]$HSMFitted)
    DF21 = DFGen(exp(MD[[3]]$AADT_Minor),MD[[3]]$TOT_OC,MD[[3]]$StateFitted)
    DF22 = DFGen(exp(MD[[3]]$AADT_Minor),MD[[3]]$TOT_OC,MD[[3]]$HSMFitted)
    Var1 = 'AADT Major'
    Var2 = 'AADT Minor'
  }
  if(FType == 'R2U'){
    DF11 = DFGen(exp(MD[[3]]$AADT),MD[[3]]$TOT_OC,MD[[3]]$StateFitted)
    DF12 = DFGen(exp(MD[[3]]$AADT),MD[[3]]$TOT_OC,MD[[3]]$HSMFitted)
    DF21 = DFGen(exp(MD[[3]]$Length),MD[[3]]$TOT_OC,MD[[3]]$StateFitted)
    DF22 = DFGen(exp(MD[[3]]$Length),MD[[3]]$TOT_OC,MD[[3]]$HSMFitted)
    Var1 = 'AADT'
    Var2 = 'Length'
  }  
  if(FType %in% c('R4U','R4D','U2U','U3T','U4U','U4D','U5T','R4F','U4F','U6F')){
    DF11 = DFGen(exp(MD[[3]]$AADT),MD[[3]]$TOT_OC,MD[[3]]$StateFitted)
    DF12 = DFGen(exp(MD[[3]]$AADT),MD[[3]]$TOT_OC,MD[[3]]$HSMFitted)
    DF21 = DFGen(exp(MD[[3]]$Length),MD[[3]]$TOT_OC,MD[[3]]$StateFitted)
    DF22 = DFGen(exp(MD[[3]]$Length),MD[[3]]$TOT_OC,MD[[3]]$HSMFitted)
    Var1 = 'AADT'
    Var2 = 'Length'
  }  
  
  SaveImage(Scope,paste(Title,'7 CURE'),'png')
  CUREPlot2(DF11,DF12,DF21,DF22,CF,Var1,Var2,Title)
  dev.off()
  CUREPlot2(DF11,DF12,DF21,DF22,CF,Var1,Var2,Title)
}

# Calibration Factors: -------------------------------------------------------------------------------------------
BootFun       = function(D,NBoot=10000,CFDef = 'HSM'){
  n = length(D[,1])
  b = NULL
  if(CFDef  == 'HSM'){
    for(i in 1:NBoot){
      r = sample(1:n,replace=TRUE)
      o = D$TOT_OC[r]
      p = D$TOT_PC[r]
      b[i] = sum(o)/sum(p)
    }
  }
  if(CFDef  == 'Fitted'){
    for(i in 1:NBoot){
      r = sample(1:n,replace=TRUE)
      o = D$StateFitted[r]
      p = D$TOT_PC[r]
      b[i] = sum(o)/sum(p)
    }
  }  
  if(CFDef == 'LSE'){
    for(i in 1:NBoot){
      r = sample(1:n,replace=TRUE)
      o = D$TOT_OC[r]
      p = D$TOT_PC[r]
      b[i] = sum(o*p)/sum(p*p)
    }
  }
  if(CFDef == 'Mehta'){
    b = exp(glm(TOT_OC~offset(log(TOT_PC)),data=D,family=negative.binomial(theta=(var(D$TOT_OC)-mean(D$TOT_OC))/mean(D$TOT_OC)^2))$coeff[[1]])
    bv = vcov(glm(TOT_OC~offset(log(TOT_PC)),data=D,family=negative.binomial(theta=(var(D$TOT_OC)-mean(D$TOT_OC))/mean(D$TOT_OC)^2)))[[1]]
    #for(i in 1:NBoot){
      #r = sample(1:n,replace=TRUE)
      #o = D$TOT_OC[r]
      #p = D$TOT_PC[r]
      #b[i] = mean(o/p)
    #}
  }
  if(CFDef == 'MLE'){
    for(i in 1:NBoot){
      r = sample(1:n,replace=TRUE)
      o = D$TOT_OC[r]
      p = D$TOT_PC[r]
      b[i] = MLE_CF(o,p)
    }
  }
  if(CFDef == 'Srini1'){
    for(i in 1:NBoot){
      r = sample(1:n,replace=TRUE)
      o = D$TOT_OC[r]
      p = D$TOT_PC[r]
      m = mean(o)
      v = var(o)
      if(v<=m){
        Theta = 1000
      }
      else{
        Theta = m^2 / (v-m)
      }
      if(v==0 | m==0){
        b[i] = NULL
      }
      else{
        M = glm(o ~ log(p), family= negative.binomial(theta = Theta,link= "log"))
        b[i] = exp(M$coef[[1]])
        
      }
    }
  }
  return(b)
}
CFactor       = function(D,Areas=c(0,0),NBoot = 10000,ConfLevel = 0.95,CFDef = 'HSM'){
  if (Areas[1] != 0){D = D[which(D$Area1 == Areas[1]),]}
  if (Areas[2] != 0){D = D[which(D$Area2 == Areas[2]),]}
  SampleSize = length(D[,1])
  OC = sum(D$TOT_OC)
  C = 0;C2 = 0;C_se = 0;C_UB = 0;C_LB = 0;CV = 0;NCF = 0
  if(SampleSize>0 & OC>0){
    if(CFDef == 'HSM'){
      C = sum(D$TOT_OC)/sum(D$TOT_PC)
      B = BootFun(D,NBoot,CFDef)
    }
    if(CFDef == 'Fitted'){
      C = sum(D$StateFitted)/sum(D$TOT_PC)
      B = BootFun(D,NBoot,CFDef)
    }
    if(CFDef == 'LSE'){
      C = sum(D$TOT_OC*D$TOT_PC)/sum(D$TOT_PC*D$TOT_PC)
      B = BootFun(D,NBoot,CFDef)
    }
    if(CFDef == 'Mehta'){
      C = exp(glm(TOT_OC~offset(log(TOT_PC)),data=D,family=negative.binomial(theta=(var(D$TOT_OC)-mean(D$TOT_OC))/mean(D$TOT_OC)^2))$coeff[[1]])
      #C = mean(D$TOT_OC/D$TOT_PC)
      B = BootFun(D,100,CFDef)
    }    
    if(CFDef == 'MLE'){
      C = MLE_CF(D$TOT_OC,D$TOT_PC)
      B = BootFun(D,1000,CFDef)
    } 
    if(CFDef == 'Srini1'){
      m = mean(D$TOT_OC)
      v = var(D$TOT_OC)
      Theta = m^2 / (v-m)
      M = glm(D$TOT_OC ~ log(D$TOT_PC), family= negative.binomial(theta = Theta,link= "log"))
      C = exp(M$coef[[1]])
      C2 = M$coef[[2]]
      B = BootFun(D,NBoot,CFDef)
    } 
    VB = var(B)
    if(CFDef == 'Mehta'){VB=vcov(glm(TOT_OC~offset(log(TOT_PC)),data=D,family=negative.binomial(theta=(var(D$TOT_OC)-mean(D$TOT_OC))/mean(D$TOT_OC)^2)))[[1]]}
    if(!is.na(VB)){
      C_se = sqrt(VB)
      t = qt((1+ConfLevel)/2,SampleSize)
      C_LB = C - C_se*t
      C_UB = C + C_se*t
      CV = C_se/C
    }
  }
  CF = data.frame('CFactor'= C,'C SE'= C_se,'C UB' = C_UB, 'C LB' = C_LB,'Size' = SampleSize, 'OC' = OC,'CV' = CV)
  return(CF)
}
CoefSummary   = function(MD,ConfLevel = 0.95){
  AllInt = c('R3ST'  ,'R4ST'  ,'R4SG'  ,'RM3ST'  ,'RM4ST'  ,'RM4SG'  ,'U3ST'  ,'U4ST'  ,'U3SG'  ,'U4SG'  )
  AllSeg = c('R2U','R4D','R4U','U2U','U3T','U4U','U4D','U5T')
  AllP = NULL;for(Type in c(AllInt,AllSeg)){AllP = c(AllP,ListPredictors(Type))};AllP = c('(Intercept)',unique(AllP))
  M = MD[[1]]
  D = MD[[2]]
  Type = toString(unique(D$FType)[1])
  AV = length(D[,1]);names(AV) = 'Sample Size'
  t  = qt((1+ConfLevel)/2,AV)
  P  = data.frame(summary(M)$coef)
  #BI = P['(Intercept)',1]
  #BISE = P['(Intercept)',2]
  #if(length(rownames(P))>3){
  #  for(p in rownames(P)){
  #    if(!(p %in% c('(Intercept)','AADT_Major','AADT_Minor','Length','AADT'))){
  #      BI = BI + P[p,1]*ListBaseValues(Type)[p][[1]]
  #      BISE = BISE + P[p,2]*ListBaseValues(Type)[p][[1]]^2
  #    }}
  #}
  #AV = c(AV,BI       );names(AV) = c(names(AV)[1:length(names(AV))-1],'BInt.Es')
  #AV = c(AV,BISE     );names(AV) = c(names(AV)[1:length(names(AV))-1],'BInt.SE')
  #AV = c(AV,BI+BISE*t);names(AV) = c(names(AV)[1:length(names(AV))-1],'BInt.UB')
  #AV = c(AV,BI-BISE*t);names(AV) = c(names(AV)[1:length(names(AV))-1],'BInt.LB')
  for(p in AllP){
    if(p %in% rownames(P)){
      AV = c(AV,P[p,1]            );names(AV) = c(names(AV)[1:length(names(AV))-1],paste(p,'.Es',sep=''))
      AV = c(AV,P[p,2]            );names(AV) = c(names(AV)[1:length(names(AV))-1],paste(p,'.SE'  ,sep=''))
      AV = c(AV,P[p,4]            );names(AV) = c(names(AV)[1:length(names(AV))-1],paste(p,'.Pv'  ,sep=''))
      AV = c(AV,P[p,1]-P[p,2]*t   );names(AV) = c(names(AV)[1:length(names(AV))-1],paste(p,'.LB'  ,sep=''))
      AV = c(AV,P[p,1]+P[p,2]*t   );names(AV) = c(names(AV)[1:length(names(AV))-1],paste(p,'.UB'  ,sep=''))
      AV = c(AV,abs(P[p,2]/P[p,1]));names(AV) = c(names(AV)[1:length(names(AV))-1],paste(p,'.CV' ,sep=''))
    }
    else{
      AV = c(AV,0);names(AV) = c(names(AV)[1:length(names(AV))-1],paste(p,'.Es',sep=''))
      AV = c(AV,0);names(AV) = c(names(AV)[1:length(names(AV))-1],paste(p,'.SE'  ,sep=''))
      AV = c(AV,0);names(AV) = c(names(AV)[1:length(names(AV))-1],paste(p,'.Pv'  ,sep=''))
      AV = c(AV,0);names(AV) = c(names(AV)[1:length(names(AV))-1],paste(p,'.LB'  ,sep=''))
      AV = c(AV,0);names(AV) = c(names(AV)[1:length(names(AV))-1],paste(p,'.UB'  ,sep=''))
      AV = c(AV,0);names(AV) = c(names(AV)[1:length(names(AV))-1],paste(p,'.CV' ,sep=''))
    }
  }
  Out = data.frame(t(data.frame(AV)))
  rownames(Out) = Type
  return(Out)
}  
HSMValues     = function(Type,CalibrationFactors = 1){
  IntTypes      = c('R3ST','R4ST','R4SG','RM3ST','RM4ST','RM4SG','U3ST','U4ST','U3SG','U4SG','R2U' ,'R4U' ,'R4D' ,'U2U' ,'U3T' ,'U4U' ,'U4D' ,'U5T' ,'AllInt' , 'AllSeg')
  HSM_A         = c(-9.86 ,-8.56 ,-5.13 ,-12.526,-10.008,-7.182 ,-13.36 ,-8.9   ,-12.13 ,-10.99 ,-0.312,-9.653,-9.205,-15.22,-12.4 ,-11.63,-12.34,-9.7  ,0        ,0        )
  HSM_B         = c(0.79  ,0.6   ,0.6   ,1.204  ,0.848  ,0.722  ,1.11   ,0.82   ,1.11   ,1.07   ,0     ,1.176 ,1.049 ,1.68  ,1.41  ,1.33  ,1.36  ,1.17  ,0        ,0        )   
  HSM_C         = c(0.49  ,0.61  ,0.2   ,0.236  ,0.448  ,0.337  ,0.41   ,0.25   ,0.26   ,0.23   ,0     ,0     ,0     ,0     ,0     ,0     ,0     ,0     ,0        ,0        )
  AADT_Major_UB = c(19500 ,14700 ,25200 ,78300  ,78300  ,43500  ,45700  ,46800  ,58100  ,67700  ,17800 ,33200 ,89300 ,32600 ,32900 ,40100 ,66000 ,53800 ,78300    ,89300    )
  AADT_Minor_UB = c(4300  ,3500  ,12500 ,23000  ,7400   ,18500  ,9300   ,5900   ,16400  ,33400  ,0     ,0     ,0     ,0     ,0     ,0     ,0     ,0     ,33400    ,0        )
  Length_LB     = c(0     ,0     ,0     ,0      ,0      ,0      ,0      ,0      ,0      ,0      ,0.1   ,0.1   ,0.1   ,0.1   ,0.1   ,0.1   ,0.1   ,0.1   ,0        ,0.1      )
  k             = c(0.54  ,0.24  ,0.11  ,0.46   ,0.494  ,0.277  ,1.14   ,0.65   ,0.36   ,0.36   ,0.236 ,1.675 ,1.549 ,0.84  ,0.66  ,1.01  ,1.32  ,0.81  ,0        ,0        )
  SC_Cal        = CalibrationFactors
  HSM_D         = c(09    ,0     ,0     ,0      ,0      ,0      ,0      ,0      ,0      ,0      ,0     ,0     ,0     ,-5.47 ,-5.74 ,-7.99 ,-5.05 ,-4.82 ,0        ,0        )
  HSM_E         = c(0     ,0     ,0     ,0      ,0      ,0      ,0      ,0      ,0      ,0      ,0     ,0     ,0     ,0.56  ,0.54  ,0.81  ,0.47  ,0.54  ,0        ,0        )   
  
  D = data.frame('A'=HSM_A, 'B'=HSM_B, 'C'=HSM_C, 'C_Factor' = SC_Cal, 'AADT_Major_UB' = AADT_Major_UB,'AADT_Minor_UB' = AADT_Minor_UB,'Length_LB' = Length_LB,k,'D'=HSM_D,'E'=HSM_E)
  rownames(D) = IntTypes
  return(D[Type,])
}
MLE_CF        = function(OC,PC){
  ObjFun = function(CF){
    Obs = get('OC',envir=.GlobalEnv)
    Fit = get('PC',envir=.GlobalEnv) * CF
    return(-LogLike(Obs,Fit))
  }
  ObjFun2 = function(CF){
    Obs = get('OC',envir=.GlobalEnv)
    Fit = get('PC',envir=.GlobalEnv) * CF
    k = mean(Obs)^2/(var(Obs)-mean(Obs))
    if(!is.finite(k)){k=0}
    abs(sum((Obs+k)/(Fit+k))-length(Obs))
  }
  assign('OC',OC,envir=.GlobalEnv)
  assign('PC',PC,envir=.GlobalEnv)
  C = sum(OC)/sum(PC)
  O = optim(C,ObjFun2,lower = 0.1,upper = 10,method='L-BFGS-B')
  return(O$par)
}
LogLike       = function(Obs,Fit){
  k = (var(Obs)-mean(Obs)) / mean(Obs)^2
  if(k==0){k=10}
  k = 1/k
  if(k<0){k=0.1}
  term <- (Obs+k)*log(Fit+k) - Obs*log(Fit) + lgamma(Obs+1) - k*log(k) + lgamma(k) - lgamma(k+Obs)
  return(-sum(term))
}
GOFMeasure    = function(Obs,Fit,Method){
  Out = NULL
  if(Method=='SSE'){Out = sum((Obs-Fit)^2)}
  if(Method=='ASE'){Out = abs(sum(Obs-Fit))}
  if(Method=='LL' ){Out = LogLike(Obs,Fit)}
  if(Method=='MAD'){Out = mean(abs(Obs-Fit))}
  if(Method=='SE' ){Out = sum(Obs-Fit)}
  if(Method=='Dev'){
    #a = Obs*log(Obs/Fit)-(Obs-Fit)
    a = -log(Obs/Fit)+(Obs-Fit)/Fit
    a[which(!is.finite(a))] = 0
    Out = sum(a)}
  return(Out)
}
CFPredict     = function(PC,CF,CFDef){
  if(CFDef %in% c('HSM','Fitted','LSE','Mehta','MLE')){
    Fitted = CF$CFactor*PC
  }
  if(CFDef == 'Srini1'){
    Fitted = exp(log(CF$CFactor[[1]])+log(PC)*CF$CFactor2)
  }
  return(Fitted)
}
NullTest      = function(CF1,CF2){
  
  if('CFactor'     %in% names(CF1)){C1    = CF1$CFactor} 
  if('CFactor'     %in% names(CF2)){C2    = CF2$CFactor} 
  if('CV'          %in% names(CF1)){C1Var = (CF1$CV*C1)^2} 
  if('CV'          %in% names(CF2)){C2Var = (CF2$CV*C2)^2} 
  if('CFactor CV'  %in% names(CF1)){C1Var = (CF1$'CFactor CV'*C1)^2} 
  if('CFactor CV'  %in% names(CF2)){C2Var = (CF2$'CFactor CV'*C2)^2} 
  if('Size'        %in% names(CF1)){Df1   = CF1$Size-1} 
  if('Size'        %in% names(CF2)){Df2   = CF2$Size-1} 
  if('Sample.Size' %in% names(CF1)){Df1   = CF1$'Sample.Size'-1} 
  if('Sample.Size' %in% names(CF2)){Df2   = CF2$'Sample.Size'-1} 
  
  CDiff = abs(C1 - C2)
  CDVar = C1Var + C2Var
  #DfD = CDVar^2/(C1Var^2/Df1+C2Var^2/Df2)
  #varp = (C1Var*Df1^2+C2Var*Df2^2)/(Df1+Df2)
  #CDVar = varp*(1/(Df1+1)+1/(Df2+1))
  DfD = Df1+Df2
  p = 1-2 * (1-pt(CDiff/sqrt(CDVar),DfD))
  return(p)
}

# Output Table: -------------------------------------------------------------------------------------------
SitesSummary  = function(Scope,Type,CrashAssign = 'Var',Year=2014){
  SegSummary   = function(Tab,oFN){
    
    #if(substr(iFN,nchar(iFN)-2,nchar(iFN))=='txt'){Tab = read.table(iFN,header = TRUE)}
    #if(substr(iFN,nchar(iFN)-2,nchar(iFN))=='csv'){Tab = read.csv(iFN,header = TRUE)}
    Tab['AADT']   = exp(Tab['AADT'])
    Tab['Length'] = exp(Tab['Length'])*5280
    AllSeg = c('R2U','R4D','R4U','U2U','U3T','U4U','U4D','U5T','R4F','U4F','U6F')
    AllLab = NULL
    
    Tot.SampleSize      = NULL
    Tot.Mileage         = NULL
    Tot.AveAADT         = NULL
    Tot.TotOC           = NULL
    Tot.KABC            = NULL
    
    Coastal.SampleSize  = NULL
    Coastal.Mileage     = NULL
    Coastal.AveAADT     = NULL
    Coastal.TotOC       = NULL
    Coastal.KABC        = NULL
    
    Midstate.SampleSize = NULL
    Midstate.Mileage    = NULL
    Midstate.AveAADT    = NULL
    Midstate.TotOC      = NULL
    Midstate.KABC       = NULL
    
    Upstate.SampleSize  = NULL
    Upstate.Mileage     = NULL
    Upstate.AveAADT     = NULL
    Upstate.TotOC       = NULL
    Upstate.KABC        = NULL
    
    Dense.SampleSize    = NULL
    Dense.Mileage       = NULL
    Dense.AveAADT       = NULL
    Dense.TotOC         = NULL
    Dense.KABC          = NULL
    
    Sparse.SampleSize   = NULL
    Sparse.Mileage      = NULL
    Sparse.AveAADT      = NULL
    Sparse.TotOC        = NULL
    Sparse.KABC         = NULL
    
    for(Type in AllSeg){
      if(Type %in% unique(Tab$FType)){
        if(!(Type == '')){AllLab = c(AllLab, Type)}
        if( (Type == '')){AllLab = c(AllLab, 'Other')}
        Tot.SampleSize = c(Tot.SampleSize,length(Tab[which(Tab$FType==Type),][,1]))
        Tot.Mileage    = c(Tot.Mileage   ,sum   (Tab[which(Tab$FType==Type),]['Length'][[1]])/5280)
        Tot.AveAADT    = c(Tot.AveAADT   ,sum   (Tab[which(Tab$FType==Type),]['AADT'][[1]]*Tab[which(Tab$FType==Type),]['Length']/5280)/sum   (Tab[which(Tab$FType==Type),]['Length'][[1]])*5280)
        Tot.TotOC      = c(Tot.TotOC     ,sum   (Tab[which(Tab$FType==Type),]['TOT_OC'][[1]]))
        Tot.KABC       = c(Tot.KABC      ,sum   (Tab[which(Tab$FType==Type),]['FI_OC'] [[1]]))
        
        Coastal.SampleSize = c(Coastal.SampleSize,length(Tab[which(Tab$Area1==1 & Tab$FType==Type),][,1]))
        Coastal.Mileage    = c(Coastal.Mileage   ,sum   (Tab[which(Tab$Area1==1 & Tab$FType==Type),]['Length'][[1]])/5280)
        Coastal.AveAADT    = c(Coastal.AveAADT   ,sum   (Tab[which(Tab$Area1==1 & Tab$FType==Type),]['AADT'][[1]]*Tab[which(Tab$Area1==1 & Tab$FType==Type),]['Length'][[1]]/5280)/sum   (Tab[which(Tab$Area1==1 & Tab$FType==Type),]['Length'][[1]])*5280)
        Coastal.TotOC      = c(Coastal.TotOC     ,sum   (Tab[which(Tab$Area1==1 & Tab$FType==Type),]['TOT_OC'][[1]]))
        Coastal.KABC       = c(Coastal.KABC      ,sum   (Tab[which(Tab$Area1==1 & Tab$FType==Type),]['FI_OC'] [[1]]))
        
        Midstate.SampleSize = c(Midstate.SampleSize,length(Tab[which(Tab$Area1==2 & Tab$FType==Type),][,1]))
        Midstate.Mileage    = c(Midstate.Mileage   ,sum   (Tab[which(Tab$Area1==2 & Tab$FType==Type),]['Length'][[1]])/5280)
        Midstate.AveAADT    = c(Midstate.AveAADT   ,sum   (Tab[which(Tab$Area1==2 & Tab$FType==Type),]['AADT'][[1]]*Tab[which(Tab$Area1==2 & Tab$FType==Type),]['Length'][[1]]/5280)/sum   (Tab[which(Tab$Area1==2 & Tab$FType==Type),]['Length'][[1]])*5280)
        Midstate.TotOC      = c(Midstate.TotOC     ,sum   (Tab[which(Tab$Area1==2 & Tab$FType==Type),]['TOT_OC'][[1]]))
        Midstate.KABC       = c(Midstate.KABC      ,sum   (Tab[which(Tab$Area1==2 & Tab$FType==Type),]['FI_OC'] [[1]]))
        
        Upstate.SampleSize = c(Upstate.SampleSize,length(Tab[which(Tab$Area1==3 & Tab$FType==Type),][,1]))
        Upstate.Mileage    = c(Upstate.Mileage   ,sum   (Tab[which(Tab$Area1==3 & Tab$FType==Type),]['Length'][[1]])/5280)
        Upstate.AveAADT    = c(Upstate.AveAADT   ,sum   (Tab[which(Tab$Area1==3 & Tab$FType==Type),]['AADT'][[1]]*Tab[which(Tab$Area1==3 & Tab$FType==Type),]['Length'][[1]]/5280)/sum   (Tab[which(Tab$Area1==3 & Tab$FType==Type),]['Length'][[1]])*5280)
        Upstate.TotOC      = c(Upstate.TotOC     ,sum   (Tab[which(Tab$Area1==3 & Tab$FType==Type),]['TOT_OC'][[1]]))
        Upstate.KABC       = c(Upstate.KABC      ,sum   (Tab[which(Tab$Area1==3 & Tab$FType==Type),]['FI_OC'] [[1]]))
        
        Dense.SampleSize = c(Dense.SampleSize,length(Tab[which(Tab$Area2==1 & Tab$FType==Type),][,1]))
        Dense.Mileage    = c(Dense.Mileage   ,sum   (Tab[which(Tab$Area2==1 & Tab$FType==Type),]['Length'][[1]])/5280)
        Dense.AveAADT    = c(Dense.AveAADT   ,sum   (Tab[which(Tab$Area2==1 & Tab$FType==Type),]['AADT'][[1]]*Tab[which(Tab$Area2==1 & Tab$FType==Type),]['Length'][[1]]/5280)/sum   (Tab[which(Tab$Area2==1 & Tab$FType==Type),]['Length'][[1]])*5280)
        Dense.TotOC      = c(Dense.TotOC     ,sum   (Tab[which(Tab$Area2==1 & Tab$FType==Type),]['TOT_OC'][[1]]))
        Dense.KABC       = c(Dense.KABC      ,sum   (Tab[which(Tab$Area2==1 & Tab$FType==Type),]['FI_OC'] [[1]]))
        
        Sparse.SampleSize = c(Sparse.SampleSize,length(Tab[which(Tab$Area2==2 & Tab$FType==Type),][,1]))
        Sparse.Mileage    = c(Sparse.Mileage   ,sum   (Tab[which(Tab$Area2==2 & Tab$FType==Type),]['Length'][[1]])/5280)
        Sparse.AveAADT    = c(Sparse.AveAADT   ,sum   (Tab[which(Tab$Area2==2 & Tab$FType==Type),]['AADT'][[1]]*Tab[which(Tab$Area2==2 & Tab$FType==Type),]['Length'][[1]]/5280)/sum   (Tab[which(Tab$Area2==2 & Tab$FType==Type),]['Length'][[1]])*5280)
        Sparse.TotOC      = c(Sparse.TotOC     ,sum   (Tab[which(Tab$Area2==2 & Tab$FType==Type),]['TOT_OC'][[1]]))
        Sparse.KABC       = c(Sparse.KABC      ,sum   (Tab[which(Tab$Area2==2 & Tab$FType==Type),]['FI_OC'] [[1]]))
      }
    }
    Tot.KABC       = Tot.KABC/Tot.TotOC
    
    Coastal.SampleSize = Coastal.SampleSize/Tot.SampleSize
    Coastal.Mileage    = Coastal.Mileage/Tot.Mileage
    Coastal.KABC       = Coastal.KABC/Coastal.TotOC
    Coastal.TotOC      = Coastal.TotOC/Tot.TotOC
    
    Midstate.SampleSize = Midstate.SampleSize/Tot.SampleSize
    Midstate.Mileage    = Midstate.Mileage/Tot.Mileage
    Midstate.KABC       = Midstate.KABC/Midstate.TotOC
    Midstate.TotOC      = Midstate.TotOC/Tot.TotOC
    
    Upstate.SampleSize = Upstate.SampleSize/Tot.SampleSize
    Upstate.Mileage    = Upstate.Mileage/Tot.Mileage
    Upstate.KABC       = Upstate.KABC/Upstate.TotOC
    Upstate.TotOC      = Upstate.TotOC/Tot.TotOC
    
    Dense.SampleSize = Dense.SampleSize/Tot.SampleSize
    Dense.Mileage    = Dense.Mileage/Tot.Mileage
    Dense.KABC       = Dense.KABC/Dense.TotOC
    Dense.TotOC      = Dense.TotOC/Tot.TotOC
    
    Sparse.SampleSize = Sparse.SampleSize/Tot.SampleSize
    Sparse.Mileage    = Sparse.Mileage/Tot.Mileage
    Sparse.KABC       = Sparse.KABC/Sparse.TotOC
    Sparse.TotOC      = Sparse.TotOC/Tot.TotOC
    
    Out = data.frame(Tot.SampleSize,Tot.Mileage,Tot.AveAADT,Tot.TotOC,Tot.KABC,
                     Coastal.Mileage,Coastal.AveAADT,Coastal.TotOC,Coastal.KABC,
                     Midstate.Mileage,Midstate.AveAADT,Midstate.TotOC,Midstate.KABC,
                     Upstate.Mileage,Upstate.AveAADT,Upstate.TotOC,Upstate.KABC,
                     Dense.Mileage,Dense.AveAADT,Dense.TotOC,Dense.KABC,
                     Sparse.Mileage,Sparse.AveAADT,Sparse.TotOC,Sparse.KABC)
    rownames(Out) = AllLab
    write.csv(Out,oFN)
    return(Out)
  }
  IntSummary   = function(Tab,oFN){
    
    #if(substr(iFN,nchar(iFN)-2,nchar(iFN))=='txt'){Tab = read.table(iFN,header = TRUE)}
    #if(substr(iFN,nchar(iFN)-2,nchar(iFN))=='csv'){Tab = read.csv(iFN,header = TRUE)}
    Tab['AADT_Major'] = exp(Tab['AADT_Major'])
    Tab['AADT_Minor'] = exp(Tab['AADT_Minor'])
    
    AllInt = c('R3ST'  ,'R4ST'  ,'R4SG'  ,'RM3ST'  ,'RM4ST'  ,'RM4SG'  ,'U3ST'  ,'U4ST'  ,'U3SG'  ,'U4SG')
    AllLab = c('R3ST'  ,'R4ST'  ,'R4SG'  ,'RM3ST'  ,'RM4ST'  ,'RM4SG'  ,'U3ST'  ,'U4ST'  ,'U3SG'  ,'U4SG')
    
    Tot.SampleSize      = NULL
    Tot.AveAADT1        = NULL
    Tot.AveAADT2        = NULL
    Tot.TotOC           = NULL
    Tot.KABC            = NULL
    
    Coastal.SampleSize  = NULL
    Coastal.AveAADT1    = NULL
    Coastal.AveAADT2    = NULL
    Coastal.TotOC       = NULL
    Coastal.KABC        = NULL
    
    Midstate.SampleSize = NULL
    Midstate.AveAADT1   = NULL
    Midstate.AveAADT2   = NULL
    Midstate.TotOC      = NULL
    Midstate.KABC       = NULL
    
    Upstate.SampleSize  = NULL
    Upstate.AveAADT1    = NULL
    Upstate.AveAADT2    = NULL
    Upstate.TotOC       = NULL
    Upstate.KABC        = NULL
    
    Dense.SampleSize    = NULL
    Dense.AveAADT1      = NULL
    Dense.AveAADT2      = NULL
    Dense.TotOC         = NULL
    Dense.KABC          = NULL
    
    Sparse.SampleSize   = NULL
    Sparse.AveAADT1     = NULL
    Sparse.AveAADT2     = NULL
    Sparse.TotOC        = NULL
    Sparse.KABC         = NULL
    
    
    for(Type in AllInt){
      Tot.SampleSize = c(Tot.SampleSize,length(Tab[which(Tab$FType==Type),][,1]))
      Tot.AveAADT1   = c(Tot.AveAADT1  ,mean  (Tab[which(Tab$FType==Type),]['AADT_Major'][[1]]))
      Tot.AveAADT2   = c(Tot.AveAADT2  ,mean  (Tab[which(Tab$FType==Type),]['AADT_Minor'][[1]]))
      Tot.TotOC      = c(Tot.TotOC     ,sum   (Tab[which(Tab$FType==Type),]['TOT_OC'][[1]]))
      Tot.KABC       = c(Tot.KABC      ,sum   (Tab[which(Tab$FType==Type),]['FI_OC'] [[1]]))
      
      Coastal.SampleSize = c(Coastal.SampleSize,length(Tab[which(Tab$Area1==1 & Tab$FType==Type),][,1]))
      Coastal.AveAADT1   = c(Coastal.AveAADT1  ,mean  (Tab[which(Tab$Area1==1 & Tab$FType==Type),]['AADT_Major'][[1]]))
      Coastal.AveAADT2   = c(Coastal.AveAADT2  ,mean  (Tab[which(Tab$Area1==1 & Tab$FType==Type),]['AADT_Minor'][[1]]))
      Coastal.TotOC      = c(Coastal.TotOC     ,sum   (Tab[which(Tab$Area1==1 & Tab$FType==Type),]['TOT_OC'][[1]]))
      Coastal.KABC       = c(Coastal.KABC      ,sum   (Tab[which(Tab$Area1==1 & Tab$FType==Type),]['FI_OC'] [[1]]))
      
      Midstate.SampleSize = c(Midstate.SampleSize,length(Tab[which(Tab$Area1==2 & Tab$FType==Type),][,1]))
      Midstate.AveAADT1   = c(Midstate.AveAADT1  ,mean  (Tab[which(Tab$Area1==2 & Tab$FType==Type),]['AADT_Major'][[1]]))
      Midstate.AveAADT2  = c(Midstate.AveAADT2  ,mean  (Tab[which(Tab$Area1==2 & Tab$FType==Type),]['AADT_Minor'][[1]]))
      Midstate.TotOC      = c(Midstate.TotOC     ,sum   (Tab[which(Tab$Area1==2 & Tab$FType==Type),]['TOT_OC'][[1]]))
      Midstate.KABC       = c(Midstate.KABC      ,sum   (Tab[which(Tab$Area1==2 & Tab$FType==Type),]['FI_OC'] [[1]]))
      
      Upstate.SampleSize = c(Upstate.SampleSize,length(Tab[which(Tab$Area1==3 & Tab$FType==Type),][,1]))
      Upstate.AveAADT1   = c(Upstate.AveAADT1  ,mean  (Tab[which(Tab$Area1==3 & Tab$FType==Type),]['AADT_Major'][[1]]))
      Upstate.AveAADT2   = c(Upstate.AveAADT2  ,mean  (Tab[which(Tab$Area1==3 & Tab$FType==Type),]['AADT_Minor'][[1]]))
      Upstate.TotOC      = c(Upstate.TotOC     ,sum   (Tab[which(Tab$Area1==3 & Tab$FType==Type),]['TOT_OC'][[1]]))
      Upstate.KABC       = c(Upstate.KABC      ,sum   (Tab[which(Tab$Area1==3 & Tab$FType==Type),]['FI_OC'] [[1]]))
      
      Dense.SampleSize = c(Dense.SampleSize,length(Tab[which(Tab$Area2==1 & Tab$FType==Type),][,1]))
      Dense.AveAADT1   = c(Dense.AveAADT1  ,mean  (Tab[which(Tab$Area2==1 & Tab$FType==Type),]['AADT_Major'][[1]]))
      Dense.AveAADT2   = c(Dense.AveAADT2  ,mean  (Tab[which(Tab$Area2==1 & Tab$FType==Type),]['AADT_Minor'][[1]]))
      Dense.TotOC      = c(Dense.TotOC     ,sum   (Tab[which(Tab$Area2==1 & Tab$FType==Type),]['TOT_OC'][[1]]))
      Dense.KABC       = c(Dense.KABC      ,sum   (Tab[which(Tab$Area2==1 & Tab$FType==Type),]['FI_OC'] [[1]]))
      
      Sparse.SampleSize = c(Sparse.SampleSize,length(Tab[which(Tab$Area2==2 & Tab$FType==Type),][,1]))
      Sparse.AveAADT1   = c(Sparse.AveAADT1  ,mean  (Tab[which(Tab$Area2==2 & Tab$FType==Type),]['AADT_Major'][[1]]))
      Sparse.AveAADT2   = c(Sparse.AveAADT2  ,mean  (Tab[which(Tab$Area2==2 & Tab$FType==Type),]['AADT_Minor'][[1]]))
      Sparse.TotOC      = c(Sparse.TotOC     ,sum   (Tab[which(Tab$Area2==2 & Tab$FType==Type),]['TOT_OC'][[1]]))
      Sparse.KABC       = c(Sparse.KABC      ,sum   (Tab[which(Tab$Area2==2 & Tab$FType==Type),]['FI_OC'] [[1]]))
      
    }
    Tot.KABC       = Tot.KABC/Tot.TotOC
    
    Coastal.SampleSize = Coastal.SampleSize/Tot.SampleSize
    Coastal.KABC       = Coastal.KABC/Coastal.TotOC
    Coastal.TotOC      = Coastal.TotOC/Tot.TotOC
    
    Midstate.SampleSize = Midstate.SampleSize/Tot.SampleSize
    Midstate.KABC       = Midstate.KABC/Midstate.TotOC
    Midstate.TotOC      = Midstate.TotOC/Tot.TotOC
    
    Upstate.SampleSize = Upstate.SampleSize/Tot.SampleSize
    Upstate.KABC       = Upstate.KABC/Upstate.TotOC
    Upstate.TotOC      = Upstate.TotOC/Tot.TotOC
    
    Dense.SampleSize = Dense.SampleSize/Tot.SampleSize
    Dense.KABC       = Dense.KABC/Dense.TotOC
    Dense.TotOC      = Dense.TotOC/Tot.TotOC
    
    Sparse.SampleSize = Sparse.SampleSize/Tot.SampleSize
    Sparse.KABC       = Sparse.KABC/Sparse.TotOC
    Sparse.TotOC      = Sparse.TotOC/Tot.TotOC
    
    Out = data.frame(Tot.SampleSize,Tot.AveAADT1,Tot.AveAADT2,Tot.TotOC,Tot.KABC,
                     Coastal.SampleSize,Coastal.AveAADT1,Coastal.AveAADT2,Coastal.TotOC,Coastal.KABC,
                     Midstate.SampleSize,Midstate.AveAADT1,Midstate.AveAADT2,Midstate.TotOC,Midstate.KABC,
                     Upstate.SampleSize,Upstate.AveAADT1,Upstate.AveAADT2,Upstate.TotOC,Upstate.KABC,
                     Dense.SampleSize,Dense.AveAADT1,Dense.AveAADT2,Dense.TotOC,Dense.KABC,
                     Sparse.SampleSize,Sparse.AveAADT1,Sparse.AveAADT2,Sparse.TotOC,Sparse.KABC)
    
    rownames(Out) = AllLab
    write.csv(Out,oFN)
    return(Out)
  }
  Out = NULL
  Tab = Text2Dat(Scope,Type,Year,CrashAssign,ListHeaders(Type))
  Tab[is.na(Tab)] = 0
  O = paste('Outputs/',Scope,'/Sum_',Type,'_',Year,'_',CrashAssign,'.csv',sep='')
  if(Type=='AllSeg'){Out = SegSummary(Tab,O)}
  if(Type=='AllInt'){Out = IntSummary(Tab,O)}
  return(Out)
}
CrashSummary  = function(Scope,Years){
  All     = NULL
  AllKABC = NULL
  IntAll  = NULL
  IntKABC = NULL
  SegAll  = NULL
  SegKABC = NULL
  NotAll  = NULL
  NotKABC = NULL
  for(Year in Years){
    Tab = read.csv(FileNames(Scope,'Crash',Year,'Var'),header = TRUE) 
    Tab[is.na(Tab)] = 0
    All     = c(All    ,length(Tab[,1]))
    IntAll  = c(IntAll ,length(Tab[which(Tab$ICrash == 1),1])/length(Tab[,1]))
    SegAll  = c(SegAll ,length(Tab[which(Tab$RCrash == 1),1])/length(Tab[,1]))
    NotAll  = c(NotAll ,length(Tab[which((Tab$RCrash == 0 & Tab$ICrash == 0)),1])/length(Tab[,1]))
    if(Scope %in% c('SCSEL','SCALL')){
      AllKABC = c(AllKABC,length(Tab[which(Tab$INJ>0 | Tab$FAT>0),1])/length(Tab[,1]))
      IntKABC = c(IntKABC,length(Tab[which(Tab$ICrash == 1 & (Tab$INJ>0 | Tab$FAT>0)),1])/length(Tab[which(Tab$ICrash == 1),1]))
      SegKABC = c(SegKABC,length(Tab[which(Tab$RCrash == 1 & (Tab$INJ>0 | Tab$FAT>0)),1])/length(Tab[which(Tab$RCrash == 1),1]))
      NotKABC = c(NotKABC,length(Tab[which(Tab$RCrash == 0 & Tab$ICrash == 0 & (Tab$INJ>0 | Tab$FAT>0)),1])/length(Tab[which(Tab$RCrash == 0 & Tab$ICrash == 0),1]))
    }
    if(Scope %in% c('NCSEL','NCALL')){
      AllKABC = c(AllKABC,length(Tab[which(Tab$INJ>0),1])/length(Tab[,1]))
      IntKABC = c(IntKABC,length(Tab[which(Tab$ICrash == 1 & (Tab$INJ>0 | Tab$FAT>0)),1])/length(Tab[which(Tab$ICrash == 1),1]))
      SegKABC = c(SegKABC,length(Tab[which(Tab$RCrash == 1 & (Tab$INJ>0 | Tab$FAT>0)),1])/length(Tab[which(Tab$RCrash == 1),1]))
      NotKABC = c(NotKABC,length(Tab[which(Tab$RCrash == 0 & Tab$ICrash == 0 & (Tab$INJ>0 | Tab$FAT>0)),1])/length(Tab[which(Tab$RCrash == 0 & Tab$ICrash == 0),1]))
    }
  }
  Out = data.frame(All,AllKABC,IntAll,IntKABC,SegAll,SegKABC, NotAll, NotKABC)
  rownames(Out) = Years
  write.csv(Out,'Outputs/CrashSummary.csv')
  return(Out)
}
CFTable       = function(Scope,TitleList,Years,Areas,CrashAssign,CFDef = 'HSM'){
  n = length(TitleList)
  TotTable = data.frame()
  IntTypes = c('R3ST','R4ST','R4SG','RM3ST','RM4ST','RM4SG','U3ST','U4ST','U3SG','U4SG')
  SegTypes = c('R2U','R4D','R4U','U2U','U3T','U4U','U4D','U5T','R4F','U4F','U6F')
  AllInt   = c('AllInt','AllIntSig','AllIntStp')
  AllSeg   = c('AllSeg')
  for(i in 1:n){
    Type = TitleList[i]
    D1 = Text2Dat(Scope,Type,Years,CrashAssign,ListHeaders(Type))
    
    #Apply Volume Ranges --> D2
    VolRange = FilterData(Scope,Type,Years,'Volumes')
    D2 = D1
    if(Type %in% c(IntTypes,AllInt)){
      D2     = D1[ (D1$AADT_Major<log(VolRange[1]) & D1$AADT_Minor<log(VolRange[2])),]
    }
    if(Type %in% c(SegTypes,AllSeg)){
      D2     = D1[ (D1$AADT  <log(VolRange[1]) & D1$Length>log(VolRange[2])),]
    }
    
    #Appply Outliers --> D3
    Os = FilterData(Scope,Type,Years,'Outliers')
    MD2 = GLMModels(D2 ,'Base')
    D3  = D2
    if(1>0){
      D_Out = Outliers(Scope,MD2 ,TRUE,Type ,Os)
      D3    = D2[!(rownames(D2)  %in% rownames(D_Out)) ,]}
    
    #Calibration Factor
    CF = CFactor(D3,Areas = Areas,CFDef = CFDef)
    HSMCoef = HSMValues(Type,CF$CFactor)
    PlotXX(Scope,D2,D1,VolRange,HSMCoef,Type,F)
    PlotCF(Scope,D3,Type,Years,CFDef = CFDef)
    if (Areas[1] != 0){D3 = D3[which(D3$Area1 == Areas[1]),]}
    if (Areas[2] != 0){D3 = D3[which(D3$Area2 == Areas[2]),]}
    Table = data.frame('Sample Size' = length(D3[,1]))
    rownames(Table) = TitleList[i]
    if(Type %in% c(IntTypes,AllInt)){
      Table['Total Length']       = 0
      Table['Average AADT Major'] = exp(mean(D3$AADT_Major))
      Table['Average AADT Minor'] = exp(mean(D3$AADT_Minor))   
    }    
    if(Type %in% c(SegTypes,AllSeg)){
      Table['Total Length']       = sum(exp(D3$Length))
      Table['Average AADT Major'] = exp(mean(D3$AADT))
      Table['Average AADT Minor'] = 0   
    }    
    Table['Tot Observed Crashes']  =  sum(D3$TOT_OC)
    Table['Tot Predicted Crashes'] =  sum(D3$TOT_PC)
    Table['CFactor']    =  CF$CFactor
    Table['CFactor CV'] =  CF$C.SE/CF$CFactor
    TotTable = rbind(TotTable,Table)
  }
  return(TotTable)  
}
LocalSPF      = function(Scope,Type,Years,CrashAssign,SPFType,CF){
  IntTypes = c('R3ST','R4ST','R4SG','RM3ST','RM4ST','RM4SG','U3ST','U4ST','U3SG','U4SG')
  SegTypes = c('R2U','R4D','R4U','U2U','U3T','U4U','U4D','U5T','R4F','U4F','U6F')
  AllInt   = c('AllInt','AllIntSig','AllIntStp')
  AllSeg   = c('AllSeg')
  InSPFType = SPFType
  
  #Read Data --> D1
  D1 = Text2Dat(Scope,Type,Years,CrashAssign,ListHeaders(Type))
  
  #Apply Volume Ranges --> D2
  VolRange = FilterData(Scope,Type,Years,'Volumes')
  D2 = D1
  if(Type %in% c(IntTypes,AllInt)){
    D2     = D1[ (D1$AADT_Major<log(VolRange[1]) & D1$AADT_Minor<log(VolRange[2])),]
    D_Out1 = D1[!(D1$AADT_Major<log(VolRange[1])),]
    D_Out2 = D1[!(D1$AADT_Minor<log(VolRange[2])),]
    D_Out1['AADT_Major'] = exp(D_Out1['AADT_Major']);D_Out1['AADT_Minor'] = exp(D_Out1['AADT_Minor'])
    D_Out2['AADT_Major'] = exp(D_Out2['AADT_Major']);D_Out2['AADT_Minor'] = exp(D_Out2['AADT_Minor'])
  }
  if(Type %in% c(SegTypes,AllSeg)){
    D2     = D1[ (D1$AADT  <log(VolRange[1]) & D1$Length>log(VolRange[2])),]
    D_Out1 = D1[!(D1$AADT  <log(VolRange[1])),]
    D_Out2 = D1[!(D1$Length>log(VolRange[2])),]
    D_Out1['AADT'] = exp(D_Out1['AADT']);D_Out1['Length'] = exp(D_Out1['Length'])
    D_Out2['AADT'] = exp(D_Out2['AADT']);D_Out2['Length'] = exp(D_Out2['Length'])
  }
  #print(paste('Volume Ranges: ',length(D1[,1])-length(D2[,1]),' Deleted'))
  #print((unique(D_Out1[c('FID','AADT_Major')])))
  #print((unique(D_Out2[c('FID','AADT_Minor')])))
  #Apply Outliers --> D3
  Os = FilterData(Scope,Type,Years,'Outliers')
  MD2   = GLMModels(D2 ,'Base',Type)
  D3    = D2
  if(sum(Os)>0){
    D_Out = Outliers(Scope,MD2 ,1,Type ,Os)
    #print(unique(D_Out[c('FID','Outlier','TOT_OC')]))
    D3    = D2[!(rownames(D2)  %in% rownames(D_Out)) ,]}
  
  #State SPF --> D4, MD4
  D4 = D3
  if(SPFType=='Base'){
    D4  = BaseData(D3)
    print(paste('Base Data: ',length(D3[,1])-length(D4[,1]),' Deleted'))
  }
  if(SPFType=='GAADT'){SPFType = 'Base'}
  MD4 = GLMModels(D4 ,SPFType,Type)
  HSMCoef = HSMValues(Type,CF$CFactor)
  
  HSMFittedb = rep(-1,length(D4[,1]))
  HSMFitted  = rep(-1,length(D3[,1]))
  
  if(Type %in% c(IntTypes,AllInt)){StateFittedb  = GLMPredict(MD4$M, D4, Type)}
  if(Type %in% c(SegTypes,AllSeg)){StateFittedb  = GLMPredict(MD4$M, D4, Type)}
  if('TOT_PC' %in% names(D4)){HSMFittedb = D4$TOT_PC*CF$CFactor}
  if(!('TOT_PC' %in% names(D4))){
    if(Type %in% c(IntTypes,AllInt)){HSMFittedb  = HSMPredict(MD4$M, D4,CF$CFactor)}
    if(Type %in% c(SegTypes,AllSeg)){HSMFittedb  = HSMPredict(MD4$M, D4,CF$CFactor)}
  }
  
  
  if(Type %in% c(IntTypes,AllInt)){StateFitted  = GLMPredict(MD4$M, D3, Type)}
  if(Type %in% c(SegTypes,AllSeg)){StateFitted  = GLMPredict(MD4$M, D3, Type)}
  if('CCMF' %in% names(D3) & InSPFType == 'Base'){StateFitted = StateFitted * D3$CCMF}
  if('TOT_PC' %in% names(D3)){HSMFitted = D3$TOT_PC*CF$CFactor}
  if(!('TOT_PC' %in% names(D3))){
    if(Type %in% c(IntTypes,AllInt)){HSMFitted  = HSMPredict(MD4, D3,CF$CFactor)}
    if(Type %in% c(SegTypes,AllSeg)){HSMFitted  = HSMPredict(MD4, D3,CF$CFactor)}
  }
  
  MD4$Data['StateFitted'] = StateFittedb
  MD4$Data['HSMFitted'  ] = HSMFittedb
  AllData = D3
  AllData['StateFitted'] = StateFitted
  AllData['HSMFitted'  ] = HSMFitted
  AllData['StudRes'    ] = studres(MD4$M)
  M = paste(Type,'M',sep='_')
  D = paste(Type,'D',sep='_')
  A = paste(Type,'A',sep='_')
  MD4 = list(M=MD4$M,D=MD4$D,A=AllData)
  names(MD4) = c(M,D,A)
  
  #Plots
  PlotXX(Scope,D2,D1,VolRange,HSMCoef,Type,F)
  #PlotCF(D3,Type,Years)
  #PlotCFCompare(MD3,Type)
  Plot2D(Scope,Type,MD4 ,FALSE,Type ,data.matrix(HSMCoef[Type,]),TRUE)
  PlotAllHist(Scope,MD4 ,CF,Type,TRUE)
  CUREPlot(Scope,MD4,CF,Type)
  
  #Output
  return(MD4) 
}
SPFTable      = function(MDList,SPFType,ConfLevel = 0.95){
  ExportSPF = function(InTable){
    AllInt = c('R3ST'  ,'R4ST'  ,'R4SG'  ,'RM3ST'  ,'RM4ST'  ,'RM4SG'  ,'U3ST'  ,'U4ST'  ,'U3SG'  ,'U4SG'  )
    AllSeg = c('R2U','R4D','R4U','U2U','U3T','U4U','U4D','U5T')
    AllP = NULL;for(Type in c(AllInt,AllSeg)){AllP = c(AllP,ListPredictors(Type))};AllP = c('X.Intercept.',unique(AllP))
    Type = NULL
    Coef = NULL
    SampleSize   = NULL
    Es   = NULL
    SE   = NULL
    Pv   = NULL
    LB   = NULL
    UB   = NULL
    CV   = NULL
    for(r in rownames(InTable)){
      ss = InTable[r,'Sample.Size']
      CL = NULL
      for(n in names(InTable)){
        if(!(n=='Sample.Size')){
          for(p in AllP){
            if(n %in% c(paste(p,'.Es',sep=''),paste(p,'.SE',sep=''),paste(p,'.Pv',sep=''),paste(p,'.UB',sep=''),paste(p,'.LB',sep=''),paste(p,'.CV',sep=''))){
              if(!(p %in% CL)){
                Type = c(Type,r)
                SampleSize = c(SampleSize,ss)
                if(p=="X.Intercept."){Coef=c(Coef,"Intercept")}
                else{Coef=c(Coef,p)}
                CL = c(CL,p)
                cf = p
              }
            }
          }
          Flag = FALSE
          if(paste(cf,'.Es',sep='')==n){Es = c(Es,InTable[r,n]);Flag = TRUE}
          if(paste(cf,'.SE',sep='')==n){SE = c(SE,InTable[r,n]);Flag = TRUE}
          if(paste(cf,'.Pv',sep='')==n){Pv = c(Pv,InTable[r,n]);Flag = TRUE}
          if(paste(cf,'.LB',sep='')==n){LB = c(LB,InTable[r,n]);Flag = TRUE}
          if(paste(cf,'.UB',sep='')==n){UB = c(UB,InTable[r,n]);Flag = TRUE}
          if(paste(cf,'.CV',sep='')==n){CV = c(CV,InTable[r,n]);Flag = TRUE}
          if(!Flag){
            a=1
          }
        }
      }
    }
    D = data.frame(Type,SampleSize,Coef,Es,Pv,CV,SE,LB,UB)
    Out = NULL
    for(i in 1:length(D[,1])){
      if(sum(D[i,4:9])==0){
        Out = rbind(Out,D[i,])
      }
    }
    D = D[!(rownames(D)  %in% rownames(Out)) ,]
    return(D)  
  }
  TAll = data.frame()
  for(i in 1:(length(MDList)/3)){
    M = MDList[3*i-2][[1]]
    D = MDList[3*i-1][[1]]
    TAll = rbind(TAll,CoefSummary(list('Model'=M,'Data'=D)))
  }
  Out = ExportSPF(TAll)
  rownames(Out) = paste(rownames(Out),SPFType,sep='.')
  return(Out)
}  
SPFEvaluate   = function(MDList,SPFType){
  Types      = NULL
  SampleSize = NULL
  DegFreedom = NULL
  NumCoef    = NULL
  AIC = NULL
  BIC = NULL
  MAD = NULL
  MSPSE = NULL
  SSE  = NULL
  SSR = NULL
  SST = NULL
  Rsq = NULL
  AdRsq = NULL
  VarOC = NULL
  
  for(n in names(MDList)){
    Type = substr(n,1,nchar(n)-2)
    md = substr(n,nchar(n),nchar(n))
    if(md=='M'){
      M = MDList[paste(Type,'_M',sep='')][[1]]
      D = MDList[paste(Type,'_A',sep='')][[1]]
      Types = c(Types,Type)
      s = length(D[,1])
      p = length(M$coef)
      SampleSize = c(SampleSize,s)
      DegFreedom = c(DegFreedom,df.residual(M))
      NumCoef = c(NumCoef,p)
      AIC = c(AIC,extractAIC(M)[2])
      BIC = c(BIC,BIC(M))
      MAD = c(MAD,sum(abs(D$StateFitted-D$TOT_OC))/length(D[,1]))
      MSPSE = c(MSPSE,sum(abs(D$StateFitted-D$TOT_OC)^2)/length(D[,1]))
      m = mean(D$TOT_OC)
      ssr = sum((D$StateFitted-m)^2)
      sst = sum((D$TOT_OC-m)^2)
      SST = c(SST,sst)
      SSR = c(SSR,ssr)
      SSE = c(SSE,sum((D$StateFitted-D$TOT_OC)^2))
      Rsq = c(Rsq,ssr/sst)
      AdRsq = c(AdRsq,1-(1-ssr/sst)*((s-1)/(s-p-1)))
      VarOC = c(VarOC,var(D$TOT_OC))
    }
  }
  D = data.frame(SampleSize,DegFreedom,NumCoef,AIC,BIC,MAD,MSPSE,SST,SSR,SSE,Rsq,AdRsq,VarOC)
  rownames(D) = paste(Types,SPFType,sep='_')
  return(D)  
}

# Safety Project: -------------------------------------------------------------------------------------------
TreeStat      = function(Years){
  EventLookup = function(Loc,Unit,Event){
    SOEExtract = function(SOE,ANO){
      DF = data.frame('SOE' = SOE,'ANO' = ANO)
      DF['Len'] = nchar(paste(DF$SOE))
      DF1 = DF[which(DF$Len == 1),]
      n = length(DF1[,1])
      DF1['SOE1'] = as.integer(DF1$SOE)
      DF1['SOE2'] = rep(0,n)
      DF1['SOE3'] = rep(0,n)
      DF1['SOE4'] = rep(0,n)
      DF2 = DF[which(DF$Len == 2),]
      n = length(DF2[,1])
      DF2['SOE1'] = as.integer(DF2$SOE)
      DF2['SOE2'] = rep(0,n)
      DF2['SOE3'] = rep(0,n)
      DF2['SOE4'] = rep(0,n)
      DF3 = DF[which(DF$Len == 3),]
      n = length(DF3[,1])
      DF3['SOE1'] = as.integer(substr(DF3$SOE,1,1))
      DF3['SOE2'] = as.integer(substr(DF3$SOE,2,3))
      DF3['SOE3'] = rep(0,n)
      DF3['SOE4'] = rep(0,n)
      DF4 = DF[which(DF$Len == 4),]
      n = length(DF4[,1])
      DF4['SOE1'] = as.integer(substr(DF4$SOE,1,2))
      DF4['SOE2'] = as.integer(substr(DF4$SOE,3,4))
      DF4['SOE3'] = rep(0,n)
      DF4['SOE4'] = rep(0,n)
      DF5 = DF[which(DF$Len == 5),]
      n = length(DF5[,1])
      DF5['SOE1'] = as.integer(substr(DF5$SOE,1,1))
      DF5['SOE2'] = as.integer(substr(DF5$SOE,2,3))
      DF5['SOE3'] = as.integer(substr(DF5$SOE,4,5))
      DF5['SOE4'] = rep(0,n)
      DF6 = DF[which(DF$Len == 6),]
      n = length(DF6[,1])
      DF6['SOE1'] = as.integer(substr(DF6$SOE,1,2))
      DF6['SOE2'] = as.integer(substr(DF6$SOE,3,4))
      DF6['SOE3'] = as.integer(substr(DF6$SOE,5,6))
      DF6['SOE4'] = rep(0,n)
      DF7 = DF[which(DF$Len == 7),]
      n = length(DF7[,1])
      DF7['SOE1'] = as.integer(substr(DF7$SOE,1,1))
      DF7['SOE2'] = as.integer(substr(DF7$SOE,2,3))
      DF7['SOE3'] = as.integer(substr(DF7$SOE,4,5))
      DF7['SOE4'] = as.integer(substr(DF7$SOE,6,7))
      DF8 = DF[which(DF$Len == 8),]
      n = length(DF8[,1])
      DF8['SOE1'] = as.integer(substr(DF8$SOE,1,2))
      DF8['SOE2'] = as.integer(substr(DF8$SOE,3,4))
      DF8['SOE3'] = as.integer(substr(DF8$SOE,5,6))
      DF8['SOE4'] = as.integer(substr(DF8$SOE,7,8))
      DF = rbind(DF1,DF2,DF3,DF4,DF5,DF6,DF7,DF8)
      # 
      #     P = txtProgressBar(style=3,min=1,max=length(SOE))
      #     i = 0
      #     Out = NULL
      #     for(soe in SOE){
      #       i = i + 1
      #       soe1 = 0;soe2 = 0;soe3 = 0; soe4 = 0
      #       if(nchar(soe)<=2){soe1 = soe}
      #       if(nchar(soe)==3){soe1 = substr(soe,1,1);soe2 = substr(soe,2,3)}
      #       if(nchar(soe)==4){soe1 = substr(soe,1,2);soe2 = substr(soe,3,4)}
      #       if(nchar(soe)==5){soe1 = substr(soe,1,1);soe2 = substr(soe,2,3);soe3 = substr(soe,4,5)}
      #       if(nchar(soe)==6){soe1 = substr(soe,1,2);soe2 = substr(soe,3,4);soe3 = substr(soe,5,6)}
      #       if(nchar(soe)==7){soe1 = substr(soe,1,1);soe2 = substr(soe,2,3);soe3 = substr(soe,4,5);soe4 = substr(soe,6,7)}
      #       if(nchar(soe)==8){soe1 = substr(soe,1,2);soe2 = substr(soe,3,4);soe3 = substr(soe,5,6);soe4 = substr(soe,7,8)}
      #       Out = rbind(Out,data.frame('SOE1'=strtoi(soe1),'SOE2'=strtoi(soe2),'SOE3'=strtoi(soe3),'SOE4'=strtoi(soe4)))
      #       setTxtProgressBar(P,i)
      #    }
      #    rownames(Out) = ANO
      return(DF)
    }
    MHE = Unit$MHE
    names(MHE) = Unit$ANO
    MHE = MHE[MHE == Event]
    SelANO = as.integer(names(MHE))
    SOE = SOEExtract(Unit$SOE,Unit$ANO)  
    SOE = SOE[which(SOE$SOE1 == Event | SOE$SOE2 == Event | SOE$SOE3 == Event | SOE$SOE4 == Event),]
    SelANO = unique(c(SelANO,SOE$ANO))
    FHE = Loc$FHE
    names(FHE) = Loc$ANO
    FHE = FHE[FHE == Event]
    SelANO = unique(c(SelANO,as.integer(names(FHE))))
    Loc['ERelated'] = (Loc$ANO %in% SelANO)*1
    return(Loc)
  }
  Out = NULL
  for(Year in Years){
    L = read.csv(paste("Inputs/SC_SCrash_",Year,"_Loc.csv",sep=''),header=T)
    U = read.csv(paste("Inputs/SC_SCrash_",Year,"_Unit.csv",sep=''),header=T)
    L = EventLookup(L,U,60)

    LO = L
    Total     = length(L[,1])
    Tot.PDO   = length(L[which(L$FAT==0 & L$INJ==0),1])
    Tot.Inj  = length(L[which(L$FAT==0 & L$INJ>0),1])
    Tot.Fatal = length(L[which(L$FAT>0),1])

    L = LO[which(LO['ERelated']==1),]
    Tree       = length(L[,1])
    Tree.PDO   = length(L[which(L$FAT==0 & L$INJ==0),1])
    Tree.Inj  = length(L[which(L$FAT==0 & L$INJ>0),1])
    Tree.Fatal = length(L[which(L$FAT>0),1])

    L = LO[which(LO['ERelated']==1 & LO['RCT']==1),]
    RCT1       = length(L[,1])
    RCT1.PDO   = length(L[which(L$FAT==0 & L$INJ==0),1])
    RCT1.Inj   = length(L[which(L$FAT==0 & L$INJ>0),1])
    RCT1.Fatal = length(L[which(L$FAT>0),1])
    
    L = LO[which(LO['ERelated']==1 & LO['RCT']==2),]
    RCT2       = length(L[,1])
    RCT2.PDO   = length(L[which(L$FAT==0 & L$INJ==0),1])
    RCT2.Inj   = length(L[which(L$FAT==0 & L$INJ>0),1])
    RCT2.Fatal = length(L[which(L$FAT>0),1])

    L = LO[which(LO['ERelated']==1 & LO['RCT']==3),]
    RCT3       = length(L[,1])
    RCT3.PDO   = length(L[which(L$FAT==0 & L$INJ==0),1])
    RCT3.Inj   = length(L[which(L$FAT==0 & L$INJ>0),1])
    RCT3.Fatal = length(L[which(L$FAT>0),1])

    L = LO[which(LO['ERelated']==1 & LO['RCT']==4),]
    RCT4       = length(L[,1])
    RCT4.PDO   = length(L[which(L$FAT==0 & L$INJ==0),1])
    RCT4.Inj   = length(L[which(L$FAT==0 & L$INJ>0),1])
    RCT4.Fatal = length(L[which(L$FAT>0),1])

    L = LO[which(LO['ERelated']==1 & LO['RCT']==5),]
    RCT5       = length(L[,1])
    RCT5.PDO   = length(L[which(L$FAT==0 & L$INJ==0),1])
    RCT5.Inj   = length(L[which(L$FAT==0 & L$INJ>0),1])
    RCT5.Fatal = length(L[which(L$FAT>0),1])
    
    D = data.frame(Total,Tot.PDO,Tot.Inj,Tot.Fatal,
                   Tree,Tree.PDO,Tree.Inj,Tree.Fatal,
                   RCT1,RCT1.PDO,RCT1.Inj,RCT1.Fatal,
                   RCT2,RCT2.PDO,RCT2.Inj,RCT2.Fatal,
                   RCT3,RCT3.PDO,RCT3.Inj,RCT3.Fatal,
                   RCT4,RCT4.PDO,RCT4.Inj,RCT4.Fatal,
                   RCT5,RCT5.PDO,RCT5.Inj,RCT5.Fatal)
    rownames(D) = Year
    Out = rbind(Out,D)
  }
  return(Out)
}
PedSev = function(Years){
  Out = NULL
  for(Year in Years){
    L = read.csv(paste("Inputs/SC_SCrash_",Year,"_Loc.csv",sep=''),header=T)
    if(Year == 2013){L = L[which(!is.na(L$RCT)),]}
    
    Total       = length(L[,1])
    Total.PDO   = length(L[which(L$FAT==0 & L$INJ==0),1])
    Total.Inj   = length(L[which(L$FAT==0 & L$INJ>0),1])
    Total.Fatal = length(L[which(L$FAT>0),1])
    
    L = read.csv(paste("Inputs/SC_Crash_",Year,"_PedSev.csv",sep=''),header=T)
    LO = L
    Ped         = length(L[,1])
    Ped.NoInj   = length(L[which(L$PedSev==0),1])
    Ped.PossInj = length(L[which(L$PedSev==1),1])
    Ped.NIncInj = length(L[which(L$PedSev==2),1])
    Ped.IncInj  = length(L[which(L$PedSev==3),1])
    Ped.Fatal   = length(L[which(L$PedSev==4),1])
    
    
    L = LO[which(LO['RCT']==1),]
    RCT1         = length(L[,1])
    RCT1.NoInj   = length(L[which(L$PedSev==0),1])
    RCT1.PossInj = length(L[which(L$PedSev==1),1])
    RCT1.NIncInj = length(L[which(L$PedSev==2),1])
    RCT1.IncInj  = length(L[which(L$PedSev==3),1])
    RCT1.Fatal   = length(L[which(L$PedSev==4),1])
    
    L = LO[which(LO['RCT']==2),]
    RCT2         = length(L[,1])
    RCT2.NoInj   = length(L[which(L$PedSev==0),1])
    RCT2.PossInj = length(L[which(L$PedSev==1),1])
    RCT2.NIncInj = length(L[which(L$PedSev==2),1])
    RCT2.IncInj  = length(L[which(L$PedSev==3),1])
    RCT2.Fatal   = length(L[which(L$PedSev==4),1])
    
    L = LO[which(LO['RCT']==3),]
    RCT3         = length(L[,1])
    RCT3.NoInj   = length(L[which(L$PedSev==0),1])
    RCT3.PossInj = length(L[which(L$PedSev==1),1])
    RCT3.NIncInj = length(L[which(L$PedSev==2),1])
    RCT3.IncInj  = length(L[which(L$PedSev==3),1])
    RCT3.Fatal   = length(L[which(L$PedSev==4),1])
    
    L = LO[which(LO['RCT']==4),]
    RCT4         = length(L[,1])
    RCT4.NoInj   = length(L[which(L$PedSev==0),1])
    RCT4.PossInj = length(L[which(L$PedSev==1),1])
    RCT4.NIncInj = length(L[which(L$PedSev==2),1])
    RCT4.IncInj  = length(L[which(L$PedSev==3),1])
    RCT4.Fatal   = length(L[which(L$PedSev==4),1])
    
    L = LO[which(LO['RCT']==5),]
    RCT5         = length(L[,1])
    RCT5.NoInj   = length(L[which(L$PedSev==0),1])
    RCT5.PossInj = length(L[which(L$PedSev==1),1])
    RCT5.NIncInj = length(L[which(L$PedSev==2),1])
    RCT5.IncInj  = length(L[which(L$PedSev==3),1])
    RCT5.Fatal   = length(L[which(L$PedSev==4),1])
    
    D = data.frame(Total,Total.PDO,Total.Inj,Total.Fatal,
                   Ped , Ped.NoInj , Ped.PossInj , Ped.NIncInj , Ped.IncInj , Ped.Fatal,
                   RCT1, RCT1.NoInj, RCT1.PossInj, RCT1.NIncInj, RCT1.IncInj, RCT1.Fatal,
                   RCT2, RCT2.NoInj, RCT2.PossInj, RCT2.NIncInj, RCT2.IncInj, RCT2.Fatal,
                   RCT3, RCT3.NoInj, RCT3.PossInj, RCT3.NIncInj, RCT3.IncInj, RCT3.Fatal,
                   RCT4, RCT4.NoInj, RCT4.PossInj, RCT4.NIncInj, RCT4.IncInj, RCT4.Fatal,
                   RCT5, RCT5.NoInj, RCT5.PossInj, RCT5.NIncInj, RCT5.IncInj, RCT5.Fatal)
    rownames(D) = Year
    Out = rbind(Out,D)
  }
  return(Out)
}
Moped = function(Years){
  Out = NULL
  for(Year in Years){

    U = read.csv(paste("Inputs/SC_SCrash_",Year,"_Unit.csv",sep=''),header=T)
    U = U[which(U$UTC==26),]
    UTC = U$UTC
    AUN = as.integer(U$AUN)
    names(UTC) = U$ANO
    SelANO = as.integer(names(UTC))
    O = read.csv(paste("Inputs/SC_SCrash_",Year,"_Occ.csv",sep=''),header=T)
    O = O[which(O$ANO %in% SelANO),]
    L = read.csv(paste("Inputs/SC_SCrash_",Year,"_Loc.csv",sep=''),header=T)
    if(Year == 2013){L = L[which(!is.na(L$RCT)),]}
    L = L[which(L$ANO %in% SelANO),]
    SEV = NULL
    DLC = NULL
    for(ano in L$ANO){
      u = U[which(U$ANO==ano),]
      aun = u$AUN
      dlc = 1
      if('' %in% u$DLC){dlc = 0}
      DLC = c(DLC,dlc)
      o = O[which(O$ANO %in% ano & O$AUN %in% aun),]
      SEV = c(SEV,max(as.integer(o$SEV),0))
    }
    L['SEV'] = SEV
    L['DLC'] = DLC
    LO = L
    Mop         = length(L[,1])
    Mop.NoInj   = length(L[which(L$SEV==0),1])
    Mop.PossInj = length(L[which(L$SEV==1),1])
    Mop.NIncInj = length(L[which(L$SEV==2),1])
    Mop.IncInj  = length(L[which(L$SEV==3),1])
    Mop.Fatal   = length(L[which(L$SEV==4),1])
    Mop.Fatalities = sum(L$FAT)
    
    L = LO[which(LO$PRC==16),]
    DUI         = length(L[,1])
    DUI.NoInj   = length(L[which(L$SEV==0),1])
    DUI.PossInj = length(L[which(L$SEV==1),1])
    DUI.NIncInj = length(L[which(L$SEV==2),1])
    DUI.IncInj  = length(L[which(L$SEV==3),1])
    DUI.Fatal   = length(L[which(L$SEV==4),1])
    DUI.Fatalities = sum(L$FAT)
    
    L = LO[which(LO$DLC==0),]
    Lic         = length(L[,1])
    Lic.NoInj   = length(L[which(L$SEV==0),1])
    Lic.PossInj = length(L[which(L$SEV==1),1])
    Lic.NIncInj = length(L[which(L$SEV==2),1])
    Lic.IncInj  = length(L[which(L$SEV==3),1])
    Lic.Fatal   = length(L[which(L$SEV==4),1])
    Lic.Fatalities = sum(L$FAT)
    
    D = data.frame(Mop,Mop.NoInj,Mop.PossInj,Mop.NIncInj,Mop.IncInj,Mop.Fatal,Mop.Fatalities,
                   DUI,DUI.NoInj,DUI.PossInj,DUI.NIncInj,DUI.IncInj,DUI.Fatal,DUI.Fatalities,
                   Lic,Lic.NoInj,Lic.PossInj,Lic.NIncInj,Lic.IncInj,Lic.Fatal,Lic.Fatalities)
    rownames(D) = Year
    Out = rbind(Out,D)
  }
  return(Out)
}
PedNight = function(Years){
  Out = NULL
  ALCOut  = NULL
  RACPOut = NULL
  RACDOut = NULL
  FDAVOut = NULL
  FDAPOut = NULL
  APIPOut = NULL
  APIVOut = NULL
  ANOOut  = NULL
  REPOut  = NULL
  REUOut  = NULL
  for(Year in Years){
    O = read.csv(paste("Inputs/SC_SCrash_",Year,"_Occ.csv",sep=''),header=T)
    if(is.factor(O$ANO )){O$ANO = as.numeric(levels(O$ANO))[O$ANO]}
    if(is.factor(O$OSL )){O$OSL = as.numeric(levels(O$OSL))[O$OSL]}
    if(is.factor(O$SEV )){O$SEV = as.numeric(levels(O$SEV))[O$SEV]}
    if(is.factor(O$REU )){O$REU = as.numeric(levels(O$REU))[O$REU]}
    
    L = read.csv(paste("Inputs/SC_SCrash_",Year,"_Loc.csv",sep=''),header=T)
    if(Year == 2013){L = L[which(!is.na(L$RCT)),]}
    if(is.factor(L$ANO)){L$ANO = as.numeric(levels(L$ANO))[L$ANO]}
    if(is.factor(L$REPORT)){L$REPORT = lapply(L$REPORT,as.character)}
    if(is.factor(L$TIM)){L$TIM = as.numeric(levels(L$TIM))[L$TIM]}
    if(is.factor(L$FAT)){L$FAT = as.numeric(levels(L$FAT))[L$FAT]}
    if(is.factor(L$PRC)){L$PRC = as.numeric(levels(L$PRC))[L$PRC]}
    if(is.factor(L$FHE)){L$FHE = as.numeric(levels(L$FHE))[L$FHE]}
    if(is.factor(L$ALC)){L$ALC = as.numeric(levels(L$ALC))[L$ALC]}
    
    U = read.csv(paste("Inputs/SC_SCrash_",Year,"_Unit.csv",sep=''),header=T)
    if(is.factor(U$ANO)){U$ANO = as.numeric(levels(U$ANO))[U$ANO]}
    if(is.factor(U$UTC)){U$UTC = as.numeric(levels(U$UTC))[UL$UTC]}
    if(is.factor(U$FDA)){U$FDA = as.numeric(levels(U$FDA))[U$FDA]}
    if(is.factor(U$API)){U$API = as.numeric(levels(U$API))[U$API]}

    # All Fatal Peds
    OP = O[which(O$OSL == 20 & O$SEV == 4),]
    SelANO = unique(OP$ANO)
    
    #Divide All Fatal Peds to day and night
    Tot.Fat.Night = sum(L[which(L$TIM< 600 | L$TIM> 1800),'FAT'])
    Tot.Fat.Day   = sum(L[which(L$TIM>=600 & L$TIM<=1800),'FAT'])

    LNight = L[which(L$ANO %in% SelANO & (L$TIM< 600 | L$TIM> 1800)),]
    LDay   = L[which(L$ANO %in% SelANO & (L$TIM>=600 & L$TIM<=1800)),]
    
    SelANOD = LDay$ANO
    SelANON = LNight$ANO
    
    Ped.Fat.Night = length(OP[which(OP$ANO %in% SelANON),1])
    Ped.Fat.Day   = length(OP[which(OP$ANO %in% SelANOD),1])
    
    # Distributions of night fatal peds
    UNightP = U[which(U$ANO %in% SelANON & U$UTC==41),]
    UNightV = U[which(U$ANO %in% SelANON & U$UTC!=41),]
    ONightP = O[which(O$ANO %in% SelANON & O$OSL==20),]
    ONightD = O[which(O$ANO %in% SelANON & O$OSL==1),]
    
    REU  = ONightP$REU
    ALC  = LNight$ALC
    RACP = as.character(ONightP$ORAC)
    RACD = as.character(ONightD$ORAC)
    FDAV = UNightV$FDA
    FDAP = UNightP$FDA
    APIP = UNightP$API
    APIV = UNightV$API
    Rep  = LNight$REPORT
    
    D = data.frame(Tot.Fat.Day,Tot.Fat.Night , Ped.Fat.Day,Ped.Fat.Night)
    rownames(D) = Year
    Out = rbind(Out,D)

    REUOut  = c(REUOut,REU)
    ANOOut  = c(ANOOut,SelANON)
    ALCOut  = c(ALCOut,ALC)
    RACPOut = c(RACPOut,RACP)
    RACDOut = c(RACDOut,RACD)
    FDAVOut = c(FDAVOut,FDAV)
    FDAPOut = c(FDAPOut,FDAP)
    APIPOut = c(APIPOut,APIP)
    APIVOut = c(APIVOut,APIV)
    REPOut  = c(REPOut,Rep)
  }
  return(list('Sum'=Out,
              'ALC'=ALCOut,
              'RACP'=RACPOut,'RACD'=RACDOut,
              'FDAV'=FDAVOut,'FDAP'=FDAPOut,
              'APIP'=APIPOut,'APIV'=APIVOut,
              'ANO'=ANOOut,
              'Rep'=REPOut,
              'REU'=REUOut))
}

# Paper 1: -------------------------------------------------------------------------------------------
PlotCFSECompare = function(Scope,D){
  PlotCFCompare1 = function(D){
    ConfLevel = 0.95
    yr = NULL
    SE = data.frame('Boot' = D[,'se CF'])
    SE['EQ10'] = D[,'se Mean']
    SE['EQB6'] = D[,'se EQ6']
    SE['EQB8'] = D[,'se EQ8']
    rownames(SE) = rownames(D)
    for(Type in rownames(D)){
      SampleSize = D[Type,'Sample.Size']
      CF         = D[Type,'CF']
      t = qt((1+ConfLevel)/2,SampleSize)
      for(j in names(SE)){
        yr = range(yr,CF,CF+t*SE[Type,j],CF-t*SE[Type,j])
      }
    }
    yr[2] = yr[2] * 1.25
    xr   = c(0,length(D[,1]+1))
    
    layout(1)
    plot.new()
    layout(matrix(c(1,1),2,1),heights=c(7,2))
    Color1 = c('blue','red','green4','black')
    pch1 = c(24,10,13,12)
    pch2 = c(25,10,13,12)
    for(i in 1:length(D[,1])){
      SampleSize = D[i,'Sample.Size']
      CF         = D[i,'CF']
      t = qt((1+ConfLevel)/2,SampleSize)
      for (j in 1:length(SE[1,])){
        xof = -0.3+0.15*(j-1);LB = CF-t*SE[i,j];UB= CF+t*SE[i,j]
        par(new=TRUE);plot(x=i+xof,y=CF,ylim=yr,xlim=xr,xaxt='n',xlab='',ylab='',col=Color1[j],pch=19)
        segments(i+xof,UB,i+xof,LB,col=Color1[j])
        par(new=TRUE);plot(x=i+xof,y=LB,ylim=yr,xlim=xr,pch=pch1[j],xaxt='n',yaxt='n',xlab='',ylab='',col=Color1[j])
        par(new=TRUE);plot(x=i+xof,y=UB,ylim=yr,xlim=xr,pch=pch2[j],xaxt='n',yaxt='n',xlab='',ylab='',col=Color1[j])
      }
    }
    l=c('SD Estimates by Bootstrap','SD Estimates by EQ 10','SD Estimates by (Bahar 2014) EQ B6','SD Estimates by (Bahar 2014) EQ B8')
    legend(legend = l,horiz=FALSE,lty=1,pch=pch1,'topright',text.col = Color1)
    axis(1,1:length(D[,1]),rownames(D),line=0,cex=.8,las=2)
    
    
    grid(nx=0,ny=NULL)
    title(c(paste('Calibration Factors ', ConfLevel * 100,'% CI',sep='')),outer=TRUE,line=-1.8)
    layout(1)
  }
  SaveImage(Scope,paste('SE Compare'),'png')
  PlotCFCompare1(D)
  dev.off()
  PlotCFCompare1(D)
}
PlotBootHist = function(Scope,Type,Years,CrashAssign,NumBoot){
  PlotBootHist1 = function(Scope,Type,Years,CrashAssign){
    a=Text2Dat(Scope,Type,Years,CrashAssign,c('TOT_OC','TOT_PC'))
    b=BootFun(a,NumBoot)
    hist(b,xlab='Calibration Factor',main='')
    abline(v=mean(b),col='red')
    CF = sum(a$TOT_OC)/sum(a$TOT_PC)
    k = HSMValues(Type)$k
    se2 = sqrt(var(a$TOT_OC)/length(a[,1]))/mean(a$TOT_PC)
    se3 = sqrt(sum(a$TOT_OC+k*a$TOT_OC^2))/sum(a$TOT_PC)
    se4 = sqrt(sum(a$TOT_OC+k*a$TOT_OC^2))/sum(a$TOT_OC)*CF
    se5 = sqrt(sum(CF*a$TOT_PC+k*CF^2*a$TOT_PC^2))/sum(a$TOT_PC)
    print(paste('TOT_OC:',sum(a$TOT_OC)))
    print(paste('TOT_PC:',sum(a$TOT_PC)))
    print(paste('SE 2:',se2))
    print(paste('SE EQ6:',se3))
    print(paste('SE EQ7:',se4))
    print(paste('SE EQ8:',se5))
    legend(legend=c(
      #paste('HSM Calibration Factor =',format(CF,digits = 4)),
      paste('Original Sample Size =',length(a$TOT_OC)),
      paste('Calibration Factor =',format(sum(a$TOT_OC)/sum(a$TOT_PC),digits=4)),
      paste('Number of Bootstrap Samples =',NumBoot),
      paste('Mean =',format(mean(b),digits=4)),
      paste('Standard Deviation =',format(sqrt(var(b)),digits=4))
      #paste('Bahar(2014) Eq. B8 S.E. =',format(se3,digits = 4))
    ),'topright')
  }
  SaveImage(Scope,paste(Type,'Boot Hist'),'png')
  PlotBootHist1(Scope,Type,Years,CrashAssign)
  dev.off()
  PlotBootHist1(Scope,Type,Years,CrashAssign)
}
PlotCFReg = function(CFEval,LogScale = FALSE){
  Divisions = 3
  Res = 100
  ColM = c('red','blue','green')
  OCr = range(CFEval$'cv OC')
  cvr = range(CFEval$'cv CF')
  X1 = seq(OCr[1],OCr[2],length.out=Res)
  X2 = seq(cvr[1],cvr[2],length.out=Divisions+1)
  #CFM = lm(log(CFEval$Sample.Size)~log(CFEval$'cv OC') + log(CFEval$'cv CF'))
  CFM = glm(CFEval$'Sample.Size' ~ log(CFEval$'cv OC') + log(CFEval$'cv CF'), family= negative.binomial(theta = 1000,link= "log"))
  print(summary(CFM))
  Coef = CFM$coef
  p = length(Coef)
  plot.new()
  xr  = range((X1))
  yr  = range((CFEval$'Sample.Size'))
  xrl = range(log(X1))
  yrl = range(log(CFEval$'Sample.Size'))
  l = NULL
  for(i in 1:Divisions){
    DF = matrix(nrow=p,ncol=Res,c(rep(1,Res),log(X1),rep(log(mean(X2[i:i+1])),Res)),byrow= T)
    Y = exp(Coef%*%DF)
    D = CFEval[which(CFEval$'cv CF'>=X2[i] & CFEval$'cv CF'<X2[i+1]),]
    par(new=T)
    if( LogScale){plot(log(D$'cv OC'),log(D$'Sample.Size'),col = ColM[i],xlim = rev(xrl), ylim = yrl,ylab = 'Sample Size (Log scale)',xlab = 'CV of Observed Crashes (Log scale)')}
    if(!LogScale){plot(   (D$'cv OC'),   (D$'Sample.Size'),col = ColM[i],xlim = rev(xr) , ylim = yr ,ylab = 'Sample Size',xlab = 'CV of Observed Crashes')}
    if( LogScale){lines(log(X1),log(Y),col = ColM[i])}
    if(!LogScale){lines(   (X1),   (Y),col = ColM[i])}
    l = c(l,paste('CV of CF =',round((X2[i]+X2[i+1])/2*1000)/1000))
  }
  legend(legend = l,'topleft',text.col=ColM)
  legend(legend = c(paste('Sample Size:',length(CFEval[,1])),
                    paste('Intercept:',format(Coef[1],digits=4)),
                    paste('Coef of log(CV of Observed Crash):',format(Coef[2],digits=4)),
                    paste('Coef of log(CV of CF):',format(Coef[3],digits=4)))
         ,'topright')
  title('Required Sample Size Estimation')
}
Plot3DSSR = function(M,D,LogScale = FALSE){
  GridSize = 30
  Texture = 'line'
  if(LogScale){
    plot3d(x=log(D[,2]), y=log(D[,3]), z=log(D[,1]), col = 'red',xlab=paste(names(D)[2], '(Log Scale)'),ylab=paste(names(D)[3],'(Log Scale)'),zlab=paste(names(D)[1],'(Log Scale)'))
    xr = rev(range(log(D[,2])))
    yr = rev(range(log(D[,3])))
    x  = seq(xr[1],xr[2],by=(xr[2]-xr[1])/GridSize)
    y  = seq(yr[1],yr[2],by=(yr[2]-yr[1])/GridSize)
    z  = matrix(0,length(x),length(y))
    for (i in 1:length(x)){
      for (j in 1:length(y)){
        N = data.frame(A = exp(x[i]), B = exp(y[j]))
        names(N) = names(D)[c(2,3)]
        z[i,j] = (predict.lm(M,N))
      }
    }
    surface3d(x=x,y=y,z=z,ambient='blue',front=Texture,back=Texture)
  }
  if(!LogScale){
    plot3d(x=D[,2], y=D[,3], z=D[,1], col = 'red',xlab=paste(names(D)[2]),ylab=paste(names(D)[3]),zlab=names(D)[1])
    xr = rev(range(D[,2]))
    yr = rev(range(D[,3]))
    x  = seq(xr[1],xr[2],by=(xr[2]-xr[1])/GridSize)
    y  = seq(yr[1],yr[2],by=(yr[2]-yr[1])/GridSize)
    z  = matrix(0,length(x),length(y))
    for (i in 1:length(x)){
      for (j in 1:length(y)){
        N = data.frame(A = (x[i]), B = (y[j]))
        names(N) = names(D)[c(2,3)]
        z[i,j] = exp(predict.lm(M,(N)))
      }
    }
    surface3d(x=x,y=y,z=z,col='blue',front=Texture,back=Texture)
  }
}
PlotBootHistOC = function(Scope,Type,Years,CrashAssign,NumBoot){
  PlotBootHistOC1 = function(Scope,Type,Years,CrashAssign,NumBoot){
    a=Text2Dat(Scope,Type,Years,CrashAssign,c('TOT_OC','TOT_PC'))
    n = length(a[,1])
    b = NULL
    for(i in 1:NumBoot){
      r = sample(1:n,replace=TRUE)
      o = a$TOT_OC[r]
      b[i] = mean(o)
    }
    hist(b,xlab='Mean of Observed Crahses',main='')
    abline(v=mean(b),col='red')
    CF = sum(a$TOT_OC)/sum(a$TOT_PC)
    k = HSMValues(Type)$k
    se2 = sqrt(var(a$TOT_OC)/length(a[,1]))/mean(a$TOT_PC)
    se3 = sqrt(sum(a$TOT_OC+k*a$TOT_OC^2))/sum(a$TOT_PC)
    se4 = sqrt(sum(a$TOT_OC+k*a$TOT_OC^2))/sum(a$TOT_OC)*CF
    se5 = sqrt(sum(CF*a$TOT_PC+k*CF^2*a$TOT_PC^2))/sum(a$TOT_PC)
    print(paste('TOT_OC:',sum(a$TOT_OC)))
    print(paste('TOT_PC:',sum(a$TOT_PC)))
    print(paste('SE 2:',se2))
    print(paste('SE EQ6:',se3))
    print(paste('SE EQ7:',se4))
    print(paste('SE EQ8:',se5))
    legend(legend=c(
      #paste('HSM Calibration Factor =',format(CF,digits = 4)),
      paste('Original Sample Size =',length(a$TOT_OC)),
      paste('Calibration Factor =',format(sum(a$TOT_OC)/sum(a$TOT_PC),digits=4)),
      paste('Number of Bootstrap Samples =',NumBoot),
      paste('Mean =',format(mean(b),digits=4)),
      paste('Standard Error =',format(sqrt(var(b)),digits=4))
      #paste('Bahar(2014) Eq. B8 S.E. =',format(se3,digits = 4))
    ),'topright')
  }
  SaveImage(Scope,paste(Type,'OC Boot Hist'),'png')
  PlotBootHistOC1(Scope,Type,Years,CrashAssign,NumBoot)
  dev.off()
  PlotBootHistOC1(Scope,Type,Years,CrashAssign,NumBoot)
}
PlotCVOC = function(Scope,Types,Years,CrashAssign){
    PlotCrashRate1 = function(Scope,Type,Years,CrashAssign){
      Col = c('blue','green4','red','black','green','purple','orange','blue','green4','red','black','green','purple','orange')
      PCH = c(1:10)
      plot.new()
      par = TRUE
      Grid = 20
      CVOC = NULL
      X = data.frame(Grid= rep(0,Grid))
      Y = data.frame(Grid= rep(0,Grid))
      j = 0
      for(Type in Types){
        j = j+1
        a = Text2Dat(Scope,Type,Years,CrashAssign,c('TOT_OC','TOT_PC'))
        n = length(a[,1])
        x = NULL
        y = NULL
        for(i in 1:Grid){
          s = sample(1:n,n/Grid*i)
          D = a[s,]
          x[i] = length(s)/n
          y[i] = var(D$TOT_OC)^0.5/mean(D$TOT_OC)
          if(mean(D$TOT_OC)==0){y[i]=0}
        }
        X[Type] = x
        Y[Type] = y
        CVOC[j] = var(a$TOT_OC)^0.5/mean(a$TOT_OC)
      }
      xr = range(X)
      yr = range(Y)
      yr[2]=yr[2]*1.5
      
      plot.new()
      for(i in 2:length(X)){
        par(new=TRUE);plot(X[,i],Y[,i],type='b',xlim = xr,ylim=yr,xlab='',ylab = '',xaxt='n',col=Col[i-1],pch=PCH[i-1])
        abline(h=Y[Grid,i],col=Col[i-1])
      }
      axis(1,at=(X[,2]),lab = paste(round(X[,2]*100),'%'),las=FALSE)
      title(xlab='Sample Size Portion',ylab = 'C.V. Observed Crashes')
      o = order(CVOC,decreasing = TRUE)
      l = paste(Types[o],'= ',round(CVOC[o]*100)/100) 
      legend('topright',title='C.V. Observed Crashes',title.col='black',legend=l,text.col = Col[o],lty=1,pch=PCH[o],col=Col[o])
    }  
  SaveImage(Scope,paste('CVRate'),'png')
  PlotCrashRate1(Scope,Type,Years,CrashAssign)
  dev.off()
  PlotCrashRate1(Scope,Type,Years,CrashAssign)
}
PlotOCRate = function(Scope,Types,Years,CrashAssign){
  PlotCrashRate1 = function(Scope,Type,Years,CrashAssign){
    Col = c('blue','green4','red','black','green','purple','orange','blue','green4','red','black','green','purple','orange')
    PCH = c(1:10)
    plot.new()
    par = TRUE
    Grid = 20
    CVOC = NULL
    X = data.frame(Grid= rep(0,Grid))
    Y = data.frame(Grid= rep(0,Grid))
    j = 0
    for(Type in Types){
      j = j+1
      a = Text2Dat(Scope,Type,Years,CrashAssign,c('TOT_OC','TOT_PC'))
      n = length(a[,1])
      x = NULL
      y = NULL
      for(i in 1:Grid){
        s = sample(1:n,n/Grid*i)
        D = a[s,]
        x[i] = length(s)/n
        y[i] = sum(D$TOT_OC)
      }
      X[Type] = x
      Y[Type] = y
      CVOC[j] = sum(a$TOT_OC)/n
    }
    xr = range(X)
    yr = range(Y)
    yr[2]=yr[2]*1.5
    
    plot.new()
    for(i in 2:length(X)){
      par(new=TRUE);plot(X[,i],Y[,i],type='b',xlim = xr,ylim=yr,xlab='',ylab = '',xaxt='n',col=Col[i-1],pch=PCH[i-1])
      abline(a=0,b=Y[Grid,i],col=Col[i-1])
    }
    axis(1,at=(X[,2]),lab = paste(round(X[,2]*100),'%'),las=FALSE)
    title(xlab='Sample Size Portion',ylab = 'Total Observed Crashes')
    o = order(CVOC)
    l = paste(Types[o],'= ',round(CVOC[o]*100)/100) 
    legend('topright',title='Tot. Obs. Crashes per Site',title.col='black',legend=l,text.col = Col[o],lty=1,pch=PCH[o],col=Col[o])
  }  
  SaveImage(Scope,paste('CrashRate'),'png')
  PlotCrashRate1(Scope,Type,Years,CrashAssign)
  dev.off()
  PlotCrashRate1(Scope,Type,Years,CrashAssign)
}
CFStatesCFBoxPlot = function(Input,Types){
  Plot1 = function(Input,Types){
    layout(1)
    layout(matrix(c(2,3,1,4),2,2),widths=c(1,7),heights=c(7,3))
    D  = Input[which(Input$CF>0 & Input$State != 'HSM.OneStateCF' & Input$FType %in% Types),]
    D1 = Input[which(Input$CF>0 & Input$State == 'HSM.OneStateCF' & Input$FType %in% Types),]
    Types = Types[Types %in% unique(D$FType)]
    n = length(Types)
    xr = c(0,n+1)
    yr = c(0.1,10)
    i = 0
    CF = NULL
    for(Type in c(AllSeg,AllInt)){
      if(Type %in% Types){
        i = i+1
        d = D[which(D$FType == Type),]
        CF[i] = mean(d$CF)
        d1 = D1[which(D1$FType == Type),]
        if(i!=1){par(new=T)}
        boxplot(at=i,d$CF,xlim=xr,ylim=yr,border ='blue',log='y')  
        par(new=T)
        plot(i,d1$CF,xlim=xr,ylim=yr,col ='red',pch=20,cex=2,xlab='',ylab='',xaxt='n',yaxt='n',log='y')
      }
    }
    abline(h=1,col='brown',lty=2)
    axis(1,1:n,Types,line=0,cex=.8,las=2)
    axis(1,1:n,Round(CF,3),line=5,cex=.8,las=1)
    axis(1,1:n,Round(D1$CF,3),line=9,cex=.8,las=1)
    mtext('States CFs Ave. :',1,col='blue',at=-.5,line=5)
    mtext('Initial CFs:',col='red',1,at=-.1,line=9)
    title(ylab = 'Calibration Factor')
    legend('topright',legend=c('State CFs','Initial CFs'),text.col = c('blue','red'))
    layout(1)
  }
  SaveImage('SCSEL',paste('CFStatesBox'),'png')
  Plot1(Input,Types)
  dev.off()
  Plot1(Input,Types)
}
CFStatesRateBoxPlot = function(D){
  plot.new()  
  AMj = NULL
  AMn = NULL
  Rate = NULL
  SampleSize = NULL
  Len = NULL
  
  AllSeg = c('R2U','R4D','R4U','U2U','U3T','U4U','U4D','U5T')
  AllInt = c('R3ST'  ,'R4ST'  ,'R4SG'  ,'RM3ST'  ,'RM4ST'  ,'RM4SG'  ,'U3ST'  ,'U4ST'  ,'U3SG'  ,'U4SG')
  Types = c(AllSeg,AllInt)
  Types = Types[Types %in% unique(D$FType)]
  n = length(Types)
  xr = c(0,n+1)
  yr = range(0:13)
  i = 0
  for(Type in c(AllSeg,AllInt)){
    if(Type %in% Types){
      i = i+1
      d = D[which(D$FType == Type),]
      d$Mileage = as.numeric(as.character(d$Mileage))
      d$AADT_Minor = as.numeric(as.character(d$AADT_Minor))

      if(Type %in% AllInt){
        R = (d$TOT_OC/d$Years)/((d$AADT_Major+d$AADT_Minor)*365*d$Sample.Size/10^6)
        C = data.frame(Rate=R,CF=d$CF)
        row.names(C)=d$State
        print(Type)
        print(C)
      }
      if(Type %in% AllSeg){
        R = (d$TOT_OC/d$Years)/((d$AADT_Major)*365*d$Mileage/10^6)
        C = data.frame(Rate=R,CF=d$CF)
        row.names(C)=d$State
        print(Type)
        print(C)
      }
      Rate[i] = median(R[which(R>0)])
      SampleSize[i] = length(R[which(R>0)])
      AMj[i] = mean(d$AADT_Major[which(d$AADT_Major>0)])
      AMn[i] = mean(d$AADT_Minor[which(d$AADT_Minor>0)])
      Len[i] = mean(d$Mileage[which((d$Mileage+d$Sample.Size)>0)]/d$Sample.Size[which((d$Mileage+d$Sample.Size)>0)])
      par(new=T)
      boxplot(at=i,R,xlim=xr,ylim=yr,outline = FALSE)  
    }
  }
  Out = data.frame(SampleSize,AMj,AMn,Len,Rate)
  row.names(Out) = Types
  axis(1,1:n,Types,line=0,cex=.8,las=2)
  title(ylab = 'Number of Observed crashes per site')
  return(Out)
}
PlotCVk = function(){
  PlotCVk1 = function(){
    xr = c(0,6)
    yr = c(0,3)
    kr = c(0,3)
    x=seq(.3,xr[2],.01)
    plot.new()
    #layout(matrix(nrow=2,ncol=2,data=c(2,1,2,1),byrow=T),widths = c(1,5),heights = c(1,1))
    Col= c('blue','red','green4','orange')
    l = ''
    for(k in kr[1]:kr[2]){
      if(k!=0){par(new=T)}
      plot(x,sqrt(1/x+k),'l',xlim=xr,ylim=yr,col=Col[k+1],xlab='',ylab='',yaxt='n')
      abline(h=sqrt(k),col=Col[k+1],lty=2)
    }
    abline(v=1,lty=2)
    axis(2,at=(yr[1]:yr[2]),lab = paste(round(yr[1]:yr[2]*100),'%'),las=TRUE)
    title(xlab='Mean')
    title(line=-1.5,ylab = 'Coefficent of Variance (CV)')
    legend(title='Overdispersion Factor (k)',title.col='black',legend=paste('k =',kr[1]:kr[2]),'topright',lty=1,col=Col,text.col = Col)
  }
  SaveImage('SCSEL',paste('CVk'),'png')
  PlotCVk1()
  dev.off()
  PlotCVk1()
  
  }
ResampleCompare = function(){
  Resample = function(Scope,Types,Years,CrashAssign,Method,DesCVCF=0.15){
    Sample.Size = NULL
    TOT_OC = NULL
    TOT_PC = NULL
    CF = NULL
    CVCF = NULL
    CVOC = NULL
    Len = NULL
    if(Method == 'HSM'){
      for(Type in Types){
        D = Text2Dat(Scope,Type,Years,CrashAssign,c('TOT_OC','TOT_PC','Length'))
        n = length(D[,1])
        r = sample(1:n,min(30,n))
        d = D[r,]
        toc = sum(d$TOT_OC)
        while(toc<100 & length(d[,1])<n){
          rem = (1:n)[which(!(1:n %in% r))]
          i = sample(rem,1)
          r = c(r,i)
          d = D[r,]
          toc = sum(d$TOT_OC)
        }
        Sample.Size = c(Sample.Size,length(d[,1]))
        TOT_OC = c(TOT_OC,toc)
        TOT_PC = c(TOT_PC,sum(d$TOT_PC))
        CF = c(CF,sum(d$TOT_OC)/sum(d$TOT_PC))
        CVOC = c(CVOC,var(d$TOT_OC)^0.5/mean(d$TOT_OC))
        CVCF = c(CVCF,var(BootFun(d))^0.5/(sum(d$TOT_OC)/sum(d$TOT_PC)))
        #Len = c(Len,sum(exp(d$Length)))
      }
      Out = data.frame(Sample.Size,TOT_OC,CVOC,TOT_PC,CF,CVCF)
      row.names(Out) = Types
      Out = Out[which(Out$TOT_OC >= 100 & Out$Sample.Size >= 30),]
    }
    if(Method == 'Mahdi'){
      for(Type in Types){
        D = Text2Dat(Scope,Type,Years,CrashAssign,c('TOT_OC','TOT_PC','Length'))
        n = length(D[,1])
        r = sample(1:n,min(20,n))
        d = D[r,]
        Len = NULL
        toc = var(d$TOT_OC)^0.5/mean(d$TOT_OC)
        while(mean(d$TOT_OC)==0){
          r = sample(1:n,min(20,n))
          d = D[r,]
          toc = var(d$TOT_OC)^0.5/mean(d$TOT_OC)
        }
        #while(length(d[,1])<exp(0.0288+2.2077*log(toc)-1.8636*log(DesCVCF)) & length(d[,1])<n){
        while(length(d[,1])<0.937535*(toc)**2/(DesCVCF)**2 & length(d[,1])<n){
          rem = (1:n)[which(!(1:n %in% r))]
          i = sample(rem,1)
          r = c(r,i)
          d = D[r,]
          toc = var(d$TOT_OC)^0.5/mean(d$TOT_OC)
        }
        Sample.Size = c(Sample.Size,length(d[,1]))
        TOT_OC = c(TOT_OC,sum(d$TOT_OC))
        TOT_PC = c(TOT_PC,sum(d$TOT_PC))
        CF = c(CF,sum(d$TOT_OC)/sum(d$TOT_PC))
        CVOC = c(CVOC,var(d$TOT_OC)^0.5/mean(d$TOT_OC))
        CVCF = c(CVCF,var(BootFun(d))^0.5/(sum(d$TOT_OC)/sum(d$TOT_PC)))
        #Len = c(Len,sum(exp(d$Length)))
      }
      Out = data.frame(Sample.Size,TOT_OC,CVOC,TOT_PC,CF,CVCF)
      row.names(Out) = Types
      Out = Out[which(Out$Sample.Size>=exp(0.0288+2.2077*log(Out$CVOC)-1.8636*log(DesCVCF))),]
    }
    return(Out)
  }
  AllSeg = c('R2U','R4D','R4U','U2U','U3T','U4U','U4D','U5T')
  AllInt = c('R3ST'  ,'R4ST'  ,'R4SG'  ,'RM3ST'  ,'RM4ST'  ,'RM4SG'  ,'U3ST'  ,'U4ST'  ,'U3SG'  ,'U4SG')
  HSMSampleSC = Resample('SCSEL',c(AllSeg,AllInt),2013:2014,'Var','HSM')
  MahSampleSC10 = Resample('SCSEL',c(AllSeg,AllInt),2013:2014,'Var','Mahdi',0.10)
  MahSampleSC15 = Resample('SCSEL',c(AllSeg,AllInt),2013:2014,'Var','Mahdi',0.15)
  MahSampleSC20 = Resample('SCSEL',c(AllSeg,AllInt),2013:2014,'Var','Mahdi',0.20)
  AllSeg = c('R4D','U2U','U3T','U4U','U4D','U5T')
  AllInt = c('R3ST'  ,'R4ST'  ,'R4SG'  ,'RM3ST'  ,'RM4ST'  ,'RM4SG'  ,'U3ST'  ,'U4ST'  ,'U3SG'  ,'U4SG')
  HSMSampleNC = Resample('NCSEL',c(AllSeg,AllInt),2007:2009,'250','HSM')
  MahSampleNC10 = Resample('NCSEL',c(AllSeg,AllInt),2007:2009,'250','Mahdi',0.10)
  MahSampleNC15 = Resample('NCSEL',c(AllSeg,AllInt),2007:2009,'250','Mahdi',0.15)
  MahSampleNC20 = Resample('NCSEL',c(AllSeg,AllInt),2007:2009,'250','Mahdi',0.20)
  HSMSample = rbind(HSMSampleSC,HSMSampleNC)
  MahSample10 = rbind(MahSampleSC10,MahSampleNC10)
  MahSample15 = rbind(MahSampleSC15,MahSampleNC15)
  MahSample20 = rbind(MahSampleSC20,MahSampleNC20)
  return(list(HSMSample,MahSample10,MahSample15,MahSample20))
}
PlotSampleCompare = function(HSMSample,MahSample10,MahSample15,MahSample20){
  Plot1 = function(HSMSample,MahSample10,MahSample15,MahSample20){
    layout(matrix(c(1,2,3,4),2,2,byrow = T))
    xr = c(0.05,0.40)
    xs = seq(xr[1],xr[2],length.out = 500)
    yr = c(0,40)
    Br = seq(xr[1],xr[2],0.0125)

    hist(HSMSample$CVCF  ,freq=F,xlab=expression('C.V. of Calibration Factor (CV'[CF]*')'),xlim=xr,ylim=yr,breaks=Br,main = 'Sample Size Requirement based on HSM',font.main=1)
    par(new=T);plot(xs,dnorm(xs,mean=mean(HSMSample$CVCF),sd=var(HSMSample$CVCF)^0.5),xlim=xr,ylim=yr,col='blue',xaxt='n',xaxt='n',xlab='',ylab='',type='l')
    legend('topright',legend = c(paste('Mean = ',prettyNum(mean(HSMSample$CVCF))),paste('Variance = ',prettyNum(var(HSMSample$CVCF))),'Equivalent Normal Distribution'),text.col=c('black','black','blue'),lty=c(NA,NA,1),col=c('black','black','blue'))

    hist(MahSample10$CVCF,freq=F,xlab=expression('C.V. of Calibration Factor (CV'[CF]*')'),xlim=xr,ylim=yr,breaks=Br,main = expression('Sample Size Requirement Based on Eq. (12) CV'[CF]*' = 0.10'))
    par(new=T);plot(xs,dnorm(xs,mean=mean(MahSample10$CVCF),sd=var(MahSample10$CVCF)^0.5),xlim=xr,ylim=yr,col='blue',xaxt='n',xaxt='n',xlab='',ylab='',type='l')
    legend('topright',legend = c(paste('Mean = ',prettyNum(mean(MahSample10$CVCF))),paste('Variance = ',prettyNum(var(MahSample10$CVCF))),'Equivalent Normal Distribution'),text.col=c('black','black','blue'),lty=c(NA,NA,1),col=c('black','black','blue'))
    
    hist(MahSample15$CVCF,freq=F,xlab=expression('C.V. of Calibration Factor (CV'[CF]*')'),xlim=xr,ylim=yr,breaks=Br,main = expression('Sample Size Requirement Based on Eq. (12) CV'[CF]*' = 0.15'))
    par(new=T);plot(xs,dnorm(xs,mean=mean(MahSample15$CVCF),sd=var(MahSample15$CVCF)^0.5),xlim=xr,ylim=yr,col='blue',xaxt='n',xaxt='n',xlab='',ylab='',type='l')
    legend('topright',legend = c(paste('Mean = ',prettyNum(mean(MahSample15$CVCF))),paste('Variance = ',prettyNum(var(MahSample15$CVCF))),'Equivalent Normal Distribution'),text.col=c('black','black','blue'),lty=c(NA,NA,1),col=c('black','black','blue'))
    
    hist(MahSample20$CVCF,freq=F,xlab=expression('C.V. of Calibration Factor (CV'[CF]*')'),xlim=xr,ylim=yr,breaks=Br,main = expression('Sample Size Requirement Based on Eq. (12) CV'[CF]*' = 0.20'))
    par(new=T);plot(xs,dnorm(xs,mean=mean(MahSample20$CVCF),sd=var(MahSample20$CVCF)^0.5),xlim=xr,ylim=yr,col='blue',xaxt='n',xaxt='n',xlab='',ylab='',type='l')
    legend('topright',legend = c(paste('Mean = ',prettyNum(mean(MahSample20$CVCF))),paste('Variance = ',prettyNum(var(MahSample20$CVCF))),'Equivalent Normal Distribution'),text.col=c('black','black','blue'),lty=c(NA,NA,1),col=c('black','black','blue'))
  }
  SaveImage('SCSEL',paste('SampleCompare'),'png')
  Plot1(HSMSample,MahSample10,MahSample15,MahSample20)
  dev.off()
  Plot1(HSMSample,MahSample10,MahSample15,MahSample20)
}
PlotResiduals = function(M){
  PlotRes1 = function(M){
    r = resid(M)
    f = M$fitted.values
    yr = c(-abs(max(r)),abs(max(r)))
    plot(x=f,y=r,xlab = "Fitted Values (Fitted Sample Size)",ylab="Residuals",ylim=yr)
    abline(h=0)
  }
  SaveImage('SCSEL',paste('Resplot'),'png')
  PlotRes1(M)
  dev.off()
  PlotRes1(M)
}
CFEvaluate    = function(Scope,TitleList,Years,CrashAssign,CFDef = 'HSM',NBoot = 10000){
  n = length(TitleList)
  TotTable = data.frame()
  IntTypes = c('R3ST','R4ST','R4SG','RM3ST','RM4ST','RM4SG','U3ST','U4ST','U3SG','U4SG')
  SegTypes = c('R2U','R4D','R4U','U2U','U3T','U4U','U4D','U5T')
  AllInt   = c('AllInt','AllIntSig','AllIntStp')
  AllSeg   = c('AllSeg')
  for(i in 1:n){
    Type = TitleList[i]
    D1 = Text2Dat(Scope,Type,Years,CrashAssign,ListHeaders(Type))
    
    #Apply Volume Ranges --> D2
    VolRange = FilterData(Scope,Type,Years,'Volumes')
    D2 = D1
    if(Type %in% c(IntTypes,AllInt)){
      D2     = D1[ (D1$AADT_Major<log(VolRange[1]) & D1$AADT_Minor<log(VolRange[2])),]
    }
    if(Type %in% c(SegTypes,AllSeg)){
      D2     = D1[ (D1$AADT  <log(VolRange[1]) & D1$Length>log(VolRange[2])),]
    }
    
    #Appply Outliers --> D3
    Os = FilterData(Scope,Type,Years,'Outliers')
    MD2 = GLMModels(D2 ,'Base')
    D3  = D2
    if(sum(Os)>0){
      D_Out = Outliers(Scope,MD2 ,FALSE,Type ,Os)
      D3    = D2[!(rownames(D2)  %in% rownames(D_Out)) ,]}
    
    #Calibration Factor
    CF = CFactor(D3,c(0,0),NBoot,CFDef = CFDef)
    Table = data.frame('Sample Size' = length(D3[,1]))
    rownames(Table)  = TitleList[i]
    Table['Tot Length']  = 0
    if(Type %in% SegTypes){Table['Tot Length']  =  sum(exp(D3$Length))}
    Table['Sum OC']  =  sum(D3$TOT_OC)
    Table['Sum PC']  =  sum(D3$TOT_PC)
    Table['Mean OC'] =  mean(D3$TOT_OC)
    Table['Mean PC'] =  mean(D3$TOT_PC)
    Table['se OC']   =  sqrt(var(D3$TOT_OC))
    Table['se PC']   =  sqrt(var(D3$TOT_PC))
    Table['cv OC']   =  sqrt(var(D3$TOT_OC))/mean(D3$TOT_OC)
    Table['cv PC']   =  sqrt(var(D3$TOT_PC))/mean(D3$TOT_PC)
    Table['CF']      =  CF$CFactor
    Table['se CF']   =  CF$C.SE
    Table['cv CF']   =  CF$C.SE/CF$CFactor
    k = HSMValues(Type)$k
    if(Type %in% c('R2U')){k = k/exp(D3$Length)}
    if(Type %in% c('R4U','R4D')){k = 1/exp(k + D3$Length)}
    se2 = sqrt(var(D3$TOT_OC)/length(D3[,1]))/mean(D3$TOT_PC)
    se3 = sqrt(sum(D3$TOT_OC+k*D3$TOT_OC^2))/sum(D3$TOT_PC)
    se4 = sqrt(sum(D3$TOT_OC+k*D3$TOT_OC^2))/sum(D3$TOT_OC)*CF$CFactor
    se5 = sqrt(sum(CF$CFactor*D3$TOT_PC+k*CF$CFactor^2*D3$TOT_PC^2))/sum(D3$TOT_PC)
    Table['se Mean']  =  se2
    Table['se EQ6']  =  se3
    Table['se EQ7']  =  se4
    Table['se EQ8']  =  se5
    Table['N EQ10']   =  CF$CFactor/CF$C.SE^2/mean(D3$TOT_PC)^2 + CF$CFactor^2*mean(k)/CF$C.SE^2
    TotTable = rbind(TotTable,Table)
  }
  return(TotTable)  
}

# Paper 2: -------------------------------------------------------------------------------------------
CFRange = function(Scope, Type, Years, CrashAssign, SPFType, CF, GOFs){
  IntTypes = c('R3ST','R4ST','R4SG','RM3ST','RM4ST','RM4SG','U3ST','U4ST','U3SG','U4SG')
  SegTypes = c('R2U','R4D','R4U','U2U','U3T','U4U','U4D','U5T')
  AllInt   = c('AllInt','AllIntSig','AllIntStp')
  AllSeg   = c('AllSeg')
  D1 = Text2Dat(Scope,Type,Years,CrashAssign,ListHeaders(Type))
  
  #Apply Volume Ranges --> D2
  VolRange = FilterData(Scope,Type,Years,'Volumes')
  D2 = D1
  if(Type %in% c(IntTypes,AllInt)){
    D2     = D1[ (D1$AADT_Major<log(VolRange[1]) & D1$AADT_Minor<log(VolRange[2])),]
  }
  if(Type %in% c(SegTypes,AllSeg)){
    D2     = D1[ (D1$AADT  <log(VolRange[1]) & D1$Length>log(VolRange[2])),]
  }
  
  #Appply Outliers --> D3
  Os = FilterData(Scope,Type,Years,'Outliers')
  MD2 = GLMModels(D2 ,'Base')
  D3  = D2
  if(1>0){
    D_Out = Outliers(Scope,MD2 ,TRUE,Type ,Os)
    D3    = D2[!(rownames(D2)  %in% rownames(D_Out)) ,]}
  
  D = D3
  TotTable = NULL
  for(i in CF){
    MD = GLMModels(D ,SPFType,Type)
    StateFitted  = GLMPredict(MD$M, D, Type)
    MD$Data['StateFitted'] = StateFitted
    AllData = D
    AllData['StateFitted'] = StateFitted
    Ml = paste(Type,'M',sep='_')
    Dl = paste(Type,'D',sep='_')
    Al = paste(Type,'A',sep='_')
    CF = data.frame('CFactor'=i,'CFactor SE' = 0)
    HSMFitted = D$TOT_PC*CF$CFactor
    MD$Data['HSMFitted'  ] = HSMFitted
    AllData['HSMFitted'  ] = HSMFitted
    MD = list(M=MD$M,D=MD$D,A=AllData)
    names(MD) = c(Ml,Dl,Al)
    D = AllData
    Table = data.frame('CF' = i)
    Table['Mean OC']     =  mean(D$TOT_OC)
    Table['Mean PC']     =  mean(D$HSMFitted)
    Table['Mean Fitted'] =  mean(D$StateFitted)
    for(gof in GOFs){
      Table[paste('State',gof)] = GOFMeasure(D$TOT_OC,D$StateFitted,gof)
      Table[paste('HSM'  ,gof)] = GOFMeasure(D$TOT_OC,D$HSMFitted  ,gof)
    }
    TotTable = rbind(TotTable,Table)
  }
  return(TotTable)
}
CFDefCompare = function(Scope, TitleList, Years, CrashAssign, SPFType,CFDefs){
  n = length(TitleList)
  TotTable = data.frame()
  IntTypes = c('R3ST','R4ST','R4SG','RM3ST','RM4ST','RM4SG','U3ST','U4ST','U3SG','U4SG')
  SegTypes = c('R2U','R4D','R4U','U2U','U3T','U4U','U4D','U5T')
  AllInt   = c('AllInt','AllIntSig','AllIntStp')
  AllSeg   = c('AllSeg')
  for(i in 1:n){
    Type = TitleList[i]
    D1 = Text2Dat(Scope,Type,Years,CrashAssign,ListHeaders(Type))
    
    #Apply Volume Ranges --> D2
    VolRange = FilterData(Scope,Type,Years,'Volumes')
    D2 = D1
    if(Type %in% c(IntTypes,AllInt)){
      D2     = D1[ (D1$AADT_Major<log(VolRange[1]) & D1$AADT_Minor<log(VolRange[2])),]
    }
    if(Type %in% c(SegTypes,AllSeg)){
      D2     = D1[ (D1$AADT  <log(VolRange[1]) & D1$Length>log(VolRange[2])),]
    }
    
    #Appply Outliers --> D3
    Os = FilterData(Scope,Type,Years,'Outliers')
    MD2 = GLMModels(D2 ,'Base')
    D3  = D2
    if(1>0){
      D_Out = Outliers(Scope,MD2 ,TRUE,Type ,Os)
      D3    = D2[!(rownames(D2)  %in% rownames(D_Out)) ,]}
    
    D = D3
    MD = GLMModels(D ,SPFType,Type)
    StateFitted  = GLMPredict(MD$M, D, Type)
    Table = data.frame('Sample.Size' = length(D[,1]))
    Table['Mean.OC'] =  mean(D$TOT_OC)
    Table['Mean.PC'] =  mean(D$TOT_PC)
    Table['Mean.Fitted'] =  mean(StateFitted)
    rownames(Table) = TitleList[i]
    D['StateFitted'] = StateFitted
    Table['State.LL'] = LogLike(D$TOT_OC,StateFitted)
    Table['State.SSE'] = GOFMeasure(D$TOT_OC,StateFitted,'SSE')
    
    for(CFDef in CFDefs){
      CF = CFactor(D,c(0,0),CFDef = CFDef)
      HSMFitted = CFPredict(D$TOT_PC,CF,CFDef)

      Table[paste(CFDef,'CF'  ,sep='.')] = CF$CFactor
      Table[paste(CFDef,'CFSE',sep='.')] = CF$C.SE
      Table[paste(CFDef,'CFCV',sep='.')] = CF$C.SE/CF$CFactor
      Table[paste(CFDef,'LL'  ,sep='.')] = LogLike(D$TOT_OC,HSMFitted)
      Table[paste(CFDef,'SSE' ,sep='.')] = GOFMeasure(D$TOT_OC,HSMFitted,'SSE')
    }
    TotTable = rbind(TotTable,Table)
  }
  return(TotTable)  
}
PlotCFComp = function(Type, CFSens, CFComp, CFDefs, Attribute ,Desciption){
  Plot1 = function(Type, CFSens, CFComp, CFDefs, Attribute ,Desciption){
    Color = c('blue','green4')
    Color2 = c('red','blue1','orange','brown','purple')
    PCH = c(16,18)
    LTY = c(1,1)
    CFs = NULL
    for(cfdef in CFDefs){
      CFs = c(CFs,CFComp[Type,paste(cfdef,'CF',sep='.')])
    }
    names(CFs) = CFDefs
    X = CFSens$CF
    Y_State = CFSens[paste('State',Attribute)][[1]]
    Y_HSM   = CFSens[paste('HSM',Attribute)][[1]]
    xr = range(X,CFs)
    yr = range(Y_State,Y_HSM)
    plot(X,Y_HSM,xlim = xr,ylim = yr,xlab='Calibration Factor',ylab=Desciption,col=Color[1],lty=LTY[1],pch=PCH[1],type = 'b')
    grid()
    par(new=T)
    plot(X,Y_State,col=Color[2],pch=PCH[2],xlab='',ylab='',xlim = xr,ylim = yr,lty=LTY[2],type = 'b',xaxt='n',yaxt='n')
    abline(h=0,col='black',lty=2)
    for(i in 1:length(CFDefs)){
      abline(v=CFs[i],col=Color2[i])
      yi = yr[1]+i*(yr[2]-yr[1])/(length(CFDefs)+1)
      #yi = mean(yr)
      text(x=CFs[i],y=yi,labels=CFDefs[i],col=Color2[i],srt=90)
    }
    
    l = c('Calibrated HSM SPF','State SPF')
    legend(legend=l,'right',text.col=Color,pch = PCH,lty = LTY,col=Color,title=Desciption,title.col='black')
    title(paste(Type,Desciption,'for different CFs'))
    l = NULL
    for(cfdef in CFDefs){
      l = c(l,paste('CF By ',cfdef,': ',round(CFComp[Type,paste(cfdef,'CF',sep='.')]*10000)/10000,sep=''))
    }
    or = order(CFs)
    legend(legend=l[or],'topright',text.col=Color2[or],title='Calibration Factors',title.col='black')
  }
  SaveImage(Scope,paste(Type,Attribute),'png')
  Plot1(Type, CFSens, CFComp, CFDefs, Attribute ,Desciption)
  dev.off()
  Plot1(Type, CFSens, CFComp, CFDefs, Attribute ,Desciption)
}
CFCUREPlot = function(Scope, Type, Years, CrashAssign, SPFType, CFComp, CFDefs){
  IntTypes = c('R3ST','R4ST','R4SG','RM3ST','RM4ST','RM4SG','U3ST','U4ST','U3SG','U4SG')
  SegTypes = c('R2U','R4D','R4U','U2U','U3T','U4U','U4D','U5T')
  AllInt   = c('AllInt','AllIntSig','AllIntStp')
  AllSeg   = c('AllSeg')
  D1 = Text2Dat(Scope,Type,Years,CrashAssign,ListHeaders(Type))
  
  #Apply Volume Ranges --> D2
  VolRange = FilterData(Scope,Type,Years,'Volumes')
  D2 = D1
  if(Type %in% c(IntTypes,AllInt)){
    D2     = D1[ (D1$AADT_Major<log(VolRange[1]) & D1$AADT_Minor<log(VolRange[2])),]
  }
  if(Type %in% c(SegTypes,AllSeg)){
    D2     = D1[ (D1$AADT  <log(VolRange[1]) & D1$Length>log(VolRange[2])),]
  }
  
  #Appply Outliers --> D3
  Os = FilterData(Scope,Type,Years,'Outliers')
  MD2 = GLMModels(D2 ,'Base')
  D3  = D2
  if(1>0){
    D_Out = Outliers(Scope,MD2 ,TRUE,Type ,Os)
    D3    = D2[!(rownames(D2)  %in% rownames(D_Out)) ,]}
  
  D = D3

  for(cfdef in CFDefs){
    MD = GLMModels(D ,SPFType,Type)
    D['StateFitted'] = GLMPredict(MD$M, D, Type)
    Ml = paste(Type,'M',sep='_')
    Dl = paste(Type,'D',sep='_')
    Al = paste(Type,'A',sep='_')
    CF = CFactor(D,c(0,0),10,0.95,cfdef)
    HSMFitted = CFPredict(D$TOT_PC,CF,cfdef)
    D['HSMFitted'  ] = HSMFitted
    MD = list(M=MD$M,D=D,A=D)
    names(MD) = c(Ml,Dl,Al)
    CUREPlot(Scope,MD,CF,paste(Type,'_',cfdef,sep=''))
  }
}
CFSensPlot = function(Scope, TitleList, Years, CrashAssign, SPFType,CFDefs){
  n = length(TitleList)
  TotTable = data.frame()
  IntTypes = c('R3ST','R4ST','R4SG','RM3ST','RM4ST','RM4SG','U3ST','U4ST','U3SG','U4SG')
  SegTypes = c('R2U','R4D','R4U','U2U','U3T','U4U','U4D','U5T')
  AllInt   = c('AllInt','AllIntSig','AllIntStp')
  AllSeg   = c('AllSeg')
  for(i in 1:n){
    Type = TitleList[i]
    D1 = Text2Dat(Scope,Type,Years,CrashAssign,ListHeaders(Type))
    
    #Apply Volume Ranges --> D2
    VolRange = FilterData(Scope,Type,Years,'Volumes')
    D2 = D1
    if(Type %in% c(IntTypes,AllInt)){
      D2     = D1[ (D1$AADT_Major<log(VolRange[1]) & D1$AADT_Minor<log(VolRange[2])),]
    }
    if(Type %in% c(SegTypes,AllSeg)){
      D2     = D1[ (D1$AADT  <log(VolRange[1]) & D1$Length>log(VolRange[2])),]
    }
    
    #Appply Outliers --> D3
    Os = FilterData(Scope,Type,Years,'Outliers')
    MD2 = GLMModels(D2 ,'Base')
    D3  = D2
    if(1>0){
      D_Out = Outliers(Scope,MD2 ,TRUE,Type ,Os)
      D3    = D2[!(rownames(D2)  %in% rownames(D_Out)) ,]}
    
    D = D3
    #MD = GLMModels(D ,SPFType,Type)
    #StateFitted  = GLMPredict(MD$M, D, Type)
    
    CF = NULL
    for(CFDef in CFDefs){
      CF = CFactor(D,c(0,0),CFDef = CFDef)
      HSMFitted = CFPredict(D$TOT_PC,CF,CFDef)
      
      Table[paste(CFDef,'CF'  ,sep='.')] =  CF$CFactor
      Table[paste(CFDef,'CFSE',sep='.')] =  CF$C.SE
      Table[paste(CFDef,'CFCV',sep='.')] =  CF$C.SE/CF$CFactor
    }
    TotTable = rbind(TotTable,Table)
  }
  return(TotTable)  
}
CFFunDef   = function(Scope, TitleList, Years, CrashAssign, SPFType, GOFs){
  n = length(TitleList)
  TotTable = data.frame()
  IntTypes = c('R3ST','R4ST','R4SG','RM3ST','RM4ST','RM4SG','U3ST','U4ST','U3SG','U4SG')
  SegTypes = c('R2U','R4D','R4U','U2U','U3T','U4U','U4D','U5T')
  AllInt   = c('AllInt','AllIntSig','AllIntStp')
  AllSeg   = c('AllSeg')
  for(i in 1:n){
    Type = TitleList[i]
    D1 = Text2Dat(Scope,Type,Years,CrashAssign,ListHeaders(Type))
    
    #Apply Volume Ranges --> D2
    VolRange = FilterData(Scope,Type,Years,'Volumes')
    D2 = D1
    if(Type %in% c(IntTypes,AllInt)){
      D2     = D1[ (D1$AADT_Major<log(VolRange[1]) & D1$AADT_Minor<log(VolRange[2])),]
    }
    if(Type %in% c(SegTypes,AllSeg)){
      D2     = D1[ (D1$AADT  <log(VolRange[1]) & D1$Length>log(VolRange[2])),]
    }
    
    #Appply Outliers --> D3
    Os = FilterData(Scope,Type,Years,'Outliers')
    MD2 = GLMModels(D2 ,'Base')
    D3  = D2
    if(1>0){
      D_Out = Outliers(Scope,MD2 ,TRUE,Type ,Os)
      D3    = D2[!(rownames(D2)  %in% rownames(D_Out)) ,]}
    
    D = D3
    MD = GLMModels(D ,SPFType,Type)
    StateFitted  = GLMPredict(MD$M, D, Type)
    Table = data.frame('Sample Size' = length(D[,1]))
    Table['Mean OC'] =  mean(D$TOT_OC)
    Table['Mean PC'] =  mean(D$TOT_PC)
    Table['Mean Fitted'] =  mean(StateFitted)
    rownames(Table) = TitleList[i]
    D['StateFitted'] = StateFitted

    M = glm(TOT_OC ~ log(TOT_PC), data = D, family= negative.binomial(theta = 1000,link= "log"))
    HSMFitted = exp(M$coef[[1]]+log(D$TOT_PC)*M$coef[[2]])
    D['HSMFitted'] = HSMFitted
    
    Table[paste('CFunA',sep='.')] =  M$coef[[1]]
    Table[paste('CFunB',sep='.')] =  M$coef[[2]]
    for(gof in GOFs){
      Table[paste('State',gof)] = GOFMeasure(D$TOT_OC,D$StateFitted,gof)
      Table[paste('CFun'  ,gof)] = GOFMeasure(D$TOT_OC,D$HSMFitted  ,gof)
    }
    TotTable = rbind(TotTable,Table)
  }
  return(TotTable)  
}
CFMLEPlot  = function(Scope, Type, Years, CrashAssign, CF){
  CFMLEPlot1 = function(Scope, Type, Years, CrashAssign, CF){
    Color = c('blue','green4','red')
    D1 = Text2Dat(Scope,Type,Years,CrashAssign,ListHeaders(Type))
    IntTypes = c('R3ST','R4ST','R4SG','RM3ST','RM4ST','RM4SG','U3ST','U4ST','U3SG','U4SG')
    SegTypes = c('R2U','R4D','R4U','U2U','U3T','U4U','U4D','U5T')
    AllInt   = c('AllInt','AllIntSig','AllIntStp')
    AllSeg   = c('AllSeg')
    
    #Apply Volume Ranges --> D2
    VolRange = FilterData(Scope,Type,Years,'Volumes')
    D2 = D1
    if(Type %in% c(IntTypes,AllInt)){
      D2     = D1[ (D1$AADT_Major<log(VolRange[1]) & D1$AADT_Minor<log(VolRange[2])),]
    }
    if(Type %in% c(SegTypes,AllSeg)){
      D2     = D1[ (D1$AADT  <log(VolRange[1]) & D1$Length>log(VolRange[2])),]
    }
    
    #Appply Outliers --> D3
    Os = FilterData(Scope,Type,Years,'Outliers')
    MD2 = GLMModels(D2 ,'Base')
    D3  = D2
    if(1>0){
      D_Out = Outliers(Scope,MD2 ,TRUE,Type ,Os)
      D3    = D2[!(rownames(D2)  %in% rownames(D_Out)) ,]}
    
    D = D3
    
    
    Vector_Mean  = mean(D$TOT_OC)
    Vector_Var   = var(D$TOT_OC)
    k_Factor     = (Vector_Var-Vector_Mean) / Vector_Mean^2
    k = 1/k_Factor
    Y = NULL
    for(cf in CF){
      Y = c(Y,sum((D$TOT_OC+k)/(D$TOT_PC*cf+k)))
    }
    xr = range(CF)
    yr = range(Y)
    plot(x=CF,y=Y,xlim=xr,ylim=yr,xlab='Calibration Factor',ylab=expression((N[o] + phi)/(CF*N[p]+ phi)),col=Color[1],type='l')
    abline(h=length(D$TOT_OC),col=Color[2])
    text(x=mean(CF)*1.2,y=length(D$TOT_OC)*1.05,labels=paste('n =',length(D$TOT_OC)),,col=Color[2])
    CFMLE = MLE_CF(D$TOT_OC,D$TOT_PC)
    par(new=T);plot(x=CFMLE,y=sum((D$TOT_OC+k)/(D$TOT_PC*CFMLE+k)),xlim=xr,ylim=yr,xlab='',ylab='',col=Color[3])
    abline(v=CFMLE,col=Color[3])
    text(x=CFMLE*.85,y=(length(D$TOT_OC)+yr[2])/2,labels=expression(CF[MLE]),col=Color[3],srt=90)
    grid()
    title(substitute(atop(paste(A,' ',CF[MLE],' Plot')),list(A=Type)))
  }
  SaveImage(Scope,paste(Type,'CFMLE'),'png')
  CFMLEPlot1(Scope, Type, Years, CrashAssign, CF)
  dev.off()
  CFMLEPlot1(Scope, Type, Years, CrashAssign, CF)
}
PlotMeanDev = function(Scope,CFComp,CFDefs){
  PlotMeanDev1 = function(CFComp,CFDefs){
    Color = c('blue')
    #CFDefs = CFDefs[which(CFDefs!='Fitted')]
    No = CFComp$Mean.OC
    Nu = CFComp$Mean.PC
    Nf = CFComp$Mean.Fitted
    yr = NULL
    Dev = NULL
    for(CFDef in CFDefs){
      Np = abs(Nf - Nu*CFComp[paste(CFDef,'CF',sep='.')])
      Dev = c(Dev,mean(Np[,1]))
      yr = range(yr,Np)
    }
    xr=c(0,length(CFDefs)+1)
    j=0
    l = NULL
    MDev = NULL
    for(i in order(Dev)){
      j = j+1
      Dev = abs(Nf - Nu*CFComp[paste(CFDefs[i],'CF',sep='.')])
      MDev = c(MDev,mean(Dev[,1]))
      if(j==1){
        boxplot(at=j,Dev,xlim=xr,ylim=yr,outline=T,border=Color[1],ylab=expression(paste("|",N[f] - CF, ' x ', N[u],"|")))
      }
      if(j!=1){
        par(new=TRUE)
        boxplot(at=j,Dev,xlim=xr,ylim=yr,outline=T,yaxt='n',border=Color[1])
      }
      axis(1,at=j,labels=as.expression(substitute(CF[A],list(A=CFDefs[i]))))
      l = c(l,as.expression(substitute(paste("Average ( |",N[f] - CF[B], ' x ', N[u],"| ) = ", A),list(B = CFDefs[i],A=round(mean(Dev[,1])*10000)/10000))))
    }
    #par(new=TRUE)
    #plot(MDev,type='b',xlim=xr,ylim=yr,xlab='',ylab='',xaxt='n',yaxt='n')  
    legend('topleft',legend=l)
  }
  SaveImage(Scope,paste('0 Mean Dev'),'png')
  PlotMeanDev1(CFComp,CFDefs)
  dev.off()
  PlotMeanDev1(CFComp,CFDefs)
}
PlotMeanLL = function(Scope, Type, CFYears, CrashAssign, SPFType,CFComp,CFDefs){
  PlotMeanLL1 = function(StateLL,CFComp,CFDefs){
    Color = c('blue')
    #CFDefs = CFDefs[which(CFDefs!='Fitted')]
    yr = NULL
    Dev = NULL
    for(CFDef in CFDefs){
      LL = (StateLL - CFComp[paste(CFDef,'LL',sep='.')])
      Dev = c(Dev,mean(LL[,1]))
      yr = range(yr,LL)
    }
    xr=c(0,length(CFDefs)+1)
    j=0
    l = NULL
    MDev = NULL
    for(i in order(Dev)){
      j = j+1
      Dev = StateLL - CFComp[paste(CFDefs[i],'LL',sep='.')]
      MDev = c(MDev,mean(Dev[,1]))
      if(j==1){
        boxplot(at=j,Dev,xlim=xr,ylim=yr,outline=T,border=Color[1],ylab=expression(paste('LL(',N[o],',',N[f],') - LL(',N[o],',',N[p],')')))
      }
      if(j!=1){
        par(new=TRUE)
        boxplot(at=j,Dev,xlim=xr,ylim=yr,outline=T,yaxt='n',border=Color[1])
      }
      if(CFDefs[i]=='Srini1'){
        axis(1,at=j,labels='Calib. Func.')
        l = c(l,as.expression(substitute(paste("Average (LL(",N[o],',',N[f],') - LL(',N[o],',',N[p][Calib.Func.],')) = ',A),list(A=round(mean(Dev[,1])*1000)/1000))))
      }
      if(CFDefs[i]!='Srini1'){
        axis(1,at=j,labels=as.expression(substitute(CF[A],list(A=CFDefs[i]))))
        l = c(l,as.expression(substitute(paste("Average (LL(",N[o],',',N[f],') - LL(',N[o],',',CF[B],' x ',N[u],')) = ', A),list(B = CFDefs[i],A=round(mean(Dev[,1])*10000)/10000))))
      }
    }
    #par(new=TRUE)
    #plot(MDev,type='b',xlim=xr,ylim=yr,xlab='',ylab='',xaxt='n',yaxt='n')  
    legend('topleft',legend=l)
  }
  StateLL = NULL
  for(Type in rownames(CFComp)){
    Sen = CFRange(Scope, Type, CFYears, CrashAssign, SPFType, 1,"LL")
    StateLL = c(StateLL,Sen$"State LL"[[1]])
  }
  names(StateLL) = rownames(CFComp)
  SaveImage(Scope,paste('0 Mean LL'),'png')
  PlotMeanLL1(StateLL,CFComp,CFDefs)
  dev.off()
  PlotMeanLL1(StateLL,CFComp,CFDefs)
}
PlotMeanSSE = function(Scope,CFComp,CFDefs){
  PlotMeanSSE1 = function(CFComp,CFDefs){
    Color = c('blue')
    #CFDefs = CFDefs[which(CFDefs!='Fitted')]
    yr = NULL
    Dev = NULL
    for(CFDef in CFDefs){
      LL = -CFComp['State.SSE']+CFComp[paste(CFDef,'SSE',sep='.')]
      Dev = c(Dev,mean(LL[,1]))
      yr = range(yr,LL)
    }
    yr = c(-200,500)
    xr=c(0,length(CFDefs)+1)
    j=0
    l = NULL
    MDev = NULL
    for(i in order(Dev)){
      j = j+1
      Dev = -CFComp['State.SSE']+CFComp[paste(CFDefs[i],'SSE',sep='.')]
      MDev = c(MDev,mean(Dev[,1]))
      if(j==1){
        boxplot(at=j,Dev,xlim=xr,ylim=yr,outline=F,border=Color[1],ylab=expression(paste(SSE(N[p])-SSE(N[f]))))
      }
      if(j!=1){
        par(new=TRUE)
        boxplot(at=j,Dev,xlim=xr,ylim=yr,outline=F,yaxt='n',border=Color[1])
      }
      axis(1,at=j,labels=as.expression(substitute(CF[B],list(B=CFDefs[i]))))
      l = c(l,as.expression(substitute(paste("Average (", SSE(CF[B]*N[u])-SSE(N[f]),') = ',A),list(B = CFDefs[i],A=round(mean(Dev[,1])*1000)/1000))))
    }
    #par(new=TRUE)
    #plot(MDev,type='b',xlim=xr,ylim=yr,xlab='',ylab='',xaxt='n',yaxt='n')  
    legend('topleft',legend=l)
  }
  SaveImage(Scope,paste('0 Mean SSE'),'png')
  PlotMeanSSE1(CFComp,CFDefs)
  dev.off()
  PlotMeanSSE1(CFComp,CFDefs)
}
PlotMeanCV = function(Scope,CFComp,CFDefs){
  PlotMeanCV1 = function(CFComp,CFDefs){
    Color = c('blue')
    #CFDefs = CFDefs[which(CFDefs!='Fitted')]
    yr = NULL
    Dev = NULL
    for(CFDef in CFDefs){
      LL = CFComp[paste(CFDef,'CFCV',sep='.')]
      Dev = c(Dev,mean(LL[,1]))
      yr = range(yr,LL)
    }
    xr=c(0,length(CFDefs)+1)
    j=0
    l = NULL
    MDev = NULL
    for(i in order(Dev)){
      j = j+1
      Dev = CFComp[paste(CFDefs[i],'CFCV',sep='.')]
      MDev = c(MDev,mean(Dev[,1]))
      if(j==1){
        boxplot(at=j,Dev,xlim=xr,ylim=yr,outline=T,border=Color[1],ylab=expression(paste(CV(CF))))
      }
      if(j!=1){
        par(new=TRUE)
        boxplot(at=j,Dev,xlim=xr,ylim=yr,outline=T,yaxt='n',border=Color[1])
      }
      axis(1,at=j,labels=as.expression(substitute(CF[B],list(B=CFDefs[i]))))
      l = c(l,as.expression(substitute(paste("Average (", CV(CF[B]),') = ',A),list(B = CFDefs[i],A=round(mean(Dev[,1])*1000)/1000))))
    }
    #par(new=TRUE)
    #plot(MDev,type='b',xlim=xr,ylim=yr,xlab='',ylab='',xaxt='n',yaxt='n')  
    legend('topleft',legend=l)
  }
  SaveImage(Scope,paste('0 Mean CV'),'png')
  PlotMeanCV1(CFComp,CFDefs)
  dev.off()
  PlotMeanCV1(CFComp,CFDefs)
}

# Paper 4: ---------------------------------
ExportFitted  = function(Scope,Years,CrashAssign,MDList){
  for(y in Years){
    DI = Text2Dat(Scope,'AllInt',y,CrashAssign)
    Fitted = NULL
    for(i in 1:length(DI[,1])){
      Type = matrix(DI[i,'FType'])
      M = paste(Type,'M',sep='_')
      D = paste(Type,'A',sep='_')
      if(M %in% names(MDList)){
        Ft = GLMPredict(MDList[M][[1]],MDList[D][[1]][i,])
      }
      else(
        if('AllInt_M' %in% names(MDList)){
          Ft = GLMPredict(MDList['AllInt_M'][[1]],MDList['AllInt_A'][[1]][i,])
        }
        )
      if(length(Ft)==0){Ft=0}
      Fitted[i] = Ft
    }
    DI['Fitted'] = Fitted
    DI['Diff'] = DI['TOT_OC']-DI['Fitted']
    write.csv(DI,paste('Outputs/',Scope,'/Fit_Int_',y,'_',CrashAssign,'.csv',sep=''),row.names=F)
    
    DS = Text2Dat(Scope,'AllSeg',y,CrashAssign)
    Fitted = NULL
    for(i in 1:length(DS[,1])){
      Type = matrix(DS[i,'FType'])
      M = paste(Type,'M',sep='_')
      D = paste(Type,'A',sep='_')
      if(M %in% names(MDList)){
        Ft = GLMPredict(MDList[M][[1]],MDList[D][[1]][i,])
      }
      else(
        if('AllSeg_M' %in% names(MDList)){
          Ft = GLMPredict(MDList['AllSeg_M'][[1]],MDList['AllSeg_A'][[1]][i,])
        }
      )
      if(length(Ft)==0){Ft=0}
      Fitted[i] = Ft
    }
    DS['Fitted'] = Fitted
    DS['Diff'] = DS['TOT_OC']-DS['Fitted']
    write.csv(DS,paste('Outputs/',Scope,'/Fit_Seg_',y,'_',CrashAssign,'.csv',sep=''),row.names=F)
  }
}
ExportStudRes  = function(Scope,Years,CrashAssign,MDList){
  for(y in Years){
    DI = Text2Dat(Scope,'AllInt',y,CrashAssign)
    Fitted = NULL
    StudRes = NULL
    for(i in 1:length(DI[,1])){
      Type = matrix(DI[i,'FType'])
      AADT_Major = DI[i,'AADT_Major']
      AADT_Minor = DI[i,'AADT_Minor']
      TOT_OC = DI[i,'TOT_OC']
      M = paste(Type,'M',sep='_')
      D = paste(Type,'A',sep='_')
      if(D %in% names(MDList)){
        Ft = GLMPredict(MDList[M][[1]],MDList[D][[1]][i,])
        Sr = MDList[D][[1]][which(MDList[D][[1]]$AADT_Major==AADT_Major & MDList[D][[1]]$AADT_Minor==AADT_Minor & MDList[D][[1]]$TOT_OC==TOT_OC),'StudRes'][1]
      }
      if(length(Sr)==0 | is.na(Sr)){Sr=0}
      if(length(Ft)==0 | is.na(Ft)){Ft=0}
      Fitted[i] = Ft
      StudRes[i] = Sr
    }
    DI['Fitted'] = Fitted
    DI['Diff'  ] = StudRes
    write.csv(DI,paste('Outputs/',Scope,'/Fit_Int_',y,'_',CrashAssign,'.csv',sep=''),row.names=F)
    
    DS = Text2Dat(Scope,'AllSeg',y,CrashAssign)
    Fitted = NULL
    StudRes = NULL
    for(i in 1:length(DS[,1])){
      Type = matrix(DS[i,'FType'])
      AADT = DS[i,'AADT']
      Length = DS[i,'Length']
      TOT_OC = DS[i,'TOT_OC']
      M = paste(Type,'M',sep='_')
      D = paste(Type,'A',sep='_')
      if(M %in% names(MDList)){
        Ft = GLMPredict(MDList[M][[1]],MDList[D][[1]][i,])
        Sr = MDList[D][[1]][which(MDList[D][[1]]$AADT==AADT & MDList[D][[1]]$Length==Length & MDList[D][[1]]$TOT_OC==TOT_OC),'StudRes'][1]
      }
      if(length(Sr)==0 | is.na(Sr)){Sr=0}
      if(length(Ft)==0 | is.na(Ft)){Ft=0}
      Fitted[i] = Ft
      StudRes[i] = Sr
    }
    DS['Fitted'] = Fitted
    DS['Diff'  ] = StudRes
    write.csv(DS,paste('Outputs/',Scope,'/Fit_Seg_',y,'_',CrashAssign,'.csv',sep=''),row.names=F)
  }
}
ComparisonMatrix = function(Scope,Years,Types,CrashAssign,AreaName,Areas,AreaLabels){
  i = 0
  for(Type in Types){
    i = i+1
    D = Text2Dat(Scope,Type,Years,CrashAssign,ListHeaders(Type))
    CFList = CFactor(D)
    for(Area in Areas){
      CFList = rbind(CFList,CFactor(D[which(D[AreaName]==Area),]))
    }
    n = length(Areas) + 1
    Test = NULL
    for(i1 in 1:n){
      for(i2 in 1:n){
        Test = c(Test,NullTest(CFList[i1,],CFList[i2,]))
      }
    }
    Mat = matrix(Test,n,n)
    Mat = data.frame(Mat)
    names(Mat) = c('State',AreaLabels)
    rownames(Mat) = c('State',AreaLabels)
    if(Type==Types[1]){MatList = list(Mat)}
    if(Type!=Types[1]){MatList[i] = list(Mat)}
  }
  names(MatList)=Types
  
  Sum = matrix(0,length(Areas)+1,length(Areas)+1)
  for(Type in Types){
    Sum = Sum + MatList[Type][[1]] 
  }
  Ave = Sum/length(Types)
  
  return(MatList)
}
PlotAreaBox = function(CList1,CList2,CList3){
  xr = c(0,4)
  yr = c(0,1)
  a1=NULL;for(i in 1:length(CList1)){a1=c(a1,CList1[[i]][4,2],CList1[[i]][3,2],CList1[[i]][4,3])}
  a2=NULL;for(i in 1:length(CList2)){a2=c(a2,CList2[[i]][3,2])}
  a3=NULL;for(i in 1:length(CList3)){a3=c(a3,CList3[[i]][3,2])}
  boxplot(a1,xlim=c(0,4),ylim=c(0,1),at=1,xaxt='n',xlab = 'Area Divisions',ylab='Average Level of Confidence')
  par(new=T);boxplot(a2,xlim=c(0,4),ylim=c(0,1),at=2,yaxt='n',xaxt='n')
  par(new=T);boxplot(a3,xlim=c(0,4),ylim=c(0,1),at=3,yaxt='n',xaxt='n')
  #par(new=T);plot(x=1:3,y=c(mean(a1),mean(a2),mean(a3)),xlim=c(0,4),ylim=c(0,1),col='red',type='b',xaxt='n',yaxt='n')
  axis(1,at=1:3,labels=c('Topo.','Pop. Density','Hot spots'),las=1)
}
GdStat = function(Val,X,Y,d){
  Wdij  = function(d,X,Y){
    n = length(X)
    Wd = matrix(0,n,n)
    for(i in 1:n){
      for(j in 1:n){
        xi = X[i]
        xj = X[j]
        yi = Y[i]
        yj = Y[j]
        dist = ((xi-xj)^2+(yi-yj)^2)^0.5
        if(dist<=d & i!=j){Wd[i,j]=1}
      }
    }
    return(Wd)
  }
  Gid = function(i,Val,Wd){
    n = length(Val)
    N = 0
    for(j in 1:n){
      N = N + Wd[i,j]*Val[j]
    }
    D = sum(Val)-Val[i]
    return(N/D) 
  }
  n = length(Val)
  Wd  = Wdij (d,X,Y)
  Gi = NULL
  EGi = NULL
  EGi2 = NULL
  for(i in 1:n){
    Gi[i]  = Gid (i,Val,Wd )
    Wi  = sum(Wd [i,])
    SXj2 = (sum(Val)-Val[i])^2
    SX2j = sum(Val^2)-Val[i]^2
    EGi [i]  = Wi /(n-1)
    EGi2 [i] = 1/SXj2*(Wi*SX2j /(n-1)+(Wi ^2-Wi )/(n-1)/(n-2)*(SXj2-SX2j))
  }
  VarG  = EGi2 -EGi ^2
  Zi  = (Gi -EGi )/VarG ^0.5
  return(list('Gi'=Gi,'Z'=Zi))
}
Wdij  = function(d,X,Y){
  n = length(X)
  Wd = matrix(0,n,n)
  for(i in 1:n){
    for(j in 1:n){
      xi = X[i]
      xj = X[j]
      yi = Y[i]
      yj = Y[j]
      dist = ((xi-xj)^2+(yi-yj)^2)^0.5
      if(dist<=d){Wd[i,j]=1}
    }
  }
  return(Wd)
}
GdStarStat = function(Val,X,Y,d){
  Gid = function(i,Val,Wd){
    n = length(Val)
    N = 0
    for(j in 1:n){
      N = N + Wd[i,j]*Val[j]
    }
    D = sum(Val)
    return(N/D) 
  }
  n    = length(Val)
  Wd   = Wdij (d,X,Y)
  Gi   = NULL
  EGi  = NULL
  EGi2 = NULL
  for(i in 1:n){
    Gi[i]   = Gid (i,Val,Wd )
    Wi      = sum(Wd [i,])
    SXj2    = sum(Val)^2
    SX2j    = sum(Val^2)
    EGi[i]  = Wi/(n)
    EGi2[i] = 1/SXj2*(Wi*SX2j /(n)+(Wi^2-Wi)/(n)/(n-1)*(SXj2-SX2j))
  }
  VarG  = EGi2 -EGi ^2
  Zi  = (Gi -EGi )/VarG ^0.5
  return(list('Gi'=Gi,'Z'=Zi))
}
ExampleData = function(m,c){
  A = m*(1+c)
  B = m*(1-c)
  X = rep(1:10,5)
  Y = c(rep(1,10),rep(2,10),rep(3,10),rep(4,10),rep(5,10))
  v = c(m,A,A,A,m,m,B,B,B,m)
  Val = c(rep(m,10),rep(v,3),rep(m,10))
  return(list('X'=X,'Y'=Y,'Val'=Val))
}
SpatialMethods = function(Methods,CL){
  TestRes = list()
  for(Method in Methods){
    HotRes = NULL
    ColdRes = NULL
    D = rbind(read.csv(paste('Inputs/Int_2013_',toString(Method),'.csv',sep='')),
              read.csv(paste('Inputs/Int_2014_',toString(Method),'.csv',sep='')),
              read.csv(paste('Inputs/Seg_2013_Reseg_RO250_',toString(Method),'.csv',sep='')),
              read.csv(paste('Inputs/Seg_2014_Reseg_RO250_',toString(Method),'.csv',sep='')))
    for(Type in unique(D$FType)){
      #CFState = CFactor(D[which(D$FType==Type),])
      if(CL==0.99){
        CFHot  = CFactor(D[which(D$FType==Type & D$Bin %in% c(3),),])  
        CFCold = CFactor(D[which(D$FType==Type & D$Bin %in% c(2,1,0,-1,-2,-3),),])  
      }
      if(CL==0.95){
        CFHot  = CFactor(D[which(D$FType==Type & D$Bin %in% c(3,2),),])  
        CFCold = CFactor(D[which(D$FType==Type & D$Bin %in% c(-3,-2,-1,0,1),),])  
      }
      if(CL==0.90){
        CFHot  = CFactor(D[which(D$FType==Type & D$Bin %in% c(3,2,1),),])  
        CFCold = CFactor(D[which(D$FType==Type & D$Bin %in% c(-3,-2,-1,0),),])  
      }
      if( CFHot$CFactor >0){HotRes  = c(HotRes,NullTest(CFCold,CFHot))}
      if( CFHot$CFactor==0){HotRes  = c(HotRes,NA)}
      if( CFHot$CFactor >0){ColdRes = c(ColdRes,CFHot$CFactor/CFCold$CFactor)}
      if( CFHot$CFactor==0){ColdRes = c(ColdRes,NA)}
    }
    DF = data.frame(HotRes,ColdRes)
    rownames(DF) = unique(D$FType)
    TestRes[[length(TestRes)+1]] = DF
  }
  return(TestRes)
}
PlotSpatialMethods = function(L){
  plot.new()
  layout(matrix(c(1,2),2,1),c(1,1),c(1,1))
  n = length(L)
  xr = c(1,n)
  yr = c(0,1)
  for(i in 1:n){
    if(i!=1){par(new=T)}
    boxplot(at=i,x=L[[i]]$HotRes,xlim=xr,ylim=yr)
    axis(3,at=i,length(L[[i]]$HotRes[which(L[[i]]$HotRes>0)]))
    axis(1,at=i,prettyNum(mean(L[[i]]$HotRes[which(L[[i]]$HotRes>0)])),las=2)
  }
  par(new=F)
  for(i in 1:n){
    if(i!=1){par(new=T)}
    boxplot(at=i,x=L[[i]]$ColdRes,xlim=xr,ylim=c(0,3))
    axis(3,at=i,length(L[[i]]$ColdRes[which(L[[i]]$ColdRes>0)]))
    axis(1,at=i,prettyNum(mean(L[[i]]$ColdRes[which(L[[i]]$ColdRes>0)])),las=2)
  }
  
  
}
MoranIPlot = function(D,Dist){
  m = NULL
  an = NULL
  mn = NULL
  for(d in Dist){
    w = Wdij(d*5280,D$X,D$Y)
    an = c(an,sum(colSums(w))/length(D$Y))
    mn = c(mn,min((colSums(w))))
    MI = Moran.I(D$FitDif,w)
    z=(MI$observed-MI$expected)/MI$sd
    m = c(m,z)
    #print(MI)
    print(c(sum(colSums(w))/length(D$Y),min((colSums(w))),z))
  }
  return(data.frame(Distance=Dist,ZScore=m,AveNeig = an,MinNeig=mn))
  plot(x=Dist,y=m)
}
CFAllAreasTab = function(Scope,Years){
  SWCF = NULL
  SWSD = NULL
  SWSS = NULL
  CSCF = NULL
  CSSD = NULL
  CSSS = NULL
  MSCF = NULL
  MSSD = NULL
  MSSS = NULL
  USCF = NULL
  USSD = NULL
  USSS = NULL
  SCCF = NULL
  SCSD = NULL
  SCSS = NULL
  DCCF = NULL
  DCSD = NULL
  DCSS = NULL
  HSCF = NULL
  HSSD = NULL
  HSSS = NULL
  NSCF = NULL
  NSSD = NULL
  NSSS = NULL
  D = rbind(Text2Dat(Scope,'AllInt',Years,'Var',c('TOT_OC','TOT_PC','FType','Area1','Area2','Gi_Bin')),
            Text2Dat(Scope,'AllSeg',Years,'Var',c('TOT_OC','TOT_PC','FType','Area1','Area2','Gi_Bin')))
  for(Type in unique(D$FType)){
    
    #if(Type %in% c('R2U','U2U','U4U','U4D','U5T')){print(sum(exp(D[which(D$FType==Type),'Length'])/5280)/length(D[which(D$FType==Type),'Length']))}
    CFSW = CFactor(D[which(D$FType==Type),])
    SWCF = c(SWCF,CFSW$CFactor)
    SWSD = c(SWSD,CFSW$C.SE)
    SWSS = c(SWSS,CFSW$Size)
    
    CFCS = CFactor(D[which(D$FType==Type & D$Area1 %in% c(1)),])  
    CFMS = CFactor(D[which(D$FType==Type & D$Area1 %in% c(2)),])  
    CFUS = CFactor(D[which(D$FType==Type & D$Area1 %in% c(3)),])  
    CSCF = c(CSCF,CFCS$CFactor)
    MSCF = c(MSCF,CFMS$CFactor)
    USCF = c(USCF,CFUS$CFactor)
    CSSD = c(CSSD,CFCS$C.SE)
    MSSD = c(MSSD,CFMS$C.SE)
    USSD = c(USSD,CFUS$C.SE)
    CSSS = c(CSSS,CFCS$Size)
    MSSS = c(MSSS,CFMS$Size)
    USSS = c(USSS,CFUS$Size)
    
    CFSC = CFactor(D[which(D$FType==Type & D$Area2 %in% c(1)),])  
    CFDC = CFactor(D[which(D$FType==Type & D$Area2 %in% c(2)),])  
    SCCF = c(SCCF,CFSC$CFactor)
    DCCF = c(DCCF,CFDC$CFactor)
    SCSD = c(SCSD,CFSC$C.SE)
    DCSD = c(DCSD,CFDC$C.SE)
    SCSS = c(SCSS,CFSC$Size)
    DCSS = c(DCSS,CFDC$Size)
    
    CFHS = CFactor(D[which(D$FType==Type & D$Gi_Bin %in% c(3,2)),])  
    CFNS = CFactor(D[which(D$FType==Type & D$Gi_Bin %in% c(-3,-2,-1,0,1)),])  
    HSCF = c(HSCF,CFHS$CFactor)
    NSCF = c(NSCF,CFNS$CFactor)
    HSSD = c(HSSD,CFHS$C.SE)
    NSSD = c(NSSD,CFNS$C.SE)
    HSSS = c(HSSS,CFHS$Size)
    NSSS = c(NSSS,CFNS$Size)
  }
  DF = data.frame(SWCF,SWSD,SWSS,CSCF,CSSD,CSSS,MSCF,MSSD,MSSS,USCF,USSD,USSS,DCCF,DCSD,DCSS,SCCF,SCSD,SCSS,HSCF,HSSD,HSSS,NSCF,NSSD,NSSS)
  rownames(DF) = unique(D$FType)
  return(DF)
}
CFAllAreasPlot = function(D,Types,Area,ConfLevel=0.95){
  Plot1 = function(D,Types,Area,ConfLevel){
    xr=c(0,length(Types)+1)
    xv = 1:length(Types)
    yr = c(-1,4)
    D = D[Types,]
    Color = c('black','blue','red','green4','orange','brown','green','purple')
    j = 1
    CF = D$SWCF
    CFUB = D$SWCF+qt((1+ConfLevel)/2,D$SWSS)*D$SWSD 
    CFLB = D$SWCF-qt((1+ConfLevel)/2,D$SWSS)*D$SWSD
    plot(CF,xaxt='n',ylab='Calibration Factor',xlab='',ylim=yr,xlim=xr,col=Color[j])
    grid()
    par(new=T);plot(CFUB,xaxt='n',yaxt='n',ylab='',xlab='',ylim=yr,pch=24,xlim=xr,col=Color[j])
    par(new=T);plot(CFLB,xaxt='n',yaxt='n',ylab='',xlab='',ylim=yr,pch=25,xlim=xr,col=Color[j])
    segments(x0=xv,x1=xv,y0=CFLB,y1=CFUB,col=Color[j])
    axis(1,at=xv,labels=rownames(D),las=2)
    
    if(Area==1){
      j = 2
      i = 0.15
      CF = D$CSCF
      CFUB = D$CSCF+qt((1+ConfLevel)/2,D$CSSS)*D$CSSD 
      CFLB = D$CSCF-qt((1+ConfLevel)/2,D$CSSS)*D$CSSD
      par(new=T);plot(x=xv+i,y=CF,xaxt='n',ylab='Calibration Factor',xlab='',ylim=yr,xlim=xr,col=Color[j])
      par(new=T);plot(x=xv+i,y=CFUB,xaxt='n',yaxt='n',ylab='',xlab='',ylim=yr,pch=24,xlim=xr,col=Color[j])
      par(new=T);plot(x=xv+i,y=CFLB,xaxt='n',yaxt='n',ylab='',xlab='',ylim=yr,pch=25,xlim=xr,col=Color[j])
      segments(x0=xv+i,x1=xv+i,y0=CFLB,y1=CFUB,col=Color[j])
      
      j = 3
      i = 0.3
      CF = D$MSCF
      CFUB = D$MSCF+qt((1+ConfLevel)/2,D$MSSS)*D$MSSD 
      CFLB = D$MSCF-qt((1+ConfLevel)/2,D$MSSS)*D$MSSD
      par(new=T);plot(x=xv+i,y=CF,xaxt='n',ylab='Calibration Factor',xlab='',ylim=yr,xlim=xr,col=Color[j])
      par(new=T);plot(x=xv+i,y=CFUB,xaxt='n',yaxt='n',ylab='',xlab='',ylim=yr,pch=24,xlim=xr,col=Color[j])
      par(new=T);plot(x=xv+i,y=CFLB,xaxt='n',yaxt='n',ylab='',xlab='',ylim=yr,pch=25,xlim=xr,col=Color[j])
      segments(x0=xv+i,x1=xv+i,y0=CFLB,y1=CFUB,col=Color[j])
      
      j = 4
      i = 0.45
      CF = D$USCF
      CFUB = D$USCF+qt((1+ConfLevel)/2,D$USSS)*D$USSD 
      CFLB = D$USCF-qt((1+ConfLevel)/2,D$USSS)*D$USSD
      par(new=T);plot(x=xv+i,y=CF,xaxt='n',ylab='Calibration Factor',xlab='',ylim=yr,xlim=xr,col=Color[j])
      par(new=T);plot(x=xv+i,y=CFUB,xaxt='n',yaxt='n',ylab='',xlab='',ylim=yr,pch=24,xlim=xr,col=Color[j])
      par(new=T);plot(x=xv+i,y=CFLB,xaxt='n',yaxt='n',ylab='',xlab='',ylim=yr,pch=25,xlim=xr,col=Color[j])
      segments(x0=xv+i,x1=xv+i,y0=CFLB,y1=CFUB,col=Color[j])
      legend('topleft',c('State-wide CFs','Coastal CFs','Midstate CFs','Upstate CFs'),text.col=Color)
    }
    
    if(Area==2){
      j = 2
      i = +0.2
      CF = D$SCCF
      CFUB = D$SCCF+qt((1+ConfLevel)/2,D$SCSS)*D$SCSD 
      CFLB = D$SCCF-qt((1+ConfLevel)/2,D$SCSS)*D$SCSD
      par(new=T);plot(x=xv+i,y=CF,xaxt='n',ylab='Calibration Factor',xlab='',ylim=yr,xlim=xr,col=Color[j])
      par(new=T);plot(x=xv+i,y=CFUB,xaxt='n',yaxt='n',ylab='',xlab='',ylim=yr,pch=24,xlim=xr,col=Color[j])
      par(new=T);plot(x=xv+i,y=CFLB,xaxt='n',yaxt='n',ylab='',xlab='',ylim=yr,pch=25,xlim=xr,col=Color[j])
      segments(x0=xv+i,x1=xv+i,y0=CFLB,y1=CFUB,col=Color[j])
      
      j = 3
      i = +0.4
      CF = D$DCCF
      CFUB = D$DCCF+qt((1+ConfLevel)/2,D$DCSS)*D$DCSD 
      CFLB = D$DCCF-qt((1+ConfLevel)/2,D$DCSS)*D$DCSD
      par(new=T);plot(x=xv+i,y=CF,xaxt='n',ylab='Calibration Factor',xlab='',ylim=yr,xlim=xr,col=Color[j])
      par(new=T);plot(x=xv+i,y=CFUB,xaxt='n',yaxt='n',ylab='',xlab='',ylim=yr,pch=24,xlim=xr,col=Color[j])
      par(new=T);plot(x=xv+i,y=CFLB,xaxt='n',yaxt='n',ylab='',xlab='',ylim=yr,pch=25,xlim=xr,col=Color[j])
      segments(x0=xv+i,x1=xv+i,y0=CFLB,y1=CFUB,col=Color[j])
      legend('topleft',c('State-wide CFs','Sparse Counties CFs','Dense Counties CFs'),text.col=Color)
    }
    
    if(Area==3){
      j = 2
      i = +0.2
      CF = D$HSCF
      CFUB = D$HSCF+qt((1+ConfLevel)/2,D$HSSS)*D$HSSD 
      CFLB = D$HSCF-qt((1+ConfLevel)/2,D$HSSS)*D$HSSD
      par(new=T);plot(x=xv+i,y=CF,xaxt='n',ylab='Calibration Factor',xlab='',ylim=yr,xlim=xr,col=Color[j])
      par(new=T);plot(x=xv+i,y=CFUB,xaxt='n',yaxt='n',ylab='',xlab='',ylim=yr,pch=24,xlim=xr,col=Color[j])
      par(new=T);plot(x=xv+i,y=CFLB,xaxt='n',yaxt='n',ylab='',xlab='',ylim=yr,pch=25,xlim=xr,col=Color[j])
      segments(x0=xv+i,x1=xv+i,y0=CFLB,y1=CFUB,col=Color[j])
      
      j = 3
      i = +0.4
      CF = D$NSCF
      CFUB = D$NSCF+qt((1+ConfLevel)/2,D$NSSS)*D$NSSD 
      CFLB = D$NSCF-qt((1+ConfLevel)/2,D$NSSS)*D$NSSD
      par(new=T);plot(x=xv+i,y=CF,xaxt='n',ylab='Calibration Factor',xlab='',ylim=yr,xlim=xr,col=Color[j])
      par(new=T);plot(x=xv+i,y=CFUB,xaxt='n',yaxt='n',ylab='',xlab='',ylim=yr,pch=24,xlim=xr,col=Color[j])
      par(new=T);plot(x=xv+i,y=CFLB,xaxt='n',yaxt='n',ylab='',xlab='',ylim=yr,pch=25,xlim=xr,col=Color[j])
      segments(x0=xv+i,x1=xv+i,y0=CFLB,y1=CFUB,col=Color[j])
      legend('topleft',c('State-wide CFs','Hot spots CFs','Not Significant Areas CFs'),text.col=Color)
    }
  }
  SaveImage('SCSEL',paste('CF Area',Area),'png')
  Plot1(D,Types,Area,ConfLevel)
  dev.off()
  Plot1(D,Types,Area,ConfLevel)
}
PlotAreaRDBox = function(CFAreaTab){
  Plot1 = function(CFAreaTab){
    xr = c(0,8)
    yr = c(0,1)
    a1=NULL;for(i in 1:length(CFAreaTab$SWCF)){a1=c(a1,abs(CFAreaTab$SWCF-CFAreaTab$CSCF)/CFAreaTab$SWCF)}
    a2=NULL;for(i in 1:length(CFAreaTab$SWCF)){a2=c(a2,abs(CFAreaTab$SWCF-CFAreaTab$MSCF)/CFAreaTab$SWCF)}
    a3=NULL;for(i in 1:length(CFAreaTab$SWCF)){a3=c(a3,abs(CFAreaTab$SWCF-CFAreaTab$USCF)/CFAreaTab$SWCF)}
    a4=NULL;for(i in 1:length(CFAreaTab$SWCF)){a4=c(a4,abs(CFAreaTab$SWCF-CFAreaTab$SCCF)/CFAreaTab$SWCF)}
    a5=NULL;for(i in 1:length(CFAreaTab$SWCF)){a5=c(a5,abs(CFAreaTab$SWCF-CFAreaTab$DCCF)/CFAreaTab$SWCF)}
    a6=NULL;for(i in 1:length(CFAreaTab$SWCF)){a6=c(a6,abs(CFAreaTab$SWCF-CFAreaTab$NSCF)/CFAreaTab$SWCF)}
    a7=NULL;for(i in 1:length(CFAreaTab$SWCF)){a7=c(a7,abs(CFAreaTab$SWCF-CFAreaTab$HSCF)/CFAreaTab$SWCF)}
    boxplot(a1,xlim=xr,ylim=yr,at=1,xaxt='n',xlab = '',ylab='Relative Difference',border = colors()[373])
    par(new=T);boxplot(a2,xlim=xr,ylim=yr,at=2,yaxt='n',xaxt='n',border = colors()[456])
    par(new=T);boxplot(a3,xlim=xr,ylim=yr,at=3,yaxt='n',xaxt='n',border = colors()[614])
    par(new=T);boxplot(a4,xlim=xr,ylim=yr,at=4,yaxt='n',xaxt='n',border = colors()[385])
    par(new=T);boxplot(a5,xlim=xr,ylim=yr,at=5,yaxt='n',xaxt='n',border = colors()[461])
    par(new=T);boxplot(a6,xlim=xr,ylim=yr,at=6,yaxt='n',xaxt='n',border = colors()[142])
    par(new=T);boxplot(a7,xlim=xr,ylim=yr,at=7,yaxt='n',xaxt='n',border = colors()[552])
    #par(new=T);plot(x=1:3,y=c(mean(a1),mean(a2),mean(a3)),xlim=c(0,4),ylim=c(0,1),col='red',type='b',xaxt='n',yaxt='n')
    axis(1,at=1:7,labels=c('Coastal','Midstate','Upstate','Sparse Co.','Dense Co.','Not Sign.','Hot spots'),las=1)
    abline(h=.2,lty=2,col='blue')
    legend('topleft',legend = '20% Threshold',lty=2,col='blue',text.col = 'blue')
  }
  SaveImage(Scope,paste('RD CF Plot'),'png')
  Plot1(CFAreaTab)
  dev.off()
  Plot1(CFAreaTab)
}
CFProxyTab = function(){
  Predicted = function(Type,AADTMaj,AADTMin,Length){
    if(Type == 'R2U'){
      return(exp(-0.312)*365/10^6*AADTMaj*Length)
    }
    if(Type == 'R4D'){
      return(exp(-9.653+1.176*log(AADTMaj)+log(Length)))
    }
    if(Type == 'R4U'){
      return(exp(-9.025+1.049*log(AADTMaj)+log(Length)))
    }
    if(Type == 'U2U'){
      return(exp(-15.22+1.68*log(AADTMaj)+log(Length))+exp(-5.47+0.56*log(AADTMaj)+log(Length)))
    }
    if(Type == 'U3T'){
      return(exp(-12.4+1.41*log(AADTMaj)+log(Length))+exp(-5.74+0.54*log(AADTMaj)+log(Length)))
    }
    if(Type == 'U4U'){
      return(exp(-11.63+1.33*log(AADTMaj)+log(Length))+exp(-7.99+0.81*log(AADTMaj)+log(Length)))
    }
    if(Type == 'U4D'){
      return(exp(-12.34+1.36*log(AADTMaj)+log(Length))+exp(-5.05+0.47*log(AADTMaj)+log(Length)))
    }
    if(Type == 'U5T'){
      return(exp(-9.7+1.17*log(AADTMaj)+log(Length))+exp(-4.82+0.54*log(AADTMaj)+log(Length)))
    }
    if(Type == 'R3ST'){
      return(exp(-9.86+0.79*log(AADTMaj)+0.49*log(AADTMin))*Length)
    }
    if(Type == 'R4ST'){
      return(exp(-8.56+0.60*log(AADTMaj)+0.61*log(AADTMin))*Length)
    }
    if(Type == 'R4SG'){
      return(exp(-5.13+0.60*log(AADTMaj)+0.20*log(AADTMin))*Length)
    }
    if(Type == 'RM3ST'){
      return(exp(-12.526+1.204*log(AADTMaj)+0.236*log(AADTMin))*Length)
    }
    if(Type == 'RM4ST'){
      return(exp(-10.008+0.848*log(AADTMaj)+0.448*log(AADTMin))*Length)
    }
    if(Type == 'RM4SG'){
      return(exp(-7.182+0.722*log(AADTMaj)+0.337*log(AADTMin))*Length)
    }
    if(Type == 'U3ST'){
      return(exp(-13.36+1.11*log(AADTMaj)+0.41*log(AADTMin))*Length)
    }
    if(Type == 'U4ST'){
      return(exp(-8.90+0.82*log(AADTMaj)+0.25*log(AADTMin))*Length)
    }
    if(Type == 'U3SG'){
      return(exp(-12.13+1.11*log(AADTMaj)+0.26*log(AADTMin))*Length)
    }
    if(Type == 'U4SG'){
      return(exp(-10.99+1.07*log(AADTMaj)+0.23*log(AADTMin))*Length)
    }
    else(return(1))
  }
  DS = Text2Dat('SCALL','AllSeg',2014,'Var',c('FType','Length','AADT','TOT_OC','Area1','Area2','Area3'))
  DI = Text2Dat('SCALL','AllInt',2014,'Var',c('FType','AADT_Major','AADT_Minor','TOT_OC','Area1','Area2','Area3'))
  SegTypes = c('R2U','R4D','R4U','U2U','U3T','U4U','U4D','U5T')
  IntTypes = c('R3ST'  ,'R4ST'  ,'R4SG'  ,'RM3ST'  ,'RM4ST'  ,'RM4SG'  ,'U3ST'  ,'U4ST'  ,'U3SG'  ,'U4SG')
  AreaLabel = c('CS','MS','US','SC','DC','NS','HS')
  AreaField = c('Area1','Area1','Area1','Area2','Area2','Area3','Area3')
  AreaVal   = c(1,2,3,2,1,0,1)
  AADTMaj = NULL; AADTMin = NULL; Length = NULL; Pred = NULL; Obs = NULL
  for(Type in SegTypes){
    AADTMaj = c(AADTMaj,sum(exp(DS[which(DS$FType == Type),'AADT'])*exp(DS[which(DS$FType == Type),'Length']))/sum(exp(DS[which(DS$FType == Type),'Length'])))
    AADTMin = c(AADTMin,0)
    Length  = c(Length,sum(exp(DS[which(DS$FType == Type),'Length'])))
    Pred    = c(Pred,Predicted(Type,sum(exp(DS[which(DS$FType == Type),'AADT'])*exp(DS[which(DS$FType == Type),'Length']))/sum(exp(DS[which(DS$FType == Type),'Length'])),NA,sum(exp(DS[which(DS$FType == Type),'Length']))))
    Obs     = c(Obs,sum((DS[which(DS$FType == Type),'TOT_OC'])))
  }
  for(Type in IntTypes){
    AADTMaj = c(AADTMaj,mean(exp(DI[which(DI$FType == Type),'AADT_Major'])))
    AADTMin = c(AADTMin,mean(exp(DI[which(DI$FType == Type),'AADT_Minor'])))
    Length  = c(Length,length(DI[which(DI$FType == Type),1]))
    Pred    = c(Pred,Predicted(Type,mean(exp(DI[which(DI$FType == Type),'AADT_Major'])),mean(exp(DI[which(DI$FType == Type),'AADT_Minor'])),length(DI[which(DI$FType == Type),1])))
    Obs     = c(Obs,sum((DI[which(DI$FType == Type),'TOT_OC'])))
  }
  Out = data.frame(SW.AADT_Major=AADTMaj,SW.AADT_Minor=AADTMin,SW.Length=Length,SW.Pred = Pred,SW.Obs=Obs)
  rownames(Out) = c(SegTypes,IntTypes)
  for(i in 1:length(AreaLabel)){
    AADTMaj = NULL; AADTMin = NULL; Length = NULL; Pred = NULL; Obs = NULL
    for(Type in SegTypes){
      AADTMaj = c(AADTMaj,sum(exp(DS[which(DS$FType == Type & DS[,AreaField[i]] == AreaVal[i]),'AADT'])*exp(DS[which(DS$FType == Type & DS[,AreaField[i]] == AreaVal[i]),'Length']))/sum(exp(DS[which(DS$FType == Type & DS[,AreaField[i]] == AreaVal[i]),'Length'])))
      AADTMin = c(AADTMin,0)
      Length  = c(Length,sum(exp(DS[which(DS$FType == Type & DS[,AreaField[i]] == AreaVal[i]),'Length'])))
      Pred    = c(Pred,Predicted(Type,sum(exp(DS[which(DS$FType == Type & DS[,AreaField[i]] == AreaVal[i]),'AADT'])*exp(DS[which(DS$FType == Type & DS[,AreaField[i]] == AreaVal[i]),'Length']))/sum(exp(DS[which(DS$FType == Type & DS[,AreaField[i]] == AreaVal[i]),'Length'])),NA,sum(exp(DS[which(DS$FType == Type & DS[,AreaField[i]] == AreaVal[i]),'Length']))))
      Obs     = c(Obs,sum((DS[which(DS$FType == Type & DS[,AreaField[i]] == AreaVal[i]),'TOT_OC'])))
    }
    for(Type in IntTypes){
      AADTMaj = c(AADTMaj,mean(exp(DI[which(DI$FType == Type & DI[,AreaField[i]] == AreaVal[i]),'AADT_Major'])))
      AADTMin = c(AADTMin,mean(exp(DI[which(DI$FType == Type & DI[,AreaField[i]] == AreaVal[i]),'AADT_Minor'])))
      Length  = c(Length,length(DI[which(DI$FType == Type & DI[,AreaField[i]] == AreaVal[i]),1]))
      Pred    = c(Pred,Predicted(Type,mean(exp(DI[which(DI$FType == Type & DI[,AreaField[i]] == AreaVal[i]),'AADT_Major'])),mean(exp(DI[which(DI$FType == Type & DI[,AreaField[i]] == AreaVal[i]),'AADT_Minor'])),length(DI[which(DI$FType == Type & DI[,AreaField[i]] == AreaVal[i]),1])))
      Obs     = c(Obs,sum((DI[which(DI$FType == Type & DI[,AreaField[i]] == AreaVal[i]),'TOT_OC'])))
    }
    Out[paste(AreaLabel[i],'AADT_Major',sep='.')] = AADTMaj
    Out[paste(AreaLabel[i],'AADT_Minor',sep='.')] = AADTMin
    Out[paste(AreaLabel[i],'Length'    ,sep='.')] = Length
    Out[paste(AreaLabel[i],'Pred'      ,sep='.')] = Pred
    Out[paste(AreaLabel[i],'Obs'       ,sep='.')] = Obs
  }
  return(Out)
}
PlotCFProxy = function(CFp){
  xr = c(0,8)
  yr = c(0,0.9)
  SW.CF = CFp$SW.Obs/CFp$SW.Pred
  RS.CF = CFp$CS.Obs/CFp$CS.Pred
  boxplot(abs(SW.CF-RS.CF)/SW.CF,at = 1,xlim=xr,ylim=yr,xlab='Region Specific CFs',ylab='Relative Difference',xaxt='n')
  RS.CF = CFp$MS.Obs/CFp$MS.Pred
  par(new=T);boxplot(abs(SW.CF-RS.CF)/SW.CF,at = 2,xlim=xr,ylim=yr,xlab='',ylab='',xaxt='n',yaxt='n')
  RS.CF = CFp$US.Obs/CFp$US.Pred
  par(new=T);boxplot(abs(SW.CF-RS.CF)/SW.CF,at = 3,xlim=xr,ylim=yr,xlab='',ylab='',xaxt='n',yaxt='n')
  RS.CF = CFp$SC.Obs/CFp$SC.Pred
  par(new=T);boxplot(abs(SW.CF-RS.CF)/SW.CF,at = 4,xlim=xr,ylim=yr,xlab='',ylab='',xaxt='n',yaxt='n')
  RS.CF = CFp$DC.Obs/CFp$DC.Pred
  par(new=T);boxplot(abs(SW.CF-RS.CF)/SW.CF,at = 5,xlim=xr,ylim=yr,xlab='',ylab='',xaxt='n',yaxt='n')
  RS.CF = CFp$NS.Obs/CFp$NS.Pred
  par(new=T);boxplot(abs(SW.CF-RS.CF)/SW.CF,at = 6,xlim=xr,ylim=yr,xlab='',ylab='',xaxt='n',yaxt='n')
  RS.CF = CFp$HS.Obs/CFp$HS.Pred
  par(new=T);boxplot(abs(SW.CF-RS.CF)/SW.CF,at = 7,xlim=xr,ylim=yr,xlab='',ylab='',xaxt='n',yaxt='n')
  axis(1,at=1:7,las=1,labels=c('Coastal','Midstate','Upstate','Sparse Co.','Dense Co.','Not Sign.','Hot spots'))
  abline(h=.2,lty=2,col='blue')
  legend('topleft',legend = '20% Threshold',lty=2,col='blue',text.col = 'blue')
}
PlotCFvsCriticalDist = function(){
  AllSeg = c('R2U','U2U','U4U','U4D','U5T')
  AllInt = c('R3ST','R4ST','RM3ST','U3ST','U4ST','U3SG','U4SG')
  
  BGG1 = read.csv('Inputs/SC_BGG1.csv')
  BGG2 = read.csv('Inputs/SC_BGG1.csv')
  BGG3 = read.csv('Inputs/SC_BGG1.csv')
  BGG4 = read.csv('Inputs/SC_BGG1.csv')
  BGG5 = read.csv('Inputs/SC_BGG1.csv')
  BGG6 = read.csv('Inputs/SC_BGG1.csv')
  BGG7 = read.csv('Inputs/SC_BGG1.csv')
  Int2013 = read.csv('Inputs/Int_2013.csv')
  Int2014 = read.csv('Inputs/Int_2014.csv')
  Seg2013 = read.csv('Inputs/Seg_2013.csv')
  Seg2014 = read.csv('Inputs/Seg_2014.csv')
  Int = rbind(Int2013,Int2014)
  Seg = rbind(Seg2013,Seg2014)
  BGG = list(BGG1,BGG2,BGG3,BGG4,BGG5,BGG6,BGG7)
  xr = c(0,8)
  yr = c(0,1)
  for(i in 1:7){
    Test = NULL
    D = merge(BGG[[i]],Int,by='SOURCE_ID')
    D = D[,c('TOT_OC','TOT_PC','Gi_Bin.x','FType')]
    for(Type in AllInt){
      CFNS = CFactor(D[which(D$Gi_Bin.x<=1 & D$FType==Type),])
      CFHS = CFactor(D[which(D$Gi_Bin.x>1 & D$FType==Type),])
      Test = c(Test,NullTest(CFHS,CFNS))
    }
    D = merge(BGG[[i]],Seg,by='SOURCE_ID')
    D = D[,c('TOT_OC','TOT_PC','Gi_Bin.x','FType')]
    for(Type in AllSeg){
      CFNS = CFactor(D[which(D$Gi_Bin.x<=1 & D$FType==Type),])
      CFHS = CFactor(D[which(D$Gi_Bin.x>1 & D$FType==Type),])
      Test = c(Test,NullTest(CFHS,CFNS))
    }
    if(i!=1){par(new=T)}
    print(mean(Test))
    boxplot(Test,at=i,xlim=xr,ylim=yr,xaxt='n',yaxt='n')
  }
  
  
  
}

# Other: -------------------------------------------------------------------------------------------
ListPredictors_All = function(Type = NULL){
  AllInt= c('AADT_Major','AADT_Minor')
  R3ST  = c(AllInt,c('LIGHTING','LTL','RTL','SKEW1'))
  R4ST  = c(AllInt,c('LIGHTING','LTL','RTL','SKEW1','SKEW2'))
  R4SG  = c(AllInt,c('LIGHTING','LTL','RTL'))
  RM3ST = c(AllInt,c('LIGHTING','LTL','RTL','SKEW1'))
  RM4ST = c(AllInt,c('LIGHTING','LTL','RTL','SKEW1','SKEW2'))
  RM4SG = c(AllInt)
  U3ST = c(AllInt,c('LIGHTING','LTL','RTL'))
  U4ST = c(AllInt,c('LIGHTING','LTL','RTL'))
  U3SG = c(AllInt,c('LIGHTING','LTL','RTL','LTP1','LTP2','No_RTOR'))
  U4SG = c(AllInt,c('LIGHTING','LTL','RTL','LTP1','LTP2','LTP3','LTP4','No_RTOR'))
  
  AllSeg = c('AADT','Length') 
  R2U = c(AllSeg,c(           'Lane_Width','Shuold_Wid','RHR','DrwDens','HorCur','Grade'))
  R4U = c(AllSeg,c('LIGHTING','Lane_Width','Shuold_Wid','Grade'))
  R4D = c(AllSeg,c('LIGHTING','Lane_Width','Shuold_Wid','Median_Wid'))
  U2U = c(AllSeg,c('LIGHTING','OSPProp','FODensity','DrwDens'))
  U3T = c(AllSeg,c('LIGHTING'          ,'FODensity','DrwDens'))
  U4U = c(AllSeg,c('LIGHTING','OSPProp','FODensity','DrwDens'))
  U4D = c(AllSeg,c('LIGHTING'          ,'FODensity','Median_Wid','DrwDens'))
  U5T = c(AllSeg,c('LIGHTING'          ,'FODensity','DrwDens'))
  
  
  Out = list('R3ST'=R3ST,'R4ST'=R4ST,'R4SG'=R4SG,'RM3ST'=RM3ST,'RM4ST'=RM4ST,'RM4SG'=RM4SG,
             'U3ST'=U3ST,'U4ST'=U4ST,'U3SG'=U3SG,'U4SG'=U4SG,'AllInt'=AllInt,
             'R2U'=R2U,'R4D'=R4D,'R4U'=R4U,'U2U'=U2U,'U3T'=U3T,'U4D'=U4D,'U4U'=U4U,'U5T'=U5T)
  if(is.null(Type)){return(Out)}
  else(if(Type %in%  names(Out)){return(Out[Type][[1]])})
}
ListBaseValues_Old  = function(Type){
  D = data.frame('SG'=0,'TotalLanes'=2,'LEGS'=4,'PopDens'=log(300),'MedianID'=0)
  if(Type=='R3ST' ){D$LIGHTING=0;D$LTL=0;D$RTL=0;D$SKEW1=0}
  if(Type=='R4ST' ){D$LIGHTING=0;D$LTL=0;D$RTL=0;D$SKEW1=0;D$SKEW2=0}
  if(Type=='R4SG' ){D$LIGHTING=0;D$LTL=0;D$RTL=0}
  if(Type=='RM3ST'){D$LIGHTING=0;D$LTL=0;D$RTL=0;D$SKEW1=0}
  if(Type=='RM4ST'){D$LIGHTING=0;D$LTL=0;D$RTL=0;D$SKEW1=0;D$SKEW2=0}
  if(Type=='RM4SG'){}
  if(Type=='U3ST'){D$LIGHTING=0;D$LTL=0;D$RTL=0}
  if(Type=='U4ST'){D$LIGHTING=0;D$LTL=0;D$RTL=0}
  if(Type=='U3SG'){D$LIGHTING=0;D$LTL=0;D$RTL=0;D$LTP1=0;D$LTP2=0;D$No_RTOR=0}
  if(Type=='U4SG'){D$LIGHTING=0;D$LTL=0;D$RTL=0;D$LTP1=0;D$LTP2=0;D$LTP3=0;D$LTP4=0;D$No_RTOR=0}
  if(Type=='R2U'  ){             D$Lane_Width=12;D$Shuold_Wid=6;D$RHR=3;D$DrwDens=5;D$HorCur=0;D$Grade=0}
  if(Type=='R4U'  ){D$LIGHTING=0;D$Lane_Width=12;D$Shuold_Wid=6;D$Grade=0}
  if(Type=='R4D'  ){D$LIGHTING=0;D$Lane_Width=12;D$Shuold_Wid=8;D$Median_Wid=30}
  if(Type=='U2U'  ){D$LIGHTING=0;D$OSPProp=0;D$FODensity=0;D$DrwDens=0}
  if(Type=='U3T'  ){D$LIGHTING=0;D$OSPProp=0;D$FODensity=0;D$DrwDens=0}
  if(Type=='U4U'  ){D$LIGHTING=0;D$OSPProp=0;D$FODensity=0;D$DrwDens=0}
  if(Type=='U4D'  ){D$LIGHTING=0;D$OSPProp=0;D$FODensity=0;D$Median_Wid=15;D$DrwDens=0}
  if(Type=='U5T'  ){D$LIGHTING=0;D$OSPProp=0;D$FODensity=0;D$DrwDens=0}
  return(D)
  
}
GLM00 = function(D){
  M=(glm(TOT_OC ~ AADT*offset((Length)), data = D, family= negative.binomial(theta = 1000,link= "log")))
  v=predict.glm(M,data.frame('AADT'=8,'Length'=2),se.fit = TRUE)
  r=studres(M)
  M$coefficients %*% matrix(c(1,8))+2
  sqrt((matrix(c(1,8),1,2)%*%qr.solve(M$R))^2%*%rep(summary(M)$dispersion,2))
  
  return(M)
}

# CMFs: -----------------------
CMFRHR = function(Inputs){
  f = NULL
  for(i in 1:nrow(Inputs)){
    f[i]=exp(-0.6869+0.0668*Inputs$RHR[i])/exp(-0.4865)
  }
  return(f)
}
CMFDrivewayDensity = function(Inputs){
  f = NULL
  for(i in 1:nrow(Inputs)){
    f[i]= (0.322 + Inputs$DD[i]*(0.05 - .005*Inputs$AADT[i]))/(0.322 + 5*(0.05 - .005*Inputs$AADT[i]))
    if(f[i]<1){f[i]=1}
  }
  return(f)
}
ElementryEffects = function(Fun,ArgList){
  g = expand.grid(ArgList)
  n = nrow(g)
  k = ncol(g)
  m = NULL
  v = NULL
  for(i in 1:k){
    if(length(ArgList[i][[1]])>1){
      delta = ArgList[i][[1]][2]-ArgList[i][[1]][1]
      gd = g
      gd[,i]=g[,i]+delta
      d = (do.call(Fun,list(gd))-do.call(Fun,list(g)))/delta
      m = c(m,mean(d))
      v = c(v,var(d))
    }
  }  
  Out = data.frame(m,v)
  rownames(Out)=names(g)
  names(Out)=c('Mean','Variance')
  return(Out)
}

