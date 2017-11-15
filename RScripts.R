# Preparation: -------------------------------------------------------------------------------------------
rm(list=ls())
setwd('C:/Users/Mahdi/Dropbox/R Scripts')
source('RFunctions.R')

# General Variables
AllSeg = c('R4F','U4F','U6F')
AllSeg = c('R2U','R4D','R4U','U2U','U3T','U4U','U4D','U5T')
AllInt = c('R3ST'  ,'R4ST'  ,'R4SG'  ,'RM3ST'  ,'RM4ST'  ,'RM4SG'  ,'U3ST'  ,'U4ST'  ,'U3SG'  ,'U4SG')
AllCF = data.frame('CFactor'=rep(1,18),'CFactor SE' = rep(0,18));rownames(AllCF) = c(AllSeg,AllInt)

# Summary Tables: -------------------------------------------------------------------------------------------
# SC All Sites
Scope       = 'SCSEL'
SumYear     = 2014
CrashAssign = 'Var'

# NC All Sites
Scope       = 'NCALL'
SumYear     = 2013
CrashAssign = 'Var'

SegSum = SitesSummary(Scope,'AllSeg',CrashAssign,SumYear)
IntSum = SitesSummary(Scope,'AllInt',CrashAssign,SumYear)
CrsSum = CrashSummary(Scope,2013:2015)
write.csv(CrsSum,paste('Outputs/',Scope,'/CrashSum_',CrashAssign,'.csv',sep=''))

# Calibration: -------------------------------------------------------------------------------------------
# SC Selected Sites
Scope       = 'SCSEL'
CFYears     = c(2013:2015)
CrashAssign = 'Var'
CFDef       = 'HSM'

# NC Selected Sites
Scope       = 'NCSEL'
CFYears     = c(2013:2014)
CrashAssign = 'Var'
CFDef       = 'HSM'

AllSeg = c('R2U','R4D','R4U','U2U','U3T','U4U','U4D','U5T')
AllInt = c('R3ST'  ,'R4ST'  ,'R4SG'  ,'RM3ST'  ,'RM4ST'  ,'RM4SG'  ,'U3ST'  ,'U4ST'  ,'U3SG'  ,'U4SG')

AllCF = CFTable(Scope,c(AllSeg,AllInt),CFYears,c(0,0),CrashAssign,CFDef)
AllCF = CFTable(Scope,c('R4F','U4F','U6F'),CFYears,c(0,0),CrashAssign,CFDef)
write.csv(AllCF,paste('Outputs/',Scope,'/Sum_CF.csv',sep=''))


# Areas:
AllCF01 = CFTable(Scope,c(AllSeg,AllInt),CFYears,c(0,1),CrashAssign,CFDef)
AllCF02 = CFTable(Scope,c(AllSeg,AllInt),CFYears,c(0,2),CrashAssign,CFDef)
AllCF10 = CFTable(Scope,c(AllSeg,AllInt),CFYears,c(1,0),CrashAssign,CFDef)
AllCF20 = CFTable(Scope,c(AllSeg,AllInt),CFYears,c(2,0),CrashAssign,CFDef)
AllCF30 = CFTable(Scope,c(AllSeg,AllInt),CFYears,c(3,0),CrashAssign,CFDef)
AllCF00 = CFTable(Scope,c(AllSeg,AllInt),CFYears,c(0,0),CrashAssign,CFDef)
write.csv(AllCF00,paste('Outputs/',Scope,'/Sum_CF00_',CrashAssign,'.csv',sep=''))
write.csv(AllCF01,paste('Outputs/',Scope,'/Sum_CF01_',CrashAssign,'.csv',sep=''))
write.csv(AllCF02,paste('Outputs/',Scope,'/Sum_CF02_',CrashAssign,'.csv',sep=''))
write.csv(AllCF10,paste('Outputs/',Scope,'/Sum_CF10_',CrashAssign,'.csv',sep=''))
write.csv(AllCF20,paste('Outputs/',Scope,'/Sum_CF20_',CrashAssign,'.csv',sep=''))
write.csv(AllCF30,paste('Outputs/',Scope,'/Sum_CF30_',CrashAssign,'.csv',sep=''))

# Local SPFs: -------------------------------------------------------------------------------------------
# SC Selected Sites
Scope       = 'SCSEL'
CFYears     = c(2013:2015)
CrashAssign = 'Var'
SPFType     = 'Covar'

# NC Selected Sites
Scope       = 'NCSEL'
CFYears     = c(2007:2009)
CrashAssign = '250'
SPFType     = 'Covar'

Seg = NULL;for(Type in AllSeg){Seg = c(Seg,LocalSPF(Scope,Type ,CFYears,CrashAssign, SPFType,data.frame('CFactor'= AllCF[Type,]$CFactor)))}
Int = NULL;for(Type in AllInt){Int = c(Int,LocalSPF(Scope,Type ,CFYears,CrashAssign, SPFType,data.frame('CFactor'= AllCF[Type,]$CFactor)))}
write.csv(SPFTable(c(Int,Seg),SPFType),paste('Outputs/',Scope,'/Sum_SPF_',CrashAssign,'.csv',sep=''),row.names=F)

Eval = SPFEvaluate(c(Int,Seg),SPFType)
write.csv(Eval,paste('Outputs/',Scope,'/SPF_Eval_',CrashAssign,'.csv',sep=''))

D=data.frame(AADT_Major=log(seq(100,15000,50)),AADT_Minor=log(500),LIGHTING=0,LTL=0,RTL=0,SKEW1=0)
P=(predict.glm(M,D,se.fit = T))
x = exp(D$AADT_Major)
y = exp(P$fit)
y.h = exp(P$fit)+1.96*(P$se.fit)
y.l = exp(P$fit)-1.96*(P$se.fit)
yl = c(-.5,1.5)
plot.new()
plot(x,y,type='l',ylim=yl)
par(new=T)
plot(x,y.h,type='l',ylim=yl)
par(new=T)
plot(x,y.l,type='l',ylim=yl)
abline(h=0)
polygon(c(x, rev(x)), c(y.h, rev(y.l)),col = "grey30", border = NA)
mean(1.96*(P$se.fit)*2)

# PAPER 1: CF Variance: -------------------------------------------------------------------------------------------
#Bootstrap Histogram
PlotBootHist('SCSEL','R3ST',2013:2014,'250',10000)
PlotBootHist('SCSEL','R3ST',2013:2014,'250',100000)
PlotBootHistOC('SCSEL','R3ST',2013:2014,'250',10000)

AllSeg = c('R2U','R4D','R4U','U2U','U3T','U4U','U4D','U5T')
AllInt = c('R3ST'  ,'R4ST'  ,'R4SG'  ,'RM3ST'  ,'RM4ST'  ,'RM4SG'  ,'U3ST'  ,'U4ST'  ,'U3SG'  ,'U4SG')
SCCFEval = CFEvaluate('SCSEL',c(AllSeg,AllInt),2013:2015,'Var',CFDef='HSM',10000)
SCCFEval1 = CFEvaluate('SCSEL',c(AllSeg,AllInt),2013:2014,'Var',CFDef='HSM',100000)
SCCFEval2 = CFEvaluate('SCSEL',c(AllSeg,AllInt),2013:2014,'Var',CFDef='HSM',500000)
write.csv(SCCFEval,paste('Outputs/','SCSEL','/CF_Eval_','250','.csv',sep=''))
PlotCFSECompare('SCSEL',SCCFEval)
PlotCFReg(SCCFEval,T)

AllSeg = c('R4D','U2U','U3T','U4U','U4D','U5T')
AllInt = c('R3ST'  ,'R4ST'  ,'R4SG'  ,'RM3ST'  ,'RM4ST'  ,'RM4SG'  ,'U3ST'  ,'U4ST'  ,'U3SG'  ,'U4SG')
NCCFEval = CFEvaluate('NCSEL',c(AllSeg,AllInt),2007:2009,'250')
write.csv(NCCFEval,paste('Outputs/','NCSEL','/CF_Eval_','250','.csv',sep=''))
PlotCFSECompare('NCSEL',NCCFEval)
PlotCFReg(NCCFEval,F)

D = rbind(NCCFEval,SCCFEval)
D = (D[c('Sample.Size','Mean OC','Mean PC','se OC','se PC','CF','se CF','cv OC','cv PC','cv CF')])
D['Sum.Obs.Crash'] = D['Sample.Size'] * D['Mean OC']
D = (D[c('Sample.Size','cv OC','cv CF','Sum.Obs.Crash')])
names(D) = c('Sample.Size','C.V.Obs.Crash','C.V.Calib.Factor','Sum.Obs.Crash')
D = log(D)

plot((D))
cor(D)
PlotCFReg(rbind(NCCFEval,SCCFEval),F)

PlotBootHist('SCSEL','U4SG',2013:2014,'250',50000)
CF = read.csv('CF.csv')
cf = subset(CF,select=c('Sample.Size','cv.OC','cv.CF','cv.PC'))
names(cf) = c('Sample.Size','C.V.Obs.Crash','C.V.Calib.Factor','C.V.Prd.Crash')
plot(log(cf))
cor(log(cf))
D= exp(D)
m1 = lm(log(Sample.Size) ~ log(C.V.Obs.Crash) + log(C.V.Calib.Factor) , data=D)
m2 = lm(log(Sample.Size) ~ log(Sum.Obs.Crash) + log(C.V.Calib.Factor) , data=D)
m3 = lm(log(C.V.Calib.Factor) ~ log(C.V.Obs.Crash) + log(Sample.Size) , data=D)
m4 = lm(log(C.V.Calib.Factor) ~ log(Sum.Obs.Crash) + log(Sample.Size) , data=D)
summary(m1)
summary(m2)
summary(m3)
summary(m4)
Plot3DSSR(m1,D,LogScale = FALSE)
PlotResiduals(m1)

D = read.csv('Inputs/CFStates.csv')
RTab = CFStatesRateBoxPlot(D)
CFStatesCFBoxPlot(D,AllSeg)
CFStatesCFBoxPlot(D,AllInt)

PlotCVOC('SCSEL',c('R3ST','U4SG','RM4ST', 'U3ST', 'U4ST'),2013:2014,'Var')
PlotOCRate('SCSEL',c('R3ST','U4SG','RM4ST', 'U3ST', 'U4ST'),2013:2014,'Var')
PlotCVk()

L = ResampleCompare()
PlotSampleCompare(L[[1]],L[[2]],L[[3]],L[[4]])

# PAPER 2: CF Definition: ---------------------------------------------------
Scope       = 'SCSEL'
CFYears     = c(2013:2015)
CrashAssign = 'Var'
SPFType     = 'Base'
AllSeg = c('R2U','R4D','R4U','U2U','U3T','U4U','U4D','U5T')
AllInt = c('R3ST'  ,'R4ST'  ,'R4SG'  ,'RM3ST'  ,'RM4ST'  ,'RM4SG'  ,'U3ST'  ,'U4ST'  ,'U3SG'  ,'U4SG')
AllSeg = c('R4D','U2U','U3T','U4U','U4D','U5T')
AllInt = c('R3ST'  ,'R4ST'  ,'R4SG'   ,'RM4SG'  ,'U3ST'  ,'U4ST'  ,'U3SG'  ,'U4SG')
CFDefs = c('HSM','Fitted','LSE','Mehta','MLE')
CFDefs = c('HSM','LSE','Mehta','MLE')
GOFs   = c('SSE'                 ,'ASE'                      ,'LL'            ,'MAD'                   ,'Dev')
GOFDes = c('Sum of Squared Errors','Absolute of Sum of Errors','Log Likelihood','Mean Absolute Deviance','Deviance')
names(GOFDes) = GOFs

CFComp = CFDefCompare (Scope, c(AllSeg,AllInt), CFYears, CrashAssign, SPFType, CFDefs)
write.csv(CFComp,paste('Outputs/',Scope,'/CF_Comp_',CrashAssign,'.csv',sep=''))
CFComp = read.csv(paste('Outputs/',Scope,'/CF_Comp_',CrashAssign,'.csv',sep=''));rownames(CFComp)=CFComp[,'X']

CFSenList = NULL
for(Type in c(AllSeg,AllInt)){
  CFs = NULL
  for(cfdef in CFDefs){
    CFs = c(CFs,CFComp[Type,paste(cfdef,'CF',sep='.')])
  }
  cf = c(0.05,2.5*max(CFs))
  CF = c(seq(cf[1],cf[2],length.out=15),CFs)
  CFSens = CFRange(Scope, Type, CFYears, CrashAssign, SPFType, CF[order(CF)],GOFs)
  N = names(CFSenList)
  CFSenList = c(CFSenList,list(CFSens))
  names(CFSenList) = c(N,Type)
  CFCUREPlot(Scope, Type, CFYears, CrashAssign, SPFType, CFComp, CFDefs)
  CFPlot = c(seq(cf[1],cf[2],length.out=150),CFs)
  CFMLEPlot(Scope, Type, CFYears, CrashAssign,CFPlot[order(CFPlot)])
  for(gof in GOFs){
    PlotCFComp(Type, CFSens, CFComp, CFDefs,gof,GOFDes[gof][[1]])
  }
}
rm(Type,CFs,cfdef,cf,N,gof,CF)

# Figure Mean Deviance
PlotMeanDev(Scope,CFComp,CFDefs[which(CFDefs!='Srini1')])

#Figure LL
Type = 'R4D'
gof = 'LL'
CFs = NULL
for(cfdef in CFDefs){
  CFs = c(CFs,CFComp[Type,paste(cfdef,'CF',sep='.')])
}
cf = c(0.5,2)
CF = c(seq(cf[1],cf[2],length.out=15),CFs)
CFSens = CFRange(Scope, Type, CFYears, CrashAssign, SPFType, CF[order(CF)],GOFs)
PlotCFComp(Type, CFSens, CFComp, CFDefs,gof,GOFDes[gof][[1]])

#Figure Dev LL
PlotMeanLL(Scope, Type, CFYears, CrashAssign, SPFType,CFComp,CFDefs[which(CFDefs!='Srini1')])

#Figure MAD
Type = 'R4D'
gof = 'SSE'
CFs = NULL
for(cfdef in CFDefs){
  CFs = c(CFs,CFComp[Type,paste(cfdef,'CF',sep='.')])
}
cf = c(0.2,2)
CF = c(seq(cf[1],cf[2],length.out=15),CFs)
CFSens = CFRange(Scope, Type, CFYears, CrashAssign, SPFType, CF[order(CF)],GOFs)
PlotCFComp(Type, CFSens, CFComp, CFDefs[which(CFDefs!='Srini1')],gof,GOFDes[gof][[1]])

#Figure Mean SSE
PlotMeanSSE(Scope,CFComp,CFDefs[which(CFDefs!='Srini1')])
PlotMeanCV(Scope,CFComp,CFDefs)

CFFunDef(Scope, c(AllSeg,AllInt), CFYears, CrashAssign, SPFType, GOFs)
MLE_LL = NULL
for(Type in c(AllSeg,AllInt)){
  MLE_LL = c(MLE_LL,CFSenList[Type][[1]][which(CFSenList[Type][[1]]['CF']==CFComp[Type,'MLE.CF']),]['HSM LL'][[1]])
}
a['MLELL']=MLE_LL
plot(a$'State LL',ylim=c(-3000,0),type='b')
par(new=T);plot(a$'CFun LL',ylim=c(-3000,0),type='b',col='blue')
par(new=T);plot(a$'MLELL',ylim=c(-3000,0),type='b',col='green4')
mean(a$'State LL')
mean(a$'CFun LL')
mean(a$'MLELL')


D = Text2Dat(Scope,'R3ST',CFYears,CrashAssign)
M = glm(TOT_OC ~ log(TOT_PC), data = D, family= negative.binomial(theta = 1000,link= "log"))
D['Cfun'] = exp(M$coef[[1]]+log(D$TOT_PC)*M$coef[[2]])
LogLike(D$TOT_OC,D$Cfun)
mean(D$Cfun)
D['CMLE'] = MLE_CF(D$TOT_OC,D$TOT_PC) * D$TOT_PC
mean(D$CMLE)
LogLike(D$TOT_OC,D$CMLE)
mean(D$TOT_OC)

a = CFFunDef(Scope, c(AllSeg,AllInt), CFYears, CrashAssign, SPFType, GOFs)

D = Text2Dat(Scope,'R3ST',CFYears,CrashAssign)
C = sum(D$TOT_OC)/sum(D$TOT_PC)
M = glm(TOT_OC ~ TOT_PC, data = D, family= negative.binomial(theta = 1000,link= "log"))
exp(M$coef[[1]])
C = mean(D$TOT_OC/D$TOT_PC)
summary(M)
(predict.glm(M,data.frame(TOT_PC=2),se.fit=T))

# PAPER 4: Region-Specific CFs: -------------------------------------------------------------------------------------------
Scope   = 'SCALL'
SPFType = 'GAADT'
CFYears = 2013:2014
CrashAssign = 'Var'
CFDef = 'HSM'
AllCF = CFTable('SCSEL',c(AllSeg,AllInt),CFYears,c(0,0),CrashAssign,CFDef)
#Seg = LocalSPF(Scope,'AllSeg' ,CFYears,CrashAssign, SPFType, data.frame('CFactor'= AllCF['AllSeg',]$CFactor))
#Int = LocalSPF(Scope,'AllInt' ,CFYears,CrashAssign, SPFType, data.frame('CFactor'= AllCF['AllInt',]$CFactor))
Seg = NULL;for(Type in AllSeg){Seg = c(Seg,LocalSPF(Scope,Type ,CFYears,CrashAssign, SPFType,data.frame('CFactor'= AllCF[Type,]$CFactor)))}
Int = NULL;for(Type in AllInt){Int = c(Int,LocalSPF(Scope,Type ,CFYears,CrashAssign, SPFType,data.frame('CFactor'= AllCF[Type,]$CFactor)))}
STab = SPFTable(c(Int,Seg),SPFType)
write.csv(STab,paste('Outputs/',Scope,'/Sum_SPF_',CrashAssign,'.csv',sep=''),row.names=F)

ExportFitted(Scope,CFYears,CrashAssign,c(Int,Seg))
ExportStudRes(Scope,CFYears,CrashAssign,c(Int,Seg))

D = ExampleData(1,1)
R  = GdStat    (D$Val,D$X,D$Y,1.9)
RS = GdStarStat(D$Val,D$X,D$Y,1.9)

BGH = read.csv('Inputs/BG_HSMRes.csv')
BGG = read.csv('Inputs/BG_GADTStudRes.csv')
CN = read.csv('Inputs/Counties.csv')
TR = read.csv('Inputs/Tracts.csv')
BK = read.csv('Inputs/Blocks.csv')
R  = GdStat    (BG$FitDif,BG$X,BG$Y,5280*10)
RS = GdStarStat(BG$FitDif,BG$X,BG$Y,5280*10)
BG['Z' ] = R$Z
BG['G' ] = R$G
BG['ZS'] = RS$Z
layout(matrix(c(1,2),T))
hist(BG$GiZScore)
hist(R$G)
MoranIPlot(CN,c(40,50,60))


library(spdep)

AllSeg = c('R2U','U2U','U4U','U4D','U5T')
AllInt = c('R3ST','R4ST','RM3ST','U3ST','U4ST','U3SG','U4SG')
Scope       = 'SCSEL'
Types = c(AllSeg,AllInt)
CFYears = 2013:2014
CrashAssign = 'Var'

AreaName = 'Area1'
Areas = c(1,2,3)
AreaLabels = c('Cos','Mid','Up')
CList1 = ComparisonMatrix(Scope,CFYears,c(AllSeg,AllInt),CrashAssign,AreaName,Areas,AreaLabels)

AreaName = 'Area2'
Areas = c(1,2)
AreaLabels = c('Up','Dens')
CList2 = ComparisonMatrix(Scope,CFYears,c(AllSeg,AllInt),CrashAssign,AreaName,Areas,AreaLabels)

AreaName = 'Area3'
Areas = c(0,1)
AreaLabels = c('Neut','Hot')
CList3 = ComparisonMatrix(Scope,CFYears,c(AllSeg,AllInt),CrashAssign,AreaName,Areas,AreaLabels)

PlotAreaBox(CList1,CList2,CList3)

CFAreaTab = CFAllAreasTab('SCSEL',2013:2014)
CFAllAreasPlot(CFAreaTab,c('R2U','U2U','U4U','U4D','U5T'),1)
CFAllAreasPlot(CFAreaTab,c('R2U','U2U','U4U','U4D','U5T'),2)
CFAllAreasPlot(CFAreaTab,c('R2U','U2U','U4U','U4D','U5T'),3)
CFAllAreasPlot(CFAreaTab,c('R3ST','R4ST','RM3ST','U3ST','U4ST','U3SG','U4SG'),1)
CFAllAreasPlot(CFAreaTab,c('R3ST','R4ST','RM3ST','U3ST','U4ST','U3SG','U4SG'),2)
CFAllAreasPlot(CFAreaTab,c('R3ST','R4ST','RM3ST','U3ST','U4ST','U3SG','U4SG'),3)

BGG1 = read.csv('Inputs/SC_BGG1.csv')
Int2013 = read.csv('Inputs/Int_2013.csv')

# PAPER 6:SPF Comaprison: -------------------------------------------------------------------------------------------
Scope       = 'SCSEL'
CFYears     = c(2013:2014)
CrashAssign = 'Var'
SPFType     = 'Covar'
CFDef = 'HSM'

AllCF = CFTable('SCSEL',c(AllSeg,AllInt),CFYears,c(0,0),CrashAssign,CFDef)

Seg     = NULL;for(Type in AllSeg){Seg     = c(Seg    ,LocalSPF(Scope,Type ,CFYears,CrashAssign, 'Covar',data.frame('CFactor'= AllCF[Type,]$CFactor)))}
SegAll  = NULL;for(Type in AllSeg){SegAll  = c(SegAll ,LocalSPF(Scope,Type ,CFYears,CrashAssign, 'All'  ,data.frame('CFactor'= AllCF[Type,]$CFactor)))}
#SegFull = NULL;for(Type in AllSeg){SegFull = c(SegFull,LocalSPF(Scope,Type ,CFYears,CrashAssign, 'Full' ,data.frame('CFactor'= AllCF[Type,]$CFactor)))}
SegSPF  = rbind(SPFTable(Seg,SPFType),SPFTable(SegAll,'All'))
# Intersections SPF
Int     = NULL;for(Type in AllInt){Int     = c(Int    ,LocalSPF(Scope,Type ,CFYears,CrashAssign, 'Covar',data.frame('CFactor'= AllCF[Type,]$CFactor)))}
IntAll  = NULL;for(Type in AllInt){IntAll  = c(IntAll ,LocalSPF(Scope,Type ,CFYears,CrashAssign, 'All'  ,data.frame('CFactor'= AllCF[Type,]$CFactor)))}
IntFull = NULL;for(Type in AllInt){IntFull = c(IntFull,LocalSPF(Scope,Type ,CFYears,CrashAssign, 'Full' ,data.frame('CFactor'= AllCF[Type,]$CFactor)))}
IntSPF  = rbind(SPFTable(Int,SPFType),SPFTable(Int,'All'))
# Write Summary Tables
write.csv(rbind(IntSPF,SegSPF),paste('Outputs/',Scope,'/Sum_SPF_',CrashAssign,'.csv',sep=''),row.names=F)
write.csv(AllCF,paste('Outputs/',Scope,'/Sum_CF_',CrashAssign,'.csv',sep=''))
Eval = rbind(SPFEvaluate(c(Int,Seg),SPFType),SPFEvaluate(c(IntAll,SegAll),'All'),SPFEvaluate(c(IntFull,SegFull),'Full'))
write.csv(Eval,paste('Outputs/',Scope,'/SPF_Eval_',CrashAssign,'.csv',sep=''))


U = read.csv('Inputs/SC_scrash_2014_unit.csv')
L = read.csv('Inputs/SC_scrash_2014_loc.csv')
O = read.csv('Inputs/SC_scrash_2014_occ.csv')

L  = L[which(L$MAC==10 & L$UNT==2),]
U  = U[which(U$ANO %in% L$ANO),]
UF = U[which(U$CTA =='N' & U$API==9),]
UR = U[which(U$CTA =='Y' & U$API==6),]
SelANO = unique(UR$ANO[which(UR$ANO %in% UF$ANO)])
UF = UF[which(UF$ANO %in% SelANO),]
UR = UR[which(UR$ANO %in% SelANO),]
L  = L [which(L$ANO %in% SelANO),]

# Safety Project: -----------------------------------------------------------------------
Mo = Moped(2010:2014)
write.csv(Mo,'Moped.csv')

D = PedNight(2007:2015)
write.csv(D$Sum,'PedSum.csv')
write.csv(data.frame(table(D$ALC)) ,'PedALC.csv')
write.csv(data.frame(table(D$RACP)),'PedRACP.csv')
write.csv(data.frame(table(D$RACD)),'PedRACD.csv')
write.csv(data.frame(table(D$FDAV)),'PedFDAV.csv')
write.csv(data.frame(table(D$APIP)),'PedAPIP.csv')
write.csv(data.frame(table(D$APIV)),'PedAPIV.csv')
write.csv(data.frame(table(D$ANO )),'PedANO.csv')
write.csv(data.frame(table(D$Rep )),'PedREPORT.csv')
write.csv(data.frame(table(D$REU )),'PedREU.csv')


# Crash Geocode Paper: -------------------------------------
D4  = read.csv("Inputs/SC_SCrash_2004_Loc.csv")
D5  = read.csv("Inputs/SC_SCrash_2005_Loc.csv")
D6  = read.csv("Inputs/SC_SCrash_2006_Loc.csv")
D7  = read.csv("Inputs/SC_SCrash_2007_Loc.csv")
D8  = read.csv("Inputs/SC_SCrash_2008_Loc.csv")
D9  = read.csv("Inputs/SC_SCrash_2009_Loc.csv")
D10 = read.csv("Inputs/SC_SCrash_2010_Loc.csv")
D11 = read.csv("Inputs/SC_SCrash_2011_Loc.csv")
D12 = read.csv("Inputs/SC_SCrash_2012_Loc.csv")
D13 = read.csv("Inputs/SC_SCrash_2013_Loc.csv")
D14 = read.csv("Inputs/SC_SCrash_2014_Loc.csv")
D15 = read.csv("Inputs/SC_SCrash_2015_Loc.csv")

D13 = D13[which(!is.na(D13$RCT)),]

sum(c(length(D4[,1]),length(D5[,1]),length(D6[,1]),length(D7[,1]),length(D8[,1]),length(D9[,1]),
      length(D10[,1]),length(D11[,1]),length(D12[,1]),length(D13[,1]),length(D14[,1]),length(D15[,1])))

365*24/sum(c(sum(D9$FAT),sum(D10$FAT),sum(D11$FAT),sum(D12$FAT),sum(D13$FAT),sum(D14$FAT),sum(D15$FAT)))*7
365*24*60/sum(c(sum(D9$INJ),sum(D10$INJ),sum(D11$INJ),sum(D12$INJ),sum(D13$INJ),sum(D14$INJ),sum(D15$INJ)))*7

N4  = read.csv("Inputs/New_Group_Layer_SC_Crash_2004_SPJ.csv")
N5  = read.csv("Inputs/New_Group_Layer_SC_Crash_2005_SPJ.csv")
N6  = read.csv("Inputs/New_Group_Layer_SC_Crash_2006_SPJ.csv")
N7  = read.csv("Inputs/New_Group_Layer_SC_Crash_2007_SPJ.csv")
N8  = read.csv("Inputs/New_Group_Layer_SC_Crash_2008_SPJ.csv")
N9  = read.csv("Inputs/New_Group_Layer_SC_Crash_2009_SPJ.csv")
N10 = read.csv("Inputs/New_Group_Layer_SC_Crash_2010_SPJ.csv")
N11 = read.csv("Inputs/New_Group_Layer_SC_Crash_2011_SPJ.csv")
N12 = read.csv("Inputs/New_Group_Layer_SC_Crash_2012_SPJ.csv")
N13 = read.csv("Inputs/New_Group_Layer_SC_Crash_2013_SPJ.csv")
N14 = read.csv("Inputs/New_Group_Layer_SC_Crash_2014_SPJ.csv")
N15 = read.csv("Inputs/New_Group_Layer_SC_Crash_2015_SPJ.csv")

O4HP  = D4 [which(Reduce('&',data.frame(!(D4$ANO  %in% N4$ANO ),substr(D4$JUR ,1,1)=='H'))),]
O5HP  = D5 [which(Reduce('&',data.frame(!(D5$ANO  %in% N5$ANO ),substr(D5$JUR ,1,1)=='H'))),]
O6HP  = D6 [which(Reduce('&',data.frame(!(D6$ANO  %in% N6$ANO ),substr(D6$JUR ,1,1)=='H'))),]
O7HP  = D7 [which(Reduce('&',data.frame(!(D7$ANO  %in% N7$ANO ),substr(D7$JUR ,1,1)=='H'))),]
O8HP  = D8 [which(Reduce('&',data.frame(!(D8$ANO  %in% N8$ANO ),substr(D8$JUR ,1,1)=='H'))),]
O9HP  = D9 [which(Reduce('&',data.frame(!(D9$ANO  %in% N9$ANO ),substr(D9$JUR ,1,1)=='H'))),]
O10HP = D10[which(Reduce('&',data.frame(!(D10$ANO %in% N10$ANO),substr(D10$JUR,1,1)=='H'))),]
O11HP = D11[which(Reduce('&',data.frame(!(D11$ANO %in% N11$ANO),substr(D11$JUR,1,1)=='H'))),]
O12HP = D12[which(Reduce('&',data.frame(!(D12$ANO %in% N12$ANO),substr(D12$JUR,1,1)=='H'))),]
O13HP = D13[which(Reduce('&',data.frame(!(D13$ANO %in% N13$ANO),substr(D13$JUR,1,1)=='H'))),]
O14HP = D14[which(Reduce('&',data.frame(!(D14$ANO %in% N14$ANO),substr(D14$JUR,1,1)=='H'))),]
O15HP = D15[which(Reduce('&',data.frame(!(D15$ANO %in% N15$ANO),substr(D15$JUR,1,1)=='H'))),]

N4HP  = N4 [which(N4$ANO  %in% D4 [which(substr(D4$JUR ,1,1) =='H'),'ANO']),'Nei30ft']
N5HP  = N5 [which(N5$ANO  %in% D5 [which(substr(D5$JUR ,1,1) =='H'),'ANO']),'Nei30ft']
N6HP  = N6 [which(N6$ANO  %in% D6 [which(substr(D6$JUR ,1,1) =='H'),'ANO']),'Nei30ft']
N7HP  = N7 [which(N7$ANO  %in% D7 [which(substr(D7$JUR ,1,1) =='H'),'ANO']),'Nei30ft']
N8HP  = N8 [which(N8$ANO  %in% D8 [which(substr(D8$JUR ,1,1) =='H'),'ANO']),'Nei30ft']
N9HP  = N9 [which(N9$ANO  %in% D9 [which(substr(D9$JUR ,1,1) =='H'),'ANO']),'Nei30ft']
N10HP = N10[which(N10$ANO %in% D10[which(substr(D10$JUR,1,1) =='H'),'ANO']),'Nei30ft']
N11HP = N11[which(N11$ANO %in% D11[which(substr(D11$JUR,1,1) =='H'),'ANO']),'Nei30ft']
N12HP = N12[which(N12$ANO %in% D12[which(substr(D12$JUR,1,1) =='H'),'ANO']),'Nei30ft']
N13HP = N13[which(N13$ANO %in% D13[which(substr(D13$JUR,1,1) =='H'),'ANO']),'Nei30ft']
N14HP = N14[which(N14$ANO %in% D14[which(substr(D14$JUR,1,1) =='H'),'ANO']),'Nei30ft']
N15HP = N15[which(N15$ANO %in% D15[which(substr(D15$JUR,1,1) =='H'),'ANO']),'Nei30ft']

D = data.frame("TOT_IS"    =c(length(N4[,1]),length(N5[,1]),length(N6[,1]),length(N7[,1]),length(N8[,1]),length(N9[,1]),length(N10[,1]),length(N11[,1]),length(N12[,1]),length(N13[,1]),length(N14[,1]),length(N15[,1])),
               "TOT"       =c(length(D4[,1]),length(D5[,1]),length(D6[,1]),length(D7[,1]),length(D8[,1]),length(D9[,1]),length(D10[,1]),length(D11[,1]),length(D12[,1]),length(D13[,1]),length(D14[,1]),length(D15[,1])),
               "TOT_HP_OS" =c(length(O4HP[,1]),length(O5HP[,1]),length(O6HP[,1]),length(O7HP[,1]),length(O8HP[,1]),length(O9HP[,1]),length(O10HP[,1]),length(O11HP[,1]),length(O12HP[,1]),length(O13HP[,1]),length(O14HP[,1]),length(O15HP[,1])),
               "TOT_HP_IS" =c(length(N4HP),length(N5HP),length(N6HP),length(N7HP),length(N8HP),length(N9HP),length(N10HP),length(N11HP),length(N12HP),length(N13HP),length(N14HP),length(N15HP)),
               "Ave_NB"    =c(mean(N4$Nei30ft),mean(N5$Nei30ft),mean(N6$Nei30ft),mean(N7$Nei30ft),mean(N8$Nei30ft),mean(N9$Nei30ft),mean(N10$Nei30ft),mean(N11$Nei30ft),mean(N12$Nei30ft),mean(N13$Nei30ft),mean(N14$Nei30ft),mean(N15$Nei30ft)),
               "Ave_NB_HP" =c(mean(N4HP),mean(N5HP),mean(N6HP),mean(N7HP),mean(N8HP),mean(N9HP),mean(N10HP),mean(N11HP),mean(N12HP),mean(N13HP),mean(N14HP),mean(N15HP)))
D['TOT_OS'] = D['TOT']-D['TOT_IS']
D['NAve_NB_HP'] = D["Ave_NB_HP"]/D["TOT_HP_IS"]*10^5
rownames(D)=2004:2015
x1 = 2004:2010
y1 = D[paste(x1),'Ave_NB_HP']
x2 = 2011:2015
y2 = D[paste(x2),'Ave_NB_HP']
M1 = lm(y1~x1)
M2 = lm(y2~x2)
xl = range(2004:2015)
yl = range(y1,y2)
plot.new()
plot(y=y1,x=x1,pch=20,col='blue',xlim=xl,ylim=yl,xlab='Year',ylab='Ave. Numb. of 30ft Ngb. HP in-state')
par(new=TRUE)
plot(y=y2,x=x2,pch=20,col='red',xlim=xl,ylim=yl,xlab='',ylab='')
abline(M1,col='blue')
abline(M2,col='red')
summary(M1)
summary(M2)

CList = list(read.csv("Inputs/SC_Crash_2004.csv"),
             read.csv("Inputs/SC_Crash_2005.csv"),
             read.csv("Inputs/SC_Crash_2006.csv"),
             read.csv("Inputs/SC_Crash_2007.csv"),
             read.csv("Inputs/SC_Crash_2008.csv"),
             read.csv("Inputs/SC_Crash_2009.csv"),
             read.csv("Inputs/SC_Crash_2010.csv"),
             read.csv("Inputs/SC_Crash_2011.csv"),
             read.csv("Inputs/SC_Crash_2012.csv"),
             read.csv("Inputs/SC_Crash_2013.csv"),
             read.csv("Inputs/SC_Crash_2014.csv"),
             read.csv("Inputs/SC_Crash_2015.csv"))
names(CList) =2004:2015
Area = 79887780071
Y = 2013:2014
p = length(Y)
N    = NULL
RA   = NULL
RO   = NULL
SRA  = NULL
SRA2 = NULL
SD = NULL
R = list()
for(y in Y){
  n = length(CList[paste(y)][[1]]['NEAR_DIST'][[1]])
  N = c(N,n)
  sra = sum(CList[paste(y)][[1]]['NEAR_DIST'][[1]])
  SRA = c(SRA,sra)
  RA = c(RA,sra/n)
  sra2 = sum(CList[paste(y)][[1]]['NEAR_DIST'][[1]]^2)
  SRA2 = c(SRA2,sra2)
  RO = c(RO,n/Area)
  SD = c(SD,2*sqrt(n/Area*var(CList[paste(y)][[1]]['NEAR_DIST'][[1]])/n))
  r = 2*sqrt(n/Area)*CList[paste(y)][[1]]['NEAR_DIST'][[1]]
  R = c(list(r),R)
}
rm(n,sra,sra2,y,r)
RE = 0.5/sqrt(RO)
names(R) = Y
names(N) = Y
names(SRA) = Y
names(SRA2) = Y
names(RA) = Y
names(RO) = Y
names(RE) = Y
a = sum(RO*SRA2)
b = sum(sqrt(RO)*SRA)/sum(N)
c = sum(RO*SRA^2/N)
F = (c-b)*(sum(N)-p)/(a-c)/(p-1)
pf(F,p-1,sum(N)-p)



t.test(R[paste(2013)][[1]],R[paste(2014)][[1]])

Sp = sqrt(sum((N-1)*SD)/(sum(N)-2))
t = (R[1]-R[2])/Sp/sqrt(1/N[1]+1/N[2])

PairTTest = function(CList,Y1,Y2,Area){
  D1 = CList[paste(Y1)][[1]]['NEAR_DIST'][[1]]
  D2 = CList[paste(Y2)][[1]]['NEAR_DIST'][[1]]
  R1 = 2*sqrt(length(D1)/Area)*D1
  R2 = 2*sqrt(length(D2)/Area)*D2
  Test = t.test(R1,R2)
  return(Test$p.value)
}
n = length(Y)
Res = NULL
for(y1 in Y){
  for(y2 in Y){
    Res = c(Res,PairTTest(CList,y1,y2,Area))  
  }
}
RES = matrix(Res,n,n)
PairTTest(CList,2004,2005,Area)
PairTTest(CList,2006,2005,Area)
PairTTest(CList,2007,2006,Area)
PairTTest(CList,2008,2007,Area)
PairTTest(CList,2009,2008,Area)
PairTTest(CList,2010,2009,Area)
PairTTest(CList,2010,2011,Area)
PairTTest(CList,2012,2013,Area)
PairTTest(CList,2013,2014,Area)
a=PairTTest(CList,2014,2015,Area)

# Crash Statistics for New Geocode Method: -------------

# Read input files
Years = 2013:2015
In = list()
All = list()
New = list()
for(y in Years){
  In  = c(In ,list(read.csv(paste("Inputs/Loc",y,"_Old.csv",sep=''))))
  All = c(All,list(read.csv(paste("Inputs/SC_SCrash_",y,"_Loc.csv",sep=''))))
  New = c(New,list(read.csv(paste("Inputs/Loc",y,".csv",sep=''))))
}
names(In) = Years
names(All) = Years
names(New) = Years

# Instate figure both new and old methods
LIn = NULL
LAll = NULL
LGeocoded = NULL
MLRS = NULL
LNewJoined = NULL
LOldJoined = NULL
for(y in Years){
  LAll = c(LAll,length(All[paste(y)][[1]][,1]))
  LIn  = c(LIn ,length(In [paste(y)][[1]][,1]))
  LGeocoded = c(LGeocoded,length(New[paste(y)][[1]][which(New[paste(y)][[1]]$GCXY>0),1]))
  LNewJoined = c(LNewJoined,length(New[paste(y)][[1]][which(New[paste(y)][[1]]$GMET>0),1]))
  LOldJoined  = c(LOldJoined ,length(In [paste(y)][[1]][which(In [paste(y)][[1]]$Join_Count>0),1]))
  MLRS = c(MLRS,length(New[paste(y)][[1]][which(New[paste(y)][[1]]$MLRS!=''),1]))
}
D = data.frame(LIn,LAll,LNew,LGeocoded,LNewJoined,LOldJoined,MLRS)
rownames(D) = Years
png(filename = 'Instate.png', width = 10   , height = 7   , units = "in", pointsize = 12              , res =200,bg='transparent' )
yr=c(0.6,1)
Col = c('blue','purple','red','orange4','red4')
Pch = c(1,2,3,4,5)
plot.new()
plot(x=Years,y=D$LIn/D$LAll,col=Col[1],ylim = yr,type='b',ylab='Instate Percentage',pch=Pch[1])
par(new=T)
plot(x=Years,y=D$LOldJoined/D$LAll,col=Col[2],ylim = yr,type='b',ylab='',xlab='',pch=Pch[2])
par(new=T)
plot(x=Years,y=D$MLRS/D$LAll,col=Col[3],ylim = yr,type='b',ylab='',xlab='',pch=Pch[3])
par(new=T)
plot(x=Years,y=D$LNew/D$LAll,col=Col[4],ylim = yr,type='b',ylab='',xlab='',pch=Pch[4])
par(new=T)
plot(x=Years,y=D$LNewJoined/D$LAll,col=Col[5],ylim = yr,type='b',ylab='',xlab='',pch=Pch[5])
legend(legend=c('Curr. Met. - Instate','Cur. Met. - Assigned to Routes','Prop. Met. MLRS Matched','Prop. Met. Instate','Prop. Met. Milepost Found'),x='topleft',text.col = Col,pch=Pch,col=Col)
grid()
dev.off()

# joined figure both new and old methods
#LIn = c(74896,74105,84603,83202,79559,77462,78097,90557,97395,114624,111508,128047)
LIn = NULL
LAll = NULL
LNew = NULL
for(y in Years){
  LAll = c(LAll,length(All[paste(y)][[1]][,1]))
  LNew = c(LNew,length(New[paste(y)][[1]][which(New[paste(y)][[1]]$GMET>0),1]))
  LIn  = c(LIn ,length(In [paste(y)][[1]][which(In [paste(y)][[1]]$Join_Count>0),1]))
}
D = data.frame(LIn,LAll,LNew)
rownames(D) = Years
png(filename = 'OutPer.png', width = 10   , height = 5   , units = "in", pointsize = 12              , res =200,bg='transparent' )
yr=c(0.6,1)
plot.new()
plot(x=Years,y=D$LIn/D$LAll,col='blue',ylim = yr,type='b',ylab='Assigned Crashes Percentage')
par(new=T)
plot(x=Years,y=D$LNew/D$LAll,col='red',ylim = yr,type='b',ylab='',xlab='')
legend(legend=c('Current Method','Proposed Method'),x='topleft',text.col = c('blue','red'))
grid()
dev.off()

# With MLRS figure
MLRS = NULL
for(y in Years){
  MLRS = c(MLRS,length(New[paste(y)][[1]][which(New[paste(y)][[1]]$MLRS==''),1]))
}
M = data.frame(MLRS)
rownames(M) = Years
yr=c(0.7,1)
png(filename = 'WithMLRS.png', width = 10   , height = 5   , units = "in", pointsize = 12              , res =200,bg='transparent' )
plot.new()
plot(x=Years,y=1-M$MLRS/D$LAll,ylim = yr,type='b',ylab='Crashes With MLRS (%)')
grid()
dev.off()

# LATLON dist table
NADYMin =	74263.2145
NADYMax =	1233475.5534

NADXMin =	1292515.4781
NADXMax	= 2744469.5352

WGSYMin =	32020134.0000
WGSYMax	= 35120775.0000

WGSXMin =	78321969.0000
WGSXMax =	83211274.0000
D = data.frame()
for(y in Years){
  if(y<2011){
    LAT = as.numeric(levels(All[paste(y)][[1]]$LAT))[All[paste(y)][[1]]$LAT]
    LON = as.numeric(levels(All[paste(y)][[1]]$LON))[All[paste(y)][[1]]$LON]
  }
  else{
    LAT = as.numeric(All[paste(y)][[1]]$LAT)
    LON = as.numeric(All[paste(y)][[1]]$LON)
  }
  Tot1 = length(LAT)
  NADY1 = length(LAT[which(LAT>=NADYMin & LAT<=NADYMax)])
  NADX1 = length(LAT[which(LAT>=NADXMin & LAT<=NADXMax)])
  WGSY1 = length(LAT[which(LAT>=WGSYMin & LAT<=WGSYMax)])
  WGSX1 = length(LAT[which(LAT>=WGSXMin & LAT<=WGSXMax)])
  Other1= length(LAT) - NADY1 - NADX1 - WGSY1 - WGSX1
  Tot2 = length(LON)
  NADY2 = length(LON[which(LON>=NADYMin & LON<=NADYMax)])
  NADX2 = length(LON[which(LON>=NADXMin & LON<=NADXMax)])
  WGSY2 = length(LON[which(LON>=WGSYMin & LON<=WGSYMax)])
  WGSX2 = length(LON[which(LON>=WGSXMin & LON<=WGSXMax)])
  Other2= length(LON) - NADY2 - NADX2 - WGSY2 - WGSX2
  D = rbind(D,data.frame(LAT.NADX = NADX1/Tot1,
                         LAT.NADY = NADY1/Tot1,
                         LAT.WGSX = WGSX1/Tot1,
                         LAT.WGSY = WGSY1/Tot1,
                         LAT.Other= Other1/Tot1,
                         LON.NADX = NADX2/Tot2,
                         LON.NADY = NADY2/Tot2,
                         LON.WGSX = WGSX2/Tot2,
                         LON.WGSy = WGSY2/Tot2,
                         LON.Other= Other2/Tot2))
}
rownames(D) = Years

# XY Comm dist
D = data.frame()
for(y in Years){
  GCXY = New[paste(y)][[1]]$GCXY
  Tot1 = length(GCXY)
  Out     = length(GCXY[which(GCXY==0)])
  NoMLRS  = length(GCXY[which(GCXY==1)])
  OffMLRS = length(GCXY[which(GCXY==2)])
  OnMLRS  = length(GCXY[which(GCXY==9)])
  D = rbind(D,data.frame(OutOFState = Out/Tot1,
                         NoMLRS = NoMLRS/Tot1,
                         OffMLRS = OffMLRS/Tot1,
                         OnMLRS = OnMLRS/Tot1))
}
rownames(D) = Years

# GMET dist
D = data.frame()
for(y in Years){
  GMET = New[paste(y)][[1]]$GMET
  Tot1 = length(GMET)
  Out  = length(GMET[which(GMET==0)])
  Mp   = length(GMET[which(GMET==5)])
  SC   = length(GMET[which(GMET==6)])
  DD   = length(GMET[which(GMET==7)])
  DMS  = length(GMET[which(GMET==8)])
  DMSM = length(GMET[which(GMET==9)])
  D = rbind(D,data.frame(NotGeocoded = Out/Tot1,
                         Milepost = Mp/Tot1,
                         StateCoordinate = SC/Tot1,
                         DD = DD/Tot1,
                         DMS = DMS/Tot1,
                         DMSM = DMSM/Tot1))
}
rownames(D) = Years

# MP Comm dist
D = data.frame()
for(y in Years){
  GCMP = New[paste(y)][[1]]$GCMP
  Tot1 = length(GCMP)
  NoMLRS  = length(GCMP[which(GCMP==1)])
  NoBLRS = length(GCMP[which(GCMP==2)])
  NoBI  = length(GCMP[which(GCMP==3)])
  FalseBDO = length(GCMP[which(GCMP==4)])
  OnlyBI = length(GCMP[which(GCMP==5)])
  BetBS  = length(GCMP[which(GCMP==9)])
  D = rbind(D,data.frame(NoMLRS = NoMLRS/Tot1,
                         NoBLRS = NoBLRS/Tot1,
                         NoBI = NoBI/Tot1,
                         FalseBDO = FalseBDO/Tot1,
                         OnlyBI = OnlyBI/Tot1,
                         BetBS = BetBS/Tot1))
}
rownames(D) = Years

# histogram of cross offset for DMS geocoded crashes
newco = NULL
for(y in Years){
  newco = c(newco, New[paste(y)][[1]][which(New[paste(y)][[1]][,'GMET'] %in% c(6,7,8)),'GDXY'])
}
inco = NULL
for(y in 2007:2015){
  inco = c(inco, In[paste(y)][[1]]['NEAR_DIST'][[1]])
}

png(filename = 'CrossOffset.png', width = 10   , height = 5   , units = "in", pointsize = 12              , res =200,bg='transparent' )
par(mfrow=c(1,2))
yr = c(0,600000)
hist(abs(newco),main='',xlab='Cross Offset (Feet)',ylim=yr)
dev.off()
png(filename = 'CrossOffset.png', width = 10   , height = 5   , units = "in", pointsize = 12              , res =200,bg='transparent' )
par(new=T)
hist(inco[which(inco>0)],main='',xlab='Cross Offset (Feet)',ylim = yr)
dev.off()
mean(inco[which(inco>0)])
mean(abs(newco))

# CMF Sensitivity Analysis: ----------------------------------
ArgList = list('DD'=seq(0,20,,20),'AADT'=seq(500,10000,,20))
n =  5;ElementryEffects(CMFRHR,list('RHR'=seq(1,5,,n)))
n = 20;ElementryEffects(CMFDrivewayDensity,list('DD'=seq(0,20,,n),'AADT'=seq(500,10000,,n)))

# Import ATR: ------------------
library(chron)
ATR = read.table('ATR.txt',sep=',',header = T)
ATR = ATR[with(ATR, order(ATR_NUM)), ]
P = NULL
S = NULL
MinV = NULL
MaxV = NULL
LenV = NULL
fs = NULL
ts = NULL
TL = NULL
for(f in 1:166){
  fn = paste('ATR Data/2015/',f,'.txt',sep='')
  D = read.table(fn,sep='\n')
  #site = as.numeric(substr(unique(as.character(D$V1[which(substr(D$V1,1,4)=='Site')])),7,10))
  site = f
  if(!(site %in% ATR$ATR_NUM)){fs = c(fs,site)}
  if( (site %in% ATR$ATR_NUM)){ts = c(ts,site)}
  if(site %in% ATR$ATR_NUM){
    totallanes = ATR[which(ATR$ATR_NUM %in% site),5]
    print(c(totallanes,site,which(ATR$ATR_NUM %in% site),ATR[which(ATR$ATR_NUM %in% site),5]))
    S = c(S,site)
    D = as.character(D$V1[which(substr(D$V1,3,3)==':')])
    L = strsplit(D,' ')
    h = NULL
    NV = NULL
    SV = NULL
    NS = NULL
    SS = NULL

    if(length(L)>0){
      for(i in 1:length(L)){
        if(length(L[[i]])==7){
          h = c(h,as.numeric(substr(L[[i]][1],1,2)))
          NV = c(NV,as.numeric(L[[i]][2]))
          NS = c(NS,as.numeric(L[[i]][4]))
          SV = c(SV,as.numeric(L[[i]][5]))
          SS = c(SS,as.numeric(L[[i]][7]))
        }
      }
    }
    V = c(NV,SV)
    VH = V[which(V>=1000*totallanes/2)]
    MaxV = c(MaxV,max(V))
    MinV = c(MinV,min(V))
    LenV = c(LenV,length(V))
    P = c(P,length(VH)/length(V))
    TL = c(TL,totallanes)
    #print(c(totallanes,min(V),max(V),length(VH),length(V)))
  }  
}
D = data.frame(S,LenV,MinV,MaxV,P,TL)

# Ped Approach:
Loc = list()
for(y in 2007:2015){
  Loc = c(Loc,list(read.csv(paste('Inputs/',y,'.csv',sep=''))))
}
names(Loc) = 2007:2015

for(y in 2007:2015){
  png(filename = paste('Hist_All_',y,'.png',sep=''), width = 10   , height = 5   , units = "in", pointsize = 12              , res =200,bg='transparent' )
  par(mfrow=c(1,3))
  d = Loc[paste(y)][[1]]
  hist(d$PedAppr,breaks=seq(1,4.25,0.25),xlab='Ped Approach Direction',main=paste('All Ped Crashes',y), xaxt="n")
  axis(label=c('Same','Opposite','Right','Left'),at=1:4,1)
  legend(legend=paste('Total: ',length(d$PedAppr)),'topright')
  #dev.off()

  #png(filename = paste('Hist_Night_',y,'.png',sep=''), width = 10   , height = 7   , units = "in", pointsize = 12              , res =200,bg='transparent' )
  d = d[which(d$ALC %in% c(4,5,6,7)),]
  hist(d$PedAppr,breaks=seq(1,4.25,0.25),xlab='Ped Approach Direction',main=paste('Night Ped Crashes',y), xaxt="n")
  axis(label=c('Same','Opposite','Right','Left'),at=1:4,1)
  legend(legend=paste('Total: ',length(d$PedAppr)),'topright')
  #dev.off()
  
  #png(filename = paste('Hist_Night_Fatal_',y,'.png',sep=''), width = 10   , height = 7   , units = "in", pointsize = 12              , res =200,bg='transparent' )
  d = d[which(d$FAT>0),]
  hist(d$PedAppr,breaks=seq(1,4.25,0.25),xlab='Ped Approach Direction',main=paste('Night Fatal Ped Crashes',y), xaxt="n")
  axis(label=c('Same','Opposite','Right','Left'),at=seq(1,4,1),1)
  legend(legend=paste('Total: ',length(d$PedAppr)),'topright')
  dev.off()
  
}

# MotorCycle Crashes:   ------------
Years = 2013:2015
Occ = list()
Unit = list()
Loc = list()
for(y in Years){
  Unit  = c(Unit ,list(read.csv(paste("Inputs/SC_SCrash_",y,"_Unit.csv",sep=''))))
  Occ  = c(Occ ,list(read.csv(paste("Inputs/SC_SCrash_",y,"_Occ.csv",sep=''))))
  Loc  = c(Loc,list(read.csv(paste("Inputs/SC_SCrash_",y,"_Loc.csv",sep=''))))
}
names(Unit) = Years
names(Occ) = Years
names(Loc) = Years

McycleOcc = list()
McycleUnit = list()
McycleLoc = list()
for(y in Years){
  L = Loc[paste(y)][[1]]
  ANO = L[,'ANO']
  if(is.factor(L$ANO)){
    ANO = as.numeric(levels(L[,'ANO']))[L[,'ANO']]
  }
  L$ANO = ANO
  U = Unit[paste(y)][[1]]
  ANO = U[,'ANO']
  if(is.factor(U$ANO)){
    ANO = as.numeric(levels(U[,'ANO']))[U[,'ANO']]
  }
  U$ANO = ANO
  AUN = U[,'AUN']
  U['ID'] = paste(ANO,AUN,sep='')
  O = Occ[paste(y)][[1]]
  ANO = O[,'ANO']
  if(is.factor(O$ANO)){
    ANO = as.numeric(levels(O[,'ANO']))[O[,'ANO']]
  }
  O$ANO = ANO
  AUN = O[,'AUN']
  O['ID'] = paste(ANO,AUN,sep='')
  McycleID = U[which(U$UTC==25),"ID"]
  McycleANO = U[which(U$UTC==25),"ANO"]
  McycleOcc = c(McycleOcc,list(O[which(O$ID %in% McycleID),]))
  McycleUnit = c(McycleUnit,list(U[which(U$ID %in% McycleID),]))
  McycleLoc = c(McycleLoc,list(U[which(L$ANO %in% McycleANO),]))
}
names(McycleUnit) = Years
names(McycleLoc) = Years
names(McycleOcc) = Years

Res = data.frame(Tot = rep(0,12),Hel = rep(0,12),NHel = rep(0,12),Unk = rep(0,12))
rownames(Res) = c('Tot','DUIT','SPD','AGE','BLK','WTE','OTH','K','A','B','C','O')
for(y in Years){
  O = McycleOcc[paste(y)][[1]]
  U = McycleUnit[paste(y)][[1]]
  L = McycleLoc[paste(y)][[1]]
  
  Res$Tot[1] = Res$Tot[1] + length(O[,1])
  Res$Hel[1] = Res$Hel[1] + length(O[which(O$REU==31),1])
  Res$NHel[1] = Res$NHel[1] + length(O[which(O$REU==0),1])
  Res$Unk[1] = Res$Unk[1] + length(O[,1])-(length(O[which(O$REU==31),1])+length(O[which(O$REU==0),1]))
  
  UDUI = U[which(!is.na(U$DTR)),]
  ODUI = O[which(O$ID %in% UDUI$ID),]
  Res$Tot[2] = Res$Tot[2] + length(ODUI[,1])
  Res$Hel[2] = Res$Hel[2] + length(ODUI[which(ODUI$REU==31),1])
  Res$NHel[2] = Res$NHel[2] + length(ODUI[which(ODUI$REU==0),1])
  Res$Unk[2] = Res$Unk[2] + length(ODUI[,1])-(length(ODUI[which(ODUI$REU==31),1])+length(ODUI[which(ODUI$REU==0),1]))
  
  LSPD = L[which(L$PRC==3),]
  ANOSPD = LSPD$ANO
  USPD = U[which(U$ECS>U$SPL),]
  ANOSPD = c(ANOSPD,USPD$ANO)
  IDSPD = U[which(U$ANO %in% ANOSPD),'ID']
  OSPD = O[which(O$ID %in% IDSPD),]
  Res$Tot[3] = Res$Tot[3] + length(OSPD[,1])
  Res$Hel[3] = Res$Hel[3] + length(OSPD[which(OSPD$REU==31),1])
  Res$NHel[3] = Res$NHel[3] + length(OSPD[which(OSPD$REU==0),1])
  Res$Unk[3] = Res$Unk[3] + length(OSPD[,1])-(length(OSPD[which(OSPD$REU==31),1])+length(OSPD[which(OSPD$REU==0),1]))

  OAGE = O[which(!is.na(O$AGE)),]
  Res$Tot[4] = Res$Tot[4] + mean(OAGE$AGE)
  Res$Hel[4] = Res$Hel[4] + mean(OAGE[which(OAGE$REU==31),'AGE'])
  Res$NHel[4] = Res$NHel[4] + mean(OAGE[which(OAGE$REU==0),'AGE'])
  Res$Unk[4] = Res$Unk[4] + mean(OAGE[which(!(OAGE$REU==31 | OAGE$REU==0)),'AGE'])
  
  OBLK = O[which(O$ORAC=='B'),]
  Res$Tot[5] = Res$Tot[5] + length(OBLK[,1])
  Res$Hel[5] = Res$Hel[5] + length(OBLK[which(OBLK$REU==31),1])
  Res$NHel[5] = Res$NHel[5] + length(OBLK[which(OBLK$REU==0),1])
  Res$Unk[5] = Res$Unk[5] + length(OBLK[,1])-(length(OBLK[which(OBLK$REU==31),1])+length(OBLK[which(OBLK$REU==0),1]))
  
  OWTE = O[which(O$ORAC=='W'),]
  Res$Tot[6] = Res$Tot[6] + length(OWTE[,1])
  Res$Hel[6] = Res$Hel[6] + length(OWTE[which(OWTE$REU==31),1])
  Res$NHel[6] = Res$NHel[6] + length(OWTE[which(OWTE$REU==0),1])
  Res$Unk[6] = Res$Unk[6] + length(OWTE[,1])-(length(OWTE[which(OWTE$REU==31),1])+length(OWTE[which(OWTE$REU==0),1]))

  OOTH = O[which(!O$ORAC %in% c('W','B')),]
  Res$Tot[7] = Res$Tot[7] + length(OOTH[,1])
  Res$Hel[7] = Res$Hel[7] + length(OOTH[which(OOTH$REU==31),1])
  Res$NHel[7] = Res$NHel[7] + length(OOTH[which(OOTH$REU==0),1])
  Res$Unk[7] = Res$Unk[7] + length(OOTH[,1])-(length(OOTH[which(OOTH$REU==31),1])+length(OOTH[which(OOTH$REU==0),1]))
  
  OK = O[which(O$SEV==4),]
  Res$Tot[8] = Res$Tot[8] + length(OK[,1])
  Res$Hel[8] = Res$Hel[8] + length(OK[which(OK$REU==31),1])
  Res$NHel[8] = Res$NHel[8] + length(OK[which(OK$REU==0),1])
  Res$Unk[8] = Res$Unk[8] + length(OK[,1])-(length(OK[which(OK$REU==31),1])+length(OK[which(OK$REU==0),1]))
  
  OK = O[which(O$SEV==3),]
  Res$Tot[9] = Res$Tot[9] + length(OK[,1])
  Res$Hel[9] = Res$Hel[9] + length(OK[which(OK$REU==31),1])
  Res$NHel[9] = Res$NHel[9] + length(OK[which(OK$REU==0),1])
  Res$Unk[9] = Res$Unk[9] + length(OK[,1])-(length(OK[which(OK$REU==31),1])+length(OK[which(OK$REU==0),1]))
  
  OK = O[which(O$SEV==2),]
  Res$Tot[10] = Res$Tot[10] + length(OK[,1])
  Res$Hel[10] = Res$Hel[10] + length(OK[which(OK$REU==31),1])
  Res$NHel[10] = Res$NHel[10] + length(OK[which(OK$REU==0),1])
  Res$Unk[10] = Res$Unk[10] + length(OK[,1])-(length(OK[which(OK$REU==31),1])+length(OK[which(OK$REU==0),1]))

  OK = O[which(O$SEV==1),]
  Res$Tot[11] = Res$Tot[11] + length(OK[,1])
  Res$Hel[11] = Res$Hel[11] + length(OK[which(OK$REU==31),1])
  Res$NHel[11] = Res$NHel[11] + length(OK[which(OK$REU==0),1])
  Res$Unk[11] = Res$Unk[11] + length(OK[,1])-(length(OK[which(OK$REU==31),1])+length(OK[which(OK$REU==0),1]))
  
  OK = O[which(O$SEV==0),]
  Res$Tot[12] = Res$Tot[12] + length(OK[,1])
  Res$Hel[12] = Res$Hel[12] + length(OK[which(OK$REU==31),1])
  Res$NHel[12] = Res$NHel[12] + length(OK[which(OK$REU==0),1])
  Res$Unk[12] = Res$Unk[12] + length(OK[,1])-(length(OK[which(OK$REU==31),1])+length(OK[which(OK$REU==0),1]))
}
View(Res)


# Test SCDOT MP Script  --------
Years = 2011:2015
n = length(Years)
Res = data.frame(ST = rep(0,n),SI = rep(0,n),SE = rep(0,n),
                 MT = rep(0,n),MI = rep(0,n),ME = rep(0,n),
                 BI = rep(0,n),BE = rep(0,n))
rownames(Res) = Years
for(y in Years){
  M = read.csv(paste('Inputs/SC_Crash_',y,'_NG.csv',sep=''))
  S = read.csv(paste('Inputs/SC_Crash_',y,'_Loc.txt',sep=''))
  SI = read.csv(paste('Inputs/SC_Crash_',y,'_Loc_Inv.txt',sep=''))
  MEx = M[which(is.na(M$MEAS)),]
  MExANO = MEx$ANO
  MInANO = M[which(!is.na(M$MEAS)),'ANO']
  SExANO = SI$ANO
  SInANO = S$ANO
  
  BothIn = Reduce(intersect, list(MInANO,SInANO))
  BothEx = Reduce(intersect, list(MExANO,SExANO))
  
  Res[paste(y),'ST'] = length(S[,1]) + length(SI[,1])
  Res[paste(y),'SI'] = length(S[,1])
  Res[paste(y),'SE'] = length(SI[,1])
  Res[paste(y),'MT'] = length(M[,1])
  Res[paste(y),'MI'] = length(MInANO)
  Res[paste(y),'ME'] = length(MExANO)
  Res[paste(y),'BI'] = length(BothIn)
  Res[paste(y),'BE'] = length(BothEx)
}
MapOnlyS = SInANO[which(!(SInANO %in% MInANO) & SInANO %in% M$ANO)]
d = S[which(S$ANO == MapOnlyS[1]),]










