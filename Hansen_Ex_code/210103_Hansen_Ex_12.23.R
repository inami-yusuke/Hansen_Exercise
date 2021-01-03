##### Hansen 「Econometric」p408 Exercise 12.23
## Topic:操作変数法
## Acemoglu, Johnson and Robinson (2001)の分析を再現
## 政治的な制度が経済的なパフォーマンス(高いGDP)に与える影響を19世紀の死亡率をIVとして利用

## 前提となる知識
# 植民地政策を行っていた国は植民地をextractive stateかmigrant colonyに設定していた
# extractive stateは資源を搾取するための植民地
# migrant colonyはヨーロッパから移住するために作られる植民地なので、植民地政策を行う国は法整備の必要している
# →このような植民地制度のばらつきは近代の経済成長率を予想する

## 変数
# political institutionsの指標としてriskと呼ばれるものを使っている。法制度に守られるかどうかを測定
# しかし"risk"という変数は内生的なので、19世紀におけるその植民地の国の死亡率をIVとして使っている
# 死亡率が高い植民地は移住してくる人が住もうと思わないため、extractive stateになる可能性が高い
# 逆に死亡率が低い植民地はmigrant colonyになる可能性が高い
# このようなメカニズムで死亡率はrisk変数と相関して、経済的なパフォーマンスを相関を持たないと考えられるのでIVとして機能する


## ライブラリーを読み込む 
library(dplyr)
library(tidyverse)
library(AER)
library(sandwich)
library(lmtest)
library(stargazer)
library(ivmodel)
library(corrr)
library(psych)
library(broom)



## データを読み込む 
# logに変換した変数と2乗の変数を作っておく
df_ajr <- read.csv("AJR2001.csv",header = T,fileEncoding = "CP932") %>% 
  mutate(mortality = exp(logmort0)) %>% 
  mutate(logmort0_square = logmort0^2)

## データの確認
head(df_ajr)
describe(df_ajr)


## 目的変数、説明変数、操作変数の相関を見る
df_ajr %>% 
  select(risk, logmort0, loggdp ) %>% correlate() %>% shave() 




### 問題 (a) Estimate the OLS regression (12.88), the reduced form regression (12.89) and the 2SLS regression (12.90).


## p408の式(12.88)式を再現する

df_ajr %>% lm(loggdp ~ risk ,data = .) %>% tidy()

ajr_normal_ols <-  df_ajr %>% 
  lm(loggdp ~ risk ,data = .) 



## p408の式(12.89)式を再現する
 
df_ajr %>% lm(risk ~ logmort0, data=.) %>% 
  tidy()

ajr_result_first_stage <- df_ajr %>% lm(risk ~ logmort0, data=.) 



## p409の式(12.90)式を再現する

# IVパッケージを使わずに第一段階の予測値をそのまま代入して式(12.90)式を再現する
df_ajr %>% lm(loggdp ~ ajr_result_first_stage$fitted.values ,data = .) %>% 
  tidy()

ajr_result_second_stage_self2sls <- df_ajr %>% 
  lm(loggdp ~ ajr_result_first_stage$fitted.values ,data = .) 



# IVのパッケージを使って(12.90)式を再現する
df_ajr %>% ivreg(formula = loggdp ~ risk | logmort0, data = . ) %>% 
  tidy()

ajr_result_second_stage_package <- df_ajr %>% ivreg(formula = loggdp ~ risk | logmort0, data = . ) 



## F-statisticを出力する
ajr_result_second_stage <- df_ajr %>% 
  lm(loggdp ~ ajr_result_first_stage$fitted.values ,data = .) 

anova(ajr_result_first_stage)



## 結果を出力して比べる

stargazer(ajr_normal_ols
          ,ajr_result_first_stage
          ,ajr_result_second_stage_self2sls
          ,ajr_result_second_stage_package
          ,column.labels = c("Normal OLS","First stage","Second stage","IV Package")
          ,df = F
          ,type = "text")







### 問題 (b) For the above estimates, calculate both homoskedastic and heteroskedastic-robust standard errors. dealing with heteroskedasity 
## 頑健な標準誤差で結果を出力する

## 12.88を頑健な標準誤差で出力
ajr_normal_ols_hetero <- ajr_normal_ols %>% 
  coeftest(vcov = vcovHC(ajr_normal_ols, type = "HC0"))
ajr_normal_ols_hetero


## 第一段階推定の12.89を頑健な標準誤差で出力
ajr_result_first_stage_hetero <- ajr_result_first_stage %>% 
  coeftest(vcov = vcovHC(ajr_result_first_stage, type = "HC0"))
ajr_result_first_stage_hetero


## 第2段階推定の12.90を頑健な標準誤差で出力
ajr_result_second_stage_self2sls_hetero <- ajr_result_second_stage_self2sls %>%  
  coeftest(vcov = vcovHC(ajr_result_second_stage_self2sls, type = "HC0"))



## IVのパッケージを使って12.90を頑健な標準誤差で出力
ajr_result_second_stage_package_hetero <- ajr_result_second_stage_package %>% 
  coeftest(vcov = vcovHC(ajr_result_second_stage_package, type = "HC0"))
ajr_result_second_stage_package_hetero



## 結果の出力

stargazer(ajr_normal_ols
          ,ajr_normal_ols_hetero
          ,ajr_result_first_stage
          ,ajr_result_first_stage_hetero
          ,ajr_result_second_stage_self2sls
          ,ajr_result_second_stage_self2sls_hetero
          ,ajr_result_second_stage_package
          ,ajr_result_second_stage_package_hetero 
          ,column.labels = c("OLS","OLS(H)","First stage","First stage(H)"
                             ,"Second stage","Second stage(H)","IV Package","IV Package(H)")
          ,df = F
          ,type = "text")







### 問題 (c)  Calculate the 2SLS estimates by the Indirect Least Squares formula.Are they the same?

## 間接最小二乗法で推定
# logmortの係数を取り出す
gamma <- df_ajr %>% lm(risk ~ logmort0 ,data = .) %>% 
  tidy() %>% .[2,2]


lambda <- df_ajr  %>% lm(loggdp ~ logmort0, data = .) %>% 
  tidy() %>% .[2,2]

# 割ると今まで推定した係数と一致する
lambda/gamma

# 例えば以下の推定結果の係数と比較してみる
df_ajr %>% ivreg(formula = loggdp ~ risk | logmort0, data = . ) %>% 
  tidy() %>% .[2,2]






### 問題 (d) Calculate the 2SLS estimates by the two-stage approach.Are they the same?
# 普通に予測を代入する方法
# 上でやった結果と同じ

ajr_result_first_stage <- df_ajr %>% lm(risk ~ logmort0, data=.) 

df_ajr %>% lm(loggdp ~ ajr_result_first_stage$fitted.values ,data = .)  %>% 
  tidy()






### 問題 (e) Calculate the 2SLS estimates by the control variable approach.Are they the same ?
## コントロール変数アプローチ
# 一段階目の残差を使って推定する
# 結果は今までのと同じ

ajr_result_first_stage_control <- df_ajr %>% lm(risk ~ logmort0, data=.) 


df_ajr %>% lm(loggdp ~ ajr_result_first_stage_control$residuals + risk ,data = .)  %>% 
  tidy()







### 問題 (f) Estimate by least squares the equation for log- GDP adding latitude and africa as regressors. 
### 問題 (g) Now estimate the same equationasin (f) but by 2SLS using logmortality as an instrument for risk.


## いくつかのコントロール変数を追加する
# ここではlatitudeとアフリカかどうかのダミー変数を追加する
# コントロール変数のみの回帰

ajr_normal_ols_control_only <- df_ajr %>% lm(loggdp ~ latitude + africa ,data = .) 
ajr_normal_ols_control_only 


## 12.88式を再現
# コントロール変数とriskを追加した回帰

ajr_normal_ols_control <-  df_ajr %>% lm(loggdp ~ risk + latitude + africa ,data = .) 
ajr_normal_ols_control


## 予測値をそのまま代入して12.90式を再現

# 第一段階
ajr_result_first_stage_control <- df_ajr %>% 
  lm(risk ~ logmort0 + latitude + africa, data=.) 
ajr_result_first_stage_control

# 第二段階
ajr_result_second_stage_self2sls_control <- df_ajr %>% 
  lm(loggdp ~ ajr_result_first_stage_control$fitted.values + latitude + africa ,data = .) 
ajr_result_second_stage_self2sls_control 



## IVパッケージを使って12.90式を再現

ajr_result_second_stage_package_control <- df_ajr %>% 
  ivreg(formula = loggdp ~ risk+ latitude + africa | logmort0+ latitude + africa, data = . ) 


## F-statisticを出力
anova(ajr_result_first_stage_control)


## 結果の出力

stargazer(ajr_normal_ols_control_only
          ,ajr_normal_ols_control
          ,ajr_result_first_stage_control
          ,ajr_result_second_stage_self2sls_control
          ,ajr_result_second_stage_package_control
          ,column.labels = c("Normal OLS","Normal OLS","First stage","Second stage","IV Package")
          ,df = F
          ,type = "text")








### 問題 (h) Estimate the reduced form for risk with mortality as the instrument. 
## log(mortality)ではなくmortalityで分析する
# logを戻す exp()をかけてやればいい→予め変数は作成済み


## 12.88式を再現
ajr_normal_ols_mortality <-  df_ajr %>% lm(loggdp ~ risk,data = .) 
ajr_normal_ols_mortality



## 予測値をそのまま代入して12.90式を再現


# 第一段階
ajr_result_first_stage_mortality <- df_ajr %>% lm(risk ~ mortality, data=.) 
ajr_result_first_stage_mortality 

# 第二段階
ajr_result_second_stage_self2sls_mortality <- df_ajr %>% lm(loggdp ~ ajr_result_first_stage_mortality$fitted.values ,data = .) 
ajr_result_second_stage_self2sls_mortality 



##  IVパッケージを使って12.90式を再現
ajr_result_second_stage_package_mortality <- df_ajr %>% ivreg(formula = loggdp ~ risk | mortality, data = . ) 



## 分析結果の出力
stargazer(ajr_normal_ols_mortality
          ,ajr_result_first_stage_mortality
          ,ajr_result_second_stage_self2sls_mortality
          ,ajr_result_second_stage_package_mortality
          ,column.labels = c("Normal OLS","First stage","Second stage","IV Package")
          ,df = F
          ,type = "text")








## 問題 (i) Try an alternative reduced form, including both log(mortality) and the square of log(mortality).
## 問題 (j) For the estimates in(i),are the instruments strong or weak using theStock-Yogo test?

## square of log(mortality)を追加して分析を回す


## 12.88式を再現
ajr_normal_ols_squre <-  df_ajr %>% lm(loggdp ~ risk,data = .) 
ajr_normal_ols_squre


## 予測値をそのまま代入して12.90式を再現

# 第一段階
ajr_result_first_stage_square <- df_ajr %>% lm(risk ~ logmort0 + logmort0_square, data=.) 
ajr_result_first_stage_square


# 第二段階
ajr_result_second_stage_self2sls_square <- df_ajr %>% 
  lm(loggdp ~ ajr_result_first_stage_square$fitted.values ,data = .) 
ajr_result_second_stage_self2sls_square



##  IVパッケージを使って12.90式を再現

ajr_result_second_stage_package_square <- df_ajr %>%
  ivreg(formula = loggdp ~ risk | logmort0+ logmort0_square, data = . ) 
ajr_result_second_stage_package_square


## 結果の出力
stargazer(ajr_normal_ols_squre
          ,ajr_result_first_stage_square
          ,ajr_result_second_stage_self2sls_square
          ,ajr_result_second_stage_package_square
          ,column.labels = c("Normal OLS","First stage","Second stage","IV Package")
          ,df = F
          ,type = "text")


## F-statistic値を出力
anova(ajr_result_first_stage_square)






### 問題 (k) Calculate and interpret a test for exogeneity of the instruments. 

## IVパッケージで結果を出力
ajr_result_second_stage_package_square <- df_ajr %>% 
  ivreg(formula = loggdp ~ risk | logmort0+ logmort0_square, data = . ) 


## 外生性検定の結果を出力
summary(ajr_result_second_stage_package_square, diagnostics=TRUE)








### 問題(l) Estimate the equation by LIML,using the instruments log(mortality) and the square of log(mortality).

## 変数を指定
Y <- df_ajr$loggdp
D <- df_ajr$risk
Z <- data.frame( df_ajr$logmort0,df_ajr$logmort0_square)

## モデルの設定
LIML_model <-  ivmodel(Y = df_ajr$loggdp, D = df_ajr$risk, Z = Z)

summary(LIML_model)

LIML(LIML_model)




