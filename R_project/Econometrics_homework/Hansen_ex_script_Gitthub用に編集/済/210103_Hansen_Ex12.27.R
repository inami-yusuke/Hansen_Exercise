##### Hansen 「Econometric」p419 Exercise 12.27
## Topic:操作変数法
## 教育が賃金に与える影響を生まれた月をIVとして推定する
## 今回はIVのパッケージを使わずに予測値をそのまま代入して分析を行う
## パッケージを使う場合とそのまま予測値を代入する場合とでは標準誤差が若干ずれるので注意


## ライブラリーを読み込む
library(dplyr)
library(tidyverse)
library(stargazer)
library(psych)


## データを読み込む
# いくつかの変数をまとめてfactorに変換
df_ak <- read.csv("ak1991.csv", header = T,fileEncoding = "CP932") %>% 
  mutate_at(vars(region,yob,state,qob),as.factor)


## データの特性
head(df_ak)
describe(df_ak)



## 生まれた月(四半期)でグループ分けしたときの教育年数と賃金の平均
df_ak %>% 
  group_by(qob) %>% 
  summarise_at(vars(edu,logwage),funs(mean(.))) 



### p410の教科書の(12.91)式を再現する

# 一段階目の推定
first_step_result_12.91 <- df_ak %>% 
  lm(edu ~ qob*yob + black + smsa + region + married,data =.)


# 一段階目の推定の予測値を使って、2段階目の推定を行う
second_step_result_12.91 <- df_ak %>% 
  lm(logwage ~ first_step_result_12.91$fitted.values  + black + smsa + married +region +yob ,data =.)
  

# 結果の出力
stargazer(first_step_result_12.91,second_step_result_12.91
          ,column.labels = c("First stage", "Second stage")
          ,omit = c("yob","region")
          ,df = F
          ,type = "text")


# Fstatisticを出力
anova(first_step_result_12.91)




### p410の教科書の(12.92)式を再現する

# 一段階目の推定
first_step_result_12.92 <- df_ak %>% 
  lm(edu ~ qob*yob + qob*state + black + smsa + married + region,data =.) 


# 一段階目の推定の予測値を使って、2段階目の推定を行う
second_step_result_12.92 <- df_ak %>% 
  lm(logwage ~ first_step_result_12.92$fitted.values + black + smsa + married + region + yob + state,data=.)


# 結果の出力
stargazer(first_step_result_12.92,second_step_result_12.92
          ,column.labels = c("First stage", "Second stage")
          ,omit = c("yob","region","state")
          ,df = F
          ,type = "text")


# Fstatisticを出力
anova(first_step_result_12.92)




### p410の教科書の(12.93)式を再現する

# 一段階目の推定 
first_step_result_12.93 <- df_ak %>% select(black,smsa,qob,edu,married,region,yob) %>% 
  lm(edu ~ .,data = .)


# 一段階目の推定の予測値を使って、2段階目の推定を行う
second_step_result_12.93 <- df_ak %>% 
  lm(logwage ~ first_step_result_12.93$fitted.values  + black + smsa + married + region + yob ,data =.)


# 結果の出力
stargazer(first_step_result_12.93,second_step_result_12.93
          ,column.labels = c("First stage", "Second stage")
          ,omit = c("yob","region")
          ,df = F
          ,type = "text")
# Fstatistic 
anova(first_step_result_12.93)




### エクササイズに従ってBlack == 1のsubsampleを取って同じ分析を実行 ###


## Black == 1に絞ったデータを作る
df_ak_black <- df_ak %>% filter(black == 1)




### p410の教科書の(12.92)式をblack == 1のサブサンプルで実行

# 一段階目の推定 
first_step_result_black_12.92<- df_ak_black %>% 
  lm(edu ~ qob*yob + qob*state + smsa + married + region, data =. )


# 一段階目の推定の予測値を使って、2段階目の推定を行う
second_step_result_black_12.92 <- df_ak_black %>% 
  lm(logwage ~ first_step_result_black_12.92$fitted.values + smsa + married + region + yob + state ,data=.)

# 結果の出力
stargazer(first_step_result_black_12.92,second_step_result_black_12.92
          ,column.labels = c("First stage", "Second stage")
          ,omit = c("yob","region","state")
          ,df = F
          ,type = "text")


# Fstatisticの出力
anova(first_step_result_black_12.92)





### p410の教科書の(12.91)式をblack == 1のサブサンプルで実行

# 一段階目の推定
first_step_result_black_12.91 <- df_ak_black %>% 
  lm(edu ~ qob*yob  + smsa + region + married ,data =.)


# 一段階目の推定の予測値を使って、2段階目の推定を行う
second_step_result_black_12.91 <- df_ak_black %>% 
  lm(logwage ~ first_step_result_black_12.91$fitted.values  + smsa + married +region +yob ,data =.)


# 結果の出力
stargazer(first_step_result_black_12.91,second_step_result_black_12.91
          ,column.labels = c("First stage", "Second stage")
          ,omit = c("yob","region")
          ,df = F
          ,type = "text")


# Fstatisticの出力
anova(first_step_result_black_12.91)



### p410の教科書の(12.93)式をblack == 1のサブサンプルで実行


# 一段階目の推定
first_step_result_black_12.93 <- df_ak_black %>%  
  lm(edu ~ qob + smsa + married + yob + region ,data = .)


# 一段階目の推定の予測値を使って、2段階目の推定を行う
second_step_result_black_12.93 <- df_ak_black %>% 
  lm(logwage ~ first_step_result_black_12.93$fitted.values + smsa + married + region + yob ,data =.)

# 結果の出力
stargazer(first_step_result_black_12.93,second_step_result_black_12.93
          ,column.labels = c("First stage", "Second stage")
          ,omit = c("yob","region")
          ,df = F
          ,type = "text")

# Fstatisticの出力
anova(first_step_result_black_12.93)




### 自分でモデリングした分析を実行する

#  一段階目の推定
first_step_result_original <- df_ak_black %>%  
  lm(edu ~ qob + smsa + married + region + yob + state,data = .)

# 一段階目の推定の予測値を使って、2段階目の推定を行う
second_step_result_original <- df_ak_black %>% 
  lm(logwage ~ first_step_result_original$fitted.values + smsa + married + region + yob + state ,data =.)

# 結果の出力
stargazer(first_step_result_original,second_step_result_original
          ,column.labels = c("First stage", "Second stage")
          ,omit = c("yob","region","state")
          ,df = F
          ,type = "text")

# Fstatisticの出力
anova(first_step_result_original)



### すべての分析結果を比較する

stargazer(second_step_result_12.91
          ,second_step_result_12.92
          ,second_step_result_12.93
          ,second_step_result_black_12.91
          ,second_step_result_black_12.92
          ,second_step_result_black_12.93
          ,second_step_result_original
          ,column.labels = c("Full sample.91", "Full sample.92"
                             ,"Full sample.93","black.91","black.92","black.93","black orig")
          ,omit = c("yob","region","state","smsa","married","qob")
          ,df = F
          ,type = "text")
