# M-1 グランプリ決勝データ

## Description

|変数名|内容|備考|
|---|---|---|
|ID|||
|No|回||
|Year|開催年||
|Rank|最終順位||
|Name|コンビ名||
|Company|所属事務所名||
|Entry_No|エントリー番号||
|Since|コンビ結成年||
|No_Finals|これまでの決勝進出回数|当該年を除く|
|Catchphrase|キャッチフレーズ||
|Final|最終決戦進出有無||
|Order10|決勝における入場順番||
|Order3|最終決戦における入場順番|最終決戦に進出しなかったコンビは欠損値|
|Score10|決勝における評価||
|Score3|最終決戦における評価|最終決戦に進出しなかったコンビは欠損値|
|No_Reviewer|審査委員数||

---

## Source

Wikipedia「[M-1グランプリ](https://ja.wikipedia.org/wiki/M-1%E3%82%B0%E3%83%A9%E3%83%B3%E3%83%97%E3%83%AA)」

---

## Sample Code

```r
library(tidyverse)
library(prediction)

df <- read_csv("M1_Grand_Pix.csv")
df <- df %>%
  mutate(Zombie = if_else(Catchphrase == "（敗者復活）", 1, 0),
         Winner = if_else(Rank == 1, 1, 0))
  
fit1 <- glm(Final ~ Order10 + Since + No_Finals + Zombie, 
           data = df, family = binomial("logit"))

summary(fit1)
```

```
Call:
glm(formula = Final ~ Order10 + Since + No_Finals + Zombie, family = binomial("logit"), 
    data = df)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.5994  -0.8690  -0.6032   1.1086   2.0139  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept) 58.15103   57.45438   1.012 0.311478    
Order10      0.24337    0.07199   3.380 0.000724 ***
Since       -0.03026    0.02869  -1.055 0.291550    
No_Finals    0.34455    0.13553   2.542 0.011016 *  
Zombie       0.12551    0.55866   0.225 0.822239    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 210.22  on 168  degrees of freedom
Residual deviance: 189.49  on 164  degrees of freedom
AIC: 199.49

Number of Fisher Scoring iterations: 4
```

```r
bar_df1 <- df %>%
  group_by(Order10) %>%
  summarise(Mean_Final = mean(Final))

fit1 %>% 
  prediction(at = list(Order10 = 1:10)) %>%
  summary() %>%
  rename("Order" = "at(Order10)") %>%
  ggplot() +
  geom_bar(data = bar_df1, aes(x = Order10, y = Mean_Final), 
           stat = "Identity", fill = "gray70") +
  geom_pointrange(aes(x = Order, y = Prediction, 
                      ymin = lower, ymax = upper), size = 1.2) +
  geom_line(aes(x = Order, y = Prediction), size = 1.2) +
  scale_x_continuous(breaks = 1:10, labels = 1:10) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "出場順番", y = "最終決戦へ進出する確率",
       title = "M-1グランプリ (第1回〜第17回)") +
  theme_minimal(base_family = "HiraKakuProN-W3") +
  theme(panel.grid.minor = element_blank(),
        text = element_text(size = 16))
```

![サンプル](/Figs/M1_1.png)

```r
fit2 <- glm(Winner ~ Order3 + Since + No_Finals + Zombie, 
            data = df, family = binomial("logit"))

summary(fit2)
```

```
Call:
glm(formula = Winner ~ Order3 + Since + No_Finals + Zombie, family = binomial("logit"), 
    data = df)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.4283  -0.8374  -0.6557   1.0890   2.1613  

Coefficients:
             Estimate Std. Error z value Pr(>|z|)  
(Intercept) 179.13728  113.77383   1.575   0.1154  
Order3        0.77460    0.41079   1.886   0.0593 .
Since        -0.09031    0.05691  -1.587   0.1125  
No_Finals    -0.28788    0.22062  -1.305   0.1919  
Zombie       -1.29609    0.98143  -1.321   0.1866  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 67.923  on 52  degrees of freedom
Residual deviance: 60.598  on 48  degrees of freedom
  (116 observations deleted due to missingness)
AIC: 70.598

Number of Fisher Scoring iterations: 4
```

```r
bar_df2 <- df %>%
  group_by(Order3) %>%
  summarise(Mean_Winner = mean(Winner)) %>%
  drop_na()

fit2 %>% 
  prediction(at = list(Order3 = 1:3)) %>%
  summary() %>%
  rename("Order" = "at(Order3)") %>%
  ggplot() +
  geom_bar(data = bar_df2, aes(x = Order3, y = Mean_Winner), 
           stat = "Identity", fill = "gray70") +
  geom_pointrange(aes(x = Order, y = Prediction, 
                      ymin = lower, ymax = upper), size = 1.2) +
  geom_line(aes(x = Order, y = Prediction), size = 1.2) +
  scale_x_continuous(breaks = 1:10, labels = 1:10) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "出場順番", y = "優勝する確率",
       title = "M-1グランプリ (第1回〜第17回)") +
  theme_minimal(base_family = "HiraKakuProN-W3") +
  theme(panel.grid.minor = element_blank(),
        text = element_text(size = 16))
```

![サンプル](/Figs/M1_2.png)

---

## 更新履歴

* 2022年12月19日: 第18回 (2022年)のデータを追加、所属事務所名を追加
* 2021年12月20日: 第17回 (2021年)のデータを追加
* 2020年12月21年: 公開