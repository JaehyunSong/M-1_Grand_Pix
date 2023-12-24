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
df <- df |>
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

Coefficients:
            Estimate Std. Error z value Pr(>|z|)   
(Intercept) 42.86406   52.81898   0.812  0.41706   
Order10      0.19528    0.06671   2.928  0.00342 **
Since       -0.02248    0.02636  -0.853  0.39382   
No_Finals    0.32239    0.13046   2.471  0.01347 * 
Zombie       0.18055    0.53886   0.335  0.73758   
---
Signif. codes:  
0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 222.45  on 178  degrees of freedom
Residual deviance: 205.76  on 174  degrees of freedom
AIC: 215.76

Number of Fisher Scoring iterations: 4
```

```r
bar_df1 <- df |>
  group_by(Order10) |>
  summarise(Mean_Final = mean(Final))

fit1 |> 
  predictions(newdata = datagrid(Order10 = 1:10)) |>
  ggplot(aes(x = Order10, y = estimate)) +
  geom_bar(data = bar_df1, aes(x = Order10, y = Mean_Final), 
           stat = "Identity", fill = "gray70") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  geom_line() +
  scale_x_continuous(breaks = 1:10, labels = 1:10) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "出場順番", y = "最終決戦へ進出する確率",
       title = "M-1グランプリ (第1回〜第19回)") +
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

Coefficients:
            Estimate Std. Error z value Pr(>|z|)
(Intercept) 126.1479   100.0296   1.261    0.207
Order3        0.5540     0.3815   1.452    0.146
Since        -0.0636     0.0500  -1.272    0.203
No_Finals    -0.3187     0.2278  -1.399    0.162
Zombie       -1.1556     0.9552  -1.210    0.226

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 71.743  on 55  degrees of freedom
Residual deviance: 66.207  on 51  degrees of freedom
  (123 observations deleted due to missingness)
AIC: 76.207

Number of Fisher Scoring iterations: 4
```

```r
bar_df2 <- df |>
  group_by(Order3) |>
  summarise(Mean_Winner = mean(Winner)) |>
  drop_na()

fit2 |> 
  predictions(newdata = datagrid(Order3 = 1:3)) |>
  ggplot(aes(x = Order3, y = estimate)) +
  geom_bar(data = bar_df2, aes(x = Order3, y = Mean_Winner), 
           stat = "Identity", fill = "gray70") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  geom_line() +
  scale_x_continuous(breaks = 1:10, labels = 1:10) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "出場順番", y = "優勝する確率",
       title = "M-1グランプリ (第1回〜第19回)") +
  theme_minimal(base_family = "HiraKakuProN-W3") +
  theme(panel.grid.minor = element_blank(),
        text = element_text(size = 16))
```

![サンプル](/Figs/M1_2.png)

---

## 更新履歴

* 2023年12月24日: 第19回 (2023年)のデータを追加
* 2022年12月19日: 第18回 (2022年)のデータを追加、所属事務所名を追加
* 2021年12月20日: 第17回 (2021年)のデータを追加
* 2020年12月21年: 公開