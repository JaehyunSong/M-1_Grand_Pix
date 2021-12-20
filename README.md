# M-1 グランプリ決勝データ

## Description

|変数名|内容|備考|
|---|---|---|
|ID|||
|No|回||
|Year|開催年||
|Rank|最終順位||
|Name|コンビ名||
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
  mutate(Zombie = if_else(Catchphrase == "（敗者復活）", 1, 0))
  
bar_df <- df %>%
  group_by(Order10) %>%
  summarise(Mean_Final = mean(Final))

fit <- glm(Final ~ Order10 + Since + No_Finals + Zombie, 
           data = df, family = binomial("logit"))

summary(fit)
```

```
Call:
glm(formula = Final ~ Order10 + Since + No_Finals + Zombie, family = binomial("logit"), 
    data = df)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.5656  -0.8580  -0.5963   1.0818   2.0285  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)   
(Intercept) 50.00850   62.98874   0.794  0.42724   
Order10      0.23348    0.07487   3.118  0.00182 **
Since       -0.02619    0.03146  -0.832  0.40525   
No_Finals    0.33450    0.13499   2.478  0.01321 * 
Zombie       0.41894    0.59718   0.702  0.48297   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 198.00  on 158  degrees of freedom
Residual deviance: 178.14  on 154  degrees of freedom
AIC: 188.14

Number of Fisher Scoring iterations: 4
```

```r
fit %>% 
  prediction(at = list(Order10 = 1:10)) %>%
  summary() %>%
  rename("Order" = "at(Order10)") %>%
  ggplot() +
  geom_bar(data = bar_df, aes(x = Order10, y = Mean_Final), 
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

![サンプル](/Figs/M1.png)

---

## 更新履歴

* 2021年12月20日: 第17回 (2021年)のデータを追加
* 2020年12月21年: 公開