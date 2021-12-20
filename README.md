# M-1 グランプリ決勝データ

改訂: 2021年12月20日

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
|No_Finals|これまでの決勝進出回数||
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

df     <- read_csv("M1_Grand_Pix.csv")
bar_df <- df %>%
    group_by(Order10) %>%
    summarise(Mean_Final = mean(Final))

df %>% 
    glm(Final ~ Order10, data = ., family = binomial("logit")) %>% 
    predict(newdata = data.frame(Order10 = 1:10), se.fit = TRUE) %>% 
    as.data.frame() %>% 
    mutate(ll    = fit + qnorm(0.025) * se.fit, 
           ul    = fit + qnorm(0.975) * se.fit,
           fit   = 1 / (1 + exp(-fit)), 
           ll    = 1 / (1 + exp(-ll)), 
           ul    = 1 / (1 + exp(-ul)),
           Order = 1:10) %>% 
    select(Order, fit, ll, ul) %>%
    ggplot() +
    geom_bar(data = bar_df, aes(x = Order10, y = Mean_Final), 
             stat = "Identity", fill = "gray70") +
    geom_pointrange(aes(x = Order, y = fit, ymin = ll, ymax = ul), size = 1.2) +
    geom_line(aes(x = Order, y = fit), size = 1.2) +
    scale_x_continuous(breaks = 1:10, labels = 1:10) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(x = "出場順番", y = "最終決戦へ進出する確率",
         title = "M-1グランプリ (第1回〜第16回)") +
    theme_minimal(base_family = "HiraKakuProN-W3") +
    theme(panel.grid.minor = element_blank(),
          text = element_text(size = 16))
```

![サンプル](/Figs/M1.png)
