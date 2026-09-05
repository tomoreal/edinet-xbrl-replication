#!/usr/bin/env Rscript
# verify_kpi_from_csv.R — 公開用CSVだけを入力に、論文の主要数値を再現する
#
# DBには一切触れない。公開データを受け取った第三者が、本ディレクトリのCSVと
# 本スクリプトのみで、次を再現できることを確認するためのもの。
#
#   1. セクション別のKPI採用率（表2）
#   2. 経営方針で両カテゴリーを掲げた企業の2×2クロス表と反映率・McNemar検定（表5・4.1）
#   3. 採用の組合せ（単独／併用）と併用率（表4・4.2）
#   4. P期とP-4期の反映率の比較（4.4）
#   5. 指標名レベルの一致率（5章。indicator_kpi.csv を使用）
#
# 依存パッケージなし（base Rのみ）。
#
# 実行:
#   Rscript verify_kpi_from_csv.R

# データの所在: 環境変数 JAMA2026_DATA > スクリプトと同じディレクトリ >
# リポジトリ内の docs/JAMA2026_data > カレントディレクトリ の順に探す。
find_data_dir <- function(sentinel) {
  sp  <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
  sd  <- if (!is.na(sp) && nzchar(sp)) dirname(normalizePath(sp)) else getwd()
  for (d in c(Sys.getenv("JAMA2026_DATA", ""), sd,
              file.path(sd, "..", "docs", "JAMA2026_data"),
              file.path(getwd(), "docs", "JAMA2026_data"), getwd())) {
    if (nzchar(d) && file.exists(file.path(d, sentinel))) return(normalizePath(d))
  }
  stop(sprintf("%s が見つかりません。環境変数 JAMA2026_DATA にデータの場所を指定してください。",
               sentinel))
}
DIR <- find_data_dir("panel_kpi.csv")

d <- read.csv(file.path(DIR, "panel_kpi.csv"), fileEncoding = "UTF-8",
              stringsAsFactors = FALSE)
cat(sprintf("CSV読込: %s 行（P %s 社 / P-4 %s 社）\n\n",
            format(nrow(d), big.mark = ","),
            format(sum(d$rp == "P"), big.mark = ","),
            format(sum(d$rp == "P-4"), big.mark = ",")))

# カテゴリー（列語幹と表示名）。定義は MANIFEST_kpi.txt を参照。
CATS <- c(acc = "金額指標", ratio = "財務比率",
          nonfin = "非財務指標", stock = "株主還元・株価関連指標")

# McNemar mid-p（両側）
#   mid-p = 2 * [ P(X<=k) - 0.5*P(X=k) ],  X ~ Binomial(b+c, 0.5),  k = min(b,c)
# 正確二項検定の保守性を補正した準正確検定（Fagerland, Lydersen & Laake 2013）。
midp <- function(b, c) {
  n <- b + c
  if (n == 0) return(1)
  k <- min(b, c)
  min(1, 2 * (pbinom(k, n, 0.5) - 0.5 * dbinom(k, n, 0.5)))
}
fmt_p <- function(p) if (p < 0.001) "<0.001" else sprintf("%.3f", p)

P  <- d[d$rp == "P", ]
P4 <- d[d$rp == "P-4", ]

# ── 1. セクション別の採用率（表2）───────────────────────────────
cat("=== 1. セクション別のKPI採用率（P期、表2）===\n")
cat(sprintf("%-22s %10s %10s %10s\n", "カテゴリー", "分析標本", "プライム", "スタンダード"))
for (slug in names(CATS)) {
  for (sec in c("pol", "pay")) {
    v <- P[[paste0(sec, "_", slug)]]
    lab <- paste0(if (sec == "pol") "経営方針 " else "役員報酬 ", CATS[[slug]])
    cat(sprintf("%-22s %9.0f%% %9.0f%% %9.0f%%\n", lab, mean(v) * 100,
                mean(v[P$market == "プライム"]) * 100,
                mean(v[P$market == "スタンダード"]) * 100))
  }
}
cat("\n")

# ── 2. 経営方針で両カテゴリーを掲げた企業の2×2（表5・4.1）─────────
both <- P[P$pol_acc == 1 & P$pol_ratio == 1, ]
a <- sum(both$pay_acc == 1 & both$pay_ratio == 1)
b <- sum(both$pay_acc == 1 & both$pay_ratio == 0)   # 金額指標のみ反映
c_ <- sum(both$pay_acc == 0 & both$pay_ratio == 1)  # 財務比率のみ反映
dd <- sum(both$pay_acc == 0 & both$pay_ratio == 0)
n <- nrow(both)
cat(sprintf("=== 2. 経営方針で両カテゴリーを掲げた企業（n=%s、表5）===\n",
            format(n, big.mark = ",")))
cat(sprintf("  両方反映 %d (%.0f%%)  金額指標のみ %d (%.0f%%)  財務比率のみ %d (%.0f%%)  いずれも非反映 %d (%.0f%%)\n",
            a, a / n * 100, b, b / n * 100, c_, c_ / n * 100, dd, dd / n * 100))
cat(sprintf("  反映率: 金額指標 %.0f%%  財務比率 %.0f%%  差 %+.0fpt\n",
            (a + b) / n * 100, (a + c_) / n * 100, (b - c_) / n * 100))
cat(sprintf("  McNemar mid-p = %s（不一致セル b=%d, c=%d）\n",
            fmt_p(midp(b, c_)), b, c_))
# 対応あり標本の比率差の95%信頼区間（Tango 1998 の漸近スコア法）
#   Z(delta) = (b - c - n*delta) / sqrt(n*(p21~ + p12~ - delta^2))
#   p21~, p12~ は p21 - p12 = delta の制約のもとでの最尤推定量（数値解）。
tango_ci <- function(b, c, n, conf = 0.95) {
  z <- qnorm(1 - (1 - conf) / 2)
  rest <- n - b - c
  zstat <- function(delta) {
    nll <- function(p21) {
      p12 <- p21 - delta
      p0  <- 1 - p21 - p12
      if (p21 <= 0 || p12 <= 0 || p0 <= 0) return(1e12)
      -(b * log(p21) + c * log(p12) + rest * log(p0))
    }
    lo <- max(delta, 0) + 1e-9
    hi <- (1 + delta) / 2 - 1e-9
    p21 <- optimize(nll, c(lo, hi))$minimum
    p12 <- p21 - delta
    (b - c - n * delta) / sqrt(n * (p21 + p12 - delta^2))
  }
  point <- (b - c) / n
  lo <- uniroot(function(x) zstat(x) - z, c(-0.999, point - 1e-6))$root
  hi <- uniroot(function(x) zstat(x) + z, c(point + 1e-6, 0.999))$root
  c(lo, hi) * 100
}
ci <- tango_ci(b, c_, n)
cat(sprintf("  差の95%%信頼区間（Tango 1998）: [%+.0f, %+.0f]pt\n", ci[1], ci[2]))
cat(sprintf("  「いずれも非反映」の%d社を除いても mid-p = %s\n\n",
            dd, fmt_p(midp(b, c_))))

# ── 3. 採用の組合せと併用率（表4・4.2）──────────────────────────
cat("=== 3. 単独採用と併用（P期、表4）===\n")
for (sec in c("pol", "pay")) {
  acc <- P[[paste0(sec, "_acc")]]; rat <- P[[paste0(sec, "_ratio")]]
  lab <- if (sec == "pol") "経営方針" else "役員報酬"
  n_acc <- sum(acc == 1); n_rat <- sum(rat == 1); n_both <- sum(acc == 1 & rat == 1)
  cat(sprintf("  %s: 金額指標の採用 %s 社（うち併用 %s 社 = %.0f%%）  財務比率の採用 %s 社（うち併用 %s 社 = %.0f%%）\n",
              lab, format(n_acc, big.mark = ","), format(n_both, big.mark = ","),
              n_both / n_acc * 100, format(n_rat, big.mark = ","),
              format(n_both, big.mark = ","), n_both / n_rat * 100))
}
cat("\n")

# ── 4. 時点間の比較（4.4）───────────────────────────────────────
cat("=== 4. 反映率の時点間比較（P期 vs P-4期）===\n")
cat(sprintf("%-22s %12s %12s\n", "カテゴリー", "反映率(P期)", "反映率(P-4期)"))
for (slug in c("acc", "ratio")) {
  rf <- function(s) {
    p <- s[[paste0("pol_", slug)]]; y <- s[[paste0("pay_", slug)]]
    sum(p == 1 & y == 1) / sum(p == 1) * 100
  }
  cat(sprintf("%-22s %11.1f%% %11.1f%%\n", CATS[[slug]], rf(P), rf(P4)))
}
cat("\n")

# ── 5. 指標名レベルの一致率（5章）───────────────────────────────
ind_path <- file.path(DIR, "indicator_kpi.csv")
if (file.exists(ind_path)) {
  ind <- read.csv(ind_path, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  cat(sprintf("=== 5. 指標名レベルの一致率（P期、%s 社。5章）===\n",
              format(length(unique(ind$firm)), big.mark = ",")))
  cat("経営方針で当該指標名を掲げた企業のうち、役員報酬でも同一指標名を用いる割合\n")
  pol <- ind[ind$pol == 1, ]
  agg <- aggregate(cbind(n = pol$pol, hit = pol$pay),
                   by = list(indicator = pol$indicator, category = pol$category), FUN = sum)
  agg <- agg[agg$n >= 100, ]
  agg <- agg[order(-agg$hit / agg$n), ]
  cat(sprintf("%-16s %-14s %8s %10s\n", "指標名", "カテゴリー", "経営方針", "一致率"))
  for (i in seq_len(nrow(agg))) {
    cat(sprintf("%-16s %-14s %8s %9.0f%%\n", agg$indicator[i], CATS[[agg$category[i]]],
                format(agg$n[i], big.mark = ","), agg$hit[i] / agg$n[i] * 100))
  }
  cat("\nカテゴリー水準の反映率（金額指標97%・財務比率39%）は、これらの指標名の一致を\n")
  cat("カテゴリー内の論理和で束ねた値である（同一指標の一致率ではない）。\n")
} else {
  cat("（indicator_kpi.csv が見つからないため、指標名レベルの一致率は省略）\n")
}
