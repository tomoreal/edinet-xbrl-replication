#!/usr/bin/env Rscript
# verify_kpi_from_csv.R — 公開用CSV（docs/JAMA2026_data/panel_kpi.csv）だけを入力に
#                          論文5.2節のKPI分析の主要数値を再現する
#
# DBには一切触れない。公開データを受け取った第三者が、CSVと本スクリプトのみで
# McNemar検定（mid-p）・不一致オッズ比・報酬への反映率、およびその市場区分別・
# 時点別（P期／P-4期）の内訳を再現できることを確認するためのもの。
#
# 標本構築ロジックそのものをDBから独立に再現する検証は verify_in_r_kpi_mcnemar.R が
# 担う。本スクリプトはその一段外側で、確定した標本から論文の表が導けることを示す。
#
# 実行:
#   Rscript db_exploration/verify_kpi_from_csv.R
#
# 依存パッケージなし（base Rのみ）。

# データの所在: 環境変数 JAMA2026_DATA > スクリプトと同じディレクトリ >
# リポジトリ内の docs/JAMA2026_data > カレントディレクトリ の順に探す。
# 公開パッケージを任意の場所へ展開しても動くようにするため、絶対パスは持たない。
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
CSV <- file.path(DIR, "panel_kpi.csv")

d <- read.csv(CSV, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
cat(sprintf("CSV読込: %s 行（P %s 社 / P-4 %s 社）\n\n",
            format(nrow(d), big.mark = ","),
            format(sum(d$rp == "P"), big.mark = ","),
            format(sum(d$rp == "P-4"), big.mark = ",")))

# カテゴリー（列語幹と表示名）。定義は MANIFEST_kpi.txt を参照。
CATS <- c(acc = "会計数値", ratio = "財務比率", nongaap = "non-GAAP指標",
          nonfin = "非財務指標", stock = "株価関連指標")

# McNemar mid-p（両側）
#   mid-p = 2 * [ P(X<=k) - 0.5*P(X=k) ],  X ~ Binomial(b+c, 0.5),  k = min(b,c)
# 正確二項検定の保守性を補正した準正確検定（Fagerland, Lydersen & Laake 2013）。
midp <- function(b, c) {
  n <- b + c
  if (n == 0) return(1)
  k <- min(b, c)
  min(1, 2 * (pbinom(k, n, 0.5) - 0.5 * dbinom(k, n, 0.5)))
}

# 2×2分割表: a=両方採用, b=方針のみ, c=報酬のみ, d=両方不採用
cells <- function(sub, slug) {
  p <- sub[[paste0("pol_", slug)]]
  y <- sub[[paste0("pay_", slug)]]
  c(a = sum(p == 1 & y == 1), b = sum(p == 1 & y == 0),
    c = sum(p == 0 & y == 1), d = sum(p == 0 & y == 0))
}

fmt_p <- function(p) if (p < 0.001) "<0.001" else sprintf("%.3f", p)

report <- function(sub, title) {
  cat(sprintf("=== %s（n=%s） ===\n", title, format(nrow(sub), big.mark = ",")))
  cat(sprintf("%-14s %6s %6s %7s %9s %10s\n",
              "カテゴリー", "b", "c", "OR(c/b)", "mid-p", "反映率"))
  for (slug in names(CATS)) {
    x <- cells(sub, slug)
    or <- if (x["b"] > 0) x["c"] / x["b"] else NA
    rf <- if (x["a"] + x["b"] > 0) x["a"] / (x["a"] + x["b"]) else NA
    cat(sprintf("%-14s %6d %6d %7.2f %9s %9.1f%%\n",
                CATS[[slug]], x["b"], x["c"], or,
                fmt_p(midp(x["b"], x["c"])), rf * 100))
  }
  cat("\n")
}

P  <- d[d$rp == "P", ]
P4 <- d[d$rp == "P-4", ]

# ── 表: 不一致の非対称性（プール・P期）────────────────────────────
report(P, "P期 プール（プライム+スタンダード）")

# ── 表: 市場区分別（P期）───────────────────────────────────────
report(P[P$market == "プライム", ],     "P期 プライム")
report(P[P$market == "スタンダード", ], "P期 スタンダード")

# ── 表: 時点間比較（プール）────────────────────────────────────
cat("=== 時点間比較（プール、OR と反映率）===\n")
cat(sprintf("%-14s %9s %9s %11s %11s\n",
            "カテゴリー", "OR(P期)", "OR(P-4期)", "反映率(P期)", "反映率(P-4期)"))
for (slug in names(CATS)) {
  x <- cells(P, slug); x4 <- cells(P4, slug)
  cat(sprintf("%-14s %9.2f %9.2f %10.1f%% %10.1f%%\n", CATS[[slug]],
              x["c"] / x["b"], x4["c"] / x4["b"],
              x["a"] / (x["a"] + x["b"]) * 100,
              x4["a"] / (x4["a"] + x4["b"]) * 100))
}
cat("\n")

# ── 表: 株価関連指標の市場区分×時点 ───────────────────────────
cat("=== 株価関連指標 市場区分×時点（OR と mid-p）===\n")
for (rp in c("P", "P-4")) {
  s <- if (rp == "P") P else P4
  for (mk in c("プライム", "スタンダード")) {
    x <- cells(s[s$market == mk, ], "stock")
    cat(sprintf("  %-4s %-8s OR=%.2f  (b=%d, c=%d, mid-p=%s)\n",
                rp, mk, x["c"] / x["b"], x["b"], x["c"],
                fmt_p(midp(x["b"], x["c"]))))
  }
}
cat("\n")

# ── 参考: セクション別の採用率 ─────────────────────────────────
cat("=== 参考: セクション別の採用率（P期）===\n")
for (slug in names(CATS)) {
  cat(sprintf("  %-14s 経営方針 %5.1f%%   役員報酬 %5.1f%%\n", CATS[[slug]],
              mean(P[[paste0("pol_", slug)]]) * 100,
              mean(P[[paste0("pay_", slug)]]) * 100))
}
