# edinet-xbrl-replication

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.21804805.svg)](https://doi.org/10.5281/zenodo.21804805)

EDINET（金融庁）で公開されている有価証券報告書のXBRLを解析した研究の、**確定データと再現用
スクリプト**を収めたリポジトリです。データベースへの接続は不要で、各ディレクトリのCSVと
スクリプトだけで論文の主要数値を再現できます。

著者: 塘 誠（Makoto Tomo, 成城大学経済学部）

---

## 収録パッケージ

| ディレクトリ | 対応する研究 | 内容 |
|---|---|---|
| [`jama2026/`](jama2026/) | 日本管理会計学会 2026年全国大会 自由論題 | KPI採用パネル（4,087行）とR再現スクリプト |

各パッケージの構成・実行手順・列定義・データ生成条件は、それぞれの `README.md` と
`MANIFEST*.txt` に記載しています。

今後の稿の再現データも本リポジトリに追加します。論文ごとの引用は、後述のとおり
リリースタグ単位のDOIで区別してください。

---

## 使い方

```bash
git clone https://github.com/tomoreal/edinet-xbrl-replication.git
cd edinet-xbrl-replication/jama2026
Rscript verify_kpi_from_csv.R     # 数十秒・base Rのみ
```

スクリプトはデータをスクリプト自身のディレクトリから探すため、パス設定は不要です。
別の場所にCSVを置く場合のみ環境変数 `JAMA2026_DATA` で指定してください。

CSVのSHA256は各 `MANIFEST*.txt` に記録しています。取得後に照合できます。

```bash
sha256sum jama2026/panel_kpi.csv        # Linux
shasum -a 256 jama2026/panel_kpi.csv    # macOS
```

---

## データについて

- 出典は金融庁EDINETで公開されている有価証券報告書です。二次利用にあたっては
  [EDINETの利用規約](https://disclosure2.edinet-fsa.go.jp/)をあわせてご確認ください。
- 企業の識別子は**EDINETコードのみ**で、企業名は含みません。
- いずれも**分析基準日を固定した凍結データ**です。元のデータベースは日次更新されるため、
  後日に再抽出しても同じ値にはなりません。再現には必ず本リポジトリのCSVを用いてください。
- 生のXBRLファイルそのものは含みません。EDINETから取得してください。

---

## 引用

本データを利用した場合は、下記のDOIを引用してください。DOIはリリースごとに発行され、
すべてのバージョンを指す代表DOI（Concept DOI）と、特定バージョンを指すDOIがあります。
**再現性のためには特定バージョンのDOIを引用してください。**

| DOI | 指す対象 |
|---|---|
| [10.5281/zenodo.21806185](https://doi.org/10.5281/zenodo.21806185) | **v1.1.0**（特定バージョン。引用にはこちら） |
| [10.5281/zenodo.21804805](https://doi.org/10.5281/zenodo.21804805) | 全バージョン代表（Concept DOI。常に最新版を指す） |

```
塘 誠 (2026) edinet-xbrl-replication: EDINET/XBRL解析研究の再現データ (Version 1.1.0)
[Data set]. Zenodo. https://doi.org/10.5281/zenodo.21806185
```

```
Tomo, M. (2026). edinet-xbrl-replication: Replication data for research based on
EDINET/XBRL filings (Version 1.1.0) [Data set]. Zenodo.
https://doi.org/10.5281/zenodo.21806185
```

機械可読な引用情報は [`CITATION.cff`](CITATION.cff) にあります。

---

## ライセンス

| 対象 | ライセンス |
|---|---|
| データ（`*.csv`, `MANIFEST*.txt`） | [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/deed.ja) |
| スクリプト（`*.R`） | [MIT License](LICENSE) |

詳細は [`LICENSE-DATA.md`](LICENSE-DATA.md) を参照してください。

---

## 問い合わせ

塘 誠（成城大学経済学部）
