library(rstudioapi)

convert_script_to_rmd <- function() {
  # アクティブなRスクリプトを取得
  context <- rstudioapi::getActiveDocumentContext()
  script_content <- context$contents
  script_path <- context$path

  # Rスクリプトの内容を###で分割
  lines <- unlist(strsplit(script_content, "\n"))
  chunks <- split(lines, cumsum(grepl("^###", lines)))

  # 新しいRmdファイルのパスを決定
  if (nzchar(script_path)) {
    new_file <- sub("\\.R$", ".Rmd", script_path)
  } else {
    new_file <- file.path(getwd(), "new_file.Rmd")
  }

  if (file.exists(new_file)) {
    # 既存のRmdファイルを読み込む
    existing_rmd_content <- readLines(new_file)

    for (chunk in chunks) {
      if (grepl("^###", chunk[[1]])) {
        chunk_header <- gsub("^###\\s*", "", chunk[[1]])
        chunk_content <- paste(chunk[-1], collapse = "\n")
        chunk_text <- paste0("```{r ", chunk_header, "}\n", chunk_content, "\n```")

        # 既存のRmdファイルに同一のrチャンクが存在するか確認
        pattern <- paste0("```\\{r\\s+", gsub("\\s", "\\\\s", chunk_header), "\\s*\\}")
        existing_chunk_start <- grep(pattern, existing_rmd_content)

        if (length(existing_chunk_start) > 0) {
          # 同一のrチャンクが存在する場合、そのチャンクを置き換える
          existing_chunk_end <- grep("^```$", existing_rmd_content[existing_chunk_start:length(existing_rmd_content)]) + existing_chunk_start - 1
          existing_rmd_content <- c(existing_rmd_content[1:(existing_chunk_start - 1)], chunk_text, existing_rmd_content[(existing_chunk_end + 1):length(existing_rmd_content)])
        } else {
          # 同一のrチャンクが存在しない場合はエラーを出力して停止
          stop("同一のrチャンクが存在しません。Rmdファイルを上書きできません。")
        }
      }
    }

    # 既存のRmdファイルを更新
    writeLines(existing_rmd_content, new_file)
  } else {
    # 新しいRmdファイルの内容を生成
    rmd_header <- "---
title: \"タイトルを入力\"
output:
  pdf_document:
    latex_engine: lualatex
header-includes:
  - \\usepackage{siunitx}
  - \\usepackage{float}
documentclass: ltjsarticle
---

```{r RmdSetup, include=FALSE}
library(knitr)
library(imager)

# 出力フォーマットが TeX（PDF含む）の場合のみ対処する
if (knitr::opts_knit$get(\"rmarkdown.pandoc.to\") %in% c(\"beamer\", \"latex\")) {
  # conversion failure on '...' in 'mbcsToSbcs' の Warning 発生の workaround
  options(device = function(file, width = 7, height = 7, ...) {
    cairo_pdf(tempfile(), width = width, height = height, ...)
  })
  # cairo_pdf を使う方法
  # family には OS にインストールされているフォント名を指定する。
  knitr::opts_chunk$set(dev=\"cairo_pdf\", dev.args=list(family=\"Yu Gothic UI\"))
}
```"

    # Rmdフォーマットに変換
    rmd_content <- rmd_header
    for (chunk in chunks) {
      if (grepl("^###", chunk[[1]])) {
        chunk_header <- gsub("^###\\s*", "", chunk[[1]])
        chunk_content <- paste(chunk[-1], collapse = "\n")
        chunk_text <- paste0("```{r ", chunk_header, "}\n", chunk_content, "\n```")
        rmd_content <- paste(rmd_content, chunk_text, sep = "\n\n\n")
      }
    }

    # 新しいRmdファイルを作成
    writeLines(rmd_content, new_file)
  }

  # 新しいファイルをRStudioで開く
  if (file.exists(new_file)) {
    rstudioapi::navigateToFile(new_file)
  } else {
    stop("ファイルが存在しません: ", new_file)
  }
}

# アドインとして関数を登録
convert_script_to_rmd
