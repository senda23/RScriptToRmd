library(rstudioapi)

convert_script_to_rmd <- function() {
  # アクティブなRスクリプトを取得
  context <- rstudioapi::getActiveDocumentContext()
  script_content <- context$contents
  
  # Rスクリプトの内容を###で分割
  lines <- unlist(strsplit(script_content, "\n"))
  chunks <- split(lines, cumsum(grepl("^###", lines)))
  
  # Rmdフォーマットに変換
  rmd_content <- ""
  for (chunk in chunks) {
    chunk_content <- paste(chunk, collapse = "\n")
    chunk_content <- sub("^###", "```{r}", chunk_content)
    chunk_content <- paste0(chunk_content, "\n```")
    rmd_content <- paste(rmd_content, chunk_content, sep = "\n")
  }
  
  # 新しいRmdファイルを作成
  new_file <- sub("\\.R$", ".Rmd", context$path)
  writeLines(rmd_content, new_file)
  
  # 新しいファイルをRStudioで開く
  rstudioapi::navigateToFile(new_file)
}

# アドインとして関数を登録
convert_script_to_rmd
