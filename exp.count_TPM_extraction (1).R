#####################################################################
####### RSEMで出力されたファイルを結合する ##########################
#####################################################################

# 各サンプルのカウントデータを遺伝子名で紐付けて一つのテーブルにする
# edgeRにはexpected_countを利用する（注意　T検定、ヒートマップなどはTPMを用いる）
# edgeRの際にSに対するRのFCを計算させるため、groupingのところを工夫する
# current directoryの下にresultディレクトリ作成し、そこに*genes.results を格納しておく
# RSEMのときに使用したfastq.list.txtを利用する


####################################################################
####### expected.countのファイル ###################################
####################################################################

library(dplyr)

# ファイル名の読み込み。current directoryの下のresultディレクトリのファイル名を読み込む。
# full.names=Tでresultからのpathの表示

file.name <- list.files("result",full.names=T)
file.name[1]

# ループ用に数をカウント
n = length(file.name)

# ループ用にn=1のサンプルについてファイル作成
# 1列名はgene_id、5列名はexpected_count（TPMは6列名）
d1 <- read.table(file.name[1], head = T, sep = "\t") %>% dplyr::select(1,5)
head(d1)

for (i in 2:n){
  temp <- read.table(file.name[i], head = T, sep = "\t") %>% dplyr::select(1,5)
  d1 <- d1 %>% left_join(temp, by = "gene_id")
}
head(d1)

#列名をサンプルIDに書き換える
L1 <-read.table("fastq.SE.list.txt", head = F, sep = "\t")
colnames(d1) <- c("gene_id",L1$V1)
head(d1)

#edgeR用に、行名をtranscriptIDに置換し、transcriptID列を削除
rownames(d1) <- d1$gene_id
d1 <- d1 %>% select(!(1))
head(d1)

#edgeR用に、ファイルを書き出し
write.table(d1,"exp.counts.txt", sep="\t",quote=F)



####################################################################
####### TPMのファイル ##############################################
####################################################################

library(dplyr)

# ファイル名の読み込み。current directoryの下のresultディレクトリのファイル名を読み込む。
# full.names=Tでresultからのpathの表示

file.name <- list.files("result",full.names=T)
file.name[1]
file.name[2]

# ループ用に数をカウント
n = length(file.name)

# ループ用にn=1のサンプルについてファイル作成
# 1列名はgene_id、5列名はexpected_count（TPMは6列名）
d2 <- read.table(file.name[1], head = T, sep = "\t") %>% dplyr::select(1,6)
head(d2)

for (i in 2:n){
  temp <- read.table(file.name[i], head = T, sep = "\t") %>% dplyr::select(1,6)
  d2 <- d2 %>% left_join(temp, by = "gene_id")
}
head(d2)

#列名をサンプルIDに書き換える
L1 <-read.table("fastq.SE.list.txt", head = F, sep = "\t")
colnames(d2) <- c("gene_id",L1$V1)
head(d2)

#行名をtranscriptIDに置換し、transcriptID列を削除
rownames(d2) <- d2$gene_id
d2 <- d2 %>% select(!(1))
head(d2)

#ファイルを書き出し
write.table(d2,"TPM.txt", sep="\t",quote=F)

