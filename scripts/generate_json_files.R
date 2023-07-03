library(stringr)
library(jsonlite)
library(data.table)

rice <- fread("rice_name_20230629.tsv")
colnames(rice) <- c("ped_finder_id","cultivar_name", "NAROGeneBank")
rice <- rice[rice$cultivar_name != "",]
# RiceURI.txt
rice_url <- fread("RiceURI.txt", sep="\t", header=T)
rice_url <- rice_url[, c(1,3)]
colnames(rice_url) <- c("NAROGeneBank", "URI")
rice_naro <- rice[rice$NAROGeneBank != "",]
rice_naro <- merge(rice_naro, rice_url, by="NAROGeneBank")
rice_naro$NAROGeneBank <- sub(" ", "", rice_naro$NAROGeneBank)
rice_naro$cultivar_name <- paste(rice_naro$NAROGeneBank, rice_naro$cultivar_name, sep="_")
plants_attr <- fread("plants_attr.tsv", sep = "\t", header = TRUE)
# tax_id = 39947 (Oryza sativa Japonica Group)
oryza <- plants_attr[plants_attr$attribute_name == "cultivar" & plants_attr$taxon_id == 39947, ]
oryza <- oryza[, c(1, 4)]
colnames(oryza)[colnames(oryza) == "attribute_value"] <- "cultivar"
# NARO Dataset
naro <- plants_attr[plants_attr$attribute_name == "sample_name" & plants_attr$taxon_id == 39947, ]
naro <- naro[, c(1, 4)]
naro <- naro[grep("JRC", naro$attribute_value),]
colnames(naro)[colnames(naro) == "attribute_value" ] <- "sample_name"
naro <- merge(oryza, naro, by="biosample")
naro$cultivar_name <- str_to_upper(naro$sample_name)
tb2 <- merge(naro, rice_naro, by="cultivar_name")
fwrite(tb2, "pedigree_finder_biosample_ddbj_naro.tsv", sep="\t", row.names=F, col.names=T)
# DDBJ Experiment Dataset
drx <- fread("dblink_ddbj_devel/trace/drmdb.drx_ssub.csv", sep=",", header=T)
bs2smpid <- fread("dblink_ddbj_devel/trace/bsDB_biosample_smp_id.csv", sep=",", header=F)
colnames(bs2smpid) <- c("biosample", "smp_id")
drx <- merge(bs2smpid, drx, by="smp_id")
drx <- drx[, 2:3]
tb2 <- merge(drx, tb2, by="biosample", all.y=T)
colnames(tb2)[colnames(tb2) == "biosample" ] <- "germplasmDbId"
colnames(tb2)[colnames(tb2) == "drx" ] <- "studyDbIds"
tb2$germplasmName <- paste(tb2$sample_name, tb2$cultivar, sep=" - ")
tb2$subtaxa <- paste("var.", tb2$cultivar, sep = " ")
tb2$germplasmPUI <- paste("https://ddbj.nig.ac.jp/resource/biosample", tb2$germplasmDbId, sep="/")
tb2$documentationURL <- paste("https://ddbj.nig.ac.jp/resource/biosample", tb2$germplasmDbId, sep="/")
tb2$node <- "DDBJ"
tb2$databaseName <- "BioSample"
tb2$holdingInstitute.instituteName <- input_json$properties$Owner$Name$content
tb2$genus <- organism_name$V1
tb2$species <- organism_name$V2
tb2 <- subset(tb2, select=-c(studyDbIds))
tb2$temp <- tb2$studyDbIds
for (j in 1:29){
    studyDbIds[j] <- list(tb2$temp[j])
}
tb2$studyDbIds <- studyDbIds
tb2$taxonIds.sourceName <- "ncbiTaxon"
tb2$taxonIds.taxonId <- input_json$properties$Description$Organism$taxonomy_id
# Experiment
list_germoplasm <- unique(tb2$germplasmDbId)
for (i in 1:lenght(list_germoplasm)){
    ids <- tb2$germplasmDbId[i]
    filename <- "https://ddbj.nig.ac.jp/resource/biosample/BIOSAMPLE_ID.json"
    filename <- sub("BIOSAMPLE_ID", ids, filename)
    input_json <- jsonlite::fromJSON(filename)
    organism_name <- as.data.frame(str_split(input_json$organism$name, " ", simplify=T)) 
    tb2$genus[i] <- organism_name$V1
    tb2$species[i] <- organism_name$V2
}
tb2 <- tb2[, c("germplasmDbId", "studyDbIds",  "germplasmName", "genus", "species", "subtaxa", "germplasmPUI", "documentationURL", "node", "databaseName", "holdingInstitute.instituteName")]
tb2$accessionNumber <- tb2$germplasmDbId
jsontb2 <- toJSON(tb2, pretty=TRUE)
write(jsontb2, "sample_rice_naro_germoplasm.json")
# Study
tbstudy$studyName <- c()
tbstudy$studyType <-  "Genomic Study"
tbstudy <- subset(tbstudy, select=-c(germplasmDbId))
tbstudy$documentationURL <- paste("https://ddbj.nig.ac.jp/resource/sra-experiment", tbstudy$studyDbIds, sep="/")

for (i in 3:length(tbstudy$studyDbId)){
    filename <- tbstudy$documentationURL[i]
    input_json <- jsonlite::fromJSON(filename)
    tbstudy$studyName[i] <- input_json$title
}
tbstudy <- tbstudy[, c("studyDbIds", "studyName", "studyType", "documentationURL")]
colnames(tbstudy)[colnames(tbstudy) == "studyDbIds"] <- "studyDbId"
jsontbstudy <- toJSON(tbstudy, pretty=TRUE)
write(jsontbstudy, "sample_rice_naro_study.json")
