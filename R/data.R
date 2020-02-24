#' Tissue specific genes data frame 
#'
#' The `ector_tissue_specific_genes` data frame describes tissue-specific genes. 
#' The rows correspond to genes while the columns correspond to their related informations, 
#' including position in GRCh38 reference genome.
#' The data presentation is inspired by BED (Browser Extensible Data) format.
#' Tissue specific genes are determined using threshold algorithme [ref. EpiMed]. ons.
#'
#' @format A data frame with 3433 rows and 7 columns
#' @source \url{http://epimed.univ-grenoble-alpes.fr/database}
"ector_tissue_specific_genes"

#' Transcriptome matrix
#'
#' The ector_transcriptome matrix...
#'
#' @format A matrix with 3000 rows and 44 columns
#' @source \url{https://portal.gdc.cancer.gov}
"ector_transcriptome"

#' Clinicals data frame
#'
#' The `ector_clinicals` data frame describes clinical information. 
#'
#' @format A data frame with XXX rows and 7 columns
#' @source \url{https://portal.gdc.cancer.gov}
"ector_clinicals"