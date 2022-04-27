library(xml2)
library(methods)
library(tidyverse)


#' RPU
#'
#' Classe "mal" utilisée pour le moment
#' Sert à stocker les RPU dans 1 unifié
#'
#' @slot PASSAGES data.frame.
#' @slot FINESS character.
#' @slot DDEBUT Date de début du fichier.
#' @slot DFIN Date de fin du fichier.
#' @slot xml data.frame
#' @slot ACTES
#' @slot DIAGS
#'
#' @return
#' @export
#'
#' @examples
setClass("RPU", slots = c(
  xml       = "xml_document",
  PASSAGES  = "data.frame",
  ACTES     = "data.frame",
  DIAGS     = "data.frame",
  FINESS    = "character",
  DDEBUT    = "POSIXlt",
  DFIN      = "POSIXlt"
))

rpu_table <- function(xml) {
  xml_list <- xml %>% as_list
  xml_df <- xml_list %>% as.tibble()
}

#' rpu_patients
#' transformation d'un objet xml2 en tibble RPU v2006
#' 
#' @param xml  objet xml2 correspondant à un export RPU au format OSCOUR 2006
#'
#' @return dataframe contenant les champs du RPU dans une structure "à plat"
#'         sauf pour les diagnostics et les actes qui sont des
#'         vecteurs de character().
#' @export
#'
#' @examples
#' rpu <- rpu_patient(xml)
rpu_patients <- function(xml, config = NA) {

  #----- Configuration par défaut -----
  colonnesUnaires <- c(
    "CP",
    "COMMUNE",
    "NAISSANCE",
    "SEXE",
    "ENTREE",
    "MODE_ENTREE",
    "PROVENANCE",
    "TRANSPORT",
    "TRANSPORT_PEC",
    "MOTIF",
    "GRAVITE",
    "DP",
    "SORTIE",
    "MODE_SORTIE",
    "DESTINATION",
    "ORIENT"
  )
  colonnesMult <- c("LISTE_ACTES", "LISTE_DA")

  #----- Début du traitement -----

  xml_df <- rpu_table(xml)

  base <- xml_df %>% unnest_longer(OSCOUR, indices_to = "OID")

  ret <- base %>%
    filter(OID == "PATIENT") %>%
    select(-OID) %>%
    unnest_wider(OSCOUR) %>%
    unnest(cols = any_of(colonnesUnaires)) %>%
    unnest(cols = any_of(colonnesUnaires)) %>%
    mutate(
      MODE_ENTREE = ifelse(is.na(MODE_ENTREE), "", MODE_ENTREE),
      MODE_SORTIE = ifelse(is.na(MODE_SORTIE), "", MODE_SORTIE),
      PROVENANCE = ifelse(is.na(PROVENANCE), "", PROVENANCE),
      DESTINATION = ifelse(is.na(DESTINATION), "", DESTINATION),
      ENTREE = as.POSIXlt(ENTREE, format = "%d/%m/%Y %H:%M"),
      SORTIE = as.POSIXlt(SORTIE, format = "%d/%m/%Y %H:%M")
    ) %>%
    mutate(MCENT = paste(MODE_ENTREE, PROVENANCE, sep = "-"),
           MCSORT = paste(MODE_SORTIE, DESTINATION, sep = "-"),
           DUREE = difftime(SORTIE, ENTREE, units = "mins"))


  unnest_perso <- Vectorize(function(x) {
    if (is.null(x)) {
    } else {
      as.vector(unlist(x, use.names = F))
    }
  }, vectorize.args = "x")
  ret <-
    ret %>% mutate(DA = unnest_perso(LISTE_DA),
                   ACTES = unnest_perso(LISTE_ACTES))

  ret %>% select(-colonnesMult)
}

extrait_Diags <- function(x) {
  rbind(
    x %>%
      mutate(ID = row.names(.)) %>%
      select(ID, DA) %>%
      unnest_longer(DA) %>%
      mutate(TYPE = "DA") %>%
      rename(DIAG = DA),
    x %>%
      mutate(ID = row.names(.)) %>%
      select(ID, DP) %>%
      mutate(TYPE = "DP") %>%
      rename(DIAG = DP)
  ) %>%
   arrange(ID)

}

extrait_Actes <- function(x) {
  x %>% mutate(ID = row.names(.)) %>% select(ID, ACTES) %>% unnest_longer(ACTES)
}

charge_rpu <- function(file, garde_xml=F, traite_diags=F, traite_actes=F) {
  rpu <- new("RPU")

  xmldoc <- read_xml(file)

  if (garde_xml) rpu@xml <- xmldoc

  rpu@PASSAGES <- rpu_patients(xmldoc)

  if (traite_diags) rpu@DIAGS <- rpu@PASSAGES %>% extrait_Diags()
  if (traite_actes) rpu@ACTES <- rpu@PASSAGES %>% extrait_Actes()

  rpu
}