
#libraries----
library(tidyverse)
library(ggthemes)
library(ggsci)

#pH functions----

#strong acids
ph.st.ac <- function(conc){ #conc is concentration in mol/L
  -log10(conc)
}

#strong bases
ph.st.ba <- function(conc){
  14 + log10(conc)
}

#weak acids
ph.we.ac <- function(conc, pka){ #pka is pKa
  (pka - log10(conc))/2
}

#weak bases
ph.we.ba <- function(conc, pka){
  7 + (pka + log10(conc))/2
}

#conjugated species
ph.conj <- function(base.conc, acid.conc, pka){
  pka + log10(base.conc/acid.conc)
}

#equivalent volume function----
vol.eq <- function(conc.titrant, conc.titrated, vol.titrated){
  conc.titrated * vol.titrated / conc.titrant
}

#reaction table----
reac.table <- function(conc.titrant, conc.titrated, vol.titrated, ab, strength, pka, burette, grad){
  
  data.frame(vol.titrant = seq(0, burette, by=grad),
             vol.eq = vol.eq(conc.titrant, conc.titrated, vol.titrated)) %>%
    mutate(
      vol.total = vol.titrated + vol.titrant,
      n.titrant.total = conc.titrant*vol.titrant,
      n.titrant = dplyr::if_else(vol.titrant < vol.eq, 
                                 0,
                                 n.titrant.total-vol.eq*conc.titrant),
      n.titrated = dplyr::if_else(vol.titrant < vol.eq,
                                  (conc.titrated * vol.titrated)-n.titrant.total,
                                  0),
      n.conj = dplyr::if_else(vol.titrant < vol.eq,
                              n.titrant.total,
                              conc.titrated * vol.titrated)
    ) %>%
    mutate(experiment = if_else(ab == 'acid' & strength == 'strong',
                                "strong acid",
                                if_else(ab == 'acid' & strength == 'weak',
                                        "weak acid",
                                        if_else(ab == 'base' & strength == 'strong',
                                                "strong base",
                                                "weak base"
                                        )
                                )
    )
    ) %>%
    mutate(pH = 
             if_else(
               experiment == 'strong acid',
               case_when(
                 vol.titrant == 0 ~ ph.st.ac(conc.titrated),
                 vol.titrant > 0 & vol.titrant < vol.eq ~ ph.st.ac(n.titrated/vol.total),
                 vol.titrant == vol.eq ~ 7,
                 vol.titrant > vol.eq ~ ph.st.ba(n.titrant/vol.total)
               ),
               if_else(
                 experiment == 'strong base',
                 case_when(
                   vol.titrant == 0 ~ ph.st.ba(conc.titrated),
                   vol.titrant > 0 & vol.titrant < vol.eq ~ ph.st.ba(n.titrated/vol.total),
                   vol.titrant == vol.eq ~ 7,
                   vol.titrant > vol.eq ~ ph.st.ac(n.titrant/vol.total)
                 ),
                 if_else(
                   experiment == 'weak acid',
                   case_when(
                     vol.titrant == 0 ~ ph.we.ac(conc.titrated, pka),
                     vol.titrant > 0 & vol.titrant < vol.eq ~ ph.conj(base.conc = n.conj, acid.conc = n.titrated, pka = pka),
                     vol.titrant == vol.eq ~ ph.we.ba(n.conj, pka = pka),
                     vol.titrant > vol.eq ~ ph.st.ba(n.titrant/vol.total)
                   ),
                   case_when(
                     vol.titrant == 0 ~ ph.we.ba(conc.titrated, pka),
                     vol.titrant > 0 & vol.titrant < vol.eq ~ ph.conj(base.conc = n.titrated, acid.conc = n.conj , pka = pka),
                     vol.titrant == vol.eq ~ ph.we.ac(n.conj, pka = pka),
                     vol.titrant > vol.eq ~ ph.st.ac(n.titrant/vol.total)
                   )
                 )
               )
             )
    )
}


# reac.table <- reac.table(conc.titrant = 0.1, conc.titrated = 0.05, vol.titrated = 25, ab = "base", strength = "weak", pka =7)
# 
# 
# p.ph <- ggplot(data = reac.table, aes(x = vol.titrant, y = pH, colour = pH)) +
#   geom_point() +
#   theme_pander() +
#   theme(legend.position = 'none')
# 
# p.ph
# 
# 
# p.n <- reac.table %>%
#   pivot_longer(cols = c(n.titrant, n.titrant.total, n.titrated, n.conj),
#                names_to = "species",
#                values_to = "n") %>%
#   ggplot(aes(x = vol.titrant, y = n, color = species)) +
#   geom_point() +
#   theme_pander() +
#   scale_colour_d3(labels = c('Conjuguée (A/B faibles)', 'Titrante', 'Titrante (total ajoutée)', 'Titrée')) +
#   labs(colour = 'Espèces')
# 
# p.n




