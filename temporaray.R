library(DBI)        # paket za povezivanje s bazom podataka
library(RMySQL)     # paket za povezivanje s RMySQL bazom podataka  
library(data.table) # data wrangling
library(ggplot2)    # vizualizacija
library(dplyr)      # data wrangling tidy way


# pomocna funkcija za povezivanje s GFI MySQL bazom podataka
gfiConnect <- function() {
  db <- dbConnect(MySQL(), dbname = "odvjet12_gfi",
                  host = "91.234.46.219",
                  port = 3306L,
                  user = "odvjet12_mislav",
                  password = "Contentio0207")
  return(db)
}

# pomo'na funkcija za slanje queya na GFI bazu podataka
gfiSelect <- function(query) {
  db <- gfiConnect()
  dbSendQuery(db, 'set character set "utf8"')
  dbSendQuery(db, 'SET NAMES utf8')
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  return(data)
}

# preuzimanje podataka za sektor programiranja i turistički sektor
q <- sprintf("SELECT b335/1000000000 AS Export,
                     b370/1000000000 AS Import,
                     b372 AS Zaposleni,
                     b266/1000000000 AS Software,
                     b249/1000000000 AS Razvoj,
                     b250/1000000000 AS Tech, 
                     b004/1000000000 AS Razvoj1,
                     b005/1000000000 AS Tech1,
                     b003/1000000000 AS Nematerijalna,
                     b110/1000000000 AS Prihod,
                     b120/1000000000 AS Placa,
                     b147/1000000000 AS Nett,
                     b329/1000000000 AS Grants,
                     reportyear, subjectid, employeecounteop, foreigncontrol,
                     nacerev21, nacerev22, nacerev23, nacerev24
                FROM gfi_all 
                WHERE reportyear >= 2000 AND nacerev21 IN ('J', 'I', 'C');")
gfi <- gfiSelect(q)       # preuzimanje podataka
# gfi <- as.data.table(gfi) # koristenje data.table paketa za data.wangling (alternativa base R ili dplyr)
gfi$nacerev22 <- as.character(gfi$nacerev22)

# sifrarnici
nkd <- read.csv2(file = "D:/data/poslovni_subjekti/sifrarnik/cb_reg_activity_translations.csv")
nkd <- nkd[, c("ActivityID", "ActivityName_.HRV.")]



# TIDY WAY WRANGLING ------------------------------------------------------

# agregacija za sektor J i I TIDY WAY
gfi_nacerev <- gfi %>% 
  select(subjectid, reportyear, nacerev21, Export, Import, Prihod, Placa, Nett, Zaposleni, Grants) %>% # odabiremo kolone
  group_by(nacerev21, reportyear) %>% # grupoiramo prema dvije varijable kada racunamo sumu
  summarise(across(where(is.double), sum)) %>% # grupirmao prema svim kolonama osim grupiranim varijablama (nacerev21, reportyear)
  ungroup() %>% # ungroup, ovo je bilo potrebno u starijoj verziji dplyra, nisam siguran je li jos uvijek, ali neka bude...
  mutate( # kreiramo nove varijable
    NetoMarza = Nett / Prihod,
    IzvovPrihod = Export / Prihod,
    Produktivnost = Placa / Zaposleni,
    NetoIzvozPrihodi = (Export - Import) / Prihod
  )



# DATA.TABLE WAY WRANGLING ------------------------------------------------
# agregacija za sektor J i I data.table WAY
# gfi_sample <- gfi[, .(subjectid, reportyear, nacerev21, Export, Import, Prihod, Placa, Nett, Zaposleni, Grants)] # trebamo samo ove variajble
# gfi_nacerev <- gfi_sample[, lapply(.SD, sum), by = c("nacerev21", "reportyear"), 
#                           .SDcols = c("Export", "Import", "Prihod", "Placa", "Nett", "Zaposleni", "Grants")]
# setorderv(gfi_nacerev, c("nacerev21", "reportyear"))


# racunanje varijabli
# gfi_nacerev[, `:=`(
#   NetoMarza = Nett / Prihod,
#   IzvovPrihod = Export / Prihod,
#   Produktivnost = Placa / Zaposleni,
#   NetoIzvozPrihodi = (Export - Import) / Prihod
# )]



# VIZUALIZACIJA -----------------------------------------------------------
# vizualizazija varijabli
# udio izvoza u prihodu
ggplot(gfi_nacerev %>% select(nacerev21, reportyear, IzvovPrihod) %>% filter(nacerev21 %in% c("I", "J")), 
       aes(x = reportyear, y = IzvovPrihod, color = nacerev21)) +
  geom_line() +
  ggtitle("Udi izvoza u prihodu za NACEREV-2007 sektore I i J") +
  labs(x = "Godine", y = "Udio izvoza u prihodu")
# Neto marza
ggplot(gfi_nacerev %>% select(nacerev21, reportyear, NetoMarza) %>% filter(nacerev21 %in% c("I", "J")), 
       aes(x = reportyear, y = NetoMarza, color = nacerev21)) +
  geom_line() +
  ggtitle("Udi neto dobiti u prihodu za NACEREV-2007 sektore I i J") +
  labs(x = "Godine", y = "Udio neto dobiti u prihodu")
# Produktivnost
ggplot(gfi_nacerev %>% select(nacerev21, reportyear, Produktivnost) %>% filter(nacerev21 %in% c("I", "J")), 
       aes(x = reportyear, y = Produktivnost, color = nacerev21)) +
  geom_line() +
  ggtitle("Produktivnost za NACEREV-2007 sektore I i J") +
  labs(x = "Godine", y = "Odnos plaća i broja zaposlenih")
# R & v
ggplot(gfi_nacerev %>% select(nacerev21, reportyear, NetoIzvozPrihodi) %>% filter(nacerev21 %in% c("I", "J")), 
       aes(x = reportyear, y = NetoIzvozPrihodi, color = nacerev21)) +
  geom_line() +
  ggtitle("Neto izvoz u odnosu na prihode za NACEREV-2007 sektore I i J") +
  labs(x = "Godine", y = "Odnos neto izvoza i prihoda")



# SMJESTAJ VS PROGRAMIRANJE -----------------------------------------------

# TIDY WAY
gfi_nacerev2 <- gfi %>% 
  select(subjectid, reportyear, nacerev22, Export, Import, Prihod, Placa, Nett, Zaposleni, Grants) %>% # odabiremo kolone
  group_by(nacerev22, reportyear) %>% # gruopiramo prema ove dvije varijable 
  summarise(across(where(is.double), sum)) %>% # suma svih varijabli, osim gruopiranih
  ungroup() %>% # ungroup, ovo je bilo potrebno u starijoj verziji dplyra, nisam siguran je li jos uvijek, ali neka bude...
  mutate( # kreiramo nove kolone
    NetoMarza = Nett / Prihod,
    IzvovPrihod = Export / Prihod,
    Produktivnost = Placa / Zaposleni,
    NetoIzvozPrihodi = (Export - Import) / Prihod,
    GrantsRatio = Grants / Prihod
  ) %>% 
  left_join(., nkd, by = c("nacerev22" = "ActivityID")) %>% # spajamo opise nkd djelatnosti pomocu sigrarnika
  filter(nacerev22 %in% c("55", "62", "10")) # filtriramo redove u kojima je nacerev22 55, 62 ili 
# # ovo se moze otkomentirat ako se zeli promijeniti naziv sifri i dodat %>% u red iznad
# case_when( # mijenjamo
#   nacerev22 == "55" ~ "Smještaj",
#   nacerev22 == "62" ~ "Programiranje",
#   nacerev22 == "10" ~ "Prehrambena industrija"
# )


# DATA.TABLE WAY
# agregacija za cektor J i I
# gfi_sample <- gfi[, .(subjectid, reportyear, nacerev22, Export, Import, Prihod, Placa, Nett, Zaposleni, Grants)] # trebamo samo ove variajble
# gfi_nacerev <- gfi_sample[, lapply(.SD, sum), by = c("nacerev22", "reportyear"), 
#                           .SDcols = c("Export", "Import", "Prihod", "Placa", "Nett", "Zaposleni", "Grants")]
# setorderv(gfi_nacerev, c("nacerev22", "reportyear"))

# racunanje varijabli
# gfi_nacerev[, `:=`(
#   NetoMarza = Nett / Prihod,
#   IzvovPrihod = Export / Prihod,
#   Produktivnost = Placa / Zaposleni,
#   NetoIzvozPrihodi = (Export - Import) / Prihod,
#   GrantsRatio = Grants / Prihod
# )]
# gfi_nacerev <- gfi_nacerev[nacerev22 %in% c("55", "62")]
# gfi_nacerev[, nacerev22 := ifelse(nacerev22 == "55", "Smještaj", "Programiranje") ]
# gfi_nacerev[, nacerev22 := as.factor(nacerev22)]



# vizualizazija varijabli
# udio izvoza u prihodu
ggplot(gfi_nacerev2 %>% select(nacerev22, reportyear, IzvovPrihod), 
       aes(x = reportyear, y = IzvovPrihod, color = nacerev22)) +
  geom_line() +
  ggtitle("Udi izvoza u prihodu za NACEREV-2007 sektore Smještaja i Progamiranja") +
  labs(x = "Godine", y = "Udio izvoza u prihodu")
# Neto marza
ggplot(gfi_nacerev2 %>% select(nacerev22, reportyear, NetoMarza),
       aes(x = reportyear, y = NetoMarza, color = nacerev22)) +
  geom_line() +
  ggtitle("Udi neto dobiti u prihodu za NACEREV-2007 sektore Smještaja i Progamiranja") +
  labs(x = "Godine", y = "Udio neto dobiti u prihodu")
# Produktivnost
ggplot(gfi_nacerev2 %>% select(nacerev22, reportyear, Produktivnost), 
       aes(x = reportyear, y = Produktivnost, color = nacerev22)) +
  geom_line() +
  ggtitle("Produktivnost za NACEREV-2007 sektore Smještaja i Progamiranja") +
  labs(x = "Godine", y = "Odnos plaća i broja zaposlenih")
# R & v
ggplot(gfi_nacerev2 %>% select(nacerev22, reportyear, NetoIzvozPrihodi), 
       aes(x = reportyear, y = NetoIzvozPrihodi, color = nacerev22)) +
  geom_line() +
  ggtitle("Neto izvoz u odnosu na prihode za NACEREV-2007 sektore Smještaja i Progamiranja") +
  labs(x = "Godine", y = "Odnos neto izvoza i prihoda")

