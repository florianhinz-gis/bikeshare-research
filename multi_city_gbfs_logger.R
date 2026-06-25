# =============================================================================
# Multi-City GBFS Logger
# Erfasst die Fahrradanzahl pro Station fuer mehrere Bikesharing-Systeme
# (verschiedene Staedte, verschiedene Anbieter/Technik) und haengt das
# Ergebnis an eine CSV-Datei an. Gedacht fuer wiederholten Aufruf
# (z.B. alle 5-15 Minuten via GitHub Actions Cron).
# =============================================================================

required_packages <- c("jsonlite", "dplyr", "purrr", "httr")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}

library(jsonlite)
library(dplyr)
library(purrr)
library(httr)

# Manche Anbieter (z.B. Oslo Bysykkel) verlangen einen Client-Identifier-Header.
# Wird von anderen Anbietern einfach ignoriert, schadet also nicht.
client_id <- "bikeshare-research-script-mac"

gbfs_get <- function(url) {
  res <- GET(
    url,
    add_headers(
      `Client-Identifier` = client_id,
      `User-Agent` = "Mozilla/5.0 (Bikeshare-Research-Script)"
    )
  )
  fromJSON(content(res, as = "text", encoding = "UTF-8"))
}

# -----------------------------------------------------------------------------
# 1. Konfiguration: Liste aller Systeme
#    "gbfs_url" ist jeweils die Auto-Discovery-Datei (gbfs.json), aus der das
#    Skript automatisch die korrekten station_information/station_status
#    Endpunkte ausliest - das funktioniert systemuebergreifend, auch wenn
#    Anbieter unterschiedliche URL-Strukturen verwenden.
# -----------------------------------------------------------------------------

systeme <- tibble::tibble(
  land = c(
    "DE","DE","DE","DE","DE","DE","DE","DE","DE","DE",
    "AT","BE","BE","NO","ES"
  ),
  stadt = c(
    "Wiesbaden","Duesseldorf","Leipzig","Dresden","Berlin",
    "Muenchen","Bremen (Bre.Bike)","Ruhrgebiet (metropolradruhr)",
    "Kiel/KielRegion (SprottenFlotte)","Hannover (Donkey Republic)",
    "Wien","Bruessel (Villo)","Antwerpen","Oslo (Oslo Bysykkel)","Madrid (BiciMAD)"
  ),
  anbieter = c(
    "nextbike","nextbike","nextbike","nextbike","nextbike","nextbike",
    "nextbike","nextbike",
    "Donkey Republic","Donkey Republic",
    "nextbike","cyclocity/JCDecaux","smartbike.com",
    "Urban Infrastructure Partner (urbansharing.com)","PBSC (publicbikesystem.net)"
  ),
  gbfs_url = c(
    "https://gbfs.nextbike.net/maps/gbfs/v2/nextbike_wn/gbfs.json",
    "https://gbfs.nextbike.net/maps/gbfs/v2/nextbike_dd/gbfs.json",
    "https://gbfs.nextbike.net/maps/gbfs/v2/nextbike_le/gbfs.json",
    "https://gbfs.nextbike.net/maps/gbfs/v2/nextbike_dx/gbfs.json",
    "https://gbfs.nextbike.net/maps/gbfs/v2/nextbike_bn/gbfs.json",
    "https://gbfs.nextbike.net/maps/gbfs/v2/nextbike_ml/gbfs.json",
    "https://gbfs.nextbike.net/maps/gbfs/v2/nextbike_bq/gbfs.json",
    "https://gbfs.nextbike.net/maps/gbfs/v2/nextbike_mr/gbfs.json",
    "https://stables.donkey.bike/api/public/gbfs/3.0/donkey_kielsmile/gbfs.json",
    "https://stables.donkey.bike/api/public/gbfs/3.0/donkey_hannover/gbfs.json",
    "https://gbfs.nextbike.net/maps/gbfs/v2/nextbike_wr/gbfs.json",
    "https://api.cyclocity.fr/contracts/bruxelles/gbfs/v3/gbfs.json",
    "https://gbfs.smartbike.com/antwerp/1.0/gbfs.json",
    "https://gbfs.urbansharing.com/oslobysykkel.no/gbfs.json",
    "https://madrid.publicbikesystem.net/customer/gbfs/v3.0/gbfs.json"
  )
)

# -----------------------------------------------------------------------------
# 2. Hilfsfunktion: aus der Auto-Discovery-Datei die richtigen Feed-URLs holen
# -----------------------------------------------------------------------------
get_feed_urls <- function(gbfs_url) {
  disc <- gbfs_get(gbfs_url)

  # GBFS v1/v2: data -> <sprache> -> feeds (data.frame mit name/url)
  # GBFS v3:    data -> feeds (data.frame mit name/url), keine Sprachebene
  feeds_df <- NULL

  if (!is.null(disc$data$feeds)) {
    feeds_df <- disc$data$feeds
  } else {
    # erste verfuegbare Sprache nehmen
    erste_sprache <- disc$data[[1]]
    feeds_df <- erste_sprache$feeds
  }

  info_url   <- feeds_df$url[feeds_df$name == "station_information"]
  status_url <- feeds_df$url[feeds_df$name == "station_status"]

  list(info_url = info_url, status_url = status_url)
}

# -----------------------------------------------------------------------------
# 3. Hilfsfunktion: ein einzelnes System abfragen
# -----------------------------------------------------------------------------
log_system <- function(land, stadt, anbieter, gbfs_url, zeitstempel) {
  tryCatch({
    urls <- get_feed_urls(gbfs_url)

    info   <- gbfs_get(urls$info_url)$data$stations
    status <- gbfs_get(urls$status_url)$data$stations

    info_df <- info %>%
      select(station_id, name, lat, lon, any_of("capacity"))

    # GBFS v3.0 erlaubt mehrsprachige Freitextfelder (name kann dann eine
    # Liste/data.frame mit Sprachen sein statt einfachem String). Hier auf
    # einen einzelnen String normalisieren, damit bind_rows() nicht bricht.
    info_df <- info_df %>%
      mutate(name = vapply(name, function(x) {
        if (is.list(x) || is.data.frame(x)) {
          werte <- unlist(x)
          if (length(werte) == 0) return(NA_character_)
          # Nur den ersten Sprachwert nehmen (vermeidet Duplikate wie
          # "Name / Name / Name / en / fr / es" bei mehrsprachigen Feldern)
          werte[1]
        } else {
          as.character(x)
        }
      }, character(1)))

    status_df <- status %>%
      select(station_id, any_of(c("num_bikes_available", "num_vehicles_available")), any_of("num_docks_available"))

    # GBFS v3.0 nennt das Feld "num_vehicles_available" statt "num_bikes_available" (v2.x).
    # Hier vereinheitlichen wir auf den Namen "num_bikes_available", damit der Rest
    # des Skripts unveraendert bleibt.
    if ("num_vehicles_available" %in% names(status_df) && !("num_bikes_available" %in% names(status_df))) {
      status_df <- status_df %>% rename(num_bikes_available = num_vehicles_available)
    }
    # Falls beide Spaltennamen aus irgendeinem Grund nebeneinander existieren
    # (z.B. inkonsistente Anbieter-Antwort), die jetzt ueberfluessige
    # num_vehicles_available-Spalte entfernen.
    status_df <- status_df %>% select(-any_of("num_vehicles_available"))

    ergebnis <- info_df %>%
      inner_join(status_df, by = "station_id") %>%
      mutate(
        land = land,
        stadt = stadt,
        anbieter = anbieter,
        timestamp = zeitstempel
      )

    message(sprintf("OK: %s (%s) - %d Stationen", stadt, anbieter, nrow(ergebnis)))
    return(ergebnis)

  }, error = function(e) {
    message(sprintf("FEHLER bei %s (%s): %s", stadt, anbieter, conditionMessage(e)))
    return(NULL)
  })
}

# -----------------------------------------------------------------------------
# 4. Alle Systeme durchlaufen und Ergebnisse sammeln
# -----------------------------------------------------------------------------
zeitstempel <- format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC")

alle_ergebnisse <- pmap(
  list(systeme$land, systeme$stadt, systeme$anbieter, systeme$gbfs_url),
  function(land, stadt, anbieter, gbfs_url) {
    log_system(land, stadt, anbieter, gbfs_url, zeitstempel)
  }
)

gesamt_df <- bind_rows(alle_ergebnisse)

# -----------------------------------------------------------------------------
# 5. An CSV-Datei anhaengen (Header nur beim ersten Mal schreiben)
# -----------------------------------------------------------------------------
ausgabe_datei <- "bikeshare_log.csv"

if (nrow(gesamt_df) > 0) {
  write.table(
    gesamt_df,
    file = ausgabe_datei,
    sep = ",",
    row.names = FALSE,
    col.names = !file.exists(ausgabe_datei),
    append = file.exists(ausgabe_datei),
    qmethod = "double"
  )
  message(sprintf("\n%d Zeilen geschrieben nach %s (Stand: %s UTC)",
                   nrow(gesamt_df), ausgabe_datei, zeitstempel))
} else {
  message("Keine Daten erhalten - nichts geschrieben.")
}

# -----------------------------------------------------------------------------
# 6. Test-/Plausibilitaets-Zusammenfassung
#    Zeigt auf einen Blick: welches System hat funktioniert, wie viele
#    Stationen wurden gefunden, wie viele Fahrraeder insgesamt.
#    Besonders nuetzlich beim ersten manuellen Testlauf.
# -----------------------------------------------------------------------------
zusammenfassung <- systeme %>%
  select(land, stadt, anbieter) %>%
  left_join(
    gesamt_df %>%
      group_by(land, stadt, anbieter) %>%
      summarise(
        stationen = n(),
        fahrraeder_gesamt = sum(num_bikes_available, na.rm = TRUE),
        .groups = "drop"
      ),
    by = c("land", "stadt", "anbieter")
  ) %>%
  mutate(
    status = ifelse(is.na(stationen), "FEHLER", "OK"),
    stationen = ifelse(is.na(stationen), 0, stationen),
    fahrraeder_gesamt = ifelse(is.na(fahrraeder_gesamt), 0, fahrraeder_gesamt)
  )

message("\n===================== ZUSAMMENFASSUNG =====================")
print(as.data.frame(zusammenfassung), row.names = FALSE)
message("=============================================================")
message(sprintf(
  "Erfolgreich: %d von %d Systemen | Stationen gesamt: %d | Fahrraeder gesamt: %d",
  sum(zusammenfassung$status == "OK"),
  nrow(zusammenfassung),
  sum(zusammenfassung$stationen),
  sum(zusammenfassung$fahrraeder_gesamt)
))
