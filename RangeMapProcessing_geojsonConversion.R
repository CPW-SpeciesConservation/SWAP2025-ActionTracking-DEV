# Identify and match species and habitat range maps from GIS SDE to SWAP database Species and Habitatsubtype tables

library(RPostgres)
library(dplyr)
library(stringr)

# 1. Connect to Supabase
con <- dbConnect(RPostgres::Postgres(),
                 dbname = "postgres",
                 host = "db.fsqmezpnksnvbnmemuhi.supabase.co", 
                 port = 5432,
                 user = "postgres", 
                 password = "SpeciesConservation317?",
                 sslmode = "require")

# 2A. The raw list of "Normal" tables (Habitats & Non-eBird Species)
normal_raw_tables <- c(
  "ALETES_HUMILIS", "ALETES_SESSILIFLORUS", "ALICIELLA_PENSTEMONOIDES", "ALICIELLA_STENOTHYRSA", "ALLHABITATSCOMPOSITE", "ALPINE", "AMERICANPIKA", "AMSONIA_JONESII", "ANTICLEA_VAGINATA", "AQUATIC_ZONES", 
  "AQUILEGIA_SAXIMONTANA", "ARKANSASDARTER", "ARTEMISIA_PARRYI", "ASCLEPIAS_UNCIALIS", "ASPEN", "ASTRAGALUS_ANISUS", 
  "ASTRAGALUS_DEBEQUAEUS", "ASTRAGALUS_DESPERATUS_VAR_NEESEAE", "ASTRAGALUS_DETERIOR", "ASTRAGALUS_DETRITALIS",
  "ASTRAGALUS_DUCHESNENSIS", "ASTRAGALUS_EASTWOODIAE", "ASTRAGALUS_HUMILLIMUS", "ASTRAGALUS_MICROCYMBUS", "ASTRAGALUS_MOLYBDENUS",
  "ASTRAGALUS_MUSINIENSIS", "ASTRAGALUS_NATURITENSIS", "ASTRAGALUS_OSTERHOUTII", "ASTRAGALUS_PISCATOR", "ASTRAGALUS_RAFAELENSIS", 
  "ASTRAGALUS_RIPLEYI", "ASTRAGALUS_SCHMOLLIAE", "ASTRAGALUS_SPARSIFLORUS", "ASTRAGALUS_TORTIPES", "ASTRAGALUS_WETHERILLII", 
  "AULOSPERMUM_DUCHESNENSE", "BARRENS", "BIGBROWNBAT", "BIGHORNSHEEP", "BLACKFOOTEDFERRET", "BLACKTAILEDPRAIRIEDOG", "BLUEHEADSUCKER",
  "BONYTAIL", "BOREALTOAD", "BRASSYMINNOW", "BRISTLECONE", "CALIFORNIAMYOTIS", "CANADALYNX", "CANYONBAT", "CASTILLEJA_PUBERULA", 
  "CHYLISMIA_EASTWOODIAE", "CIRSIUM_OWNBEYI", "CIRSIUM_PERPLEXANS", "CLEOMELLA_MULTICAULIS", "CLIFF_AND_CANYON", 
  "COLORADOCHECKEREDWHIPTAIL", "COLORADOPIKEMINNOW", "COLORADORIVERCUTTHROATTROUT", "COLUMBIANSHARPTAILEDGROUSE", "COMMONSHINER", 
  "CROPAGRICULTURE", "DALEA_CYLINDRICEPS", "DESERT_SHRUB", "DESERTMASSAUGA", "DRABA_EXUNGUICULATA", "DRABA_GLOBOSA", "DRABA_GRAMINEA",
  "DRABA_GRAYANA", "DRABA_SMITHII", "DRABA_STREPTOBRACHIA", "DRABA_VENTOSA", "DRY_MESIC_MIXED_CONIFER", "EAGLERIVERSCULPIN", 
  "EASTERNBLACKRAIL", "EASTERNHOGNOSESNAKE", "EASTERNREDBAT", "ERIGERON_KACHINENSIS", "ERIOGONUM_ACAULE",
  "ERIOGONUM_BRANDEGEEI", "ERIOGONUM_COLORADENSE", "ERIOGONUM_CONTORTUM", "ERIOGONUM_EPHEDROIDES",
  "ERIOGONUM_EXILIFOLIUM", "ERIOGONUM_PELINOPHILUM", "ERIOGONUM_SCABRELLUM", "ERIOGONUM_TUMULOSUM", 
  "ERYTHRANTHE_GEMMIPARA", "EUTREMA_PENLANDII", "FLANNELMOUTHSUCKER", "FLATHEADCHUB",
  "FOOTHILL_AND_MOUNTAIN_GRASSLANDS", "FRASERA_COLORADENSIS", "FRINGEDMYOTIS", "GENTIANELLA_TORTUOSA",
  "GRAYWOLF", "GREASEWOOD", "GREATERSAGEGROUSE", "GREATERSANDHILLCRANE", "GREENBACKCUTTHROATTROUT",
  "GUNNISONSAGEGROUSE", "GUNNISONSPRAIRIEDOG", "GUTIERREZIA_ELEGANS", "HACKELIA_GRACILENTA", "HAY_MEADOWS",
  "HERNANDEZSSHORTHORNEDLIZARD", "HERRICKIA_HORRIDA", "HUMPBACKCHUB", "ILIAMNA_ANGULATA", "ILIAMNA_CRANDALLII",
  "IPOMOPSIS_GLOBULARIS", "IPOMOPSIS_POLYANTHA", "LAKECHUB", "LESSERPRAIRIECHICKEN", "LITTLEBROWNMYOTIS", "LODGEPOLE",
  "LOMATIUM_CONCINNUM", "LOMATIUM_LATILOBUM", "LONGEAREDMYOTIS", "LONGLEGGEDMYOTIS", "LONGNOSEDLEOPARDLIZARD",
  "LOWER_MONTANE_FOOTHILL_SHRUBLANDS", "LUPINUS_CRASSUS", "LUZULA_SUBCAPITATA", "MAJOR_RIVERS", "MENTZELIA_CHRYSANTHA",
  "MENTZELIA_DENSA", "MENTZELIA_MULTICAULIS", "MENTZELIA_RHIZOMATA", "MESIC_MIXED_CONIFER", "MEXICANSPOTTEDOWL",
  "MIRABILIS_ROTUNDIFOLIA", "MIXED_AND_TALLGRASS_PRAIRIES", "MOUNTAINWHITEFISH", "NEOPARRYA_LITHOPHILA", "NEWMEXICOMEADOWJUMPINGMOUSE",
  "NORTHERNHOARYBAT", "NORTHERNLEOPARDFROG", "NORTHERNREDBELLYDACE", "OAK_AND_MIXED_MOUNTAIN_SHRUB", "OENOTHERA_ACUTISSIMA", 
  "OENOTHERA_COLORADENSIS", "OENOTHERA_HARRINGTONII", "OONOPSIS_MONOCEPHALA", "OONOPSIS_PUEBLOENSIS", "ORANGESPOTTEDSUNFISH", 
  "ORANGETHROATDARTER", "OREOCARYA_ELATA", "OREOCARYA_OSTERHOUTII", "OREOCARYA_PUSTULOSA", "OREOCARYA_REVEALII", "OREOCARYA_ROLLINSII", 
  "ORNATEBOXTURTLE", "PACKERA_MANCOSANA", "PARTHENIUM_LIGULATUM", "PARTHENIUM_TETRANEURIS", "PEDIOCACTUS_KNOWLTONII",
  "PEDIOMELUM_AROMATICUM", "PENSTEMON_ALBIFLUVIS", "PENSTEMON_DEBILIS", "PENSTEMON_DEGENERI", "PENSTEMON_GIBBENSII",
  "PENSTEMON_GRAHAMII", "PENSTEMON_HARRINGTONII", "PENSTEMON_LARICIFOLIUS_SSP_EXILIFOLIUS", "PENSTEMON_LUCULENTUS",
  "PENSTEMON_MENSARUM", "PENSTEMON_PENLANDII", "PENSTEMON_RETRORSUS", "PENSTEMON_SCARIOSUS_VAR_CYANOMONTANUS", "PENSTEMON_YAMPAENSIS",
  "PHACELIA_FORMOSULA", "PHACELIA_SUBMUTICA", "PHYSARIA_ALPINA", "PHYSARIA_BELLII", "PHYSARIA_CALCICOLA", "PHYSARIA_CONGESTA",
  "PHYSARIA_OBCORDATA", "PHYSARIA_PARVIFLORA", "PHYSARIA_PRUINOSA", "PHYSARIA_PULVINATA", "PHYSARIA_ROLLINSII", "PHYSARIA_VICINA",
  "PHYSARIA_VITULIFERA", "PINYON_JUNIPER", "PIPINGPLOVER", "PLAINSHOGNOSESNAKE", "PLAINSMINNOW", "PLAINSSHARPTAILEDGROUSE",
  "PLAINSSPOTTEDSKUNK", "PLAINSTOPMINNOW", "PONDEROSA_PINE", "POTENTILLA_AMBIGENS", "POTENTILLA_RUPINCOLA", "PREBLESMEADOWJUMPINGMOUSE",
  "PTILAGROSTIS_PORTERI", "RAZORBACKSUCKER", "RIOGRANDECHUB", "RIOGRANDECUTTHROATTROUT", "RIOGRANDESUCKER", 
  "RIPARIAN_WOODLANDS_AND_SHRUBLANDS", "ROUNDTAILCHUB", "ROUNDTAILEDHORNEDLIZARD", "SAGEBRUSH", "SALIX_ARIZONICA",
  "SALIX_CALCICOLA_VAR_GLANDULOSIOR", "SALTBUSH", "SAND_DUNES", "SANDSAGE", "SAUSSUREA_WEBERI", "SCLEROCACTUS_CLOVERAE", 
  "SCLEROCACTUS_DAWSONIAE", "SCLEROCACTUS_GLAUCUS", "SCLEROCACTUS_MESAEVERDAE", "SHORTGRASS_PRAIRIE", "SILVERHAIREDBAT", 
  "SISYRINCHIUM_PALLIDUM", "SOUTHERNREDBELLYDACE", "SOUTHERNWHITETAILEDPTARMIGAN", "SOUTHWESTERNWILLOWFLYCATCHER",
  "SPIRANTHES_DILUVIALIS", "SPRUCE_FIR", "STONECAT", "STREAMS_TRIBUTARIES", "SUCKERMOUTHMINNOW", "SULLIVANTIA_HAPEMANII_VAR_PURPUSII",
  "THALICTRUM_HELIOPHILUM", "TOWNSENDIA_GLABELLA", "TOWNSENDIA_ROTHROCKII", "TOWNSENDSBIGEAREDBAT", "TRICOLOREDBAT",
  "TRIFOLIUM_ANDINUM_VAR_ANDINUM", "URBANAREAS", "WESTERNNARROWMOUTHEDTOAD", "WESTERNSMALLFOOTEDMYOTIS", "WESTERNYELLOWBILLEDCUCKOO", 
  "WETLANDS", "WHITETAILEDPRAIRIEDOG", "WOLVERINE", "XANTHISMA_COLORADOENSE", "YUMAMYOTIS"
)

# 2B. The eBird specific list
ebird_raw_tables <- c(
  "EBIRDAMERICANGOSHAWK_BREEDING", "EBIRDAMERICANGOSHAWK_NONBREEDING", 
  "EBIRDAMERICANGOSHAWK_POSTBREEDINGMIGRATION", "EBIRDAMERICANGOSHAWK_PREBREEDINGMIGRATION", 
  "EBIRDAMERICANKESTREL_BREEDING", "EBIRDAMERICANKESTREL_NONBREEDING", 
  "EBIRDAMERICANKESTREL_POSTBREEDINGMIGRATION", "EBIRDAMERICANKESTREL_PREBREEDINGMIGRATION",
  "EBIRDAMERICANPEREGRINEFALCON_BREEDING", "EBIRDAMERICANPEREGRINEFALCON_NONBREEDING",
  "EBIRDAMERICANPEREGRINEFALCON_POSTBREEDINGMIGRATION", "EBIRDAMERICANPEREGRINEFALCON_PREBREEDINGMIGRATION", 
  "EBIRDBARROWSGOLDENEYE_BREEDING", "EBIRDBARROWSGOLDENEYE_NONBREEDING", 
  "EBIRDBARROWSGOLDENEYE_POSTBREEDINGMIGRATION", "EBIRDBARROWSGOLDENEYE_PREBREEDINGMIGRATION", 
  "EBIRDBLACKROSYFINCH_NONBREEDING", "EBIRDBLACKSWIFT_BREEDING",
  "EBIRDBLACKSWIFT_POSTBREEDINGMIGRATION", "EBIRDBLACKSWIFT_PREBREEDINGMIGRATION", 
  "EBIRDBROADTAILEDHUMMINGBIRD_BREEDING", "EBIRDBROADTAILEDHUMMINGBIRD_POSTBREEDINGMIGRATION", 
  "EBIRDBROADTAILEDHUMMINGBIRD_PREBREEDINGMIGRATION", "EBIRDBROWNCAPPEDROSYFINCH_BREEDING", 
  "EBIRDBROWNCAPPEDROSYFINCH_NONBREEDING", "EBIRDBURROWINGOWL_BREEDING", 
  "EBIRDBURROWINGOWL_POSTBREEDINGMIGRATION", "EBIRDBURROWINGOWL_PREBREEDINGMIGRATION",
  "EBIRDCALLIOPEHUMMINGBIRD_POSTBREEDINGMIGRATION", "EBIRDCHESTNUTCOLLAREDLONGSPUR_BREEDING",
  "EBIRDCHESTNUTCOLLAREDLONGSPUR_NONBREEDING", "EBIRDCHESTNUTCOLLAREDLONGSPUR_POSTBREEDINGMIGRATION",
  "EBIRDCHESTNUTCOLLAREDLONGSPUR_PREBREEDINGMIGRATION", "EBIRDCLARKSNUTCRACKER_RESIDENT", 
  "EBIRDGOLDENCROWNEDKINGLET_BREEDING", "EBIRDGOLDENCROWNEDKINGLET_NONBREEDING", 
  "EBIRDGOLDENCROWNEDKINGLET_POSTBREEDINGMIGRATION", "EBIRDGOLDENCROWNEDKINGLET_PREBREEDINGMIGRATION", 
  "EBIRDGOLDENEAGLE_BREEDING", "EBIRDGOLDENEAGLE_NONBREEDING", 
  "EBIRDGOLDENEAGLE_POSTBREEDINGMIGRATION", "EBIRDGOLDENEAGLE_PREBREEDINGMIGRATION", 
  "EBIRDGRACESWARBLER_BREEDING", "EBIRDGRACESWARBLER_POSTBREEDINGMIGRATION", 
  "EBIRDGRACESWARBLER_PREBREEDINGMIGRATION", "EBIRDLARKBUNTING_BREEDING", 
  "EBIRDLARKBUNTING_NONBREEDING", "EBIRDLARKBUNTING_POSTBREEDINGMIGRATION", 
  "EBIRDLARKBUNTING_PREBREEDINGMIGRATION", "EBIRDLONGBILLEDCURLEW_BREEDING", 
  "EBIRDLONGBILLEDCURLEW_POSTBREEDINGMIGRATION", "EBIRDLONGBILLEDCURLEW_PREBREEDINGMIGRATION", 
  "EBIRDMOUNTAINBLUEBIRD_BREEDING", "EBIRDMOUNTAINBLUEBIRD_NONBREEDING", 
  "EBIRDMOUNTAINBLUEBIRD_POSTBREEDINGMIGRATION", "EBIRDMOUNTAINBLUEBIRD_PREBREEDINGMIGRATION", 
  "EBIRDMOUNTAINPLOVER_BREEDING", "EBIRDMOUNTAINPLOVER_POSTBREEDINGMIGRATION",
  "EBIRDMOUNTAINPLOVER_PREBREEDINGMIGRATION", "EBIRDNORTHERNHARRIER_BREEDING", 
  "EBIRDNORTHERNHARRIER_NONBREEDING", "EBIRDNORTHERNHARRIER_POSTBREEDINGMIGRATION", 
  "EBIRDNORTHERNHARRIER_PREBREEDINGMIGRATION", "EBIRDPINYONJAY_RESIDENT",
  "EBIRDPRAIRIEFALCON_BREEDING", "EBIRDPRAIRIEFALCON_NONBREEDING", 
  "EBIRDPRAIRIEFALCON_POSTBREEDINGMIGRATION", "EBIRDPRAIRIEFALCON_PREBREEDINGMIGRATION", 
  "EBIRDRUFFOUSHUMMINGBIRD_POSTBREEDINGMIGRATION", "EBIRDSAGETHRASHER_BREEDING",
  "EBIRDSAGETHRASHER_NONBREEDING", "EBIRDSAGETHRASHER_POSTBREEDINGMIGRATION", 
  "EBIRDSAGETHRASHER_PREBREEDINGMIGRATION", "EBIRDSCALEDQUAIL_RESIDENT", 
  "EBIRDTHICKBILLEDLONGSPUR_BREEDING", "EBIRDTHICKBILLEDLONGSPUR_NONBREEDING", 
  "EBIRDTHICKBILLEDLONGSPUR_POSTBREEDINGMIGRATION", "EBIRDTHICKBILLEDLONGSPUR_PREBREEDINGMIGRATION", 
  "EBIRDVIRGINIASWARBLER_BREEDING", "EBIRDVIRGINIASWARBLER_POSTBREEDINGMIGRATION",
  "EBIRDVIRGINIASWARBLER_PREBREEDINGMIGRATION", "EBIRDWESTERNBLUEBIRD_BREEDING",
  "EBIRDWESTERNBLUEBIRD_NONBREEDING", "EBIRDWESTERNBLUEBIRD_POSTBREEDINGMIGRATION", 
  "EBIRDWESTERNBLUEBIRD_PREBREEDINGMIGRATION", "EBIRDWHITEFACEDIBIS_BREEDING", 
  "EBIRDWHITEFACEDIBIS_POSTBREEDINGMIGRATION", "EBIRDWHITEFACEDIBIS_PREBREEDINGMIGRATION",
  "EBIRDWILSONSPHALAROPE_BREEDING", "EBIRDWILSONSPHALAROPE_POSTBREEDINGMIGRATION", 
  "EBIRDWILSONSPHALAROPE_PREBREEDINGMIGRATION"
)

# --- THE AGGRESSIVE CLEANING FUNCTION ---
sanitize_name <- function(name) {
  name %>%
    toupper() %>%
    sub("[/(].*$", "", .) %>%     # Cut off after slash or open parenthesis
    gsub("[^A-Z]", "", .)         # Strip all punctuation and spaces
}

# --- PROCESS HABITATS ---
db_habitats <- dbGetQuery(con, "SELECT habitatsubtypeid, habitatsubtypename FROM proj.habitatsubtypes") %>%
  mutate(match_key = sanitize_name(habitatsubtypename))

hab_df <- data.frame(sql_table = normal_raw_tables) %>%
  mutate(match_key = sanitize_name(sql_table)) %>%
  inner_join(db_habitats, by = "match_key")

# --- PROCESS SPECIES ---
db_species <- dbGetQuery(con, "SELECT speciesid, commonname, sciname, taxonomicgroupid FROM proj.species WHERE swap_rankingid IN (1, 2) AND taxonomicgroupid != 6") %>%
  mutate(
    base_name = ifelse(taxonomicgroupid == 7, sciname, commonname),
    match_key = sanitize_name(base_name)
  )

# Manual overrides for the SDE typos
db_species$match_key[db_species$speciesid == 100] <- "RUFFOUSHUMMINGBIRD" 
db_species$match_key[db_species$speciesid == 161] <- "DESERTMASSAUGA"     
db_species$match_key[db_species$speciesid == 164] <- "EASTERNHOGNOSESNAKE"
db_species$match_key[db_species$speciesid == 315] <- "PLAINSHOGNOSESNAKE" 

sp_normal_df <- data.frame(sql_table = normal_raw_tables) %>%
  mutate(match_key = sanitize_name(sql_table), map_type = "General Range") %>%
  inner_join(db_species, by = "match_key")

sp_ebird_df <- data.frame(sql_table = ebird_raw_tables) %>%
  mutate(
    clean_sql = gsub("^EBIRD", "", sql_table),
    clean_sql = sub("_.*$", "", clean_sql), 
    match_key = sanitize_name(clean_sql),
    raw_type = sub("^.*_", "", sql_table),
    map_type = case_when(
      raw_type == "BREEDING" ~ "Breeding",
      raw_type == "NONBREEDING" ~ "Non-breeding",
      raw_type == "POSTBREEDINGMIGRATION" ~ "Post-breeding Migration",
      raw_type == "PREBREEDINGMIGRATION" ~ "Pre-breeding Migration",
      raw_type == "RESIDENT" ~ "Resident",
      grepl("EBIRD", sql_table) ~ "Post-breeding Migration", # Catch-all for Rufous exception
      TRUE ~ "eBird Survey"
    )
  ) %>%
  inner_join(db_species, by = "match_key")

all_sp_df <- bind_rows(sp_normal_df, sp_ebird_df)

# --- GENERATE SQL ---
# IMPORTANT: REPLACE THIS URL WITH YOUR ACTUAL GITHUB REPO URL
github_base <- "https://raw.githubusercontent.com/CPW-SpeciesConservation/SWAP_Resources/main/geojson/"

cat(sprintf("MATCH SUMMARY: %d Habitats and %d Species Tables Mapped!\n\n", nrow(hab_df), nrow(all_sp_df)))

hab_sql <- paste0(
  "INSERT INTO proj.habitat_spatial_links (habitatsubtypeid, sql_server_table_name, github_geojson_url) VALUES \n",
  paste(sprintf("(%d, '%s', '%s%s.geojson')", hab_df$habitatsubtypeid, hab_df$sql_table, github_base, tolower(hab_df$sql_table)), collapse = ",\n"),
  ";"
)

sp_sql <- paste0(
  "INSERT INTO proj.species_spatial_links (speciesid, sql_server_table_name, github_geojson_url, map_type) VALUES \n",
  paste(sprintf("(%d, '%s', '%s%s.geojson', '%s')", all_sp_df$speciesid, all_sp_df$sql_table, github_base, tolower(all_sp_df$sql_table), all_sp_df$map_type), collapse = ",\n"),
  ";"
)

cat("--- RUN THIS IN SUPABASE ---\n")
cat(hab_sql, "\n\n")
cat(sp_sql, "\n")

# --- DIAGNOSTIC ---
all_raw_tables <- c(normal_raw_tables, ebird_raw_tables)
matched_sql_tables <- c(hab_df$sql_table, all_sp_df$sql_table)
unmatched_tables <- setdiff(all_raw_tables, matched_sql_tables)

cat(sprintf("\n--- DIAGNOSTIC: UNMATCHED SQL TABLES (%d) ---\n", length(unmatched_tables)))
if(length(unmatched_tables) > 0) print(unmatched_tables) else cat("Perfect! Every SQL table matched.\n")

unmatched_species <- db_species %>%
  filter(!speciesid %in% all_sp_df$speciesid) %>%
  select(speciesid, commonname, sciname)
write.csv(unmatched_species, "missing_species_maps.csv", row.names = FALSE)
cat(sprintf("\nSaved 'missing_species_maps.csv' with %d unmapped SGCN species.\n", nrow(unmatched_species)))


# Convert Species and Habitat range maps to geo_json to host on github to use in SWAP implementation tracking app

library(RPostgres)
library(DBI)
library(odbc)
library(sf)
library(dplyr)

# 1. Connect to Supabase to get our final list
supa_con <- dbConnect(RPostgres::Postgres(),
                      dbname = "postgres",
                      host = "db.fsqmezpnksnvbnmemuhi.supabase.co", 
                      port = 5432, user = "postgres", password = "SpeciesConservation317?", sslmode = "require")

# Get unique SQL tables to extract
sp_tables <- dbGetQuery(supa_con, "SELECT DISTINCT sql_server_table_name FROM proj.species_spatial_links")
hab_tables <- dbGetQuery(supa_con, "SELECT DISTINCT sql_server_table_name FROM proj.habitat_spatial_links")
tables_to_extract <- unique(c(sp_tables$sql_server_table_name, hab_tables$sql_server_table_name))

# 2. Connect to SQL Server SDE
# UPDATE THIS with your actual ODBC driver name and server details!
sql_con <- dbConnect(odbc::odbc(),
                     Driver = "SQL Server", # or "ODBC Driver 17 for SQL Server"
                     Server = "DNRCPWFTCSDE22",
                     Database = "SWAP",
                     Trusted_Connection = "True") # Use "True" if using Windows Authentication

# 3. Setup Export Directory
export_dir <- "C:/Users/adamsc/Documents/Projects/SWAP/SQL Database/SWAP_GeoJSONs" 
if(!dir.exists(export_dir)) dir.create(export_dir)

# 4. The Extraction Loop
cat(sprintf("Starting extraction of %d tables...\n", length(tables_to_extract)))

for(tbl in tables_to_extract) {
  cat(paste0("Extracting ", tbl, "... "))
  
  tryCatch({
    # 1. Get column names first to build a clean query (excluding 'Shape')
    cols_query <- paste0("SELECT COLUMN_NAME FROM SWAP.INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = '", tbl, "'")
    all_cols <- dbGetQuery(sql_con, cols_query)$COLUMN_NAME
    clean_cols <- all_cols[toupper(all_cols) != "SHAPE"]
    cols_string <- paste(clean_cols, collapse = ", ")
    
    # 2. Pull the data with Shape converted to Text
    query <- paste0("SELECT ", cols_string, ", Shape.STAsText() AS WKT_GEOM FROM dbo.", tbl, " WHERE Shape IS NOT NULL")
    raw_data <- dbGetQuery(sql_con, query)
    
    if(nrow(raw_data) == 0) {
      cat("SKIPPED (No data found)\n")
      next
    }
    
    # 3. Convert to Spatial Object
    # We use 4326 if you've already re-projected in SQL, 
    # but 2232 is safer if it's raw Colorado State Plane.
    geo_data <- raw_data %>%
      st_as_sf(wkt = "WKT_GEOM", crs = 2232) %>%
      st_transform(4326) # Standard for Leaflet
    
    # 4. Final cleaning: Ensure no weird binary residuals remain
    # This keeps only the standard data types
    geo_data <- geo_data %>% select_if(function(x) !is.list(x) | inherits(x, "sfc"))
    
    # 5. Write to GeoJSON
    file_name <- file.path(export_dir, paste0(tolower(tbl), ".geojson"))
    st_write(geo_data, file_name, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)
    
    cat("Success!\n")
    
  }, error = function(e) {
    cat("FAILED: ", e$message, "\n")
  })
}

cat("\nExtraction complete! Check your export folder.\n")

#raster extraction for habitat layers
library(DBI)
library(odbc)
library(sf)
library(dplyr)

# 1. Connect to SQL Server SDE
sql_con <- dbConnect(odbc::odbc(),
                     Driver = "SQL Server", 
                     Server = "DNRCPWFTCSDE22",
                     Database = "SWAP",
                     Trusted_Connection = "True")

# 2. Targeted list of failed habitat tables
failed_habitats <- c(
  "FOOTHILL_AND_MOUNTAIN_GRASSLANDS", "MESIC_MIXED_CONIFER", "SALTBUSH", 
  "PINYON_JUNIPER", "SANDSAGE", "LODGEPOLE", "SPRUCE_FIR", 
  "LOWER_MONTANE_FOOTHILL_SHRUBLANDS", "DRY_MESIC_MIXED_CONIFER", 
  "SAND_DUNES", "CLIFF_AND_CANYON", "OAK_AND_MIXED_MOUNTAIN_SHRUB", 
  "ASPEN", "ALPINE", "GREASEWOOD", "WETLANDS", 
  "RIPARIAN_WOODLANDS_AND_SHRUBLANDS", "DESERT_SHRUB", "SHORTGRASS_PRAIRIE", 
  "PONDEROSA_PINE", "MIXED_AND_TALLGRASS_PRAIRIES", "HAY_MEADOWS", 
  "BARRENS", "SAGEBRUSH", "CROPAGRICULTURE", "URBANAREAS", "BRISTLECONE"
)

export_dir <- "C:/Users/adamsc/Documents/Projects/SWAP/SQL Database/SWAP_GeoJSONs" # Ensure this folder exists

cat(sprintf("Starting targeted extraction for %d habitat tables...\n", length(failed_habitats)))

for(tbl in failed_habitats) {
  cat(paste0("Extracting ", tbl, "... "))
  
  tryCatch({
    # We use FOOTPRINT.STAsText() because these are Raster Mosaic tables
    # We select OID and any other standard fields, but skip the heavy RASTER blob
    query <- paste0("SELECT OID, FOOTPRINT.STAsText() AS WKT_GEOM FROM dbo.", tbl, " WHERE FOOTPRINT IS NOT NULL")
    
    raw_data <- dbGetQuery(sql_con, query)
    
    if(nrow(raw_data) == 0) {
      cat("SKIPPED (No Footprint data found)\n")
      next
    }
    
    # Convert the Footprint WKT to a spatial object
    # Footprints in SDE are typically stored in the same projection as the raster
    geo_data <- raw_data %>%
      st_as_sf(wkt = "WKT_GEOM", crs = 2232) %>% # Colorado State Plane
      st_transform(4326) # Web Mercator for Leaflet
    
    # Write to GeoJSON
    file_name <- file.path(export_dir, paste0(tolower(tbl), ".geojson"))
    st_write(geo_data, file_name, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)
    
    cat("Success!\n")
    
  }, error = function(e) {
    cat("FAILED: ", e$message, "\n")
  })
}

dbDisconnect(sql_con)
cat("\nTargeted Habitat Extraction Complete.\n")
