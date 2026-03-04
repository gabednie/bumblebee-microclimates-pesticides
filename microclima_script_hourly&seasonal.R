# ============================================================
# MICROCLIMA per site × date (gridded 2-day runs)
# CHM (DSM−DTM) → LAI (cover-type specific); LC → x; x fixed at 1
# Fill DTM/CHM NAs over WATER: DTM=min(dtm), CHM=0; LAI=0 where missing
# LC rasters: D:/Gabby/PhD/Kerr Lab Paper/Land Cover/Sites/site<id>_LC_<year>.tif
# ============================================================

# If needed:
# install.packages("devtools")
# devtools::install_github("ilyamaclean/microclima")
# devtools::install_github('mrke/NicheMapR')

# ---------- packages ----------
need <- c("terra","withr","microclima","NicheMapR","RNCEP",
          "dplyr","lubridate","readr")
to_install <- need[!need %in% rownames(installed.packages())]
if (length(to_install)) install.packages(to_install, quiet = TRUE)

library(terra)
library(withr)
library(microclima)
library(NicheMapR)
library(RNCEP)
library(dplyr)
library(lubridate)
library(readr)

Sys.setenv(TZ = "UTC")
options(timeout = 600)

# ---------- micro_ncep compatibility shim ----------
micro_ncep <- function(...) {
  target <- get("micro_ncep", asNamespace("NicheMapR"))
  dots   <- list(...)
  fmls   <- names(formals(target))
  nms    <- names(dots)
  if (!is.null(nms) && any(nzchar(nms))) {
    keep <- intersect(nms, fmls)
    unnamed_idx <- which(!nzchar(nms) | is.na(nms))
    dots <- c(dots[unnamed_idx], dots[keep])
  }
  do.call(target, dots)
}

# ---------- USER SETTINGS (gridded 2-day runs) ----------
site_csv_path  <- "D:/Gabby/PhD/Kerr Lab Paper/all.years.csv"
dtm_folder     <- "D:/Gabby/PhD/Kerr Lab Paper/Lidar/Site DTM/Sites"
chm_folder     <- "D:/Gabby/PhD/Kerr Lab Paper/Lidar/CHM"
lc_folder      <- "D:/Gabby/PhD/Kerr Lab Paper/Land Cover/Sites"

output_root    <- "F:/Kerr Lab Paper/Microclima Outputs"
dir.create(output_root, showWarnings = FALSE, recursive = TRUE)

# Run ALL sites now (set to c(1,2,...) later to limit)
site_range <- 63:104

# Adaptive coarsening controls (gridded)
TARGET_RES <- 1      # start at 1 m
MAX_CELLS  <- 5e7    # stricter: ~50M cells max for DEM to limit disk usage
MAX_RES    <- 32     # never coarsen beyond this (m)

# Terra temp on disk to avoid pointer/GC issues when writing many rasters
terra_tmp <- file.path(output_root, "terra_tmp")
dir.create(terra_tmp, showWarnings = FALSE, recursive = TRUE)
terraOptions(tempdir = terra_tmp, todisk = TRUE, memfrac = 0.6, progress = 1)

# ---------- helpers ----------
`%||%` <- function(a,b) if (is.null(a)) b else a
utm_epsg_from_lon <- function(lon) 32600 + floor((lon + 180) / 6) + 1

adaptive_coarsen <- function(dem, start_res = 1, max_cells = 5e7, max_res = 32) {
  res_cur   <- start_res
  make_grid <- function(r, res_m) rast(ext(r), resolution = res_m, crs = crs(r))
  repeat {
    grid   <- make_grid(dem, res_cur)
    dem_rs <- resample(dem, grid, method = "bilinear")
    if (ncell(dem_rs) <= max_cells || res_cur >= max_res) {
      if (res_cur >= max_res && ncell(dem_rs) > max_cells) {
        message(sprintf("Reached MAX_RES=%dm with %s cells; proceeding anyway.",
                        res_cur, format(ncell(dem_rs), big.mark=",")))
      } else {
        message(sprintf("Grid OK at %dm (%s cells)", res_cur, format(ncell(dem_rs), big.mark=",")))
      }
      return(dem_rs)
    }
    res_cur <- min(max_res, res_cur * 2)   # double each step
    message(sprintf("Too many cells; coarsening to %dm ...", res_cur))
  }
}

# === write_hourly_and_extrema_for_day ===
write_hourly_and_extrema_for_day <- function(res, dem, out_dir, run_date) {
  stopifnot("temps" %in% names(res), "tme" %in% names(res))
  arr <- res$temps
  tme <- res$tme
  
  idx <- which(as.Date(tme, tz = "UTC") == run_date)
  if (!length(idx)) stop("No hourly timestamps match ", run_date, " in res$tme.")
  
  nr <- nrow(dem); nc <- ncol(dem)
  d  <- dim(arr)
  
  slice_to_raster <- function(mat, dem) {
    r <- rast(dem)
    dm <- dim(mat)
    if (length(dm) != 2) stop("slice_to_raster expects a 2D matrix")
    if (dm[1] == nr && dm[2] == nc) {
      vals <- as.vector(t(mat))
      r <- setValues(r, vals)
    } else if (dm[1] == nc && dm[2] == nr) {
      mat_rc <- t(mat)
      vals   <- as.vector(t(mat_rc))
      r <- setValues(r, vals)
    } else {
      stop(sprintf("Slice dims %dx%d do not match DEM %dx%d.", dm[1], dm[2], nr, nc))
    }
    r
  }
  
  # Build hourly stack for the chosen day
  if (length(d) == 3) {
    lyr <- vector("list", length(idx))
    for (j in seq_along(idx)) {
      mat <- arr[ , , idx[j]]
      lyr[[j]] <- slice_to_raster(mat, dem)
    }
    T_hourly <- rast(lyr)
  } else if (length(d) == 2 && d[1] == ncell(dem)) {
    lyr <- vector("list", length(idx))
    for (j in seq_along(idx)) {
      r <- rast(dem)
      r <- setValues(r, arr[, idx[j]])
      lyr[[j]] <- r
    }
    T_hourly <- rast(lyr)
  } else {
    stop("Unexpected 'temps' shape: ", paste(d, collapse=" x "),
         " (DEM cells=", ncell(dem), ", rows x cols=", paste(c(nr,nc), collapse=" x "), ")")
  }
  
  # Name layers and write
  t_hourly <- tme[idx]
  names(T_hourly) <- sprintf("Tair_%s", format(t_hourly, "%Y%m%dT%H%MZ"))
  
  wopt <- list(gdal = c("COMPRESS=LZW","TILED=YES","BIGTIFF=IF_SAFER"))
  
  writeRaster(T_hourly,
              file.path(out_dir, sprintf("Tair_hourly_%s.tif", format(run_date, "%Y%m%d"))),
              overwrite = TRUE, wopt = wopt)
  
  hdir <- file.path(out_dir, sprintf("hourly_%s", format(run_date, "%Y%m%d")))
  dir.create(hdir, showWarnings = FALSE)
  for (i in seq_len(nlyr(T_hourly))) {
    fn <- file.path(hdir, paste0(names(T_hourly)[i], ".tif"))
    writeRaster(T_hourly[[i]], fn, overwrite = TRUE, wopt = wopt)
  }
  
  # Day-1 Tmin/Tmax
  Tmin <- app(T_hourly, min, na.rm = TRUE)
  Tmax <- app(T_hourly, max, na.rm = TRUE)
  writeRaster(Tmin, file.path(out_dir, "Tair_day1_Tmin.tif"), overwrite = TRUE, wopt = wopt)
  writeRaster(Tmax, file.path(out_dir, "Tair_day1_Tmax.tif"), overwrite = TRUE, wopt = wopt)
  
  invisible(TRUE)
}

# ---------- LAI / LC helpers ----------
# Class-specific LAI maxima and half-height (H50) for a simple saturating curve:
#   LAI(h) = LMAX * (1 - exp(-log(2) * h / H50))
LAI_SPEC <- list(
  forest  = list(LMAX = 6, H50 = 10),  # tall, dense
  shrub   = list(LMAX = 3, H50 =  3),
  grass   = list(LMAX = 2, H50 =  0.5),
  crop    = list(LMAX = 4, H50 =  1),
  wetland = list(LMAX = 5, H50 =  3)
)

# Global LAI behaviour controls
CHM_SOFT_START  <- 0      # height offset before LAI starts ramping (m)
CHM_MIN_FOR_VEG <- 0      # min CHM to be considered vegetated (m)
LAI_CAP         <- 8      # global hard cap

# Land-cover class codes for 2015 Land Cover of Canada (NALCMS-style)
# Adjust if needed for your specific legend.
LC_FOREST  <- c(1, 2, 3, 4)
LC_SHRUB   <- c(5, 7)
LC_GRASS   <- c(6, 8)
LC_CROP    <- c(11)
LC_WETLAND <- c(10)
LC_NONVEG  <- c(9, 12, 13, 14, 15)   # forced LAI = 0 (barren/urban/water/snow/ice)

# Leaf-geometry (x) to approximate a generic ~45–60° canopy
LEAF_X_CONST <- 1.0  # spherical distribution (ratio vertical/horizontal projections)

# --- CHM → LAI saturating curve (single class) ---
chm_to_lai_saturating <- function(h,
                                  LMAX,
                                  H50,
                                  CHM_SOFT_START = 0,
                                  CHM_MIN_FOR_VEG = 0,
                                  LAI_CAP = 8) {
  h <- clamp(h, lower = 0)
  
  if (!is.null(CHM_SOFT_START) && CHM_SOFT_START > 0) {
    h <- ifel(h < CHM_SOFT_START, 0, h - CHM_SOFT_START)
  }
  
  if (!is.null(CHM_MIN_FOR_VEG) && CHM_MIN_FOR_VEG > 0) {
    h <- ifel(h < CHM_MIN_FOR_VEG, 0, h)
  }
  
  b   <- log(2) / H50
  lai <- LMAX * (1 - exp(-b * h))
  lai <- clamp(lai, 0, LAI_CAP)
  lai
}

# --- CHM + LC → LAI (class-aware) ---
chm_to_lai_classaware <- function(chm,
                                  lc,
                                  LAI_SPEC,
                                  CHM_SOFT_START,
                                  CHM_MIN_FOR_VEG,
                                  LAI_CAP,
                                  LC_FOREST,
                                  LC_SHRUB,
                                  LC_GRASS,
                                  LC_CROP,
                                  LC_WETLAND,
                                  LC_NONVEG) {
  
  chm <- clamp(chm, lower = 0)
  lai <- chm * 0  # initialise as 0 everywhere
  
  add_group <- function(lai, mask, chm, spec) {
    if (is.null(spec) || !all(c("LMAX", "H50") %in% names(spec))) return(lai)
    if (!terra::hasValues(mask)) return(lai)
    
    lai_g <- chm_to_lai_saturating(
      h               = chm,
      LMAX            = spec$LMAX,
      H50             = spec$H50,
      CHM_SOFT_START  = CHM_SOFT_START,
      CHM_MIN_FOR_VEG = CHM_MIN_FOR_VEG,
      LAI_CAP         = LAI_CAP
    )
    lai <- ifel(mask == 1, lai_g, lai)
    lai
  }
  
  if (length(LC_FOREST)) {
    m_forest <- lc %in% LC_FOREST
    lai      <- add_group(lai, m_forest, chm, LAI_SPEC$forest)
  }
  if (length(LC_SHRUB)) {
    m_shrub <- lc %in% LC_SHRUB
    lai     <- add_group(lai, m_shrub, chm, LAI_SPEC$shrub)
  }
  if (length(LC_GRASS)) {
    m_grass <- lc %in% LC_GRASS
    lai     <- add_group(lai, m_grass, chm, LAI_SPEC$grass)
  }
  if (length(LC_CROP)) {
    m_crop <- lc %in% LC_CROP
    lai    <- add_group(lai, m_crop, chm, LAI_SPEC$crop)
  }
  if (length(LC_WETLAND)) {
    m_wet <- lc %in% LC_WETLAND
    lai   <- add_group(lai, m_wet, chm, LAI_SPEC$wetland)
  }
  
  # Force non-vegetation LC classes to LAI = 0
  if (length(LC_NONVEG)) {
    m_nonveg <- lc %in% LC_NONVEG
    lai      <- ifel(m_nonveg == 1, 0, lai)
  }
  
  # NA LC (e.g. beyond LC extent / water) → LAI = 0
  lai[is.na(lc)] <- 0
  
  lai <- clamp(lai, 0, LAI_CAP)
  lai[is.na(lai)] <- 0
  
  lai
}

# ---------- load site table ----------
site_df <- read.csv(site_csv_path, stringsAsFactors = FALSE)
req <- c("site","lat","long","date")
stopifnot(all(req %in% names(site_df)))

site_df <- site_df %>%
  mutate(date = ymd(as.character(date))) %>%
  filter(is.finite(lat), is.finite(long), !is.na(date))

if (!is.null(site_range)) site_df <- filter(site_df, site %in% site_range)
stopifnot(nrow(site_df) > 0)

visits <- site_df %>% distinct(site, lat, long, date) %>% arrange(site, date)

# ---------- run loop (2-day gridded runs) ----------
meta_rows <- list()

for (i in seq_len(nrow(visits))) {
  # clear any stale temp files before each run
  try(terra::tmpFiles(current = TRUE, remove = TRUE), silent = TRUE)
  
  v <- visits[i, ]
  site_id <- as.character(v$site)
  lon <- as.numeric(v$long); lat <- as.numeric(v$lat)
  run_date <- as.Date(v$date)
  lc_year  <- lubridate::year(run_date)
  
  # Per-site parent folder (Site <id>)
  site_dir <- file.path(output_root, sprintf("Site %s", site_id))
  dir.create(site_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Per-visit folder inside per-site folder
  out_dir  <- file.path(site_dir, sprintf("site%s_%s", site_id, format(run_date, "%Y%m%d")))
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  cat("\n--- Site", site_id, "on", as.character(run_date), "---\n")
  
  # Paths
  dtm_path <- file.path(dtm_folder, sprintf("site%s_DTM.tif", site_id))
  chm_path <- file.path(chm_folder, sprintf("site%s_CHM.tif", site_id))
  lc_path  <- file.path(lc_folder, sprintf("site%s_LC_%d.tif", site_id, lc_year))
  
  if (!file.exists(dtm_path)) {
    warning("❌ Missing DEM for site ", site_id, " — skipping date ", run_date); next
  }
  
  # Load DEM; ensure metric CRS if lon/lat
  dem <- tryCatch(rast(dtm_path), error = function(e) NULL)
  if (is.null(dem)) { warning("⚠️ Could not open DEM: ", dtm_path); next }
  if (terra::is.lonlat(dem)) {
    epsg <- utm_epsg_from_lon(lon)
    dem  <- project(dem, paste0("EPSG:", epsg))
  }
  
  # Adaptive coarsen (no cropping)
  demr <- adaptive_coarsen(dem, start_res = TARGET_RES, max_cells = MAX_CELLS, max_res = MAX_RES)
  names(demr) <- "elev"
  
  # Fill NA with DEM min
  dmin <- suppressWarnings(global(demr, "min", na.rm = TRUE)[1,1])
  if (is.finite(dmin)) {
    dem_fill <- classify(demr, rbind(cbind(-Inf, Inf, dmin)))
    demr     <- cover(demr, dem_fill)
  }
  
  # Save inputs used (and resolution log)
  writeRaster(demr, file.path(out_dir, "DTM_used.tif"),
              overwrite = TRUE,
              wopt = list(gdal = c("COMPRESS=LZW","TILED=YES","BIGTIFF=IF_SAFER")))
  final_res <- res(demr)
  
  # CHM (+ optional LC) → LAI
  if (file.exists(chm_path)) {
    chm <- tryCatch(rast(chm_path), error = function(e) NULL)
    if (!is.null(chm)) {
      if (!compareGeom(chm, demr, crs = TRUE, stopOnError = FALSE)) {
        chm <- project(chm, demr)
      }
      chm <- resample(chm, demr, method = "bilinear")
      chm <- clamp(chm, lower = 0)
      
      lc <- if (file.exists(lc_path)) tryCatch(rast(lc_path), error = function(e) NULL) else NULL
      if (!is.null(lc)) {
        if (!compareGeom(lc, demr, crs = TRUE, stopOnError = FALSE)) {
          lc <- project(lc, demr, method = "near")
        }
        lc <- resample(lc, demr, method = "near")
        
        lai <- chm_to_lai_classaware(
          chm             = chm,
          lc              = lc,
          LAI_SPEC        = LAI_SPEC,
          CHM_SOFT_START  = CHM_SOFT_START,
          CHM_MIN_FOR_VEG = CHM_MIN_FOR_VEG,
          LAI_CAP         = LAI_CAP,
          LC_FOREST       = LC_FOREST,
          LC_SHRUB        = LC_SHRUB,
          LC_GRASS        = LC_GRASS,
          LC_CROP         = LC_CROP,
          LC_WETLAND      = LC_WETLAND,
          LC_NONVEG       = LC_NONVEG
        )
      } else {
        lai <- chm_to_lai_saturating(
          h               = chm,
          LMAX            = 3.0,
          H50             = 5.0,
          CHM_SOFT_START  = 0,
          CHM_MIN_FOR_VEG = 0,
          LAI_CAP         = 8
        )
      }
    } else {
      lai <- demr; lai[] <- 1.0
    }
  } else {
    lai <- demr; lai[] <- 1.0
  }
  
  # Force LAI = 0 where NA (e.g. water / missing CHM/LC)
  lai[is.na(lai)] <- 0
  names(lai) <- "l"
  
  # Leaf geometry: constant x ≈ mid-angle canopy (~45–60°)
  x <- demr
  x[] <- LEAF_X_CONST
  names(x) <- "x"
  
  writeRaster(lai, file.path(out_dir, "LAI_used.tif"),
              overwrite = TRUE, wopt = list(gdal = c("COMPRESS=LZW","TILED=YES")))
  writeRaster(x,   file.path(out_dir, "X_used.tif"),
              overwrite = TRUE, wopt = list(gdal = c("COMPRESS=LZW","TILED=YES")))
  
  # Two-day window (Option A) → keep Day 1
  dstart  <- format(run_date, "%d/%m/%Y")
  dfinish <- format(run_date + 1, "%d/%m/%Y")
  
  rc <- tryCatch(with_dir(out_dir, {
    res <- runauto(
      r            = demr,
      dstart       = dstart,
      dfinish      = dfinish,
      l            = lai,
      x            = x,
      hourlydata   = TRUE,
      summarydata  = TRUE,
      coastal      = FALSE,
      run.gads     = 2,
      plot.progress= TRUE
    )
    saveRDS(res, "runauto_result_2days.rds")
    
    # CSVs filtered to Day 1
    if (is.list(res)) {
      if ("hourly" %in% names(res)) {
        day1_hourly <- subset(res$hourly, as.Date(time, tz = "UTC") == run_date)
        write.csv(day1_hourly, "microclima_hourly_day1.csv", row.names = FALSE)
      }
      if ("daily" %in% names(res)) {
        day1_daily <- subset(res$daily, as.Date(date, tz = "UTC") == run_date)
        write.csv(day1_daily, "microclima_daily_day1.csv", row.names = FALSE)
      }
    }
    res
  }), error = function(e) {
    warning("⚠️ runauto failed for site ", site_id, " on ", run_date, ": ", e$message)
    NULL
  })
  
  if (is.null(rc)) next
  
  # HOURLY rasters + Day-1 Tmin/Tmax rasters
  try({
    write_hourly_and_extrema_for_day(rc, demr, out_dir, run_date)
  }, silent = TRUE)
  
  # Log metadata
  meta_rows[[length(meta_rows) + 1]] <- data.frame(
    site = site_id,
    date = as.character(run_date),
    res_x_m = final_res[1],
    res_y_m = final_res[2],
    ncols = ncol(demr),
    nrows = nrow(demr),
    ncells = ncell(demr),
    out_dir = normalizePath(out_dir),
    stringsAsFactors = FALSE
  )
  
  try(terra::tmpFiles(current = TRUE, remove = TRUE), silent = TRUE)
  gc()
}

# Run-level metadata CSV (final resolutions, etc.)
if (length(meta_rows)) {
  meta <- bind_rows(meta_rows)
  write_csv(meta, file.path(output_root, "run_metadata.csv"))
  cat("\n✅ Done. Per-visit outputs are under per-site folders in:\n",
      normalizePath(output_root),
      "\nFinal raster resolutions logged in run_metadata.csv\n", sep = "")
} else {
  cat("\n⚠️ No runs completed.\n")
}


################################################################################
################################################################################
########## FASTER SCRIPT OPTION ################################################
################################################################################
################################################################################
#------------------------------------------------------------------------------#
#----------------- Run Seasonal Temps (point extraction) ----------------------#
#------------------------------------------------------------------------------#
# Based on your SLOW-BUT-WORKS version, with ONLY these changes:
#  1) Terra tempdir moved to LOCAL: D:/Gabby/terra_tmp_point  (faster than external)
#  2) FIX bottleneck: CHM/LC are CROPPED FIRST in their native CRS, THEN projected (tiny clip only)
#  3) START AT SITE 70 (skips sites 1–69)
#
# Everything else kept as close as possible to your working script, including checks/logging.
#------------------------------------------------------------------------------#

# ---------- packages ----------
need <- c("terra","withr","microclima","NicheMapR","RNCEP","dplyr","lubridate","readr")
to_install <- need[!need %in% rownames(installed.packages())]
if (length(to_install)) install.packages(to_install, quiet = TRUE)

library(terra)
library(withr)
library(microclima)
library(NicheMapR)
library(RNCEP)
library(dplyr)
library(lubridate)
library(readr)

Sys.setenv(TZ = "UTC")
options(timeout = 600)

# ---------- micro_ncep compatibility shim ----------
micro_ncep <- function(...) {
  target <- get("micro_ncep", asNamespace("NicheMapR"))
  dots   <- list(...)
  fmls   <- names(formals(target))
  nms    <- names(dots)
  if (!is.null(nms) && any(nzchar(nms))) {
    keep <- intersect(nms, fmls)
    unnamed_idx <- which(!nzchar(nms) | is.na(nms))
    dots <- c(dots[unnamed_idx], dots[keep])
  }
  do.call(target, dots)
}

# ---------- USER SETTINGS (point runs) ----------
site_csv_path <- "D:/Gabby/PhD/Kerr Lab Paper/all.years.csv"
dtm_folder    <- "D:/Gabby/PhD/Kerr Lab Paper/Lidar/Site DTM/Sites"
chm_folder    <- "D:/Gabby/PhD/Kerr Lab Paper/Lidar/CHM"
lc_folder     <- "D:/Gabby/PhD/Kerr Lab Paper/Land Cover/Sites"

output_root   <- "F:/Kerr Lab Paper/Microclima Outputs"
dir.create(output_root, showWarnings = FALSE, recursive = TRUE)

site_range <- NULL

# Small window size around site (meters, in projected CRS)
buffer_m <- 5

# Adaptive coarsening limits (AFTER clipping; should rarely trigger now)
TARGET_RES <- 1
MAX_CELLS  <- 1e6
MAX_RES    <- 8

# ---- Terra temp on LOCAL drive (faster than external) ----
terra_tmp <- "D:/Gabby/terra_tmp_point"
dir.create(terra_tmp, showWarnings = FALSE, recursive = TRUE)
terraOptions(tempdir  = terra_tmp,
             todisk   = TRUE,
             memfrac  = 0.6,
             progress = 1,
             overwrite = TRUE)

# ---------- LAI / LC constants (same as your current script) ----------
LAI_SPEC <- list(
  forest  = list(LMAX = 6, H50 = 10),
  shrub   = list(LMAX = 3, H50 =  3),
  grass   = list(LMAX = 2, H50 =  0.5),
  crop    = list(LMAX = 4, H50 =  1),
  wetland = list(LMAX = 5, H50 =  3)
)
CHM_SOFT_START  <- 0
CHM_MIN_FOR_VEG <- 0
LAI_CAP         <- 8

LC_FOREST  <- c(1, 2, 3, 4)
LC_SHRUB   <- c(5, 7)
LC_GRASS   <- c(6, 8)
LC_CROP    <- c(11)
LC_WETLAND <- c(10)
LC_NONVEG  <- c(9, 12, 13, 14, 15)

LEAF_X_CONST <- 1.0

# ---------- helpers ----------
utm_epsg_from_lon <- function(lon) 32600 + floor((lon + 180) / 6) + 1

adaptive_coarsen <- function(dem, start_res = 1, max_cells = 1e6, max_res = 8) {
  res_cur   <- start_res
  make_grid <- function(r, res_m) rast(ext(r), resolution = res_m, crs = crs(r))
  repeat {
    grid   <- make_grid(dem, res_cur)
    dem_rs <- resample(dem, grid, method = "bilinear")
    if (ncell(dem_rs) <= max_cells || res_cur >= max_res) {
      if (res_cur >= max_res && ncell(dem_rs) > max_cells) {
        message(sprintf("Reached MAX_RES=%dm with %s cells; proceeding.",
                        res_cur, format(ncell(dem_rs), big.mark = ",")))
      } else {
        message(sprintf("Grid OK at %dm (%s cells)",
                        res_cur, format(ncell(dem_rs), big.mark = ",")))
      }
      return(dem_rs)
    }
    res_cur <- min(max_res, res_cur * 2)
    message(sprintf("Too many cells; coarsening to %dm ...", res_cur))
  }
}

chm_to_lai_saturating <- function(h, LMAX, H50, CHM_SOFT_START=0, CHM_MIN_FOR_VEG=0, LAI_CAP=8) {
  h <- clamp(h, lower = 0)
  if (CHM_SOFT_START > 0) h <- ifel(h < CHM_SOFT_START, 0, h - CHM_SOFT_START)
  if (CHM_MIN_FOR_VEG > 0) h <- ifel(h < CHM_MIN_FOR_VEG, 0, h)
  b <- log(2) / H50
  lai <- LMAX * (1 - exp(-b * h))
  clamp(lai, 0, LAI_CAP)
}

chm_to_lai_classaware <- function(chm, lc,
                                  LAI_SPEC, CHM_SOFT_START, CHM_MIN_FOR_VEG, LAI_CAP,
                                  LC_FOREST, LC_SHRUB, LC_GRASS, LC_CROP, LC_WETLAND, LC_NONVEG) {
  chm <- clamp(chm, lower = 0)
  lai <- chm * 0
  
  add_group <- function(lai, mask, spec) {
    if (is.null(spec) || !all(c("LMAX","H50") %in% names(spec))) return(lai)
    lai_g <- chm_to_lai_saturating(chm, spec$LMAX, spec$H50,
                                   CHM_SOFT_START, CHM_MIN_FOR_VEG, LAI_CAP)
    ifel(mask == 1, lai_g, lai)
  }
  
  if (length(LC_FOREST))  lai <- add_group(lai, lc %in% LC_FOREST,  LAI_SPEC$forest)
  if (length(LC_SHRUB))   lai <- add_group(lai, lc %in% LC_SHRUB,   LAI_SPEC$shrub)
  if (length(LC_GRASS))   lai <- add_group(lai, lc %in% LC_GRASS,   LAI_SPEC$grass)
  if (length(LC_CROP))    lai <- add_group(lai, lc %in% LC_CROP,    LAI_SPEC$crop)
  if (length(LC_WETLAND)) lai <- add_group(lai, lc %in% LC_WETLAND, LAI_SPEC$wetland)
  
  if (length(LC_NONVEG))  lai <- ifel(lc %in% LC_NONVEG, 0, lai)
  lai[is.na(lc)]  <- 0
  lai[is.na(lai)] <- 0
  clamp(lai, 0, LAI_CAP)
}

get_site_rowcol <- function(dem, lat, lon) {
  pt_ll  <- vect(data.frame(x = lon, y = lat), geom = c("x", "y"), crs = "EPSG:4326")
  pt_dem <- project(pt_ll, crs(dem))
  xy     <- terra::crds(pt_dem, df = TRUE)
  cell   <- terra::cellFromXY(dem, as.matrix(xy))
  if (length(cell) == 0 || is.na(cell)) stop("Site point is outside DEM extent.")
  rc <- terra::rowColFromCell(dem, cell)
  list(cell = cell, row = rc[1], col = rc[2])
}

# ---------- core per site-year function ----------
run_microclima_year_point <- function(site_id, lat, lon, year, year_dir) {
  dir.create(year_dir, showWarnings = FALSE, recursive = TRUE)
  log_file <- file.path(year_dir, "run_log_point.txt")
  if (file.exists(log_file)) file.remove(log_file)
  
  cat("Start", format(Sys.time()), "\n", file = log_file, append = TRUE)
  
  dtm_path <- file.path(dtm_folder, sprintf("site%s_DTM.tif", site_id))
  chm_path <- file.path(chm_folder, sprintf("site%s_CHM.tif", site_id))
  lc_path  <- file.path(lc_folder,  sprintf("site%s_LC_%d.tif", site_id, year))
  
  cat("DTM:", dtm_path, "\nCHM:", chm_path, "\nLC:", lc_path, "\n",
      file = log_file, append = TRUE)
  
  if (!file.exists(dtm_path)) {
    cat("Missing DTM; skipping.\n", file = log_file, append = TRUE)
    return(NULL)
  }
  
  dem_full <- tryCatch(rast(dtm_path), error = function(e) NULL)
  if (is.null(dem_full)) {
    cat("Failed to open DTM.\n", file = log_file, append = TRUE)
    return(NULL)
  }
  
  # ---- project DEM to UTM and clip a small window around the site ----
  pt_ll <- vect(data.frame(x = lon, y = lat), geom = c("x", "y"), crs = "EPSG:4326")
  
  if (terra::is.lonlat(dem_full)) {
    epsg <- utm_epsg_from_lon(lon)
    dem_prj <- project(dem_full, paste0("EPSG:", epsg))
  } else {
    dem_prj <- dem_full
  }
  
  pt_prj <- project(pt_ll, crs(dem_prj))
  pt_buf <- buffer(pt_prj, width = buffer_m)
  
  dem_clip <- crop(dem_prj, pt_buf)
  if (ncell(dem_clip) == 0) {
    cat("Cropped DEM has zero cells. Check coordinates / buffer size.\n",
        file = log_file, append = TRUE)
    return(NULL)
  }
  if (!terra::hasValues(dem_clip)) {
    cat("Cropped DEM has no valid values (all NA). Skipping site-year.\n",
        file = log_file, append = TRUE)
    return(NULL)
  }
  
  # ---- adaptive coarsen on the clipped DEM ----
  demr <- adaptive_coarsen(dem_clip, start_res = TARGET_RES, max_cells = MAX_CELLS, max_res = MAX_RES)
  names(demr) <- "elev"
  
  if (!terra::hasValues(demr)) {
    cat("DEM has no valid values after coarsening. Skipping site-year.\n",
        file = log_file, append = TRUE)
    return(NULL)
  }
  
  # Fill NA with min elevation
  dmin <- suppressWarnings(global(demr, "min", na.rm = TRUE)[1, 1])
  if (!is.finite(dmin)) {
    cat("DEM has no finite values after global(min). Skipping.\n",
        file = log_file, append = TRUE)
    return(NULL)
  }
  dem_fill <- classify(demr, rbind(cbind(-Inf, Inf, dmin)))
  demr     <- cover(demr, dem_fill)
  
  writeRaster(demr, file.path(year_dir, "DTM_used_point.tif"),
              overwrite = TRUE,
              wopt = list(gdal = c("COMPRESS=LZW","TILED=YES","BIGTIFF=IF_SAFER")))
  
  # ---- LAI grid from CHM (+ optional LC) ----
  # IMPORTANT CHANGE (speed): crop CHM/LC in their native CRS FIRST, then project tiny clip
  chm <- if (file.exists(chm_path)) tryCatch(rast(chm_path), error = function(e) NULL) else NULL
  lc  <- if (file.exists(lc_path))  tryCatch(rast(lc_path),  error = function(e) NULL) else NULL
  
  if (!is.null(chm)) {
    # Crop polygon into CHM CRS (cheap), then crop CHM
    pt_buf_chm <- tryCatch(project(pt_buf, crs(chm)), error = function(e) NULL)
    if (is.null(pt_buf_chm)) {
      cat("Could not project buffer to CHM CRS; using LAI = 1.\n", file = log_file, append = TRUE)
      lai <- demr; lai[] <- 1.0
    } else {
      chm_clip <- crop(chm, pt_buf_chm)
      
      if (ncell(chm_clip) == 0 || !terra::hasValues(chm_clip)) {
        cat("CHM clip has no cells or no values; using LAI = 1.\n",
            file = log_file, append = TRUE)
        lai <- demr; lai[] <- 1.0
      } else {
        # Project only the small CHM clip to DEM CRS, then resample
        chm_clip_prj <- tryCatch(project(chm_clip, crs(dem_prj)), error = function(e) NULL)
        if (is.null(chm_clip_prj) || !terra::hasValues(chm_clip_prj)) {
          cat("CHM clip projection produced no values; using LAI = 1.\n",
              file = log_file, append = TRUE)
          lai <- demr; lai[] <- 1.0
        } else {
          chm_rs <- resample(chm_clip_prj, demr, method = "bilinear")
          if (!terra::hasValues(chm_rs)) {
            cat("CHM resampled has no values; using LAI = 1.\n",
                file = log_file, append = TRUE)
            lai <- demr; lai[] <- 1.0
          } else {
            chm_rs <- clamp(chm_rs, lower = 0)
            
            if (!is.null(lc)) {
              # Crop polygon into LC CRS, crop LC, project tiny clip, resample
              pt_buf_lc <- tryCatch(project(pt_buf, crs(lc)), error = function(e) NULL)
              if (is.null(pt_buf_lc)) {
                cat("Could not project buffer to LC CRS; falling back to CHM-only LAI.\n",
                    file = log_file, append = TRUE)
                lai <- chm_to_lai_saturating(chm_rs, 3.0, 5.0, 0, 0, 8)
              } else {
                lc_clip <- crop(lc, pt_buf_lc)
                if (ncell(lc_clip) == 0 || !terra::hasValues(lc_clip)) {
                  cat("LC clip has no cells/values; falling back to CHM-only LAI.\n",
                      file = log_file, append = TRUE)
                  lai <- chm_to_lai_saturating(chm_rs, 3.0, 5.0, 0, 0, 8)
                } else {
                  lc_clip_prj <- tryCatch(project(lc_clip, crs(dem_prj), method = "near"),
                                          error = function(e) NULL)
                  if (is.null(lc_clip_prj) || !terra::hasValues(lc_clip_prj)) {
                    cat("LC clip projection produced no values; falling back to CHM-only LAI.\n",
                        file = log_file, append = TRUE)
                    lai <- chm_to_lai_saturating(chm_rs, 3.0, 5.0, 0, 0, 8)
                  } else {
                    lc_rs <- resample(lc_clip_prj, demr, method = "near")
                    if (!terra::hasValues(lc_rs)) {
                      cat("LC resampled has no values; falling back to CHM-only LAI.\n",
                          file = log_file, append = TRUE)
                      lai <- chm_to_lai_saturating(chm_rs, 3.0, 5.0, 0, 0, 8)
                    } else {
                      lai <- chm_to_lai_classaware(
                        chm = chm_rs, lc = lc_rs,
                        LAI_SPEC = LAI_SPEC,
                        CHM_SOFT_START = CHM_SOFT_START,
                        CHM_MIN_FOR_VEG = CHM_MIN_FOR_VEG,
                        LAI_CAP = LAI_CAP,
                        LC_FOREST = LC_FOREST,
                        LC_SHRUB = LC_SHRUB,
                        LC_GRASS = LC_GRASS,
                        LC_CROP = LC_CROP,
                        LC_WETLAND = LC_WETLAND,
                        LC_NONVEG = LC_NONVEG
                      )
                    }
                  }
                }
              }
            } else {
              cat("No LC file; using CHM-only LAI curve.\n", file = log_file, append = TRUE)
              lai <- chm_to_lai_saturating(chm_rs, 3.0, 5.0, 0, 0, 8)
            }
          }
        }
      }
    }
  } else {
    cat("No CHM file; using LAI = 1.\n", file = log_file, append = TRUE)
    lai <- demr; lai[] <- 1.0
  }
  
  # Force LAI = 0 wherever it ends up NA
  lai[is.na(lai)] <- 0
  names(lai) <- "l"
  
  x <- demr
  x[] <- LEAF_X_CONST
  names(x) <- "x"
  
  writeRaster(lai, file.path(year_dir, "LAI_used_point.tif"),
              overwrite = TRUE,
              wopt = list(gdal = c("COMPRESS=LZW","TILED=YES","BIGTIFF=IF_SAFER")))
  writeRaster(x,   file.path(year_dir, "X_used_point.tif"),
              overwrite = TRUE,
              wopt = list(gdal = c("COMPRESS=LZW","TILED=YES","BIGTIFF=IF_SAFER")))
  
  # ---- run microclima for this year (Apr 1 – Oct 31) ----
  dstart  <- paste0("01/04/", year)
  dfinish <- paste0("31/10/", year)
  cat("Running runauto for", dstart, "to", dfinish, "\n", file = log_file, append = TRUE)
  
  res <- tryCatch(
    with_dir(year_dir, {
      runauto(
        r            = demr,
        dstart       = dstart,
        dfinish      = dfinish,
        l            = lai,
        x            = x,
        hourlydata   = TRUE,
        summarydata  = TRUE,
        coastal      = FALSE,
        run.gads     = 2,
        plot.progress= TRUE
      )
    }),
    error = function(e) {
      cat("runauto error:", e$message, "\n", file = log_file, append = TRUE)
      NULL
    }
  )
  
  if (is.null(res)) {
    cat("runauto returned NULL.\n", file = log_file, append = TRUE)
    return(NULL)
  }
  
  saveRDS(res, file.path(year_dir, "runauto_result_year_point.rds"))
  
  # ---- extract temps at site location ----
  if (!("temps" %in% names(res)) || !("tme" %in% names(res))) {
    cat("No 'temps'/'tme' in result.\n", file = log_file, append = TRUE)
    return(NULL)
  }
  
  arr <- res$temps
  tme <- res$tme
  
  if (length(dim(arr)) != 3) {
    cat("Unexpected temps dims: ", paste(dim(arr), collapse = " x "), "\n",
        file = log_file, append = TRUE)
    return(NULL)
  }
  
  rc <- tryCatch(get_site_rowcol(demr, lat, lon),
                 error = function(e) {
                   cat("get_site_rowcol error:", e$message, "\n", file = log_file, append = TRUE)
                   NULL
                 })
  if (is.null(rc)) return(NULL)
  
  t_utc <- as.POSIXct(tme, tz = "UTC")
  if (any(is.na(t_utc))) {
    cat("tme contains NA after coercion.\n", file = log_file, append = TRUE)
    return(NULL)
  }
  
  temps_vec <- as.numeric(arr[rc$row, rc$col, ])
  
  t_local    <- with_tz(t_utc, tzone = "America/Toronto")
  date_local <- as.Date(t_local)
  time_local <- format(t_local, "%H:%M")
  
  df_all <- tibble(
    site       = site_id,
    year       = year,
    date_local = date_local,
    time_local = time_local,
    Tair       = temps_vec
  )
  
  df_sub <- df_all %>%
    filter(
      date_local >= ymd(paste0(year, "-04-01")),
      date_local <= ymd(paste0(year, "-10-31")),
      time_local %in% c("02:00", "14:00")
    ) %>%
    arrange(date_local, time_local)
  
  if (!nrow(df_sub)) {
    cat("No 02:00/14:00 entries found after filtering.\n", file = log_file, append = TRUE)
  } else {
    out_csv <- file.path(year_dir, sprintf("Tair_point1m_2am_2pm_%d.csv", year))
    if (file.exists(out_csv)) file.remove(out_csv)
    write_csv(df_sub, out_csv)
    cat("Wrote year CSV:", normalizePath(out_csv), "\n", file = log_file, append = TRUE)
  }
  
  df_sub
}

# ---------- load site table & main loop ----------
site_df <- read.csv(site_csv_path, stringsAsFactors = FALSE)
if (!("site" %in% names(site_df) && "lat" %in% names(site_df) && "long" %in% names(site_df))) {
  stop("all.years.csv must contain columns: site, lat, long (and year or date).")
}

if (!("year" %in% names(site_df))) {
  if (!("date" %in% names(site_df))) stop("Need either 'year' or 'date' column in all.years.csv.")
  site_df$date <- ymd(site_df$date)
  site_df$year <- year(site_df$date)
}

site_df <- site_df %>% filter(is.finite(lat), is.finite(long), !is.na(year))
if (!is.null(site_range)) site_df <- site_df %>% filter(site %in% site_range)

# ---- START AT SITE 70 ----
visits <- site_df %>%
  distinct(site, year, lat, long) %>%
  mutate(site_num = suppressWarnings(as.numeric(site))) %>%
  filter(!is.na(site_num), site_num == 2) %>%
 # filter(!is.na(site_num), between(site_num, 48, 59)) %>%
  arrange(site_num, year) %>%
  select(-site_num)

rows_all <- list()

for (i in seq_len(nrow(visits))) {
  v <- visits[i, ]
  site_id <- as.character(v$site)
  year    <- as.integer(v$year)
  lat     <- as.numeric(v$lat)
  lon     <- as.numeric(v$long)
  
  cat("\n=== Site", site_id, "year", year, "===\n")
  
  site_dir <- file.path(output_root, sprintf("Site %s", site_id))
  dir.create(site_dir, showWarnings = FALSE, recursive = TRUE)
  
  year_dir <- file.path(site_dir, sprintf("year_%d_point", year))
  dir.create(year_dir, showWarnings = FALSE, recursive = TRUE)
  
  df_year <- run_microclima_year_point(site_id, lat, lon, year, year_dir)
  if (!is.null(df_year) && nrow(df_year)) rows_all[[length(rows_all) + 1]] <- df_year
  
  try(terra::tmpFiles(current = TRUE, remove = TRUE), silent = TRUE)
  gc()
}

# ---------- final combined CSV ----------
if (length(rows_all)) {
  final <- bind_rows(rows_all) %>% arrange(as.numeric(site), year, date_local, time_local)
  out_csv <- file.path(output_root, "Tair_point1m_2am_2pm_AprOct_2.csv")
  if (file.exists(out_csv)) file.remove(out_csv)
  write_csv(final, out_csv)
  cat("\n✅ Wrote combined CSV:\n", normalizePath(out_csv), "\n")
} else {
  cat("\n⚠️ No point data captured for any site-year.\n")
}
