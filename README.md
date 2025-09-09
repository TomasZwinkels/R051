# R051: Netherlands Parliament Analysis & Data Quality Monitoring

A comprehensive R script for analyzing women's representation trends in the Dutch Parliament over time, with built-in data quality monitoring and visual deviation detection.

## 📊 What This Does

This project provides:

- **Historical Analysis**: Track women's representation in Dutch Parliament from 1901 to present
- **Election Impact Assessment**: Measure representation changes around election periods
- **Data Quality Monitoring**: Automatically detect and visualize problematic deviations in parliament size data
- **Comprehensive Visualization**: Multi-layer plot showing trends, baselines, and data integrity warnings

## 🎯 Key Features

### Visual Analysis
- **Women's Representation Trend**: Red line showing percentage of women MPs over time
- **Election-Only Fluctuations**: Green line showing representation changes attributable to elections only
- **Parliament Size Monitoring**: Blue line with baseline comparison (black step line)
- **Data Quality Alerts**: Red highlighting and warning labels during problematic periods

### Data Quality Monitoring
- **Automated Deviation Detection**: Identifies periods where parliament size deviates significantly from expected baseline
- **Configurable Thresholds**: Customize seat count and duration thresholds for detection
- **Smart Period Merging**: Automatically merges closely spaced issues for cleaner analysis
- **Visual Warnings**: Clear labels positioned at problem areas ("WARNING! Structurally too high/low")

### Performance
- **Smart Caching**: Only recalculates when data changes, dramatically improving performance
- **Comprehensive Testing**: 47 unit tests ensuring reliability and accuracy

## 🚀 Getting Started

### Prerequisites

Required R packages:
```r
install.packages(c(
  "sqldf", "stringr", "readr", "dplyr", "writexl", 
  "openxlsx", "testthat", "data.table", "ggplot2"
))
```

### Data Requirements

**Data Source Setup:**
The script now uses a centralized data repository. You need to:

1. **Clone the data repository:**
   ```bash
   git clone https://github.com/TomasZwinkels/PCCdata.git
   ```

2. **Update data paths in the script:**
   The script currently expects data files at `/home/tomas/projects/PCCdata/`. 
   **You must edit these paths in `R051.R` to match your local setup:**
   
   ```r
   # Lines to update in R051.R:
   POLI = read.csv("/path/to/your/PCCdata/POLI.csv", header = TRUE, sep = ";")
   RESE = read.csv("/path/to/your/PCCdata/RESE.csv", header = TRUE, sep = ";")
   PARL = read.csv("/path/to/your/PCCdata/PARL.csv", header = TRUE, sep = ";")
   MEME = read.csv("/path/to/your/PCCdata/MEME.csv", header = TRUE, sep = ";")
   current_data_version <- trimws(readLines("/path/to/your/PCCdata/dataversion.txt")[1])
   ```

**Required data files from PCCdata repository:**
- `POLI.csv` - Politician information (including gender data)
- `RESE.csv` - Parliamentary membership episodes 
- `PARL.csv` - Parliament period information (including `parliament_size` variable)
- `MEME.csv` - Membership episodes
- `dataversion.txt` - Data version tracking for caching

### Dependencies

This script relies on data integrity functions from the R047 project:
```r
pathtocheckerfunctions <- "/home/tomas/projects/ProjectR047_PCCIntegrity/"
```
Make sure this path exists or update it to point to your R047 functions.

## 🏃‍♂️ Running the Analysis

### Basic Usage
```bash
Rscript R051.R
```

### What Happens
1. **Data Validation**: Runs comprehensive data integrity checks
2. **Daily Calculations**: Computes daily MP counts (cached for performance)
3. **Deviation Detection**: Identifies problematic periods in parliament size data
4. **Visualization**: Generates comprehensive plot saved as `women_representation_simplified.png`
5. **Testing**: Runs 47 unit tests to ensure accuracy

## 🔧 Configuration

### Deviation Detection Parameters
Modify the `detect_parliament_deviations()` calls in the script:

```r
detect_parliament_deviations(
  DAILY_COUNTS, 
  parl_baseline, 
  seat_threshold = 5,           # Seats deviation threshold
  duration_threshold_days = 90, # Minimum duration for flagging
  merge_gap_days = 7           # Days gap for merging periods
)
```

### Election Analysis Window
Adjust the analysis window around elections:
```r
n_days <- 30  # Days before/after elections to measure
```

## 📁 Project Structure

```
R051_NewDaybyDay/
├── R051.R                    # Main analysis script
├── R051_functions.R          # Custom functions
├── R051_unittests.R         # Comprehensive test suite
├── daily_counts_cache.RData # Performance cache
├── dataversion_latest_run.txt # Cache versioning
└── women_representation_simplified.png # Output plot

External Dependencies:
├── PCCdata/                 # Centralized data repository (clone separately)
│   ├── POLI.csv
│   ├── RESE.csv
│   ├── PARL.csv
│   ├── MEME.csv
│   └── dataversion.txt
└── ProjectR047_PCCIntegrity/ # Data integrity functions
    └── R047_RESE_functions.R
```

## 🧪 Testing

The project includes comprehensive testing:
- **47 unit tests** covering all functions and edge cases
- **Automatic execution** during main script run
- **Integration tests** ensuring functions work together correctly

Run tests independently:
```r
source("R051_functions.R")
testthat::test_file("R051_unittests.R")
```

## 📈 Output

The script generates:
- **`women_representation_simplified.png`**: Comprehensive visualization showing trends and data quality
- **Console output**: Data integrity check results and processing status
- **`final_deviations` object**: Data frame of detected problematic periods for further analysis

## 🔍 Key Functions

### `grab_pct_women(term_starts, offset_days, daily_counts)`
Calculate percentage of women MPs at specified offset from election dates.

### `detect_parliament_deviations(daily_counts, parl_baseline, ...)`
Detect periods where parliament size significantly deviates from expected baseline, with configurable merging of closely spaced periods.

## 🤝 Contributing

This is an academic research project. If you find issues or have improvements:
1. Check the comprehensive test suite covers your use case
2. Ensure data integrity checks pass
3. Follow the existing code organization pattern

## 📝 Notes

- **Netherlands Focus**: Currently configured for Dutch Parliament data (`country_abb == "NL"`)
- **Historical Scope**: Analyzes from earliest available data (~1901) to present
- **Performance**: First run calculates daily counts (slow), subsequent runs use cache (fast)
- **Data Quality**: Built-in monitoring alerts to potential data integrity issues

## 🏛️ Use Cases

Perfect for:
- **Academic Research**: Women's political representation studies
- **Data Quality Assurance**: Monitoring parliamentary datasets for inconsistencies
- **Historical Analysis**: Long-term trends in political representation
- **Election Impact Studies**: Measuring representational changes around elections