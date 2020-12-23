
# forrel - Exclusion Power GUI

## Installation

This package requires `forrel` (available on CRAN) plus some other
packages to display the user interface. Installing packages from GitHub
requires the package `devtools` which is available on CRAN and may be
installed with

``` r
install.packages("devtools")
```

Once you have `devtools` you may install `forrelgui` by pasting the
following into an R interactive session:

``` r
devtools::install_github("knifecake/forrelgui")
```

## Exclusion power

To launch the exclusion power graphical user interface import the
`forrelgui` package and use the `epGUI()` function like this:

``` r
library("forrelgui")
epGUI()
```

## TODOs

  - [x] Load and save projects as `.Rdata` files
  - [x] Load Familias using `readFam`
  - [x] Load and plot pedigrees
  - [x] Select individuals available for genotyping
      - [x] Filter IDs so that only those that are possible are shown
  - [x] Load and preview frequency databases
      - [ ] Set a minimum width for columns
      - [ ] Auto-detect database format
  - [ ] Load and preview reference profiles
      - [ ] Auto-detect reference profile formats
  - [x] Pass a project file to `epGUI` to open it directly
  - [ ] Marker settings table
      - [x] Exclude markers from calculation
      - [ ] Set mutation models
  - [x] Compute EP
      - [ ] Show progress during calculation by running one marker at a
        time
      - [x] Display results in a new tab and focus it
      - [ ] Parallelize calculation by spreading different markers to
        different cores (look into vectorization?)
      - [ ] Show a warning if the current results tab is stale
  - [ ] Come up with examples for testing
      - [x] Paternity
      - [x] Paternity with reference profiles
      - [x] Familias .fam file with pedigrees
      - [ ] Familias .fam file with frequency database
      - [ ] A large frequency database
      - [ ] Mutations to showcase mutation models
      - [ ] Examples from the GHEP exercise
  - [ ] Make examples available directly in the program
  - [ ] Make testing examples loadable to demonstrate the program
  - [ ] Project settings (file input formats, allele rounding / rogue
    alleles)
  - [ ] Website with help pages?
  - [ ] Save report?
  - [ ] Better *About epGUI* dialog

## LICENSE

This project is licensed under the MIT license. See the `LICENSE` file
for details.
