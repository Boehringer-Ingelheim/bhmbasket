Maintainer: 'Stephan Wojciekowski <stephan.wojciekowski@boehringer-ingelheim.com>'

## Test environments & R CMD check results

- Macbuilder aarch64-aarch64-apple-darwin20, R Under development (unstable) (2026-03-22 r89674)
  - Status: OK
- Winbuilder Windows Server 2022 x64 (build 20348), x86_64-w64-mingw32, R 4.6.0 beta (2026-04-12 r89874 ucrt)
  - Status: OK
- GitHub Action Linux: R version 4.5.3 (2026-03-11), x86_64-pc-linux-gnu
  - Status: OK
- GitHub Action Mac: R version 4.5.3 (2026-03-11), aarch64-apple-darwin20
  - Status: OK
- GitHub Action Windows: R version 4.5.3 (2026-03-11 ucrt), x86_64-w64-mingw32  
  - Status: OK

## bhmbasket 1.1.0

### New and Altered Features

* Added mixture priors and models `"exnex_mix"`, `"exnex_adj_mix"`, `"stratified_mix"`.

* Minor changes in code