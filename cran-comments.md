Maintainer: 'Stephan Wojciekowski <stephan.wojciekowski@boehringer-ingelheim.com>'

## Test environments & R CMD check results

- Macbuilder aarch64-aarch64-apple-darwin20, macOS 26.2 (25C56), Apple M1, R 4.6.0 (r-devel-macosx-arm64), SDK 14.4 (clang-1700.6.3.2)  
  - Status: OK
- Winbuilder Windows Server 2022 x64 (build 20348), x86_64-w64-mingw32, R Under development (unstable) (2026-02-18 r89435 ucrt)
  - Status: OK
- GitHub Action Linux: R 4.5.2 (2025-10-31), x86_64-pc-linux-gnu
  - Status: OK
- GitHub Action Mac: R 4.5.2 (2025-10-31), aarch64-apple-darwin20
  - Status: OK
- GitHub Action Windows: R 4.5.2 (2025-10-31 ucrt), x86_64-w64-mingw32  
  - Status: OK

## From NEWS.md: bhmbasket 1.0.0

* Added structured unit tests for core functionality

* Integrated code coverage

* Replaced validation logic with checkmate assertions and removed unused helper functions

* Replaced dependency on R2jags to rjags

* Added Github Actions workflows for automated R CMD check and CI validation

* Minor changes in code