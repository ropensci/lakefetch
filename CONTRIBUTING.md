# Contributing to lakefetch

Thank you for your interest in contributing to lakefetch! This document provides guidelines for contributing to the project.

## Reporting Issues

- Use the [GitHub issue tracker](https://github.com/ropensci/lakefetch/issues) to report bugs or suggest features.
- For bug reports, please include a minimal reproducible example, your R version (`sessionInfo()`), and the error message.

## Pull Requests

1. Fork the repository and create a new branch from `main`.
2. Make your changes, ensuring `R CMD check` passes with no errors or warnings.
3. Add tests for new functionality in `tests/testthat/`.
4. Update documentation if you change any exported functions (run `devtools::document()`).
5. Update `NEWS.md` with a summary of your changes.
6. Submit the pull request with a clear description of the changes.

## Development Setup

```r
# Install development dependencies
install.packages(c("devtools", "testthat", "roxygen2", "knitr", "rmarkdown"))

# Clone and install
# git clone https://github.com/ropensci/lakefetch.git
devtools::load_all(".")

# Run tests
devtools::test()

# Check package
devtools::check()
```

## Code Style

- Follow standard R coding conventions.
- Use roxygen2 for documentation with markdown enabled.
- Include `@examples` for exported functions.

## Questions

Open an issue on GitHub or email the maintainer at farrej2@rpi.edu.
