## Resubmission

This is a resubmission of countyhealthR 0.1.3 (previously 0.1.2).

Changes made in response to CRAN feedback:

-   Removed countyhealthR_reset_cache.Rd, which had a missing \value tag and was unnecessary.

-   All data-download functions are now wrapped in \donttest{} to avoid running long examples automatically.

-   Added *citation* and *verbose* parameters to allow users to suppress printed messages

-   Reconfigured get_chrr_measure_metadata() to avoid writing or printing to the user’s environment.

-   Updated Rd documentation for all exported functions to include a \value{} tag explaining the structure (class) and meaning of the returned objects.

-   Ensured that no function writes by default to the user’s home directory, working directory, or package directory

## R CMD check results

0 errors \| 0 warnings \| 1 note

-   This is a new release.
