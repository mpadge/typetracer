# CRAN notes for typetrace_0.1.1 submission

This is a re-submission of a previously rejected new submission. This update addresses the following problems:

- DOI citation format in Description text has been fixed as suggested.
- Reference to R language in Description has been placed in single quotes.
- All functions now document return values.
- Tests which install packages have been switched off on CRAN machines.
- No tests or examples write to home filespace

Note that the comment about removing "This package" from the Description text appears to have been a false positive. The third sentence begins with "The 'typetracer' package ...", removal of which would make the Description text more difficult to understand.
