



##Changes from initial submission

# If there are references describing the methods in your package, please
# add these in the description field of your DESCRIPTION file in the form
# authors (year) <doi:...>
# authors (year) <arXiv:...>
# authors (year, ISBN:...)
# or if those are not available: <https:...>
# with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for
# auto-linking.
# (If you want to add a title as well please put it in quotes: "Title")

The methods in this package are novel. A publicaiton of the methods is currently
under review, and these will be included in the next version after publication

# You have examples for unexported functions. e.g.
#     break_filename() in:
#        break_filename.Rd
#     calibrate_pixel_size() in:
#        calibrate_pixel_size.Rd
#     collect_filename() in:
#        collect_filename.Rd
#     make_filename() in:
#        break_filename.Rd
#        collect_filename.Rd
#        make_filename.Rd
#   Please either omit these examples or export the functions.

These examples have been omitted

# Some code lines in examples are commented out in collect_filename.Rd,
# calibrate_pixel_size.Rd, select_roi.Rd, make_filename.Rd,
# break_filename.Rd , ...
# Please never do that. Ideally find toy examples that can be regularly
# executed and checked.
# Lengthy examples (> 5 sec), can be wrapped in \donttest.
# Functions which are supposed to only run interactively should be wrapped
# in if(interactive()){}.

These changes have been made

# Please ensure that your functions do not write by default or in your
# examples/vignettes/tests in the user's home filespace (including the
# package directory and getwd()).
# In your examples/vignettes/tests you can write to tempdir().

These have been updated. All files are now saved to tempdir() unless specified.
