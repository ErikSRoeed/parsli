## _parsli_: Quick-start
No time to read the whole read-me? No worries!

### Installation
You can install _parsli_ from GitHub with the _devtools_ package from CRAN:
```r
devtools::install_github("ErikSRoeed/parsli")
```
In addition, _parsli_ naturally depends on a working installation of SLiM, which
you can download separately from its creators at https://messerlab.org/slim/.

### Support and issues
Please reach out with any inquiries about bugs or feature requests in a GitHub
Issue. I ask for your understanding that inquiries via e-mail (or elsewhere)
may not be addressed. Thank you!

### Citation
For the time being, feel free to cite this GitHub repository if you find 
_parsli_ useful. **And please note: _parsli_ and its contributor(s) are neither 
affiliated with nor endorsed by the developers of SLiM. Don't forget to cite 
SLiM's developers as per their preference!**

## _parsli_: A parsimonious R-SLiM interface
The objective of _parsli_ is to provide a minimal, plug-and-play interface
between R and SLiM. Rather than fully integrate SLiM into the R environment,
_parsli_ seeks to provide only a convenient means of importing, modularising,
and running pre-made SLiM simulations from R. As such, _parsli_ caters primarily
to users who wish specifically to interact with both SLiM and R separately.

This package was primarily developed for personal/in-house use. But if it seems
useful to you, you are more than welcome to apply it for your own needs.

### Alternatives to _parsli_
_parsli_ is not the only R-SLiM interface out there, and not the most
comprehensive. Its scope is deliberately minimal, for users who prefer that.
But other packages provide considerably deeper integration between SLiM and R.
If you consider using _parsli_, I would suggest also considering the R packages
_slimr_ (https://github.com/rdinnager/slimr) and _slendr_
(https://github.com/bodkan/slendr) first! Many users might find these
alternatives better suit their needs.

### Instructions for use
I intend to provide a thorough demonstration of _parsli_ at a later time ...

### Legacy versions
_parsli_ was previously named _slimmr_ - the name was changed to avoid confusion
with _slimr_ (see above). For reference, the legacy code of _slimmr_ can be
found archived at https://github.com/ErikSRoeed/slimmr.
