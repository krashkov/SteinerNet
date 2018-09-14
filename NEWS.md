# News

### v3.0.1

* If vertices of input graph of `steinertree` function has a `name` attribute, vertices of the output graph won't have `realname` attribute. The original names will be still at `name` attribute.

### v3.0.0

* Critical bugs was fixed.  A list may be provided on demand.
* Structure of files and functions was refreshed
* **Backward incompatible changes were made** (behaviour of some functions and list of passed arguments was slightly changed). On the one hand, it makes the work with library simpler and more intuitive, but on the other hand, the workflow is changed too. So if you want to reproduce the results of the original article, please see [tutorial](https://github.com/krashkov/SteinerNet/blob/master/vignettes/tutorial.pdf) and documentation.


### v2.0

Support of igraph graphs is added.
