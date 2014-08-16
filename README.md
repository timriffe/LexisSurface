Package deprecated
==================

I suggest you use either the . This one was an early package of mine, and was poorly coded. Similar functionality fo gridded demographic surfaces can now be found in `LexisMap()` of the LexisUtils package. That function will do AP, AC or PC Lexis surfaces. See installation instructions for that package here: [https://github.com/timriffe/LexisUtils](https://github.com/timriffe/LexisUtils)

If you want to plot Lexis triangles as such (with an AP 1:1 aspect ratio), I suggest following the guidelines for doing so using `lattice` [here](https://sites.google.com/site/timriffepersonal/DemogBlog/apclexissurfacesinlattice), or `ggplot2` [here](https://sites.google.com/site/timriffepersonal/DemogBlog/lexissurfacesinggplot2). For base graphics, take hints from th data prep steps given there, and make a single call to base graphics `polygon()` to plot the triangles. 

Feel free to mine the code you see in this repo, which is presently deprecated. If you're interested in the data prep/plotting strategy, then look instead to the two linked blog posts, which do it more efficiently that the code here. IFF I one day systematize demographic surface functions in R in some robust modular way, then I may revive this repository in the future.

LexisSurface
============

renders demographic Lexis surfaces (triangle or gridded)
