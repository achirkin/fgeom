# fgeom
Experimental package for Haskell geometry types and functions to be used later together with OpenGL

## TODO:

* Local optimization methods are not fully implemented
* Too much code repetition in Types declaration - think of how to reduce this
* Speed has not been checked yet
* No eigen value and singular value decomposition yet
* No linear system solving yet (though can be done explicitly by inverting matrix)
* Think of extending Approximately to have more variables, e.g. eps and delta
* Root finding for general functions
* Vector "Divergence issue" - how to implement it efficiently, and other vector functions
* Polygons are really rudimentary yet
* Polygon triangulation - a lot of work here to be done
* Think of planarization and linearization of point set (N->2 or N->N-1, N->1 or N->N-2); this needs singular value decomposition if (N->2)
* Do better integration of Approximately
* Spatial trees for storing objects (KD-tree? Simple oct-tree? something else?)
* ...
