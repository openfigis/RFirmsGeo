RFirmsGeo
=========

Tools to produce geospatial objects for FIRMS stocks and fisheries factsheet 
web-resources.The geospatial objects are generated from the list of geographic
references part of the factsheet XML files, including ``water areas`` and 
``species distributions``.

The geoprocessings involves a sequential intersection (with no limitations on 
the number of intersections to compute) and a final geometry union. The most 
accurate geospatial processing is guaranteed by geometry validity controls and 
cleaning operations using the [cleangeo](https://github.com/eblondel/cleangeo) 
package.
