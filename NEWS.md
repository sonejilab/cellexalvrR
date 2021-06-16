# 0.14.6

Vignettes compile again. Version to be published.

# 0.14.5

Debug version for the pre-publications CellexalVR v0.14.

# 0.14.4

In the Timeline grouping the amount of clusters can no be forced by setting cellexalObj@usedObj$gene_clusters to e.g. 3. By default one less cluster is produced.

# 0.14.3

Session Logs are ordered consitantly.
Time logs now also trigger a stats section.
Session logs contain detailed information about the displayed groupings.
Linear stat deg.genes are now based on p.value not correlation alone.

# 0.14.2

Logs fixed 3D plots not showing colours.
server + logFigure VR screenshot integration into the log file.
R sessions now should last for the whole length of a VR session and be unique.

# 0.14.0

Add timeline features.

# 0.13.2

3D drc graph entropy calculations implemented in c++.
Network function will now only use decently expressed TFs (<10% of cells) and apply a lower correlation cutoff (0.1 new vs 0.8 old)

# 0.10.1

export2cellexalvr: The hull datasets are no longer created