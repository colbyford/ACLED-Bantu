SELECT	c.TaxaID
		,c.GuthrieZone
		,c.Taxa AS CulturalTaxa
		,mt.Taxa AS mtDNATaxa
		,y.Taxa AS YchrTaxa
		,c.Cultural_EthnogAtlas
		,mt.mtDNA
		,y.Ychr_STR
FROM Cultural c
JOIN mtDNA mt
ON mt.TaxaID = c.TaxaID
JOIN Ychr y
ON y.TaxaID = c.TaxaID
AND mt.TaxaID = y.TaxaID