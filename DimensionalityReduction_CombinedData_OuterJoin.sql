SELECT		mt.TaxaID
			,mt.GuthrieZone
			,mt.Taxa AS mtDNATaxa
			,y.Taxa AS YchrTaxa
			,c.Taxa AS CulturalTaxa
			,mt.mtDNA
			,y.Ychr_STR
			,c.Cultural_EthnogAtlas
FROM		mtDNA mt
FULL OUTER JOIN	Ychr y
ON			mt.TaxaID = y.TaxaID
LEFT JOIN	Cultural c
ON			mt.TaxaID = c.TaxaID
OR			y.TaxaID = c.TaxaID