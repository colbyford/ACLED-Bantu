SELECT		(CASE WHEN mt.TaxaID IS NULL THEN (CASE WHEN y.TaxaID IS NULL THEN c.TaxaID ELSE y.TaxaID END) ELSE mt.TaxaID END) AS TaxaID
			,(CASE WHEN mt.GuthrieZone IS NULL THEN
				(CASE WHEN y.GuthrieZone IS NULL THEN
				c.GuthrieZone ELSE y.GuthrieZone END)
				ELSE mt.GuthrieZone END) AS GuthrieZone
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