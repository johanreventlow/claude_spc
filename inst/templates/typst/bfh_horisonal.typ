#import "bfh-template/bfh-template.typ" : *

// Example usage of bfh-diagram2 template with all configurable parameters
#show: chart => bfh-diagram2(
  // Hospital & department branding
  hospital: "Bispebjerg og Frederiksberg Hospital",
  department: [Geriatrisk Sengeafsnit G16],

  // Chart metadata
  title: [Medicinsikkert hospital \ *Scanning ved medicinadsministration*],
  details: [Periode: feb. 2019 – mar. 2022 • Gns. måned: 58938/97266 • seneste måned: 60756/88509 • Nuværende niveau (gennemsnit): 64,5%],

  // Analysis and interpretation
  analysis: [*Mere end 35.000 gange om måneden administreres medicin ikke korrekt*. Processen varierer ikke naturligt, og indeholder 3 særligt afvigende målepunkter. Niveauet er under målet. Forslag: *Identificér årsager bag de afvigende målepunkter*, og understøt faktorer der kan forbedre målopfyldelsen. Stabilisér processen når niveauet er tilfredsstillende.],

  // Data definition
  data_definition: [Indikatoren måler andelen af medicindoseringer hvor medicinen scannes korrekt ved administration til patienten. Tæller: Antal korrekt scannede doseringer. Nævner: Samlet antal doseringer. Datakilde: Medicinsikkerhedssystem (MSS).],

  // SPC Statistics (Anhøj rules)
  runs_expected: 12,
  runs_actual: 92,
  crossings_expected: 16,
  crossings_actual: 10,
  outliers_expected: 0,
  outliers_actual: 3,

  // Author and date
  author: "Johan Reventlow",
  date: datetime(year: 2025, month: 1, day: 19),

  // Chart content (will be replaced with actual chart image)
  chart
)

// Example chart image (uncomment and adjust path when using real chart)
#image("bfh-template/images/BFH_medicin.png",
  height: 115mm
)
