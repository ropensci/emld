. |
{
  "@id": .["@id"],
  "@type": .["@type"],
  coverage: {
    temporalCoverage: {
      rangeOfDates: {
        beginDate : .temporalCoverage | split("/") |.[0],
        endDate : .temporalCoverage | split("/") |.[1]
      }
    },
    geographicCoverage: .spatialCoverage | {
      geographicDescription: .description,
      boundingCoordinates: {
        westBoundingCoordinate: .geo.box | split(" ") | .[1],
        eastBoundingCoordinate: .geo.box | split(" ") | .[3],
        northBoundingCoordinate: .geo.box | split(" ") | .[2],
        southBoundingCoordinate: .geo.box | split(" ") | .[0]
      }
    }
  }
}
