.dataset |
{
  id: .["@id"],
  type: .["@type"],
  temporalCoverage: .coverage.temporalCoverage.rangeOfDates |
    [.beginDate.calendarDate, .endDate.calendarDate] | join("/"),
  spatialCoverage: .coverage.geographicCoverage | {
    description: .geographicDescription,
      geo: {
        box: .boundingCoordinates |
        [.southBoundingCoordinate,
         .westBoundingCoordinate,
         .northBoundingCoordinate,
         .eastBoundingCoordinate] | join(" ")
      }
  }
}
