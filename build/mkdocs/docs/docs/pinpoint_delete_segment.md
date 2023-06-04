<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>pinpoint_delete_segment</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Deletes a segment from an application

### Description

Deletes a segment from an application.

### Usage

    pinpoint_delete_segment(ApplicationId, SegmentId)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="pinpoint_delete_segment_:_ApplicationId">ApplicationId</code></td>
<td><p>[required] The unique identifier for the application. This
identifier is displayed as the <strong>Project ID</strong> on the Amazon
Pinpoint console.</p></td>
</tr>
<tr class="even">
<td><code id="pinpoint_delete_segment_:_SegmentId">SegmentId</code></td>
<td><p>[required] The unique identifier for the segment.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      SegmentResponse = list(
        ApplicationId = "string",
        Arn = "string",
        CreationDate = "string",
        Dimensions = list(
          Attributes = list(
            list(
              AttributeType = "INCLUSIVE"|"EXCLUSIVE"|"CONTAINS"|"BEFORE"|"AFTER"|"ON"|"BETWEEN",
              Values = list(
                "string"
              )
            )
          ),
          Behavior = list(
            Recency = list(
              Duration = "HR_24"|"DAY_7"|"DAY_14"|"DAY_30",
              RecencyType = "ACTIVE"|"INACTIVE"
            )
          ),
          Demographic = list(
            AppVersion = list(
              DimensionType = "INCLUSIVE"|"EXCLUSIVE",
              Values = list(
                "string"
              )
            ),
            Channel = list(
              DimensionType = "INCLUSIVE"|"EXCLUSIVE",
              Values = list(
                "string"
              )
            ),
            DeviceType = list(
              DimensionType = "INCLUSIVE"|"EXCLUSIVE",
              Values = list(
                "string"
              )
            ),
            Make = list(
              DimensionType = "INCLUSIVE"|"EXCLUSIVE",
              Values = list(
                "string"
              )
            ),
            Model = list(
              DimensionType = "INCLUSIVE"|"EXCLUSIVE",
              Values = list(
                "string"
              )
            ),
            Platform = list(
              DimensionType = "INCLUSIVE"|"EXCLUSIVE",
              Values = list(
                "string"
              )
            )
          ),
          Location = list(
            Country = list(
              DimensionType = "INCLUSIVE"|"EXCLUSIVE",
              Values = list(
                "string"
              )
            ),
            GPSPoint = list(
              Coordinates = list(
                Latitude = 123.0,
                Longitude = 123.0
              ),
              RangeInKilometers = 123.0
            )
          ),
          Metrics = list(
            list(
              ComparisonOperator = "string",
              Value = 123.0
            )
          ),
          UserAttributes = list(
            list(
              AttributeType = "INCLUSIVE"|"EXCLUSIVE"|"CONTAINS"|"BEFORE"|"AFTER"|"ON"|"BETWEEN",
              Values = list(
                "string"
              )
            )
          )
        ),
        Id = "string",
        ImportDefinition = list(
          ChannelCounts = list(
            123
          ),
          ExternalId = "string",
          Format = "CSV"|"JSON",
          RoleArn = "string",
          S3Url = "string",
          Size = 123
        ),
        LastModifiedDate = "string",
        Name = "string",
        SegmentGroups = list(
          Groups = list(
            list(
              Dimensions = list(
                list(
                  Attributes = list(
                    list(
                      AttributeType = "INCLUSIVE"|"EXCLUSIVE"|"CONTAINS"|"BEFORE"|"AFTER"|"ON"|"BETWEEN",
                      Values = list(
                        "string"
                      )
                    )
                  ),
                  Behavior = list(
                    Recency = list(
                      Duration = "HR_24"|"DAY_7"|"DAY_14"|"DAY_30",
                      RecencyType = "ACTIVE"|"INACTIVE"
                    )
                  ),
                  Demographic = list(
                    AppVersion = list(
                      DimensionType = "INCLUSIVE"|"EXCLUSIVE",
                      Values = list(
                        "string"
                      )
                    ),
                    Channel = list(
                      DimensionType = "INCLUSIVE"|"EXCLUSIVE",
                      Values = list(
                        "string"
                      )
                    ),
                    DeviceType = list(
                      DimensionType = "INCLUSIVE"|"EXCLUSIVE",
                      Values = list(
                        "string"
                      )
                    ),
                    Make = list(
                      DimensionType = "INCLUSIVE"|"EXCLUSIVE",
                      Values = list(
                        "string"
                      )
                    ),
                    Model = list(
                      DimensionType = "INCLUSIVE"|"EXCLUSIVE",
                      Values = list(
                        "string"
                      )
                    ),
                    Platform = list(
                      DimensionType = "INCLUSIVE"|"EXCLUSIVE",
                      Values = list(
                        "string"
                      )
                    )
                  ),
                  Location = list(
                    Country = list(
                      DimensionType = "INCLUSIVE"|"EXCLUSIVE",
                      Values = list(
                        "string"
                      )
                    ),
                    GPSPoint = list(
                      Coordinates = list(
                        Latitude = 123.0,
                        Longitude = 123.0
                      ),
                      RangeInKilometers = 123.0
                    )
                  ),
                  Metrics = list(
                    list(
                      ComparisonOperator = "string",
                      Value = 123.0
                    )
                  ),
                  UserAttributes = list(
                    list(
                      AttributeType = "INCLUSIVE"|"EXCLUSIVE"|"CONTAINS"|"BEFORE"|"AFTER"|"ON"|"BETWEEN",
                      Values = list(
                        "string"
                      )
                    )
                  )
                )
              ),
              SourceSegments = list(
                list(
                  Id = "string",
                  Version = 123
                )
              ),
              SourceType = "ALL"|"ANY"|"NONE",
              Type = "ALL"|"ANY"|"NONE"
            )
          ),
          Include = "ALL"|"ANY"|"NONE"
        ),
        SegmentType = "DIMENSIONAL"|"IMPORT",
        tags = list(
          "string"
        ),
        Version = 123
      )
    )

### Request syntax

    svc$delete_segment(
      ApplicationId = "string",
      SegmentId = "string"
    )