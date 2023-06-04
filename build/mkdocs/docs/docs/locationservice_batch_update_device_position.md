<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>locationservice_batch_update_device_position</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Uploads position update data for one or more devices to a tracker resource

### Description

Uploads position update data for one or more devices to a tracker
resource. Amazon Location uses the data when it reports the last known
device position and position history. Amazon Location retains location
data for 30 days.

Position updates are handled based on the `PositionFiltering` property
of the tracker. When `PositionFiltering` is set to `TimeBased`, updates
are evaluated against linked geofence collections, and location data is
stored at a maximum of one position per 30 second interval. If your
update frequency is more often than every 30 seconds, only one update
per 30 seconds is stored for each unique device ID.

When `PositionFiltering` is set to `DistanceBased` filtering, location
data is stored and evaluated against linked geofence collections only if
the device has moved more than 30 m (98.4 ft).

When `PositionFiltering` is set to `AccuracyBased` filtering, location
data is stored and evaluated against linked geofence collections only if
the device has moved more than the measured accuracy. For example, if
two consecutive updates from a device have a horizontal accuracy of 5 m
and 10 m, the second update is neither stored or evaluated if the device
has moved less than 15 m. If `PositionFiltering` is set to
`AccuracyBased` filtering, Amazon Location uses the default value
`{ "Horizontal": 0}` when accuracy is not provided on a
`DevicePositionUpdate`.

### Usage

    locationservice_batch_update_device_position(TrackerName, Updates)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="locationservice_batch_update_device_position_:_TrackerName">TrackerName</code></td>
<td><p>[required] The name of the tracker resource to update.</p></td>
</tr>
<tr class="even">
<td><code
id="locationservice_batch_update_device_position_:_Updates">Updates</code></td>
<td><p>[required] Contains the position update details for each
device.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      Errors = list(
        list(
          DeviceId = "string",
          Error = list(
            Code = "AccessDeniedError"|"ConflictError"|"InternalServerError"|"ResourceNotFoundError"|"ThrottlingError"|"ValidationError",
            Message = "string"
          ),
          SampleTime = as.POSIXct(
            "2015-01-01"
          )
        )
      )
    )

### Request syntax

    svc$batch_update_device_position(
      TrackerName = "string",
      Updates = list(
        list(
          Accuracy = list(
            Horizontal = 123.0
          ),
          DeviceId = "string",
          Position = list(
            123.0
          ),
          PositionProperties = list(
            "string"
          ),
          SampleTime = as.POSIXct(
            "2015-01-01"
          )
        )
      )
    )