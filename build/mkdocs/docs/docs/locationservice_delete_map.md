<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>locationservice_delete_map</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Deletes a map resource from your Amazon Web Services account

### Description

Deletes a map resource from your Amazon Web Services account.

This operation deletes the resource permanently. If the map is being
used in an application, the map may not render.

### Usage

    locationservice_delete_map(MapName)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code id="locationservice_delete_map_:_MapName">MapName</code></td>
<td><p>[required] The name of the map resource to be deleted.</p></td>
</tr>
</tbody>
</table>

### Value

An empty list.

### Request syntax

    svc$delete_map(
      MapName = "string"
    )