<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>guardduty_disassociate_from_master_account</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Disassociates the current GuardDuty member account from its administrator account

### Description

Disassociates the current GuardDuty member account from its
administrator account.

### Usage

    guardduty_disassociate_from_master_account(DetectorId)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="guardduty_disassociate_from_master_account_:_DetectorId">DetectorId</code></td>
<td><p>[required] The unique ID of the detector of the GuardDuty member
account.</p></td>
</tr>
</tbody>
</table>

### Value

An empty list.

### Request syntax

    svc$disassociate_from_master_account(
      DetectorId = "string"
    )