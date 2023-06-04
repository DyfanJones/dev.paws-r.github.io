<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>ssm_register_patch_baseline_for_patch_group</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Registers a patch baseline for a patch group

### Description

Registers a patch baseline for a patch group.

### Usage

    ssm_register_patch_baseline_for_patch_group(BaselineId, PatchGroup)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="ssm_register_patch_baseline_for_patch_group_:_BaselineId">BaselineId</code></td>
<td><p>[required] The ID of the patch baseline to register with the
patch group.</p></td>
</tr>
<tr class="even">
<td><code
id="ssm_register_patch_baseline_for_patch_group_:_PatchGroup">PatchGroup</code></td>
<td><p>[required] The name of the patch group to be registered with the
patch baseline.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      BaselineId = "string",
      PatchGroup = "string"
    )

### Request syntax

    svc$register_patch_baseline_for_patch_group(
      BaselineId = "string",
      PatchGroup = "string"
    )