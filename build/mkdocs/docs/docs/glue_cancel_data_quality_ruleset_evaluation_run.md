<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>glue_cancel_data_quality_ruleset_evaluation_run</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Cancels a run where a ruleset is being evaluated against a data source

### Description

Cancels a run where a ruleset is being evaluated against a data source.

### Usage

    glue_cancel_data_quality_ruleset_evaluation_run(RunId)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="glue_cancel_data_quality_ruleset_evaluation_run_:_RunId">RunId</code></td>
<td><p>[required] The unique run identifier associated with this
run.</p></td>
</tr>
</tbody>
</table>

### Value

An empty list.

### Request syntax

    svc$cancel_data_quality_ruleset_evaluation_run(
      RunId = "string"
    )