<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>accessanalyzer_apply_archive_rule</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Retroactively applies the archive rule to existing findings that meet the archive rule criteria

### Description

Retroactively applies the archive rule to existing findings that meet
the archive rule criteria.

### Usage

    accessanalyzer_apply_archive_rule(analyzerArn, ruleName, clientToken)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="accessanalyzer_apply_archive_rule_:_analyzerArn">analyzerArn</code></td>
<td><p>[required] The Amazon resource name (ARN) of the
analyzer.</p></td>
</tr>
<tr class="even">
<td><code
id="accessanalyzer_apply_archive_rule_:_ruleName">ruleName</code></td>
<td><p>[required] The name of the rule to apply.</p></td>
</tr>
<tr class="odd">
<td><code
id="accessanalyzer_apply_archive_rule_:_clientToken">clientToken</code></td>
<td><p>A client token.</p></td>
</tr>
</tbody>
</table>

### Value

An empty list.

### Request syntax

    svc$apply_archive_rule(
      analyzerArn = "string",
      ruleName = "string",
      clientToken = "string"
    )