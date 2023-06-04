<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>globalaccelerator_list_listeners</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## List the listeners for an accelerator

### Description

List the listeners for an accelerator.

### Usage

    globalaccelerator_list_listeners(AcceleratorArn, MaxResults, NextToken)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="globalaccelerator_list_listeners_:_AcceleratorArn">AcceleratorArn</code></td>
<td><p>[required] The Amazon Resource Name (ARN) of the accelerator for
which you want to list listener objects.</p></td>
</tr>
<tr class="even">
<td><code
id="globalaccelerator_list_listeners_:_MaxResults">MaxResults</code></td>
<td><p>The number of listener objects that you want to return with this
call. The default value is 10.</p></td>
</tr>
<tr class="odd">
<td><code
id="globalaccelerator_list_listeners_:_NextToken">NextToken</code></td>
<td><p>The token for the next set of results. You receive this token
from a previous call.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      Listeners = list(
        list(
          ListenerArn = "string",
          PortRanges = list(
            list(
              FromPort = 123,
              ToPort = 123
            )
          ),
          Protocol = "TCP"|"UDP",
          ClientAffinity = "NONE"|"SOURCE_IP"
        )
      ),
      NextToken = "string"
    )

### Request syntax

    svc$list_listeners(
      AcceleratorArn = "string",
      MaxResults = 123,
      NextToken = "string"
    )