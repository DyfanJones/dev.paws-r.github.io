<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>ec2_get_network_insights_access_scope_content</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Gets the content for the specified Network Access Scope

### Description

Gets the content for the specified Network Access Scope.

### Usage

    ec2_get_network_insights_access_scope_content(
      NetworkInsightsAccessScopeId, DryRun)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="ec2_get_network_insights_access_scope_content_:_NetworkInsightsAccessScopeId">NetworkInsightsAccessScopeId</code></td>
<td><p>[required] The ID of the Network Access Scope.</p></td>
</tr>
<tr class="even">
<td><code
id="ec2_get_network_insights_access_scope_content_:_DryRun">DryRun</code></td>
<td><p>Checks whether you have the required permissions for the action,
without actually making the request, and provides an error response. If
you have the required permissions, the error response is
<code>DryRunOperation</code>. Otherwise, it is
<code>UnauthorizedOperation</code>.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      NetworkInsightsAccessScopeContent = list(
        NetworkInsightsAccessScopeId = "string",
        MatchPaths = list(
          list(
            Source = list(
              PacketHeaderStatement = list(
                SourceAddresses = list(
                  "string"
                ),
                DestinationAddresses = list(
                  "string"
                ),
                SourcePorts = list(
                  "string"
                ),
                DestinationPorts = list(
                  "string"
                ),
                SourcePrefixLists = list(
                  "string"
                ),
                DestinationPrefixLists = list(
                  "string"
                ),
                Protocols = list(
                  "tcp"|"udp"
                )
              ),
              ResourceStatement = list(
                Resources = list(
                  "string"
                ),
                ResourceTypes = list(
                  "string"
                )
              )
            ),
            Destination = list(
              PacketHeaderStatement = list(
                SourceAddresses = list(
                  "string"
                ),
                DestinationAddresses = list(
                  "string"
                ),
                SourcePorts = list(
                  "string"
                ),
                DestinationPorts = list(
                  "string"
                ),
                SourcePrefixLists = list(
                  "string"
                ),
                DestinationPrefixLists = list(
                  "string"
                ),
                Protocols = list(
                  "tcp"|"udp"
                )
              ),
              ResourceStatement = list(
                Resources = list(
                  "string"
                ),
                ResourceTypes = list(
                  "string"
                )
              )
            ),
            ThroughResources = list(
              list(
                ResourceStatement = list(
                  Resources = list(
                    "string"
                  ),
                  ResourceTypes = list(
                    "string"
                  )
                )
              )
            )
          )
        ),
        ExcludePaths = list(
          list(
            Source = list(
              PacketHeaderStatement = list(
                SourceAddresses = list(
                  "string"
                ),
                DestinationAddresses = list(
                  "string"
                ),
                SourcePorts = list(
                  "string"
                ),
                DestinationPorts = list(
                  "string"
                ),
                SourcePrefixLists = list(
                  "string"
                ),
                DestinationPrefixLists = list(
                  "string"
                ),
                Protocols = list(
                  "tcp"|"udp"
                )
              ),
              ResourceStatement = list(
                Resources = list(
                  "string"
                ),
                ResourceTypes = list(
                  "string"
                )
              )
            ),
            Destination = list(
              PacketHeaderStatement = list(
                SourceAddresses = list(
                  "string"
                ),
                DestinationAddresses = list(
                  "string"
                ),
                SourcePorts = list(
                  "string"
                ),
                DestinationPorts = list(
                  "string"
                ),
                SourcePrefixLists = list(
                  "string"
                ),
                DestinationPrefixLists = list(
                  "string"
                ),
                Protocols = list(
                  "tcp"|"udp"
                )
              ),
              ResourceStatement = list(
                Resources = list(
                  "string"
                ),
                ResourceTypes = list(
                  "string"
                )
              )
            ),
            ThroughResources = list(
              list(
                ResourceStatement = list(
                  Resources = list(
                    "string"
                  ),
                  ResourceTypes = list(
                    "string"
                  )
                )
              )
            )
          )
        )
      )
    )

### Request syntax

    svc$get_network_insights_access_scope_content(
      NetworkInsightsAccessScopeId = "string",
      DryRun = TRUE|FALSE
    )