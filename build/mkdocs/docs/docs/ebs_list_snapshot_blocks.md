<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>ebs_list_snapshot_blocks</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Returns information about the blocks in an Amazon Elastic Block Store snapshot

### Description

Returns information about the blocks in an Amazon Elastic Block Store
snapshot.

### Usage

    ebs_list_snapshot_blocks(SnapshotId, NextToken, MaxResults,
      StartingBlockIndex)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="ebs_list_snapshot_blocks_:_SnapshotId">SnapshotId</code></td>
<td><p>[required] The ID of the snapshot from which to get block indexes
and block tokens.</p></td>
</tr>
<tr class="even">
<td><code
id="ebs_list_snapshot_blocks_:_NextToken">NextToken</code></td>
<td><p>The token to request the next page of results.</p>
<p>If you specify <strong>NextToken</strong>, then
<strong>StartingBlockIndex</strong> is ignored.</p></td>
</tr>
<tr class="odd">
<td><code
id="ebs_list_snapshot_blocks_:_MaxResults">MaxResults</code></td>
<td><p>The maximum number of blocks to be returned by the request.</p>
<p>Even if additional blocks can be retrieved from the snapshot, the
request can return less blocks than <strong>MaxResults</strong> or an
empty array of blocks.</p>
<p>To retrieve the next set of blocks from the snapshot, make another
request with the returned <strong>NextToken</strong> value. The value of
<strong>NextToken</strong> is <code>null</code> when there are no more
blocks to return.</p></td>
</tr>
<tr class="even">
<td><code
id="ebs_list_snapshot_blocks_:_StartingBlockIndex">StartingBlockIndex</code></td>
<td><p>The block index from which the list should start. The list in the
response will start from this block index or the next valid block index
in the snapshot.</p>
<p>If you specify <strong>NextToken</strong>, then
<strong>StartingBlockIndex</strong> is ignored.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      Blocks = list(
        list(
          BlockIndex = 123,
          BlockToken = "string"
        )
      ),
      ExpiryTime = as.POSIXct(
        "2015-01-01"
      ),
      VolumeSize = 123,
      BlockSize = 123,
      NextToken = "string"
    )

### Request syntax

    svc$list_snapshot_blocks(
      SnapshotId = "string",
      NextToken = "string",
      MaxResults = 123,
      StartingBlockIndex = 123
    )