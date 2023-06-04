<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>glue_delete_column_statistics_for_table</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Retrieves table statistics of columns

### Description

Retrieves table statistics of columns.

The Identity and Access Management (IAM) permission required for this
operation is `delete_table`.

### Usage

    glue_delete_column_statistics_for_table(CatalogId, DatabaseName,
      TableName, ColumnName)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="glue_delete_column_statistics_for_table_:_CatalogId">CatalogId</code></td>
<td><p>The ID of the Data Catalog where the partitions in question
reside. If none is supplied, the Amazon Web Services account ID is used
by default.</p></td>
</tr>
<tr class="even">
<td><code
id="glue_delete_column_statistics_for_table_:_DatabaseName">DatabaseName</code></td>
<td><p>[required] The name of the catalog database where the partitions
reside.</p></td>
</tr>
<tr class="odd">
<td><code
id="glue_delete_column_statistics_for_table_:_TableName">TableName</code></td>
<td><p>[required] The name of the partitions' table.</p></td>
</tr>
<tr class="even">
<td><code
id="glue_delete_column_statistics_for_table_:_ColumnName">ColumnName</code></td>
<td><p>[required] The name of the column.</p></td>
</tr>
</tbody>
</table>

### Value

An empty list.

### Request syntax

    svc$delete_column_statistics_for_table(
      CatalogId = "string",
      DatabaseName = "string",
      TableName = "string",
      ColumnName = "string"
    )