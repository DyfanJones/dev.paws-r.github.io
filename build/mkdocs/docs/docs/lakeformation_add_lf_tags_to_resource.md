<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>lakeformation_add_lf_tags_to_resource</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Attaches one or more LF-tags to an existing resource

### Description

Attaches one or more LF-tags to an existing resource.

### Usage

    lakeformation_add_lf_tags_to_resource(CatalogId, Resource, LFTags)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="lakeformation_add_lf_tags_to_resource_:_CatalogId">CatalogId</code></td>
<td><p>The identifier for the Data Catalog. By default, the account ID.
The Data Catalog is the persistent metadata store. It contains database
definitions, table definitions, and other control information to manage
your Lake Formation environment.</p></td>
</tr>
<tr class="even">
<td><code
id="lakeformation_add_lf_tags_to_resource_:_Resource">Resource</code></td>
<td><p>[required] The database, table, or column resource to which to
attach an LF-tag.</p></td>
</tr>
<tr class="odd">
<td><code
id="lakeformation_add_lf_tags_to_resource_:_LFTags">LFTags</code></td>
<td><p>[required] The LF-tags to attach to the resource.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      Failures = list(
        list(
          LFTag = list(
            CatalogId = "string",
            TagKey = "string",
            TagValues = list(
              "string"
            )
          ),
          Error = list(
            ErrorCode = "string",
            ErrorMessage = "string"
          )
        )
      )
    )

### Request syntax

    svc$add_lf_tags_to_resource(
      CatalogId = "string",
      Resource = list(
        Catalog = list(),
        Database = list(
          CatalogId = "string",
          Name = "string"
        ),
        Table = list(
          CatalogId = "string",
          DatabaseName = "string",
          Name = "string",
          TableWildcard = list()
        ),
        TableWithColumns = list(
          CatalogId = "string",
          DatabaseName = "string",
          Name = "string",
          ColumnNames = list(
            "string"
          ),
          ColumnWildcard = list(
            ExcludedColumnNames = list(
              "string"
            )
          )
        ),
        DataLocation = list(
          CatalogId = "string",
          ResourceArn = "string"
        ),
        DataCellsFilter = list(
          TableCatalogId = "string",
          DatabaseName = "string",
          TableName = "string",
          Name = "string"
        ),
        LFTag = list(
          CatalogId = "string",
          TagKey = "string",
          TagValues = list(
            "string"
          )
        ),
        LFTagPolicy = list(
          CatalogId = "string",
          ResourceType = "DATABASE"|"TABLE",
          Expression = list(
            list(
              TagKey = "string",
              TagValues = list(
                "string"
              )
            )
          )
        )
      ),
      LFTags = list(
        list(
          CatalogId = "string",
          TagKey = "string",
          TagValues = list(
            "string"
          )
        )
      )
    )