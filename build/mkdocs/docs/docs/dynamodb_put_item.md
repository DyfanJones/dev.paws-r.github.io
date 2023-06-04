<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>dynamodb_put_item</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Creates a new item, or replaces an old item with a new item

### Description

Creates a new item, or replaces an old item with a new item. If an item
that has the same primary key as the new item already exists in the
specified table, the new item completely replaces the existing item. You
can perform a conditional put operation (add a new item if one with the
specified primary key doesn't exist), or replace an existing item if it
has certain attribute values. You can return the item's attribute values
in the same operation, using the `ReturnValues` parameter.

When you add an item, the primary key attributes are the only required
attributes.

Empty String and Binary attribute values are allowed. Attribute values
of type String and Binary must have a length greater than zero if the
attribute is used as a key attribute for a table or index. Set type
attributes cannot be empty.

Invalid Requests with empty values will be rejected with a
`ValidationException` exception.

To prevent a new item from replacing an existing item, use a conditional
expression that contains the `attribute_not_exists` function with the
name of the attribute being used as the partition key for the table.
Since every record must contain that attribute, the
`attribute_not_exists` function will only succeed if no matching item
exists.

For more information about `put_item`, see [Working with
Items](https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithItems.html)
in the *Amazon DynamoDB Developer Guide*.

### Usage

    dynamodb_put_item(TableName, Item, Expected, ReturnValues,
      ReturnConsumedCapacity, ReturnItemCollectionMetrics,
      ConditionalOperator, ConditionExpression, ExpressionAttributeNames,
      ExpressionAttributeValues)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code id="dynamodb_put_item_:_TableName">TableName</code></td>
<td><p>[required] The name of the table to contain the item.</p></td>
</tr>
<tr class="even">
<td><code id="dynamodb_put_item_:_Item">Item</code></td>
<td><p>[required] A map of attribute name/value pairs, one for each
attribute. Only the primary key attributes are required; you can
optionally provide other attribute name-value pairs for the item.</p>
<p>You must provide all of the attributes for the primary key. For
example, with a simple primary key, you only need to provide a value for
the partition key. For a composite primary key, you must provide both
values for both the partition key and the sort key.</p>
<p>If you specify any attributes that are part of an index key, then the
data types for those attributes must match those of the schema in the
table's attribute definition.</p>
<p>Empty String and Binary attribute values are allowed. Attribute
values of type String and Binary must have a length greater than zero if
the attribute is used as a key attribute for a table or index.</p>
<p>For more information about primary keys, see <a
href="https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.CoreComponents.html#HowItWorks.CoreComponents.PrimaryKey">Primary
Key</a> in the <em>Amazon DynamoDB Developer Guide</em>.</p>
<p>Each element in the <code>Item</code> map is an
<code>AttributeValue</code> object.</p></td>
</tr>
<tr class="odd">
<td><code id="dynamodb_put_item_:_Expected">Expected</code></td>
<td><p>This is a legacy parameter. Use <code>ConditionExpression</code>
instead. For more information, see <a
href="https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.Expected.html">Expected</a>
in the <em>Amazon DynamoDB Developer Guide</em>.</p></td>
</tr>
<tr class="even">
<td><code id="dynamodb_put_item_:_ReturnValues">ReturnValues</code></td>
<td><p>Use <code>ReturnValues</code> if you want to get the item
attributes as they appeared before they were updated with the
<code>put_item</code> request. For <code>put_item</code>, the valid
values are:</p>
<ul>
<li><p><code>NONE</code> - If <code>ReturnValues</code> is not
specified, or if its value is <code>NONE</code>, then nothing is
returned. (This setting is the default for
<code>ReturnValues</code>.)</p></li>
<li><p><code>ALL_OLD</code> - If <code>put_item</code> overwrote an
attribute name-value pair, then the content of the old item is
returned.</p></li>
</ul>
<p>The values returned are strongly consistent.</p>
<p>There is no additional cost associated with requesting a return value
aside from the small network and processing overhead of receiving a
larger response. No read capacity units are consumed.</p>
<p>The <code>ReturnValues</code> parameter is used by several DynamoDB
operations; however, <code>put_item</code> does not recognize any values
other than <code>NONE</code> or <code>ALL_OLD</code>.</p></td>
</tr>
<tr class="odd">
<td><code
id="dynamodb_put_item_:_ReturnConsumedCapacity">ReturnConsumedCapacity</code></td>
<td></td>
</tr>
<tr class="even">
<td><code
id="dynamodb_put_item_:_ReturnItemCollectionMetrics">ReturnItemCollectionMetrics</code></td>
<td><p>Determines whether item collection metrics are returned. If set
to <code>SIZE</code>, the response includes statistics about item
collections, if any, that were modified during the operation are
returned in the response. If set to <code>NONE</code> (the default), no
statistics are returned.</p></td>
</tr>
<tr class="odd">
<td><code
id="dynamodb_put_item_:_ConditionalOperator">ConditionalOperator</code></td>
<td><p>This is a legacy parameter. Use <code>ConditionExpression</code>
instead. For more information, see <a
href="https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.ConditionalOperator.html">ConditionalOperator</a>
in the <em>Amazon DynamoDB Developer Guide</em>.</p></td>
</tr>
<tr class="even">
<td><code
id="dynamodb_put_item_:_ConditionExpression">ConditionExpression</code></td>
<td><p>A condition that must be satisfied in order for a conditional
<code>put_item</code> operation to succeed.</p>
<p>An expression can contain any of the following:</p>
<ul>
<li><p>Functions:
<code>attribute_exists | attribute_not_exists | attribute_type | contains | begins_with | size</code></p>
<p>These function names are case-sensitive.</p></li>
<li><p>Comparison operators: <code
style="white-space: pre;">⁠= | &lt;&gt; | &lt; | &gt; | &lt;= | &gt;= | BETWEEN | IN ⁠</code></p></li>
<li><p>Logical operators: <code>AND | OR | NOT</code></p></li>
</ul>
<p>For more information on condition expressions, see <a
href="https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.ConditionExpressions.html">Condition
Expressions</a> in the <em>Amazon DynamoDB Developer
Guide</em>.</p></td>
</tr>
<tr class="odd">
<td><code
id="dynamodb_put_item_:_ExpressionAttributeNames">ExpressionAttributeNames</code></td>
<td><p>One or more substitution tokens for attribute names in an
expression. The following are some use cases for using
<code>ExpressionAttributeNames</code>:</p>
<ul>
<li><p>To access an attribute whose name conflicts with a DynamoDB
reserved word.</p></li>
<li><p>To create a placeholder for repeating occurrences of an attribute
name in an expression.</p></li>
<li><p>To prevent special characters in an attribute name from being
misinterpreted in an expression.</p></li>
</ul>
<p>Use the <strong>#</strong> character in an expression to dereference
an attribute name. For example, consider the following attribute
name:</p>
<ul>
<li><p><code>Percentile</code></p></li>
</ul>
<p>The name of this attribute conflicts with a reserved word, so it
cannot be used directly in an expression. (For the complete list of
reserved words, see <a
href="https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ReservedWords.html">Reserved
Words</a> in the <em>Amazon DynamoDB Developer Guide</em>). To work
around this, you could specify the following for
<code>ExpressionAttributeNames</code>:</p>
<ul>
<li><p><code>{"#P":"Percentile"}</code></p></li>
</ul>
<p>You could then use this substitution in an expression, as in this
example:</p>
<ul>
<li><p><code style="white-space: pre;">⁠#P = :val⁠</code></p></li>
</ul>
<p>Tokens that begin with the <strong>:</strong> character are
<em>expression attribute values</em>, which are placeholders for the
actual value at runtime.</p>
<p>For more information on expression attribute names, see <a
href="https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.Attributes.html">Specifying
Item Attributes</a> in the <em>Amazon DynamoDB Developer
Guide</em>.</p></td>
</tr>
<tr class="even">
<td><code
id="dynamodb_put_item_:_ExpressionAttributeValues">ExpressionAttributeValues</code></td>
<td><p>One or more values that can be substituted in an expression.</p>
<p>Use the <strong>:</strong> (colon) character in an expression to
dereference an attribute value. For example, suppose that you wanted to
check whether the value of the <em>ProductStatus</em> attribute was one
of the following:</p>
<p><code>Available | Backordered | Discontinued</code></p>
<p>You would first need to specify
<code>ExpressionAttributeValues</code> as follows:</p>
<p><code
style="white-space: pre;">⁠{ ":avail":{"S":"Available"}, ":back":{"S":"Backordered"}, ":disc":{"S":"Discontinued"} }⁠</code></p>
<p>You could then use these values in an expression, such as this:</p>
<p><code
style="white-space: pre;">⁠ProductStatus IN (:avail, :back, :disc)⁠</code></p>
<p>For more information on expression attribute values, see <a
href="https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.ConditionExpressions.html">Condition
Expressions</a> in the <em>Amazon DynamoDB Developer
Guide</em>.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      Attributes = list(
        list(
          S = "string",
          N = "string",
          B = raw,
          SS = list(
            "string"
          ),
          NS = list(
            "string"
          ),
          BS = list(
            raw
          ),
          M = list(
            list()
          ),
          L = list(
            list()
          ),
          NULL = TRUE|FALSE,
          BOOL = TRUE|FALSE
        )
      ),
      ConsumedCapacity = list(
        TableName = "string",
        CapacityUnits = 123.0,
        ReadCapacityUnits = 123.0,
        WriteCapacityUnits = 123.0,
        Table = list(
          ReadCapacityUnits = 123.0,
          WriteCapacityUnits = 123.0,
          CapacityUnits = 123.0
        ),
        LocalSecondaryIndexes = list(
          list(
            ReadCapacityUnits = 123.0,
            WriteCapacityUnits = 123.0,
            CapacityUnits = 123.0
          )
        ),
        GlobalSecondaryIndexes = list(
          list(
            ReadCapacityUnits = 123.0,
            WriteCapacityUnits = 123.0,
            CapacityUnits = 123.0
          )
        )
      ),
      ItemCollectionMetrics = list(
        ItemCollectionKey = list(
          list(
            S = "string",
            N = "string",
            B = raw,
            SS = list(
              "string"
            ),
            NS = list(
              "string"
            ),
            BS = list(
              raw
            ),
            M = list(
              list()
            ),
            L = list(
              list()
            ),
            NULL = TRUE|FALSE,
            BOOL = TRUE|FALSE
          )
        ),
        SizeEstimateRangeGB = list(
          123.0
        )
      )
    )

### Request syntax

    svc$put_item(
      TableName = "string",
      Item = list(
        list(
          S = "string",
          N = "string",
          B = raw,
          SS = list(
            "string"
          ),
          NS = list(
            "string"
          ),
          BS = list(
            raw
          ),
          M = list(
            list()
          ),
          L = list(
            list()
          ),
          NULL = TRUE|FALSE,
          BOOL = TRUE|FALSE
        )
      ),
      Expected = list(
        list(
          Value = list(
            S = "string",
            N = "string",
            B = raw,
            SS = list(
              "string"
            ),
            NS = list(
              "string"
            ),
            BS = list(
              raw
            ),
            M = list(
              list()
            ),
            L = list(
              list()
            ),
            NULL = TRUE|FALSE,
            BOOL = TRUE|FALSE
          ),
          Exists = TRUE|FALSE,
          ComparisonOperator = "EQ"|"NE"|"IN"|"LE"|"LT"|"GE"|"GT"|"BETWEEN"|"NOT_NULL"|"NULL"|"CONTAINS"|"NOT_CONTAINS"|"BEGINS_WITH",
          AttributeValueList = list(
            list(
              S = "string",
              N = "string",
              B = raw,
              SS = list(
                "string"
              ),
              NS = list(
                "string"
              ),
              BS = list(
                raw
              ),
              M = list(
                list()
              ),
              L = list(
                list()
              ),
              NULL = TRUE|FALSE,
              BOOL = TRUE|FALSE
            )
          )
        )
      ),
      ReturnValues = "NONE"|"ALL_OLD"|"UPDATED_OLD"|"ALL_NEW"|"UPDATED_NEW",
      ReturnConsumedCapacity = "INDEXES"|"TOTAL"|"NONE",
      ReturnItemCollectionMetrics = "SIZE"|"NONE",
      ConditionalOperator = "AND"|"OR",
      ConditionExpression = "string",
      ExpressionAttributeNames = list(
        "string"
      ),
      ExpressionAttributeValues = list(
        list(
          S = "string",
          N = "string",
          B = raw,
          SS = list(
            "string"
          ),
          NS = list(
            "string"
          ),
          BS = list(
            raw
          ),
          M = list(
            list()
          ),
          L = list(
            list()
          ),
          NULL = TRUE|FALSE,
          BOOL = TRUE|FALSE
        )
      )
    )

### Examples

    ## Not run: 
    # This example adds a new item to the Music table.
    svc$put_item(
      Item = list(
        AlbumTitle = list(
          S = "Somewhat Famous"
        ),
        Artist = list(
          S = "No One You Know"
        ),
        SongTitle = list(
          S = "Call Me Today"
        )
      ),
      ReturnConsumedCapacity = "TOTAL",
      TableName = "Music"
    )

    ## End(Not run)