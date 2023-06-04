<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>clouddirectory_batch_read</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Performs all the read operations in a batch

### Description

Performs all the read operations in a batch.

### Usage

    clouddirectory_batch_read(DirectoryArn, Operations, ConsistencyLevel)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="clouddirectory_batch_read_:_DirectoryArn">DirectoryArn</code></td>
<td><p>[required] The Amazon Resource Name (ARN) that is associated with
the Directory. For more information, see arns.</p></td>
</tr>
<tr class="even">
<td><code
id="clouddirectory_batch_read_:_Operations">Operations</code></td>
<td><p>[required] A list of operations that are part of the
batch.</p></td>
</tr>
<tr class="odd">
<td><code
id="clouddirectory_batch_read_:_ConsistencyLevel">ConsistencyLevel</code></td>
<td><p>Represents the manner and timing in which the successful write or
update of an object is reflected in a subsequent read operation of that
same object.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      Responses = list(
        list(
          SuccessfulResponse = list(
            ListObjectAttributes = list(
              Attributes = list(
                list(
                  Key = list(
                    SchemaArn = "string",
                    FacetName = "string",
                    Name = "string"
                  ),
                  Value = list(
                    StringValue = "string",
                    BinaryValue = raw,
                    BooleanValue = TRUE|FALSE,
                    NumberValue = "string",
                    DatetimeValue = as.POSIXct(
                      "2015-01-01"
                    )
                  )
                )
              ),
              NextToken = "string"
            ),
            ListObjectChildren = list(
              Children = list(
                "string"
              ),
              NextToken = "string"
            ),
            GetObjectInformation = list(
              SchemaFacets = list(
                list(
                  SchemaArn = "string",
                  FacetName = "string"
                )
              ),
              ObjectIdentifier = "string"
            ),
            GetObjectAttributes = list(
              Attributes = list(
                list(
                  Key = list(
                    SchemaArn = "string",
                    FacetName = "string",
                    Name = "string"
                  ),
                  Value = list(
                    StringValue = "string",
                    BinaryValue = raw,
                    BooleanValue = TRUE|FALSE,
                    NumberValue = "string",
                    DatetimeValue = as.POSIXct(
                      "2015-01-01"
                    )
                  )
                )
              )
            ),
            ListAttachedIndices = list(
              IndexAttachments = list(
                list(
                  IndexedAttributes = list(
                    list(
                      Key = list(
                        SchemaArn = "string",
                        FacetName = "string",
                        Name = "string"
                      ),
                      Value = list(
                        StringValue = "string",
                        BinaryValue = raw,
                        BooleanValue = TRUE|FALSE,
                        NumberValue = "string",
                        DatetimeValue = as.POSIXct(
                          "2015-01-01"
                        )
                      )
                    )
                  ),
                  ObjectIdentifier = "string"
                )
              ),
              NextToken = "string"
            ),
            ListObjectParentPaths = list(
              PathToObjectIdentifiersList = list(
                list(
                  Path = "string",
                  ObjectIdentifiers = list(
                    "string"
                  )
                )
              ),
              NextToken = "string"
            ),
            ListObjectPolicies = list(
              AttachedPolicyIds = list(
                "string"
              ),
              NextToken = "string"
            ),
            ListPolicyAttachments = list(
              ObjectIdentifiers = list(
                "string"
              ),
              NextToken = "string"
            ),
            LookupPolicy = list(
              PolicyToPathList = list(
                list(
                  Path = "string",
                  Policies = list(
                    list(
                      PolicyId = "string",
                      ObjectIdentifier = "string",
                      PolicyType = "string"
                    )
                  )
                )
              ),
              NextToken = "string"
            ),
            ListIndex = list(
              IndexAttachments = list(
                list(
                  IndexedAttributes = list(
                    list(
                      Key = list(
                        SchemaArn = "string",
                        FacetName = "string",
                        Name = "string"
                      ),
                      Value = list(
                        StringValue = "string",
                        BinaryValue = raw,
                        BooleanValue = TRUE|FALSE,
                        NumberValue = "string",
                        DatetimeValue = as.POSIXct(
                          "2015-01-01"
                        )
                      )
                    )
                  ),
                  ObjectIdentifier = "string"
                )
              ),
              NextToken = "string"
            ),
            ListOutgoingTypedLinks = list(
              TypedLinkSpecifiers = list(
                list(
                  TypedLinkFacet = list(
                    SchemaArn = "string",
                    TypedLinkName = "string"
                  ),
                  SourceObjectReference = list(
                    Selector = "string"
                  ),
                  TargetObjectReference = list(
                    Selector = "string"
                  ),
                  IdentityAttributeValues = list(
                    list(
                      AttributeName = "string",
                      Value = list(
                        StringValue = "string",
                        BinaryValue = raw,
                        BooleanValue = TRUE|FALSE,
                        NumberValue = "string",
                        DatetimeValue = as.POSIXct(
                          "2015-01-01"
                        )
                      )
                    )
                  )
                )
              ),
              NextToken = "string"
            ),
            ListIncomingTypedLinks = list(
              LinkSpecifiers = list(
                list(
                  TypedLinkFacet = list(
                    SchemaArn = "string",
                    TypedLinkName = "string"
                  ),
                  SourceObjectReference = list(
                    Selector = "string"
                  ),
                  TargetObjectReference = list(
                    Selector = "string"
                  ),
                  IdentityAttributeValues = list(
                    list(
                      AttributeName = "string",
                      Value = list(
                        StringValue = "string",
                        BinaryValue = raw,
                        BooleanValue = TRUE|FALSE,
                        NumberValue = "string",
                        DatetimeValue = as.POSIXct(
                          "2015-01-01"
                        )
                      )
                    )
                  )
                )
              ),
              NextToken = "string"
            ),
            GetLinkAttributes = list(
              Attributes = list(
                list(
                  Key = list(
                    SchemaArn = "string",
                    FacetName = "string",
                    Name = "string"
                  ),
                  Value = list(
                    StringValue = "string",
                    BinaryValue = raw,
                    BooleanValue = TRUE|FALSE,
                    NumberValue = "string",
                    DatetimeValue = as.POSIXct(
                      "2015-01-01"
                    )
                  )
                )
              )
            ),
            ListObjectParents = list(
              ParentLinks = list(
                list(
                  ObjectIdentifier = "string",
                  LinkName = "string"
                )
              ),
              NextToken = "string"
            )
          ),
          ExceptionResponse = list(
            Type = "ValidationException"|"InvalidArnException"|"ResourceNotFoundException"|"InvalidNextTokenException"|"AccessDeniedException"|"NotNodeException"|"FacetValidationException"|"CannotListParentOfRootException"|"NotIndexException"|"NotPolicyException"|"DirectoryNotEnabledException"|"LimitExceededException"|"InternalServiceException",
            Message = "string"
          )
        )
      )
    )

### Request syntax

    svc$batch_read(
      DirectoryArn = "string",
      Operations = list(
        list(
          ListObjectAttributes = list(
            ObjectReference = list(
              Selector = "string"
            ),
            NextToken = "string",
            MaxResults = 123,
            FacetFilter = list(
              SchemaArn = "string",
              FacetName = "string"
            )
          ),
          ListObjectChildren = list(
            ObjectReference = list(
              Selector = "string"
            ),
            NextToken = "string",
            MaxResults = 123
          ),
          ListAttachedIndices = list(
            TargetReference = list(
              Selector = "string"
            ),
            NextToken = "string",
            MaxResults = 123
          ),
          ListObjectParentPaths = list(
            ObjectReference = list(
              Selector = "string"
            ),
            NextToken = "string",
            MaxResults = 123
          ),
          GetObjectInformation = list(
            ObjectReference = list(
              Selector = "string"
            )
          ),
          GetObjectAttributes = list(
            ObjectReference = list(
              Selector = "string"
            ),
            SchemaFacet = list(
              SchemaArn = "string",
              FacetName = "string"
            ),
            AttributeNames = list(
              "string"
            )
          ),
          ListObjectParents = list(
            ObjectReference = list(
              Selector = "string"
            ),
            NextToken = "string",
            MaxResults = 123
          ),
          ListObjectPolicies = list(
            ObjectReference = list(
              Selector = "string"
            ),
            NextToken = "string",
            MaxResults = 123
          ),
          ListPolicyAttachments = list(
            PolicyReference = list(
              Selector = "string"
            ),
            NextToken = "string",
            MaxResults = 123
          ),
          LookupPolicy = list(
            ObjectReference = list(
              Selector = "string"
            ),
            NextToken = "string",
            MaxResults = 123
          ),
          ListIndex = list(
            RangesOnIndexedValues = list(
              list(
                AttributeKey = list(
                  SchemaArn = "string",
                  FacetName = "string",
                  Name = "string"
                ),
                Range = list(
                  StartMode = "FIRST"|"LAST"|"LAST_BEFORE_MISSING_VALUES"|"INCLUSIVE"|"EXCLUSIVE",
                  StartValue = list(
                    StringValue = "string",
                    BinaryValue = raw,
                    BooleanValue = TRUE|FALSE,
                    NumberValue = "string",
                    DatetimeValue = as.POSIXct(
                      "2015-01-01"
                    )
                  ),
                  EndMode = "FIRST"|"LAST"|"LAST_BEFORE_MISSING_VALUES"|"INCLUSIVE"|"EXCLUSIVE",
                  EndValue = list(
                    StringValue = "string",
                    BinaryValue = raw,
                    BooleanValue = TRUE|FALSE,
                    NumberValue = "string",
                    DatetimeValue = as.POSIXct(
                      "2015-01-01"
                    )
                  )
                )
              )
            ),
            IndexReference = list(
              Selector = "string"
            ),
            MaxResults = 123,
            NextToken = "string"
          ),
          ListOutgoingTypedLinks = list(
            ObjectReference = list(
              Selector = "string"
            ),
            FilterAttributeRanges = list(
              list(
                AttributeName = "string",
                Range = list(
                  StartMode = "FIRST"|"LAST"|"LAST_BEFORE_MISSING_VALUES"|"INCLUSIVE"|"EXCLUSIVE",
                  StartValue = list(
                    StringValue = "string",
                    BinaryValue = raw,
                    BooleanValue = TRUE|FALSE,
                    NumberValue = "string",
                    DatetimeValue = as.POSIXct(
                      "2015-01-01"
                    )
                  ),
                  EndMode = "FIRST"|"LAST"|"LAST_BEFORE_MISSING_VALUES"|"INCLUSIVE"|"EXCLUSIVE",
                  EndValue = list(
                    StringValue = "string",
                    BinaryValue = raw,
                    BooleanValue = TRUE|FALSE,
                    NumberValue = "string",
                    DatetimeValue = as.POSIXct(
                      "2015-01-01"
                    )
                  )
                )
              )
            ),
            FilterTypedLink = list(
              SchemaArn = "string",
              TypedLinkName = "string"
            ),
            NextToken = "string",
            MaxResults = 123
          ),
          ListIncomingTypedLinks = list(
            ObjectReference = list(
              Selector = "string"
            ),
            FilterAttributeRanges = list(
              list(
                AttributeName = "string",
                Range = list(
                  StartMode = "FIRST"|"LAST"|"LAST_BEFORE_MISSING_VALUES"|"INCLUSIVE"|"EXCLUSIVE",
                  StartValue = list(
                    StringValue = "string",
                    BinaryValue = raw,
                    BooleanValue = TRUE|FALSE,
                    NumberValue = "string",
                    DatetimeValue = as.POSIXct(
                      "2015-01-01"
                    )
                  ),
                  EndMode = "FIRST"|"LAST"|"LAST_BEFORE_MISSING_VALUES"|"INCLUSIVE"|"EXCLUSIVE",
                  EndValue = list(
                    StringValue = "string",
                    BinaryValue = raw,
                    BooleanValue = TRUE|FALSE,
                    NumberValue = "string",
                    DatetimeValue = as.POSIXct(
                      "2015-01-01"
                    )
                  )
                )
              )
            ),
            FilterTypedLink = list(
              SchemaArn = "string",
              TypedLinkName = "string"
            ),
            NextToken = "string",
            MaxResults = 123
          ),
          GetLinkAttributes = list(
            TypedLinkSpecifier = list(
              TypedLinkFacet = list(
                SchemaArn = "string",
                TypedLinkName = "string"
              ),
              SourceObjectReference = list(
                Selector = "string"
              ),
              TargetObjectReference = list(
                Selector = "string"
              ),
              IdentityAttributeValues = list(
                list(
                  AttributeName = "string",
                  Value = list(
                    StringValue = "string",
                    BinaryValue = raw,
                    BooleanValue = TRUE|FALSE,
                    NumberValue = "string",
                    DatetimeValue = as.POSIXct(
                      "2015-01-01"
                    )
                  )
                )
              )
            ),
            AttributeNames = list(
              "string"
            )
          )
        )
      ),
      ConsistencyLevel = "SERIALIZABLE"|"EVENTUAL"
    )