<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>kinesis_put_record</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Writes a single data record into an Amazon Kinesis data stream

### Description

Writes a single data record into an Amazon Kinesis data stream. Call
`put_record` to send data into the stream for real-time ingestion and
subsequent processing, one record at a time. Each shard can support
writes up to 1,000 records per second, up to a maximum data write total
of 1 MiB per second.

When invoking this API, it is recommended you use the `StreamARN` input
parameter rather than the `StreamName` input parameter.

You must specify the name of the stream that captures, stores, and
transports the data; a partition key; and the data blob itself.

The data blob can be any type of data; for example, a segment from a log
file, geographic/location data, website clickstream data, and so on.

The partition key is used by Kinesis Data Streams to distribute data
across shards. Kinesis Data Streams segregates the data records that
belong to a stream into multiple shards, using the partition key
associated with each data record to determine the shard to which a given
data record belongs.

Partition keys are Unicode strings, with a maximum length limit of 256
characters for each key. An MD5 hash function is used to map partition
keys to 128-bit integer values and to map associated data records to
shards using the hash key ranges of the shards. You can override hashing
the partition key to determine the shard by explicitly specifying a hash
value using the `ExplicitHashKey` parameter. For more information, see
[Adding Data to a
Stream](https://docs.aws.amazon.com/streams/latest/dev/developing-producers-with-sdk.html#kinesis-using-sdk-java-add-data-to-stream)
in the *Amazon Kinesis Data Streams Developer Guide*.

`put_record` returns the shard ID of where the data record was placed
and the sequence number that was assigned to the data record.

Sequence numbers increase over time and are specific to a shard within a
stream, not across all shards within a stream. To guarantee strictly
increasing ordering, write serially to a shard and use the
`SequenceNumberForOrdering` parameter. For more information, see [Adding
Data to a
Stream](https://docs.aws.amazon.com/streams/latest/dev/developing-producers-with-sdk.html#kinesis-using-sdk-java-add-data-to-stream)
in the *Amazon Kinesis Data Streams Developer Guide*.

After you write a record to a stream, you cannot modify that record or
its order within the stream.

If a `put_record` request cannot be processed because of insufficient
provisioned throughput on the shard involved in the request,
`put_record` throws `ProvisionedThroughputExceededException`.

By default, data records are accessible for 24 hours from the time that
they are added to a stream. You can use
`increase_stream_retention_period` or `decrease_stream_retention_period`
to modify this retention period.

### Usage

    kinesis_put_record(StreamName, Data, PartitionKey, ExplicitHashKey,
      SequenceNumberForOrdering, StreamARN)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code id="kinesis_put_record_:_StreamName">StreamName</code></td>
<td><p>The name of the stream to put the data record into.</p></td>
</tr>
<tr class="even">
<td><code id="kinesis_put_record_:_Data">Data</code></td>
<td><p>[required] The data blob to put into the record, which is
base64-encoded when the blob is serialized. When the data blob (the
payload before base64-encoding) is added to the partition key size, the
total size must not exceed the maximum record size (1 MiB).</p></td>
</tr>
<tr class="odd">
<td><code
id="kinesis_put_record_:_PartitionKey">PartitionKey</code></td>
<td><p>[required] Determines which shard in the stream the data record
is assigned to. Partition keys are Unicode strings with a maximum length
limit of 256 characters for each key. Amazon Kinesis Data Streams uses
the partition key as input to a hash function that maps the partition
key and associated data to a specific shard. Specifically, an MD5 hash
function is used to map partition keys to 128-bit integer values and to
map associated data records to shards. As a result of this hashing
mechanism, all data records with the same partition key map to the same
shard within the stream.</p></td>
</tr>
<tr class="even">
<td><code
id="kinesis_put_record_:_ExplicitHashKey">ExplicitHashKey</code></td>
<td><p>The hash value used to explicitly determine the shard the data
record is assigned to by overriding the partition key hash.</p></td>
</tr>
<tr class="odd">
<td><code
id="kinesis_put_record_:_SequenceNumberForOrdering">SequenceNumberForOrdering</code></td>
<td><p>Guarantees strictly increasing sequence numbers, for puts from
the same client and to the same partition key. Usage: set the
<code>SequenceNumberForOrdering</code> of record <em>n</em> to the
sequence number of record <em>n-1</em> (as returned in the result when
putting record <em>n-1</em>). If this parameter is not set, records are
coarsely ordered based on arrival time.</p></td>
</tr>
<tr class="even">
<td><code id="kinesis_put_record_:_StreamARN">StreamARN</code></td>
<td><p>The ARN of the stream.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      ShardId = "string",
      SequenceNumber = "string",
      EncryptionType = "NONE"|"KMS"
    )

### Request syntax

    svc$put_record(
      StreamName = "string",
      Data = raw,
      PartitionKey = "string",
      ExplicitHashKey = "string",
      SequenceNumberForOrdering = "string",
      StreamARN = "string"
    )
