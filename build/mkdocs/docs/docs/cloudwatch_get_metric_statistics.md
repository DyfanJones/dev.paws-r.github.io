<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>cloudwatch_get_metric_statistics</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Gets statistics for the specified metric

### Description

Gets statistics for the specified metric.

The maximum number of data points returned from a single call is 1,440.
If you request more than 1,440 data points, CloudWatch returns an error.
To reduce the number of data points, you can narrow the specified time
range and make multiple requests across adjacent time ranges, or you can
increase the specified period. Data points are not returned in
chronological order.

CloudWatch aggregates data points based on the length of the period that
you specify. For example, if you request statistics with a one-hour
period, CloudWatch aggregates all data points with time stamps that fall
within each one-hour period. Therefore, the number of values aggregated
by CloudWatch is larger than the number of data points returned.

CloudWatch needs raw data points to calculate percentile statistics. If
you publish data using a statistic set instead, you can only retrieve
percentile statistics for this data if one of the following conditions
is true:

-   The SampleCount value of the statistic set is 1.

-   The Min and the Max values of the statistic set are equal.

Percentile statistics are not available for metrics when any of the
metric values are negative numbers.

Amazon CloudWatch retains metric data as follows:

-   Data points with a period of less than 60 seconds are available for
    3 hours. These data points are high-resolution metrics and are
    available only for custom metrics that have been defined with a
    `StorageResolution` of 1.

-   Data points with a period of 60 seconds (1-minute) are available for
    15 days.

-   Data points with a period of 300 seconds (5-minute) are available
    for 63 days.

-   Data points with a period of 3600 seconds (1 hour) are available for
    455 days (15 months).

Data points that are initially published with a shorter period are
aggregated together for long-term storage. For example, if you collect
data using a period of 1 minute, the data remains available for 15 days
with 1-minute resolution. After 15 days, this data is still available,
but is aggregated and retrievable only with a resolution of 5 minutes.
After 63 days, the data is further aggregated and is available with a
resolution of 1 hour.

CloudWatch started retaining 5-minute and 1-hour metric data as of July
9, 2016.

For information about metrics and dimensions supported by Amazon Web
Services services, see the [Amazon CloudWatch Metrics and Dimensions
Reference](https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/aws-services-cloudwatch-metrics.html)
in the *Amazon CloudWatch User Guide*.

### Usage

    cloudwatch_get_metric_statistics(Namespace, MetricName, Dimensions,
      StartTime, EndTime, Period, Statistics, ExtendedStatistics, Unit)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="cloudwatch_get_metric_statistics_:_Namespace">Namespace</code></td>
<td><p>[required] The namespace of the metric, with or without
spaces.</p></td>
</tr>
<tr class="even">
<td><code
id="cloudwatch_get_metric_statistics_:_MetricName">MetricName</code></td>
<td><p>[required] The name of the metric, with or without
spaces.</p></td>
</tr>
<tr class="odd">
<td><code
id="cloudwatch_get_metric_statistics_:_Dimensions">Dimensions</code></td>
<td><p>The dimensions. If the metric contains multiple dimensions, you
must include a value for each dimension. CloudWatch treats each unique
combination of dimensions as a separate metric. If a specific
combination of dimensions was not published, you can't retrieve
statistics for it. You must specify the same dimensions that were used
when the metrics were created. For an example, see <a
href="https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/cloudwatch_concepts.html#dimension-combinations">Dimension
Combinations</a> in the <em>Amazon CloudWatch User Guide</em>. For more
information about specifying dimensions, see <a
href="https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/publishingMetrics.html">Publishing
Metrics</a> in the <em>Amazon CloudWatch User Guide</em>.</p></td>
</tr>
<tr class="even">
<td><code
id="cloudwatch_get_metric_statistics_:_StartTime">StartTime</code></td>
<td><p>[required] The time stamp that determines the first data point to
return. Start times are evaluated relative to the time that CloudWatch
receives the request.</p>
<p>The value specified is inclusive; results include data points with
the specified time stamp. In a raw HTTP query, the time stamp must be in
ISO 8601 UTC format (for example, 2016-10-03T23:00:00Z).</p>
<p>CloudWatch rounds the specified time stamp as follows:</p>
<ul>
<li><p>Start time less than 15 days ago - Round down to the nearest
whole minute. For example, 12:32:34 is rounded down to
12:32:00.</p></li>
<li><p>Start time between 15 and 63 days ago - Round down to the nearest
5-minute clock interval. For example, 12:32:34 is rounded down to
12:30:00.</p></li>
<li><p>Start time greater than 63 days ago - Round down to the nearest
1-hour clock interval. For example, 12:32:34 is rounded down to
12:00:00.</p></li>
</ul>
<p>If you set <code>Period</code> to 5, 10, or 30, the start time of
your request is rounded down to the nearest time that corresponds to
even 5-, 10-, or 30-second divisions of a minute. For example, if you
make a query at (HH:mm:ss) 01:05:23 for the previous 10-second period,
the start time of your request is rounded down and you receive data from
01:05:10 to 01:05:20. If you make a query at 15:07:17 for the previous 5
minutes of data, using a period of 5 seconds, you receive data
timestamped between 15:02:15 and 15:07:15.</p></td>
</tr>
<tr class="odd">
<td><code
id="cloudwatch_get_metric_statistics_:_EndTime">EndTime</code></td>
<td><p>[required] The time stamp that determines the last data point to
return.</p>
<p>The value specified is exclusive; results include data points up to
the specified time stamp. In a raw HTTP query, the time stamp must be in
ISO 8601 UTC format (for example, 2016-10-10T23:00:00Z).</p></td>
</tr>
<tr class="even">
<td><code
id="cloudwatch_get_metric_statistics_:_Period">Period</code></td>
<td><p>[required] The granularity, in seconds, of the returned data
points. For metrics with regular resolution, a period can be as short as
one minute (60 seconds) and must be a multiple of 60. For
high-resolution metrics that are collected at intervals of less than one
minute, the period can be 1, 5, 10, 30, 60, or any multiple of 60.
High-resolution metrics are those metrics stored by a
<code>put_metric_data</code> call that includes a
<code>StorageResolution</code> of 1 second.</p>
<p>If the <code>StartTime</code> parameter specifies a time stamp that
is greater than 3 hours ago, you must specify the period as follows or
no data points in that time range is returned:</p>
<ul>
<li><p>Start time between 3 hours and 15 days ago - Use a multiple of 60
seconds (1 minute).</p></li>
<li><p>Start time between 15 and 63 days ago - Use a multiple of 300
seconds (5 minutes).</p></li>
<li><p>Start time greater than 63 days ago - Use a multiple of 3600
seconds (1 hour).</p></li>
</ul></td>
</tr>
<tr class="odd">
<td><code
id="cloudwatch_get_metric_statistics_:_Statistics">Statistics</code></td>
<td><p>The metric statistics, other than percentile. For percentile
statistics, use <code>ExtendedStatistics</code>. When calling
<code>get_metric_statistics</code>, you must specify either
<code>Statistics</code> or <code>ExtendedStatistics</code>, but not
both.</p></td>
</tr>
<tr class="even">
<td><code
id="cloudwatch_get_metric_statistics_:_ExtendedStatistics">ExtendedStatistics</code></td>
<td><p>The percentile statistics. Specify values between p0.0 and p100.
When calling <code>get_metric_statistics</code>, you must specify either
<code>Statistics</code> or <code>ExtendedStatistics</code>, but not
both. Percentile statistics are not available for metrics when any of
the metric values are negative numbers.</p></td>
</tr>
<tr class="odd">
<td><code id="cloudwatch_get_metric_statistics_:_Unit">Unit</code></td>
<td><p>The unit for a given metric. If you omit <code>Unit</code>, all
data that was collected with any unit is returned, along with the
corresponding units that were specified when the data was reported to
CloudWatch. If you specify a unit, the operation returns only data that
was collected with that unit specified. If you specify a unit that does
not match the data collected, the results of the operation are null.
CloudWatch does not perform unit conversions.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      Label = "string",
      Datapoints = list(
        list(
          Timestamp = as.POSIXct(
            "2015-01-01"
          ),
          SampleCount = 123.0,
          Average = 123.0,
          Sum = 123.0,
          Minimum = 123.0,
          Maximum = 123.0,
          Unit = "Seconds"|"Microseconds"|"Milliseconds"|"Bytes"|"Kilobytes"|"Megabytes"|"Gigabytes"|"Terabytes"|"Bits"|"Kilobits"|"Megabits"|"Gigabits"|"Terabits"|"Percent"|"Count"|"Bytes/Second"|"Kilobytes/Second"|"Megabytes/Second"|"Gigabytes/Second"|"Terabytes/Second"|"Bits/Second"|"Kilobits/Second"|"Megabits/Second"|"Gigabits/Second"|"Terabits/Second"|"Count/Second"|"None",
          ExtendedStatistics = list(
            123.0
          )
        )
      )
    )

### Request syntax

    svc$get_metric_statistics(
      Namespace = "string",
      MetricName = "string",
      Dimensions = list(
        list(
          Name = "string",
          Value = "string"
        )
      ),
      StartTime = as.POSIXct(
        "2015-01-01"
      ),
      EndTime = as.POSIXct(
        "2015-01-01"
      ),
      Period = 123,
      Statistics = list(
        "SampleCount"|"Average"|"Sum"|"Minimum"|"Maximum"
      ),
      ExtendedStatistics = list(
        "string"
      ),
      Unit = "Seconds"|"Microseconds"|"Milliseconds"|"Bytes"|"Kilobytes"|"Megabytes"|"Gigabytes"|"Terabytes"|"Bits"|"Kilobits"|"Megabits"|"Gigabits"|"Terabits"|"Percent"|"Count"|"Bytes/Second"|"Kilobytes/Second"|"Megabytes/Second"|"Gigabytes/Second"|"Terabytes/Second"|"Bits/Second"|"Kilobits/Second"|"Megabits/Second"|"Gigabits/Second"|"Terabits/Second"|"Count/Second"|"None"
    )