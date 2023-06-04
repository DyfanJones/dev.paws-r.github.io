<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>lightsail_get_distribution_metric_data</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Returns the data points of a specific metric for an Amazon Lightsail content delivery network (CDN) distribution

### Description

Returns the data points of a specific metric for an Amazon Lightsail
content delivery network (CDN) distribution.

Metrics report the utilization of your resources, and the error counts
generated by them. Monitor and collect metric data regularly to maintain
the reliability, availability, and performance of your resources.

### Usage

    lightsail_get_distribution_metric_data(distributionName, metricName,
      startTime, endTime, period, unit, statistics)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="lightsail_get_distribution_metric_data_:_distributionName">distributionName</code></td>
<td><p>[required] The name of the distribution for which to get metric
data.</p>
<p>Use the <code>get_distributions</code> action to get a list of
distribution names that you can specify.</p></td>
</tr>
<tr class="even">
<td><code
id="lightsail_get_distribution_metric_data_:_metricName">metricName</code></td>
<td><p>[required] The metric for which you want to return
information.</p>
<p>Valid distribution metric names are listed below, along with the most
useful <code>statistics</code> to include in your request, and the
published <code>unit</code> value.</p>
<ul>
<li><p><strong><code>Requests</code></strong> - The total number of
viewer requests received by your Lightsail distribution, for all HTTP
methods, and for both HTTP and HTTPS requests.</p>
<p><code>Statistics</code>: The most useful statistic is
<code>Sum</code>.</p>
<p><code>Unit</code>: The published unit is <code>None</code>.</p></li>
<li><p><strong><code>BytesDownloaded</code></strong> - The number of
bytes downloaded by viewers for GET, HEAD, and OPTIONS requests.</p>
<p><code>Statistics</code>: The most useful statistic is
<code>Sum</code>.</p>
<p><code>Unit</code>: The published unit is <code>None</code>.</p></li>
<li><p><strong><code>BytesUploaded </code></strong> - The number of
bytes uploaded to your origin by your Lightsail distribution, using POST
and PUT requests.</p>
<p><code>Statistics</code>: The most useful statistic is
<code>Sum</code>.</p>
<p><code>Unit</code>: The published unit is <code>None</code>.</p></li>
<li><p><strong><code>TotalErrorRate</code></strong> - The percentage of
all viewer requests for which the response's HTTP status code was 4xx or
5xx.</p>
<p><code>Statistics</code>: The most useful statistic is
<code>Average</code>.</p>
<p><code>Unit</code>: The published unit is
<code>Percent</code>.</p></li>
<li><p><strong><code
style="white-space: pre;">⁠4xxErrorRate⁠</code></strong> - The percentage
of all viewer requests for which the response's HTTP status cod was 4xx.
In these cases, the client or client viewer may have made an error. For
example, a status code of 404 (Not Found) means that the client
requested an object that could not be found.</p>
<p><code>Statistics</code>: The most useful statistic is
<code>Average</code>.</p>
<p><code>Unit</code>: The published unit is
<code>Percent</code>.</p></li>
<li><p><strong><code
style="white-space: pre;">⁠5xxErrorRate⁠</code></strong> - The percentage
of all viewer requests for which the response's HTTP status code was
5xx. In these cases, the origin server did not satisfy the requests. For
example, a status code of 503 (Service Unavailable) means that the
origin server is currently unavailable.</p>
<p><code>Statistics</code>: The most useful statistic is
<code>Average</code>.</p>
<p><code>Unit</code>: The published unit is
<code>Percent</code>.</p></li>
</ul></td>
</tr>
<tr class="odd">
<td><code
id="lightsail_get_distribution_metric_data_:_startTime">startTime</code></td>
<td><p>[required] The start of the time interval for which to get metric
data.</p>
<p>Constraints:</p>
<ul>
<li><p>Specified in Coordinated Universal Time (UTC).</p></li>
<li><p>Specified in the Unix time format.</p>
<p>For example, if you wish to use a start time of October 1, 2018, at 8
PM UTC, specify <code>1538424000</code> as the start time.</p></li>
</ul>
<p>You can convert a human-friendly time to Unix time format using a
converter like <a href="https://www.epochconverter.com/">Epoch
converter</a>.</p></td>
</tr>
<tr class="even">
<td><code
id="lightsail_get_distribution_metric_data_:_endTime">endTime</code></td>
<td><p>[required] The end of the time interval for which to get metric
data.</p>
<p>Constraints:</p>
<ul>
<li><p>Specified in Coordinated Universal Time (UTC).</p></li>
<li><p>Specified in the Unix time format.</p>
<p>For example, if you wish to use an end time of October 1, 2018, at 9
PM UTC, specify <code>1538427600</code> as the end time.</p></li>
</ul>
<p>You can convert a human-friendly time to Unix time format using a
converter like <a href="https://www.epochconverter.com/">Epoch
converter</a>.</p></td>
</tr>
<tr class="odd">
<td><code
id="lightsail_get_distribution_metric_data_:_period">period</code></td>
<td><p>[required] The granularity, in seconds, for the metric data
points that will be returned.</p></td>
</tr>
<tr class="even">
<td><code
id="lightsail_get_distribution_metric_data_:_unit">unit</code></td>
<td><p>[required] The unit for the metric data request.</p>
<p>Valid units depend on the metric data being requested. For the valid
units with each available metric, see the <code>metricName</code>
parameter.</p></td>
</tr>
<tr class="odd">
<td><code
id="lightsail_get_distribution_metric_data_:_statistics">statistics</code></td>
<td><p>[required] The statistic for the metric.</p>
<p>The following statistics are available:</p>
<ul>
<li><p><code>Minimum</code> - The lowest value observed during the
specified period. Use this value to determine low volumes of activity
for your application.</p></li>
<li><p><code>Maximum</code> - The highest value observed during the
specified period. Use this value to determine high volumes of activity
for your application.</p></li>
<li><p><code>Sum</code> - All values submitted for the matching metric
added together. You can use this statistic to determine the total volume
of a metric.</p></li>
<li><p><code>Average</code> - The value of Sum / SampleCount during the
specified period. By comparing this statistic with the Minimum and
Maximum values, you can determine the full scope of a metric and how
close the average use is to the Minimum and Maximum values. This
comparison helps you to know when to increase or decrease your
resources.</p></li>
<li><p><code>SampleCount</code> - The count, or number, of data points
used for the statistical calculation.</p></li>
</ul></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      metricName = "Requests"|"BytesDownloaded"|"BytesUploaded"|"TotalErrorRate"|"Http4xxErrorRate"|"Http5xxErrorRate",
      metricData = list(
        list(
          average = 123.0,
          maximum = 123.0,
          minimum = 123.0,
          sampleCount = 123.0,
          sum = 123.0,
          timestamp = as.POSIXct(
            "2015-01-01"
          ),
          unit = "Seconds"|"Microseconds"|"Milliseconds"|"Bytes"|"Kilobytes"|"Megabytes"|"Gigabytes"|"Terabytes"|"Bits"|"Kilobits"|"Megabits"|"Gigabits"|"Terabits"|"Percent"|"Count"|"Bytes/Second"|"Kilobytes/Second"|"Megabytes/Second"|"Gigabytes/Second"|"Terabytes/Second"|"Bits/Second"|"Kilobits/Second"|"Megabits/Second"|"Gigabits/Second"|"Terabits/Second"|"Count/Second"|"None"
        )
      )
    )

### Request syntax

    svc$get_distribution_metric_data(
      distributionName = "string",
      metricName = "Requests"|"BytesDownloaded"|"BytesUploaded"|"TotalErrorRate"|"Http4xxErrorRate"|"Http5xxErrorRate",
      startTime = as.POSIXct(
        "2015-01-01"
      ),
      endTime = as.POSIXct(
        "2015-01-01"
      ),
      period = 123,
      unit = "Seconds"|"Microseconds"|"Milliseconds"|"Bytes"|"Kilobytes"|"Megabytes"|"Gigabytes"|"Terabytes"|"Bits"|"Kilobits"|"Megabits"|"Gigabits"|"Terabits"|"Percent"|"Count"|"Bytes/Second"|"Kilobytes/Second"|"Megabytes/Second"|"Gigabytes/Second"|"Terabytes/Second"|"Bits/Second"|"Kilobits/Second"|"Megabits/Second"|"Gigabits/Second"|"Terabits/Second"|"Count/Second"|"None",
      statistics = list(
        "Minimum"|"Maximum"|"Sum"|"Average"|"SampleCount"
      )
    )