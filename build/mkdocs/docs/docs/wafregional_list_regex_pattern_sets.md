<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>wafregional_list_regex_pattern_sets</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## This is AWS WAF Classic documentation

### Description

This is **AWS WAF Classic** documentation. For more information, see
[AWS WAF
Classic](https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html)
in the developer guide.

**For the latest version of AWS WAF**, use the AWS WAFV2 API and see the
[AWS WAF Developer
Guide](https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html).
With the latest version, AWS WAF has a single set of endpoints for
regional and global use.

Returns an array of RegexPatternSetSummary objects.

### Usage

    wafregional_list_regex_pattern_sets(NextMarker, Limit)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="wafregional_list_regex_pattern_sets_:_NextMarker">NextMarker</code></td>
<td><p>If you specify a value for <code>Limit</code> and you have more
<code>RegexPatternSet</code> objects than the value of
<code>Limit</code>, AWS WAF returns a <code>NextMarker</code> value in
the response that allows you to list another group of
<code>RegexPatternSet</code> objects. For the second and subsequent
<code>list_regex_pattern_sets</code> requests, specify the value of
<code>NextMarker</code> from the previous response to get information
about another batch of <code>RegexPatternSet</code> objects.</p></td>
</tr>
<tr class="even">
<td><code
id="wafregional_list_regex_pattern_sets_:_Limit">Limit</code></td>
<td><p>Specifies the number of <code>RegexPatternSet</code> objects that
you want AWS WAF to return for this request. If you have more
<code>RegexPatternSet</code> objects than the number you specify for
<code>Limit</code>, the response includes a <code>NextMarker</code>
value that you can use to get another batch of
<code>RegexPatternSet</code> objects.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      NextMarker = "string",
      RegexPatternSets = list(
        list(
          RegexPatternSetId = "string",
          Name = "string"
        )
      )
    )

### Request syntax

    svc$list_regex_pattern_sets(
      NextMarker = "string",
      Limit = 123
    )