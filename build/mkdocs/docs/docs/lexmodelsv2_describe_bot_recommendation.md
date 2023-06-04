<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>lexmodelsv2_describe_bot_recommendation</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Provides metadata information about a bot recommendation

### Description

Provides metadata information about a bot recommendation. This
information will enable you to get a description on the request inputs,
to download associated transcripts after processing is complete, and to
download intents and slot-types generated by the bot recommendation.

### Usage

    lexmodelsv2_describe_bot_recommendation(botId, botVersion, localeId,
      botRecommendationId)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="lexmodelsv2_describe_bot_recommendation_:_botId">botId</code></td>
<td><p>[required] The unique identifier of the bot associated with the
bot recommendation.</p></td>
</tr>
<tr class="even">
<td><code
id="lexmodelsv2_describe_bot_recommendation_:_botVersion">botVersion</code></td>
<td><p>[required] The version of the bot associated with the bot
recommendation.</p></td>
</tr>
<tr class="odd">
<td><code
id="lexmodelsv2_describe_bot_recommendation_:_localeId">localeId</code></td>
<td><p>[required] The identifier of the language and locale of the bot
recommendation to describe. The string must match one of the supported
locales. For more information, see <a
href="https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html">Supported
languages</a>.</p></td>
</tr>
<tr class="even">
<td><code
id="lexmodelsv2_describe_bot_recommendation_:_botRecommendationId">botRecommendationId</code></td>
<td><p>[required] The identifier of the bot recommendation to
describe.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      botId = "string",
      botVersion = "string",
      localeId = "string",
      botRecommendationStatus = "Processing"|"Deleting"|"Deleted"|"Downloading"|"Updating"|"Available"|"Failed"|"Stopping"|"Stopped",
      botRecommendationId = "string",
      failureReasons = list(
        "string"
      ),
      creationDateTime = as.POSIXct(
        "2015-01-01"
      ),
      lastUpdatedDateTime = as.POSIXct(
        "2015-01-01"
      ),
      transcriptSourceSetting = list(
        s3BucketTranscriptSource = list(
          s3BucketName = "string",
          pathFormat = list(
            objectPrefixes = list(
              "string"
            )
          ),
          transcriptFormat = "Lex",
          transcriptFilter = list(
            lexTranscriptFilter = list(
              dateRangeFilter = list(
                startDateTime = as.POSIXct(
                  "2015-01-01"
                ),
                endDateTime = as.POSIXct(
                  "2015-01-01"
                )
              )
            )
          ),
          kmsKeyArn = "string"
        )
      ),
      encryptionSetting = list(
        kmsKeyArn = "string",
        botLocaleExportPassword = "string",
        associatedTranscriptsPassword = "string"
      ),
      botRecommendationResults = list(
        botLocaleExportUrl = "string",
        associatedTranscriptsUrl = "string",
        statistics = list(
          intents = list(
            discoveredIntentCount = 123
          ),
          slotTypes = list(
            discoveredSlotTypeCount = 123
          )
        )
      )
    )

### Request syntax

    svc$describe_bot_recommendation(
      botId = "string",
      botVersion = "string",
      localeId = "string",
      botRecommendationId = "string"
    )