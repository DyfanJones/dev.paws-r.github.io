<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>lexmodelsv2_update_slot</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Updates the settings for a slot

### Description

Updates the settings for a slot.

### Usage

    lexmodelsv2_update_slot(slotId, slotName, description, slotTypeId,
      valueElicitationSetting, obfuscationSetting, botId, botVersion,
      localeId, intentId, multipleValuesSetting, subSlotSetting)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code id="lexmodelsv2_update_slot_:_slotId">slotId</code></td>
<td><p>[required] The unique identifier for the slot to update.</p></td>
</tr>
<tr class="even">
<td><code id="lexmodelsv2_update_slot_:_slotName">slotName</code></td>
<td><p>[required] The new name for the slot.</p></td>
</tr>
<tr class="odd">
<td><code
id="lexmodelsv2_update_slot_:_description">description</code></td>
<td><p>The new description for the slot.</p></td>
</tr>
<tr class="even">
<td><code
id="lexmodelsv2_update_slot_:_slotTypeId">slotTypeId</code></td>
<td><p>The unique identifier of the new slot type to associate with this
slot.</p></td>
</tr>
<tr class="odd">
<td><code
id="lexmodelsv2_update_slot_:_valueElicitationSetting">valueElicitationSetting</code></td>
<td><p>[required] A new set of prompts that Amazon Lex sends to the user
to elicit a response the provides a value for the slot.</p></td>
</tr>
<tr class="even">
<td><code
id="lexmodelsv2_update_slot_:_obfuscationSetting">obfuscationSetting</code></td>
<td><p>New settings that determine how slot values are formatted in
Amazon CloudWatch logs.</p></td>
</tr>
<tr class="odd">
<td><code id="lexmodelsv2_update_slot_:_botId">botId</code></td>
<td><p>[required] The unique identifier of the bot that contains the
slot.</p></td>
</tr>
<tr class="even">
<td><code
id="lexmodelsv2_update_slot_:_botVersion">botVersion</code></td>
<td><p>[required] The version of the bot that contains the slot. Must
always be <code>DRAFT</code>.</p></td>
</tr>
<tr class="odd">
<td><code id="lexmodelsv2_update_slot_:_localeId">localeId</code></td>
<td><p>[required] The identifier of the language and locale that
contains the slot. The string must match one of the supported locales.
For more information, see <a
href="https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html">Supported
languages</a>.</p></td>
</tr>
<tr class="even">
<td><code id="lexmodelsv2_update_slot_:_intentId">intentId</code></td>
<td><p>[required] The identifier of the intent that contains the
slot.</p></td>
</tr>
<tr class="odd">
<td><code
id="lexmodelsv2_update_slot_:_multipleValuesSetting">multipleValuesSetting</code></td>
<td><p>Determines whether the slot accepts multiple values in one
response. Multiple value slots are only available in the en-US locale.
If you set this value to <code>true</code> in any other locale, Amazon
Lex throws a <code>ValidationException</code>.</p>
<p>If the <code>multipleValuesSetting</code> is not set, the default
value is <code>false</code>.</p></td>
</tr>
<tr class="even">
<td><code
id="lexmodelsv2_update_slot_:_subSlotSetting">subSlotSetting</code></td>
<td><p>Specifications for the constituent sub slots and the expression
for the composite slot.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      slotId = "string",
      slotName = "string",
      description = "string",
      slotTypeId = "string",
      valueElicitationSetting = list(
        defaultValueSpecification = list(
          defaultValueList = list(
            list(
              defaultValue = "string"
            )
          )
        ),
        slotConstraint = "Required"|"Optional",
        promptSpecification = list(
          messageGroups = list(
            list(
              message = list(
                plainTextMessage = list(
                  value = "string"
                ),
                customPayload = list(
                  value = "string"
                ),
                ssmlMessage = list(
                  value = "string"
                ),
                imageResponseCard = list(
                  title = "string",
                  subtitle = "string",
                  imageUrl = "string",
                  buttons = list(
                    list(
                      text = "string",
                      value = "string"
                    )
                  )
                )
              ),
              variations = list(
                list(
                  plainTextMessage = list(
                    value = "string"
                  ),
                  customPayload = list(
                    value = "string"
                  ),
                  ssmlMessage = list(
                    value = "string"
                  ),
                  imageResponseCard = list(
                    title = "string",
                    subtitle = "string",
                    imageUrl = "string",
                    buttons = list(
                      list(
                        text = "string",
                        value = "string"
                      )
                    )
                  )
                )
              )
            )
          ),
          maxRetries = 123,
          allowInterrupt = TRUE|FALSE,
          messageSelectionStrategy = "Random"|"Ordered",
          promptAttemptsSpecification = list(
            list(
              allowInterrupt = TRUE|FALSE,
              allowedInputTypes = list(
                allowAudioInput = TRUE|FALSE,
                allowDTMFInput = TRUE|FALSE
              ),
              audioAndDTMFInputSpecification = list(
                startTimeoutMs = 123,
                audioSpecification = list(
                  maxLengthMs = 123,
                  endTimeoutMs = 123
                ),
                dtmfSpecification = list(
                  maxLength = 123,
                  endTimeoutMs = 123,
                  deletionCharacter = "string",
                  endCharacter = "string"
                )
              ),
              textInputSpecification = list(
                startTimeoutMs = 123
              )
            )
          )
        ),
        sampleUtterances = list(
          list(
            utterance = "string"
          )
        ),
        waitAndContinueSpecification = list(
          waitingResponse = list(
            messageGroups = list(
              list(
                message = list(
                  plainTextMessage = list(
                    value = "string"
                  ),
                  customPayload = list(
                    value = "string"
                  ),
                  ssmlMessage = list(
                    value = "string"
                  ),
                  imageResponseCard = list(
                    title = "string",
                    subtitle = "string",
                    imageUrl = "string",
                    buttons = list(
                      list(
                        text = "string",
                        value = "string"
                      )
                    )
                  )
                ),
                variations = list(
                  list(
                    plainTextMessage = list(
                      value = "string"
                    ),
                    customPayload = list(
                      value = "string"
                    ),
                    ssmlMessage = list(
                      value = "string"
                    ),
                    imageResponseCard = list(
                      title = "string",
                      subtitle = "string",
                      imageUrl = "string",
                      buttons = list(
                        list(
                          text = "string",
                          value = "string"
                        )
                      )
                    )
                  )
                )
              )
            ),
            allowInterrupt = TRUE|FALSE
          ),
          continueResponse = list(
            messageGroups = list(
              list(
                message = list(
                  plainTextMessage = list(
                    value = "string"
                  ),
                  customPayload = list(
                    value = "string"
                  ),
                  ssmlMessage = list(
                    value = "string"
                  ),
                  imageResponseCard = list(
                    title = "string",
                    subtitle = "string",
                    imageUrl = "string",
                    buttons = list(
                      list(
                        text = "string",
                        value = "string"
                      )
                    )
                  )
                ),
                variations = list(
                  list(
                    plainTextMessage = list(
                      value = "string"
                    ),
                    customPayload = list(
                      value = "string"
                    ),
                    ssmlMessage = list(
                      value = "string"
                    ),
                    imageResponseCard = list(
                      title = "string",
                      subtitle = "string",
                      imageUrl = "string",
                      buttons = list(
                        list(
                          text = "string",
                          value = "string"
                        )
                      )
                    )
                  )
                )
              )
            ),
            allowInterrupt = TRUE|FALSE
          ),
          stillWaitingResponse = list(
            messageGroups = list(
              list(
                message = list(
                  plainTextMessage = list(
                    value = "string"
                  ),
                  customPayload = list(
                    value = "string"
                  ),
                  ssmlMessage = list(
                    value = "string"
                  ),
                  imageResponseCard = list(
                    title = "string",
                    subtitle = "string",
                    imageUrl = "string",
                    buttons = list(
                      list(
                        text = "string",
                        value = "string"
                      )
                    )
                  )
                ),
                variations = list(
                  list(
                    plainTextMessage = list(
                      value = "string"
                    ),
                    customPayload = list(
                      value = "string"
                    ),
                    ssmlMessage = list(
                      value = "string"
                    ),
                    imageResponseCard = list(
                      title = "string",
                      subtitle = "string",
                      imageUrl = "string",
                      buttons = list(
                        list(
                          text = "string",
                          value = "string"
                        )
                      )
                    )
                  )
                )
              )
            ),
            frequencyInSeconds = 123,
            timeoutInSeconds = 123,
            allowInterrupt = TRUE|FALSE
          ),
          active = TRUE|FALSE
        ),
        slotCaptureSetting = list(
          captureResponse = list(
            messageGroups = list(
              list(
                message = list(
                  plainTextMessage = list(
                    value = "string"
                  ),
                  customPayload = list(
                    value = "string"
                  ),
                  ssmlMessage = list(
                    value = "string"
                  ),
                  imageResponseCard = list(
                    title = "string",
                    subtitle = "string",
                    imageUrl = "string",
                    buttons = list(
                      list(
                        text = "string",
                        value = "string"
                      )
                    )
                  )
                ),
                variations = list(
                  list(
                    plainTextMessage = list(
                      value = "string"
                    ),
                    customPayload = list(
                      value = "string"
                    ),
                    ssmlMessage = list(
                      value = "string"
                    ),
                    imageResponseCard = list(
                      title = "string",
                      subtitle = "string",
                      imageUrl = "string",
                      buttons = list(
                        list(
                          text = "string",
                          value = "string"
                        )
                      )
                    )
                  )
                )
              )
            ),
            allowInterrupt = TRUE|FALSE
          ),
          captureNextStep = list(
            dialogAction = list(
              type = "ElicitIntent"|"StartIntent"|"ElicitSlot"|"EvaluateConditional"|"InvokeDialogCodeHook"|"ConfirmIntent"|"FulfillIntent"|"CloseIntent"|"EndConversation",
              slotToElicit = "string",
              suppressNextMessage = TRUE|FALSE
            ),
            intent = list(
              name = "string",
              slots = list(
                list(
                  shape = "Scalar"|"List",
                  value = list(
                    interpretedValue = "string"
                  ),
                  values = list(
                    list()
                  )
                )
              )
            ),
            sessionAttributes = list(
              "string"
            )
          ),
          captureConditional = list(
            active = TRUE|FALSE,
            conditionalBranches = list(
              list(
                name = "string",
                condition = list(
                  expressionString = "string"
                ),
                nextStep = list(
                  dialogAction = list(
                    type = "ElicitIntent"|"StartIntent"|"ElicitSlot"|"EvaluateConditional"|"InvokeDialogCodeHook"|"ConfirmIntent"|"FulfillIntent"|"CloseIntent"|"EndConversation",
                    slotToElicit = "string",
                    suppressNextMessage = TRUE|FALSE
                  ),
                  intent = list(
                    name = "string",
                    slots = list(
                      list(
                        shape = "Scalar"|"List",
                        value = list(
                          interpretedValue = "string"
                        ),
                        values = list(
                          list()
                        )
                      )
                    )
                  ),
                  sessionAttributes = list(
                    "string"
                  )
                ),
                response = list(
                  messageGroups = list(
                    list(
                      message = list(
                        plainTextMessage = list(
                          value = "string"
                        ),
                        customPayload = list(
                          value = "string"
                        ),
                        ssmlMessage = list(
                          value = "string"
                        ),
                        imageResponseCard = list(
                          title = "string",
                          subtitle = "string",
                          imageUrl = "string",
                          buttons = list(
                            list(
                              text = "string",
                              value = "string"
                            )
                          )
                        )
                      ),
                      variations = list(
                        list(
                          plainTextMessage = list(
                            value = "string"
                          ),
                          customPayload = list(
                            value = "string"
                          ),
                          ssmlMessage = list(
                            value = "string"
                          ),
                          imageResponseCard = list(
                            title = "string",
                            subtitle = "string",
                            imageUrl = "string",
                            buttons = list(
                              list(
                                text = "string",
                                value = "string"
                              )
                            )
                          )
                        )
                      )
                    )
                  ),
                  allowInterrupt = TRUE|FALSE
                )
              )
            ),
            defaultBranch = list(
              nextStep = list(
                dialogAction = list(
                  type = "ElicitIntent"|"StartIntent"|"ElicitSlot"|"EvaluateConditional"|"InvokeDialogCodeHook"|"ConfirmIntent"|"FulfillIntent"|"CloseIntent"|"EndConversation",
                  slotToElicit = "string",
                  suppressNextMessage = TRUE|FALSE
                ),
                intent = list(
                  name = "string",
                  slots = list(
                    list(
                      shape = "Scalar"|"List",
                      value = list(
                        interpretedValue = "string"
                      ),
                      values = list(
                        list()
                      )
                    )
                  )
                ),
                sessionAttributes = list(
                  "string"
                )
              ),
              response = list(
                messageGroups = list(
                  list(
                    message = list(
                      plainTextMessage = list(
                        value = "string"
                      ),
                      customPayload = list(
                        value = "string"
                      ),
                      ssmlMessage = list(
                        value = "string"
                      ),
                      imageResponseCard = list(
                        title = "string",
                        subtitle = "string",
                        imageUrl = "string",
                        buttons = list(
                          list(
                            text = "string",
                            value = "string"
                          )
                        )
                      )
                    ),
                    variations = list(
                      list(
                        plainTextMessage = list(
                          value = "string"
                        ),
                        customPayload = list(
                          value = "string"
                        ),
                        ssmlMessage = list(
                          value = "string"
                        ),
                        imageResponseCard = list(
                          title = "string",
                          subtitle = "string",
                          imageUrl = "string",
                          buttons = list(
                            list(
                              text = "string",
                              value = "string"
                            )
                          )
                        )
                      )
                    )
                  )
                ),
                allowInterrupt = TRUE|FALSE
              )
            )
          ),
          failureResponse = list(
            messageGroups = list(
              list(
                message = list(
                  plainTextMessage = list(
                    value = "string"
                  ),
                  customPayload = list(
                    value = "string"
                  ),
                  ssmlMessage = list(
                    value = "string"
                  ),
                  imageResponseCard = list(
                    title = "string",
                    subtitle = "string",
                    imageUrl = "string",
                    buttons = list(
                      list(
                        text = "string",
                        value = "string"
                      )
                    )
                  )
                ),
                variations = list(
                  list(
                    plainTextMessage = list(
                      value = "string"
                    ),
                    customPayload = list(
                      value = "string"
                    ),
                    ssmlMessage = list(
                      value = "string"
                    ),
                    imageResponseCard = list(
                      title = "string",
                      subtitle = "string",
                      imageUrl = "string",
                      buttons = list(
                        list(
                          text = "string",
                          value = "string"
                        )
                      )
                    )
                  )
                )
              )
            ),
            allowInterrupt = TRUE|FALSE
          ),
          failureNextStep = list(
            dialogAction = list(
              type = "ElicitIntent"|"StartIntent"|"ElicitSlot"|"EvaluateConditional"|"InvokeDialogCodeHook"|"ConfirmIntent"|"FulfillIntent"|"CloseIntent"|"EndConversation",
              slotToElicit = "string",
              suppressNextMessage = TRUE|FALSE
            ),
            intent = list(
              name = "string",
              slots = list(
                list(
                  shape = "Scalar"|"List",
                  value = list(
                    interpretedValue = "string"
                  ),
                  values = list(
                    list()
                  )
                )
              )
            ),
            sessionAttributes = list(
              "string"
            )
          ),
          failureConditional = list(
            active = TRUE|FALSE,
            conditionalBranches = list(
              list(
                name = "string",
                condition = list(
                  expressionString = "string"
                ),
                nextStep = list(
                  dialogAction = list(
                    type = "ElicitIntent"|"StartIntent"|"ElicitSlot"|"EvaluateConditional"|"InvokeDialogCodeHook"|"ConfirmIntent"|"FulfillIntent"|"CloseIntent"|"EndConversation",
                    slotToElicit = "string",
                    suppressNextMessage = TRUE|FALSE
                  ),
                  intent = list(
                    name = "string",
                    slots = list(
                      list(
                        shape = "Scalar"|"List",
                        value = list(
                          interpretedValue = "string"
                        ),
                        values = list(
                          list()
                        )
                      )
                    )
                  ),
                  sessionAttributes = list(
                    "string"
                  )
                ),
                response = list(
                  messageGroups = list(
                    list(
                      message = list(
                        plainTextMessage = list(
                          value = "string"
                        ),
                        customPayload = list(
                          value = "string"
                        ),
                        ssmlMessage = list(
                          value = "string"
                        ),
                        imageResponseCard = list(
                          title = "string",
                          subtitle = "string",
                          imageUrl = "string",
                          buttons = list(
                            list(
                              text = "string",
                              value = "string"
                            )
                          )
                        )
                      ),
                      variations = list(
                        list(
                          plainTextMessage = list(
                            value = "string"
                          ),
                          customPayload = list(
                            value = "string"
                          ),
                          ssmlMessage = list(
                            value = "string"
                          ),
                          imageResponseCard = list(
                            title = "string",
                            subtitle = "string",
                            imageUrl = "string",
                            buttons = list(
                              list(
                                text = "string",
                                value = "string"
                              )
                            )
                          )
                        )
                      )
                    )
                  ),
                  allowInterrupt = TRUE|FALSE
                )
              )
            ),
            defaultBranch = list(
              nextStep = list(
                dialogAction = list(
                  type = "ElicitIntent"|"StartIntent"|"ElicitSlot"|"EvaluateConditional"|"InvokeDialogCodeHook"|"ConfirmIntent"|"FulfillIntent"|"CloseIntent"|"EndConversation",
                  slotToElicit = "string",
                  suppressNextMessage = TRUE|FALSE
                ),
                intent = list(
                  name = "string",
                  slots = list(
                    list(
                      shape = "Scalar"|"List",
                      value = list(
                        interpretedValue = "string"
                      ),
                      values = list(
                        list()
                      )
                    )
                  )
                ),
                sessionAttributes = list(
                  "string"
                )
              ),
              response = list(
                messageGroups = list(
                  list(
                    message = list(
                      plainTextMessage = list(
                        value = "string"
                      ),
                      customPayload = list(
                        value = "string"
                      ),
                      ssmlMessage = list(
                        value = "string"
                      ),
                      imageResponseCard = list(
                        title = "string",
                        subtitle = "string",
                        imageUrl = "string",
                        buttons = list(
                          list(
                            text = "string",
                            value = "string"
                          )
                        )
                      )
                    ),
                    variations = list(
                      list(
                        plainTextMessage = list(
                          value = "string"
                        ),
                        customPayload = list(
                          value = "string"
                        ),
                        ssmlMessage = list(
                          value = "string"
                        ),
                        imageResponseCard = list(
                          title = "string",
                          subtitle = "string",
                          imageUrl = "string",
                          buttons = list(
                            list(
                              text = "string",
                              value = "string"
                            )
                          )
                        )
                      )
                    )
                  )
                ),
                allowInterrupt = TRUE|FALSE
              )
            )
          ),
          codeHook = list(
            enableCodeHookInvocation = TRUE|FALSE,
            active = TRUE|FALSE,
            invocationLabel = "string",
            postCodeHookSpecification = list(
              successResponse = list(
                messageGroups = list(
                  list(
                    message = list(
                      plainTextMessage = list(
                        value = "string"
                      ),
                      customPayload = list(
                        value = "string"
                      ),
                      ssmlMessage = list(
                        value = "string"
                      ),
                      imageResponseCard = list(
                        title = "string",
                        subtitle = "string",
                        imageUrl = "string",
                        buttons = list(
                          list(
                            text = "string",
                            value = "string"
                          )
                        )
                      )
                    ),
                    variations = list(
                      list(
                        plainTextMessage = list(
                          value = "string"
                        ),
                        customPayload = list(
                          value = "string"
                        ),
                        ssmlMessage = list(
                          value = "string"
                        ),
                        imageResponseCard = list(
                          title = "string",
                          subtitle = "string",
                          imageUrl = "string",
                          buttons = list(
                            list(
                              text = "string",
                              value = "string"
                            )
                          )
                        )
                      )
                    )
                  )
                ),
                allowInterrupt = TRUE|FALSE
              ),
              successNextStep = list(
                dialogAction = list(
                  type = "ElicitIntent"|"StartIntent"|"ElicitSlot"|"EvaluateConditional"|"InvokeDialogCodeHook"|"ConfirmIntent"|"FulfillIntent"|"CloseIntent"|"EndConversation",
                  slotToElicit = "string",
                  suppressNextMessage = TRUE|FALSE
                ),
                intent = list(
                  name = "string",
                  slots = list(
                    list(
                      shape = "Scalar"|"List",
                      value = list(
                        interpretedValue = "string"
                      ),
                      values = list(
                        list()
                      )
                    )
                  )
                ),
                sessionAttributes = list(
                  "string"
                )
              ),
              successConditional = list(
                active = TRUE|FALSE,
                conditionalBranches = list(
                  list(
                    name = "string",
                    condition = list(
                      expressionString = "string"
                    ),
                    nextStep = list(
                      dialogAction = list(
                        type = "ElicitIntent"|"StartIntent"|"ElicitSlot"|"EvaluateConditional"|"InvokeDialogCodeHook"|"ConfirmIntent"|"FulfillIntent"|"CloseIntent"|"EndConversation",
                        slotToElicit = "string",
                        suppressNextMessage = TRUE|FALSE
                      ),
                      intent = list(
                        name = "string",
                        slots = list(
                          list(
                            shape = "Scalar"|"List",
                            value = list(
                              interpretedValue = "string"
                            ),
                            values = list(
                              list()
                            )
                          )
                        )
                      ),
                      sessionAttributes = list(
                        "string"
                      )
                    ),
                    response = list(
                      messageGroups = list(
                        list(
                          message = list(
                            plainTextMessage = list(
                              value = "string"
                            ),
                            customPayload = list(
                              value = "string"
                            ),
                            ssmlMessage = list(
                              value = "string"
                            ),
                            imageResponseCard = list(
                              title = "string",
                              subtitle = "string",
                              imageUrl = "string",
                              buttons = list(
                                list(
                                  text = "string",
                                  value = "string"
                                )
                              )
                            )
                          ),
                          variations = list(
                            list(
                              plainTextMessage = list(
                                value = "string"
                              ),
                              customPayload = list(
                                value = "string"
                              ),
                              ssmlMessage = list(
                                value = "string"
                              ),
                              imageResponseCard = list(
                                title = "string",
                                subtitle = "string",
                                imageUrl = "string",
                                buttons = list(
                                  list(
                                    text = "string",
                                    value = "string"
                                  )
                                )
                              )
                            )
                          )
                        )
                      ),
                      allowInterrupt = TRUE|FALSE
                    )
                  )
                ),
                defaultBranch = list(
                  nextStep = list(
                    dialogAction = list(
                      type = "ElicitIntent"|"StartIntent"|"ElicitSlot"|"EvaluateConditional"|"InvokeDialogCodeHook"|"ConfirmIntent"|"FulfillIntent"|"CloseIntent"|"EndConversation",
                      slotToElicit = "string",
                      suppressNextMessage = TRUE|FALSE
                    ),
                    intent = list(
                      name = "string",
                      slots = list(
                        list(
                          shape = "Scalar"|"List",
                          value = list(
                            interpretedValue = "string"
                          ),
                          values = list(
                            list()
                          )
                        )
                      )
                    ),
                    sessionAttributes = list(
                      "string"
                    )
                  ),
                  response = list(
                    messageGroups = list(
                      list(
                        message = list(
                          plainTextMessage = list(
                            value = "string"
                          ),
                          customPayload = list(
                            value = "string"
                          ),
                          ssmlMessage = list(
                            value = "string"
                          ),
                          imageResponseCard = list(
                            title = "string",
                            subtitle = "string",
                            imageUrl = "string",
                            buttons = list(
                              list(
                                text = "string",
                                value = "string"
                              )
                            )
                          )
                        ),
                        variations = list(
                          list(
                            plainTextMessage = list(
                              value = "string"
                            ),
                            customPayload = list(
                              value = "string"
                            ),
                            ssmlMessage = list(
                              value = "string"
                            ),
                            imageResponseCard = list(
                              title = "string",
                              subtitle = "string",
                              imageUrl = "string",
                              buttons = list(
                                list(
                                  text = "string",
                                  value = "string"
                                )
                              )
                            )
                          )
                        )
                      )
                    ),
                    allowInterrupt = TRUE|FALSE
                  )
                )
              ),
              failureResponse = list(
                messageGroups = list(
                  list(
                    message = list(
                      plainTextMessage = list(
                        value = "string"
                      ),
                      customPayload = list(
                        value = "string"
                      ),
                      ssmlMessage = list(
                        value = "string"
                      ),
                      imageResponseCard = list(
                        title = "string",
                        subtitle = "string",
                        imageUrl = "string",
                        buttons = list(
                          list(
                            text = "string",
                            value = "string"
                          )
                        )
                      )
                    ),
                    variations = list(
                      list(
                        plainTextMessage = list(
                          value = "string"
                        ),
                        customPayload = list(
                          value = "string"
                        ),
                        ssmlMessage = list(
                          value = "string"
                        ),
                        imageResponseCard = list(
                          title = "string",
                          subtitle = "string",
                          imageUrl = "string",
                          buttons = list(
                            list(
                              text = "string",
                              value = "string"
                            )
                          )
                        )
                      )
                    )
                  )
                ),
                allowInterrupt = TRUE|FALSE
              ),
              failureNextStep = list(
                dialogAction = list(
                  type = "ElicitIntent"|"StartIntent"|"ElicitSlot"|"EvaluateConditional"|"InvokeDialogCodeHook"|"ConfirmIntent"|"FulfillIntent"|"CloseIntent"|"EndConversation",
                  slotToElicit = "string",
                  suppressNextMessage = TRUE|FALSE
                ),
                intent = list(
                  name = "string",
                  slots = list(
                    list(
                      shape = "Scalar"|"List",
                      value = list(
                        interpretedValue = "string"
                      ),
                      values = list(
                        list()
                      )
                    )
                  )
                ),
                sessionAttributes = list(
                  "string"
                )
              ),
              failureConditional = list(
                active = TRUE|FALSE,
                conditionalBranches = list(
                  list(
                    name = "string",
                    condition = list(
                      expressionString = "string"
                    ),
                    nextStep = list(
                      dialogAction = list(
                        type = "ElicitIntent"|"StartIntent"|"ElicitSlot"|"EvaluateConditional"|"InvokeDialogCodeHook"|"ConfirmIntent"|"FulfillIntent"|"CloseIntent"|"EndConversation",
                        slotToElicit = "string",
                        suppressNextMessage = TRUE|FALSE
                      ),
                      intent = list(
                        name = "string",
                        slots = list(
                          list(
                            shape = "Scalar"|"List",
                            value = list(
                              interpretedValue = "string"
                            ),
                            values = list(
                              list()
                            )
                          )
                        )
                      ),
                      sessionAttributes = list(
                        "string"
                      )
                    ),
                    response = list(
                      messageGroups = list(
                        list(
                          message = list(
                            plainTextMessage = list(
                              value = "string"
                            ),
                            customPayload = list(
                              value = "string"
                            ),
                            ssmlMessage = list(
                              value = "string"
                            ),
                            imageResponseCard = list(
                              title = "string",
                              subtitle = "string",
                              imageUrl = "string",
                              buttons = list(
                                list(
                                  text = "string",
                                  value = "string"
                                )
                              )
                            )
                          ),
                          variations = list(
                            list(
                              plainTextMessage = list(
                                value = "string"
                              ),
                              customPayload = list(
                                value = "string"
                              ),
                              ssmlMessage = list(
                                value = "string"
                              ),
                              imageResponseCard = list(
                                title = "string",
                                subtitle = "string",
                                imageUrl = "string",
                                buttons = list(
                                  list(
                                    text = "string",
                                    value = "string"
                                  )
                                )
                              )
                            )
                          )
                        )
                      ),
                      allowInterrupt = TRUE|FALSE
                    )
                  )
                ),
                defaultBranch = list(
                  nextStep = list(
                    dialogAction = list(
                      type = "ElicitIntent"|"StartIntent"|"ElicitSlot"|"EvaluateConditional"|"InvokeDialogCodeHook"|"ConfirmIntent"|"FulfillIntent"|"CloseIntent"|"EndConversation",
                      slotToElicit = "string",
                      suppressNextMessage = TRUE|FALSE
                    ),
                    intent = list(
                      name = "string",
                      slots = list(
                        list(
                          shape = "Scalar"|"List",
                          value = list(
                            interpretedValue = "string"
                          ),
                          values = list(
                            list()
                          )
                        )
                      )
                    ),
                    sessionAttributes = list(
                      "string"
                    )
                  ),
                  response = list(
                    messageGroups = list(
                      list(
                        message = list(
                          plainTextMessage = list(
                            value = "string"
                          ),
                          customPayload = list(
                            value = "string"
                          ),
                          ssmlMessage = list(
                            value = "string"
                          ),
                          imageResponseCard = list(
                            title = "string",
                            subtitle = "string",
                            imageUrl = "string",
                            buttons = list(
                              list(
                                text = "string",
                                value = "string"
                              )
                            )
                          )
                        ),
                        variations = list(
                          list(
                            plainTextMessage = list(
                              value = "string"
                            ),
                            customPayload = list(
                              value = "string"
                            ),
                            ssmlMessage = list(
                              value = "string"
                            ),
                            imageResponseCard = list(
                              title = "string",
                              subtitle = "string",
                              imageUrl = "string",
                              buttons = list(
                                list(
                                  text = "string",
                                  value = "string"
                                )
                              )
                            )
                          )
                        )
                      )
                    ),
                    allowInterrupt = TRUE|FALSE
                  )
                )
              ),
              timeoutResponse = list(
                messageGroups = list(
                  list(
                    message = list(
                      plainTextMessage = list(
                        value = "string"
                      ),
                      customPayload = list(
                        value = "string"
                      ),
                      ssmlMessage = list(
                        value = "string"
                      ),
                      imageResponseCard = list(
                        title = "string",
                        subtitle = "string",
                        imageUrl = "string",
                        buttons = list(
                          list(
                            text = "string",
                            value = "string"
                          )
                        )
                      )
                    ),
                    variations = list(
                      list(
                        plainTextMessage = list(
                          value = "string"
                        ),
                        customPayload = list(
                          value = "string"
                        ),
                        ssmlMessage = list(
                          value = "string"
                        ),
                        imageResponseCard = list(
                          title = "string",
                          subtitle = "string",
                          imageUrl = "string",
                          buttons = list(
                            list(
                              text = "string",
                              value = "string"
                            )
                          )
                        )
                      )
                    )
                  )
                ),
                allowInterrupt = TRUE|FALSE
              ),
              timeoutNextStep = list(
                dialogAction = list(
                  type = "ElicitIntent"|"StartIntent"|"ElicitSlot"|"EvaluateConditional"|"InvokeDialogCodeHook"|"ConfirmIntent"|"FulfillIntent"|"CloseIntent"|"EndConversation",
                  slotToElicit = "string",
                  suppressNextMessage = TRUE|FALSE
                ),
                intent = list(
                  name = "string",
                  slots = list(
                    list(
                      shape = "Scalar"|"List",
                      value = list(
                        interpretedValue = "string"
                      ),
                      values = list(
                        list()
                      )
                    )
                  )
                ),
                sessionAttributes = list(
                  "string"
                )
              ),
              timeoutConditional = list(
                active = TRUE|FALSE,
                conditionalBranches = list(
                  list(
                    name = "string",
                    condition = list(
                      expressionString = "string"
                    ),
                    nextStep = list(
                      dialogAction = list(
                        type = "ElicitIntent"|"StartIntent"|"ElicitSlot"|"EvaluateConditional"|"InvokeDialogCodeHook"|"ConfirmIntent"|"FulfillIntent"|"CloseIntent"|"EndConversation",
                        slotToElicit = "string",
                        suppressNextMessage = TRUE|FALSE
                      ),
                      intent = list(
                        name = "string",
                        slots = list(
                          list(
                            shape = "Scalar"|"List",
                            value = list(
                              interpretedValue = "string"
                            ),
                            values = list(
                              list()
                            )
                          )
                        )
                      ),
                      sessionAttributes = list(
                        "string"
                      )
                    ),
                    response = list(
                      messageGroups = list(
                        list(
                          message = list(
                            plainTextMessage = list(
                              value = "string"
                            ),
                            customPayload = list(
                              value = "string"
                            ),
                            ssmlMessage = list(
                              value = "string"
                            ),
                            imageResponseCard = list(
                              title = "string",
                              subtitle = "string",
                              imageUrl = "string",
                              buttons = list(
                                list(
                                  text = "string",
                                  value = "string"
                                )
                              )
                            )
                          ),
                          variations = list(
                            list(
                              plainTextMessage = list(
                                value = "string"
                              ),
                              customPayload = list(
                                value = "string"
                              ),
                              ssmlMessage = list(
                                value = "string"
                              ),
                              imageResponseCard = list(
                                title = "string",
                                subtitle = "string",
                                imageUrl = "string",
                                buttons = list(
                                  list(
                                    text = "string",
                                    value = "string"
                                  )
                                )
                              )
                            )
                          )
                        )
                      ),
                      allowInterrupt = TRUE|FALSE
                    )
                  )
                ),
                defaultBranch = list(
                  nextStep = list(
                    dialogAction = list(
                      type = "ElicitIntent"|"StartIntent"|"ElicitSlot"|"EvaluateConditional"|"InvokeDialogCodeHook"|"ConfirmIntent"|"FulfillIntent"|"CloseIntent"|"EndConversation",
                      slotToElicit = "string",
                      suppressNextMessage = TRUE|FALSE
                    ),
                    intent = list(
                      name = "string",
                      slots = list(
                        list(
                          shape = "Scalar"|"List",
                          value = list(
                            interpretedValue = "string"
                          ),
                          values = list(
                            list()
                          )
                        )
                      )
                    ),
                    sessionAttributes = list(
                      "string"
                    )
                  ),
                  response = list(
                    messageGroups = list(
                      list(
                        message = list(
                          plainTextMessage = list(
                            value = "string"
                          ),
                          customPayload = list(
                            value = "string"
                          ),
                          ssmlMessage = list(
                            value = "string"
                          ),
                          imageResponseCard = list(
                            title = "string",
                            subtitle = "string",
                            imageUrl = "string",
                            buttons = list(
                              list(
                                text = "string",
                                value = "string"
                              )
                            )
                          )
                        ),
                        variations = list(
                          list(
                            plainTextMessage = list(
                              value = "string"
                            ),
                            customPayload = list(
                              value = "string"
                            ),
                            ssmlMessage = list(
                              value = "string"
                            ),
                            imageResponseCard = list(
                              title = "string",
                              subtitle = "string",
                              imageUrl = "string",
                              buttons = list(
                                list(
                                  text = "string",
                                  value = "string"
                                )
                              )
                            )
                          )
                        )
                      )
                    ),
                    allowInterrupt = TRUE|FALSE
                  )
                )
              )
            )
          ),
          elicitationCodeHook = list(
            enableCodeHookInvocation = TRUE|FALSE,
            invocationLabel = "string"
          )
        )
      ),
      obfuscationSetting = list(
        obfuscationSettingType = "None"|"DefaultObfuscation"
      ),
      botId = "string",
      botVersion = "string",
      localeId = "string",
      intentId = "string",
      creationDateTime = as.POSIXct(
        "2015-01-01"
      ),
      lastUpdatedDateTime = as.POSIXct(
        "2015-01-01"
      ),
      multipleValuesSetting = list(
        allowMultipleValues = TRUE|FALSE
      ),
      subSlotSetting = list(
        expression = "string",
        slotSpecifications = list(
          list(
            slotTypeId = "string",
            valueElicitationSetting = list(
              defaultValueSpecification = list(
                defaultValueList = list(
                  list(
                    defaultValue = "string"
                  )
                )
              ),
              promptSpecification = list(
                messageGroups = list(
                  list(
                    message = list(
                      plainTextMessage = list(
                        value = "string"
                      ),
                      customPayload = list(
                        value = "string"
                      ),
                      ssmlMessage = list(
                        value = "string"
                      ),
                      imageResponseCard = list(
                        title = "string",
                        subtitle = "string",
                        imageUrl = "string",
                        buttons = list(
                          list(
                            text = "string",
                            value = "string"
                          )
                        )
                      )
                    ),
                    variations = list(
                      list(
                        plainTextMessage = list(
                          value = "string"
                        ),
                        customPayload = list(
                          value = "string"
                        ),
                        ssmlMessage = list(
                          value = "string"
                        ),
                        imageResponseCard = list(
                          title = "string",
                          subtitle = "string",
                          imageUrl = "string",
                          buttons = list(
                            list(
                              text = "string",
                              value = "string"
                            )
                          )
                        )
                      )
                    )
                  )
                ),
                maxRetries = 123,
                allowInterrupt = TRUE|FALSE,
                messageSelectionStrategy = "Random"|"Ordered",
                promptAttemptsSpecification = list(
                  list(
                    allowInterrupt = TRUE|FALSE,
                    allowedInputTypes = list(
                      allowAudioInput = TRUE|FALSE,
                      allowDTMFInput = TRUE|FALSE
                    ),
                    audioAndDTMFInputSpecification = list(
                      startTimeoutMs = 123,
                      audioSpecification = list(
                        maxLengthMs = 123,
                        endTimeoutMs = 123
                      ),
                      dtmfSpecification = list(
                        maxLength = 123,
                        endTimeoutMs = 123,
                        deletionCharacter = "string",
                        endCharacter = "string"
                      )
                    ),
                    textInputSpecification = list(
                      startTimeoutMs = 123
                    )
                  )
                )
              ),
              sampleUtterances = list(
                list(
                  utterance = "string"
                )
              ),
              waitAndContinueSpecification = list(
                waitingResponse = list(
                  messageGroups = list(
                    list(
                      message = list(
                        plainTextMessage = list(
                          value = "string"
                        ),
                        customPayload = list(
                          value = "string"
                        ),
                        ssmlMessage = list(
                          value = "string"
                        ),
                        imageResponseCard = list(
                          title = "string",
                          subtitle = "string",
                          imageUrl = "string",
                          buttons = list(
                            list(
                              text = "string",
                              value = "string"
                            )
                          )
                        )
                      ),
                      variations = list(
                        list(
                          plainTextMessage = list(
                            value = "string"
                          ),
                          customPayload = list(
                            value = "string"
                          ),
                          ssmlMessage = list(
                            value = "string"
                          ),
                          imageResponseCard = list(
                            title = "string",
                            subtitle = "string",
                            imageUrl = "string",
                            buttons = list(
                              list(
                                text = "string",
                                value = "string"
                              )
                            )
                          )
                        )
                      )
                    )
                  ),
                  allowInterrupt = TRUE|FALSE
                ),
                continueResponse = list(
                  messageGroups = list(
                    list(
                      message = list(
                        plainTextMessage = list(
                          value = "string"
                        ),
                        customPayload = list(
                          value = "string"
                        ),
                        ssmlMessage = list(
                          value = "string"
                        ),
                        imageResponseCard = list(
                          title = "string",
                          subtitle = "string",
                          imageUrl = "string",
                          buttons = list(
                            list(
                              text = "string",
                              value = "string"
                            )
                          )
                        )
                      ),
                      variations = list(
                        list(
                          plainTextMessage = list(
                            value = "string"
                          ),
                          customPayload = list(
                            value = "string"
                          ),
                          ssmlMessage = list(
                            value = "string"
                          ),
                          imageResponseCard = list(
                            title = "string",
                            subtitle = "string",
                            imageUrl = "string",
                            buttons = list(
                              list(
                                text = "string",
                                value = "string"
                              )
                            )
                          )
                        )
                      )
                    )
                  ),
                  allowInterrupt = TRUE|FALSE
                ),
                stillWaitingResponse = list(
                  messageGroups = list(
                    list(
                      message = list(
                        plainTextMessage = list(
                          value = "string"
                        ),
                        customPayload = list(
                          value = "string"
                        ),
                        ssmlMessage = list(
                          value = "string"
                        ),
                        imageResponseCard = list(
                          title = "string",
                          subtitle = "string",
                          imageUrl = "string",
                          buttons = list(
                            list(
                              text = "string",
                              value = "string"
                            )
                          )
                        )
                      ),
                      variations = list(
                        list(
                          plainTextMessage = list(
                            value = "string"
                          ),
                          customPayload = list(
                            value = "string"
                          ),
                          ssmlMessage = list(
                            value = "string"
                          ),
                          imageResponseCard = list(
                            title = "string",
                            subtitle = "string",
                            imageUrl = "string",
                            buttons = list(
                              list(
                                text = "string",
                                value = "string"
                              )
                            )
                          )
                        )
                      )
                    )
                  ),
                  frequencyInSeconds = 123,
                  timeoutInSeconds = 123,
                  allowInterrupt = TRUE|FALSE
                ),
                active = TRUE|FALSE
              )
            )
          )
        )
      )
    )

### Request syntax

    svc$update_slot(
      slotId = "string",
      slotName = "string",
      description = "string",
      slotTypeId = "string",
      valueElicitationSetting = list(
        defaultValueSpecification = list(
          defaultValueList = list(
            list(
              defaultValue = "string"
            )
          )
        ),
        slotConstraint = "Required"|"Optional",
        promptSpecification = list(
          messageGroups = list(
            list(
              message = list(
                plainTextMessage = list(
                  value = "string"
                ),
                customPayload = list(
                  value = "string"
                ),
                ssmlMessage = list(
                  value = "string"
                ),
                imageResponseCard = list(
                  title = "string",
                  subtitle = "string",
                  imageUrl = "string",
                  buttons = list(
                    list(
                      text = "string",
                      value = "string"
                    )
                  )
                )
              ),
              variations = list(
                list(
                  plainTextMessage = list(
                    value = "string"
                  ),
                  customPayload = list(
                    value = "string"
                  ),
                  ssmlMessage = list(
                    value = "string"
                  ),
                  imageResponseCard = list(
                    title = "string",
                    subtitle = "string",
                    imageUrl = "string",
                    buttons = list(
                      list(
                        text = "string",
                        value = "string"
                      )
                    )
                  )
                )
              )
            )
          ),
          maxRetries = 123,
          allowInterrupt = TRUE|FALSE,
          messageSelectionStrategy = "Random"|"Ordered",
          promptAttemptsSpecification = list(
            list(
              allowInterrupt = TRUE|FALSE,
              allowedInputTypes = list(
                allowAudioInput = TRUE|FALSE,
                allowDTMFInput = TRUE|FALSE
              ),
              audioAndDTMFInputSpecification = list(
                startTimeoutMs = 123,
                audioSpecification = list(
                  maxLengthMs = 123,
                  endTimeoutMs = 123
                ),
                dtmfSpecification = list(
                  maxLength = 123,
                  endTimeoutMs = 123,
                  deletionCharacter = "string",
                  endCharacter = "string"
                )
              ),
              textInputSpecification = list(
                startTimeoutMs = 123
              )
            )
          )
        ),
        sampleUtterances = list(
          list(
            utterance = "string"
          )
        ),
        waitAndContinueSpecification = list(
          waitingResponse = list(
            messageGroups = list(
              list(
                message = list(
                  plainTextMessage = list(
                    value = "string"
                  ),
                  customPayload = list(
                    value = "string"
                  ),
                  ssmlMessage = list(
                    value = "string"
                  ),
                  imageResponseCard = list(
                    title = "string",
                    subtitle = "string",
                    imageUrl = "string",
                    buttons = list(
                      list(
                        text = "string",
                        value = "string"
                      )
                    )
                  )
                ),
                variations = list(
                  list(
                    plainTextMessage = list(
                      value = "string"
                    ),
                    customPayload = list(
                      value = "string"
                    ),
                    ssmlMessage = list(
                      value = "string"
                    ),
                    imageResponseCard = list(
                      title = "string",
                      subtitle = "string",
                      imageUrl = "string",
                      buttons = list(
                        list(
                          text = "string",
                          value = "string"
                        )
                      )
                    )
                  )
                )
              )
            ),
            allowInterrupt = TRUE|FALSE
          ),
          continueResponse = list(
            messageGroups = list(
              list(
                message = list(
                  plainTextMessage = list(
                    value = "string"
                  ),
                  customPayload = list(
                    value = "string"
                  ),
                  ssmlMessage = list(
                    value = "string"
                  ),
                  imageResponseCard = list(
                    title = "string",
                    subtitle = "string",
                    imageUrl = "string",
                    buttons = list(
                      list(
                        text = "string",
                        value = "string"
                      )
                    )
                  )
                ),
                variations = list(
                  list(
                    plainTextMessage = list(
                      value = "string"
                    ),
                    customPayload = list(
                      value = "string"
                    ),
                    ssmlMessage = list(
                      value = "string"
                    ),
                    imageResponseCard = list(
                      title = "string",
                      subtitle = "string",
                      imageUrl = "string",
                      buttons = list(
                        list(
                          text = "string",
                          value = "string"
                        )
                      )
                    )
                  )
                )
              )
            ),
            allowInterrupt = TRUE|FALSE
          ),
          stillWaitingResponse = list(
            messageGroups = list(
              list(
                message = list(
                  plainTextMessage = list(
                    value = "string"
                  ),
                  customPayload = list(
                    value = "string"
                  ),
                  ssmlMessage = list(
                    value = "string"
                  ),
                  imageResponseCard = list(
                    title = "string",
                    subtitle = "string",
                    imageUrl = "string",
                    buttons = list(
                      list(
                        text = "string",
                        value = "string"
                      )
                    )
                  )
                ),
                variations = list(
                  list(
                    plainTextMessage = list(
                      value = "string"
                    ),
                    customPayload = list(
                      value = "string"
                    ),
                    ssmlMessage = list(
                      value = "string"
                    ),
                    imageResponseCard = list(
                      title = "string",
                      subtitle = "string",
                      imageUrl = "string",
                      buttons = list(
                        list(
                          text = "string",
                          value = "string"
                        )
                      )
                    )
                  )
                )
              )
            ),
            frequencyInSeconds = 123,
            timeoutInSeconds = 123,
            allowInterrupt = TRUE|FALSE
          ),
          active = TRUE|FALSE
        ),
        slotCaptureSetting = list(
          captureResponse = list(
            messageGroups = list(
              list(
                message = list(
                  plainTextMessage = list(
                    value = "string"
                  ),
                  customPayload = list(
                    value = "string"
                  ),
                  ssmlMessage = list(
                    value = "string"
                  ),
                  imageResponseCard = list(
                    title = "string",
                    subtitle = "string",
                    imageUrl = "string",
                    buttons = list(
                      list(
                        text = "string",
                        value = "string"
                      )
                    )
                  )
                ),
                variations = list(
                  list(
                    plainTextMessage = list(
                      value = "string"
                    ),
                    customPayload = list(
                      value = "string"
                    ),
                    ssmlMessage = list(
                      value = "string"
                    ),
                    imageResponseCard = list(
                      title = "string",
                      subtitle = "string",
                      imageUrl = "string",
                      buttons = list(
                        list(
                          text = "string",
                          value = "string"
                        )
                      )
                    )
                  )
                )
              )
            ),
            allowInterrupt = TRUE|FALSE
          ),
          captureNextStep = list(
            dialogAction = list(
              type = "ElicitIntent"|"StartIntent"|"ElicitSlot"|"EvaluateConditional"|"InvokeDialogCodeHook"|"ConfirmIntent"|"FulfillIntent"|"CloseIntent"|"EndConversation",
              slotToElicit = "string",
              suppressNextMessage = TRUE|FALSE
            ),
            intent = list(
              name = "string",
              slots = list(
                list(
                  shape = "Scalar"|"List",
                  value = list(
                    interpretedValue = "string"
                  ),
                  values = list(
                    list()
                  )
                )
              )
            ),
            sessionAttributes = list(
              "string"
            )
          ),
          captureConditional = list(
            active = TRUE|FALSE,
            conditionalBranches = list(
              list(
                name = "string",
                condition = list(
                  expressionString = "string"
                ),
                nextStep = list(
                  dialogAction = list(
                    type = "ElicitIntent"|"StartIntent"|"ElicitSlot"|"EvaluateConditional"|"InvokeDialogCodeHook"|"ConfirmIntent"|"FulfillIntent"|"CloseIntent"|"EndConversation",
                    slotToElicit = "string",
                    suppressNextMessage = TRUE|FALSE
                  ),
                  intent = list(
                    name = "string",
                    slots = list(
                      list(
                        shape = "Scalar"|"List",
                        value = list(
                          interpretedValue = "string"
                        ),
                        values = list(
                          list()
                        )
                      )
                    )
                  ),
                  sessionAttributes = list(
                    "string"
                  )
                ),
                response = list(
                  messageGroups = list(
                    list(
                      message = list(
                        plainTextMessage = list(
                          value = "string"
                        ),
                        customPayload = list(
                          value = "string"
                        ),
                        ssmlMessage = list(
                          value = "string"
                        ),
                        imageResponseCard = list(
                          title = "string",
                          subtitle = "string",
                          imageUrl = "string",
                          buttons = list(
                            list(
                              text = "string",
                              value = "string"
                            )
                          )
                        )
                      ),
                      variations = list(
                        list(
                          plainTextMessage = list(
                            value = "string"
                          ),
                          customPayload = list(
                            value = "string"
                          ),
                          ssmlMessage = list(
                            value = "string"
                          ),
                          imageResponseCard = list(
                            title = "string",
                            subtitle = "string",
                            imageUrl = "string",
                            buttons = list(
                              list(
                                text = "string",
                                value = "string"
                              )
                            )
                          )
                        )
                      )
                    )
                  ),
                  allowInterrupt = TRUE|FALSE
                )
              )
            ),
            defaultBranch = list(
              nextStep = list(
                dialogAction = list(
                  type = "ElicitIntent"|"StartIntent"|"ElicitSlot"|"EvaluateConditional"|"InvokeDialogCodeHook"|"ConfirmIntent"|"FulfillIntent"|"CloseIntent"|"EndConversation",
                  slotToElicit = "string",
                  suppressNextMessage = TRUE|FALSE
                ),
                intent = list(
                  name = "string",
                  slots = list(
                    list(
                      shape = "Scalar"|"List",
                      value = list(
                        interpretedValue = "string"
                      ),
                      values = list(
                        list()
                      )
                    )
                  )
                ),
                sessionAttributes = list(
                  "string"
                )
              ),
              response = list(
                messageGroups = list(
                  list(
                    message = list(
                      plainTextMessage = list(
                        value = "string"
                      ),
                      customPayload = list(
                        value = "string"
                      ),
                      ssmlMessage = list(
                        value = "string"
                      ),
                      imageResponseCard = list(
                        title = "string",
                        subtitle = "string",
                        imageUrl = "string",
                        buttons = list(
                          list(
                            text = "string",
                            value = "string"
                          )
                        )
                      )
                    ),
                    variations = list(
                      list(
                        plainTextMessage = list(
                          value = "string"
                        ),
                        customPayload = list(
                          value = "string"
                        ),
                        ssmlMessage = list(
                          value = "string"
                        ),
                        imageResponseCard = list(
                          title = "string",
                          subtitle = "string",
                          imageUrl = "string",
                          buttons = list(
                            list(
                              text = "string",
                              value = "string"
                            )
                          )
                        )
                      )
                    )
                  )
                ),
                allowInterrupt = TRUE|FALSE
              )
            )
          ),
          failureResponse = list(
            messageGroups = list(
              list(
                message = list(
                  plainTextMessage = list(
                    value = "string"
                  ),
                  customPayload = list(
                    value = "string"
                  ),
                  ssmlMessage = list(
                    value = "string"
                  ),
                  imageResponseCard = list(
                    title = "string",
                    subtitle = "string",
                    imageUrl = "string",
                    buttons = list(
                      list(
                        text = "string",
                        value = "string"
                      )
                    )
                  )
                ),
                variations = list(
                  list(
                    plainTextMessage = list(
                      value = "string"
                    ),
                    customPayload = list(
                      value = "string"
                    ),
                    ssmlMessage = list(
                      value = "string"
                    ),
                    imageResponseCard = list(
                      title = "string",
                      subtitle = "string",
                      imageUrl = "string",
                      buttons = list(
                        list(
                          text = "string",
                          value = "string"
                        )
                      )
                    )
                  )
                )
              )
            ),
            allowInterrupt = TRUE|FALSE
          ),
          failureNextStep = list(
            dialogAction = list(
              type = "ElicitIntent"|"StartIntent"|"ElicitSlot"|"EvaluateConditional"|"InvokeDialogCodeHook"|"ConfirmIntent"|"FulfillIntent"|"CloseIntent"|"EndConversation",
              slotToElicit = "string",
              suppressNextMessage = TRUE|FALSE
            ),
            intent = list(
              name = "string",
              slots = list(
                list(
                  shape = "Scalar"|"List",
                  value = list(
                    interpretedValue = "string"
                  ),
                  values = list(
                    list()
                  )
                )
              )
            ),
            sessionAttributes = list(
              "string"
            )
          ),
          failureConditional = list(
            active = TRUE|FALSE,
            conditionalBranches = list(
              list(
                name = "string",
                condition = list(
                  expressionString = "string"
                ),
                nextStep = list(
                  dialogAction = list(
                    type = "ElicitIntent"|"StartIntent"|"ElicitSlot"|"EvaluateConditional"|"InvokeDialogCodeHook"|"ConfirmIntent"|"FulfillIntent"|"CloseIntent"|"EndConversation",
                    slotToElicit = "string",
                    suppressNextMessage = TRUE|FALSE
                  ),
                  intent = list(
                    name = "string",
                    slots = list(
                      list(
                        shape = "Scalar"|"List",
                        value = list(
                          interpretedValue = "string"
                        ),
                        values = list(
                          list()
                        )
                      )
                    )
                  ),
                  sessionAttributes = list(
                    "string"
                  )
                ),
                response = list(
                  messageGroups = list(
                    list(
                      message = list(
                        plainTextMessage = list(
                          value = "string"
                        ),
                        customPayload = list(
                          value = "string"
                        ),
                        ssmlMessage = list(
                          value = "string"
                        ),
                        imageResponseCard = list(
                          title = "string",
                          subtitle = "string",
                          imageUrl = "string",
                          buttons = list(
                            list(
                              text = "string",
                              value = "string"
                            )
                          )
                        )
                      ),
                      variations = list(
                        list(
                          plainTextMessage = list(
                            value = "string"
                          ),
                          customPayload = list(
                            value = "string"
                          ),
                          ssmlMessage = list(
                            value = "string"
                          ),
                          imageResponseCard = list(
                            title = "string",
                            subtitle = "string",
                            imageUrl = "string",
                            buttons = list(
                              list(
                                text = "string",
                                value = "string"
                              )
                            )
                          )
                        )
                      )
                    )
                  ),
                  allowInterrupt = TRUE|FALSE
                )
              )
            ),
            defaultBranch = list(
              nextStep = list(
                dialogAction = list(
                  type = "ElicitIntent"|"StartIntent"|"ElicitSlot"|"EvaluateConditional"|"InvokeDialogCodeHook"|"ConfirmIntent"|"FulfillIntent"|"CloseIntent"|"EndConversation",
                  slotToElicit = "string",
                  suppressNextMessage = TRUE|FALSE
                ),
                intent = list(
                  name = "string",
                  slots = list(
                    list(
                      shape = "Scalar"|"List",
                      value = list(
                        interpretedValue = "string"
                      ),
                      values = list(
                        list()
                      )
                    )
                  )
                ),
                sessionAttributes = list(
                  "string"
                )
              ),
              response = list(
                messageGroups = list(
                  list(
                    message = list(
                      plainTextMessage = list(
                        value = "string"
                      ),
                      customPayload = list(
                        value = "string"
                      ),
                      ssmlMessage = list(
                        value = "string"
                      ),
                      imageResponseCard = list(
                        title = "string",
                        subtitle = "string",
                        imageUrl = "string",
                        buttons = list(
                          list(
                            text = "string",
                            value = "string"
                          )
                        )
                      )
                    ),
                    variations = list(
                      list(
                        plainTextMessage = list(
                          value = "string"
                        ),
                        customPayload = list(
                          value = "string"
                        ),
                        ssmlMessage = list(
                          value = "string"
                        ),
                        imageResponseCard = list(
                          title = "string",
                          subtitle = "string",
                          imageUrl = "string",
                          buttons = list(
                            list(
                              text = "string",
                              value = "string"
                            )
                          )
                        )
                      )
                    )
                  )
                ),
                allowInterrupt = TRUE|FALSE
              )
            )
          ),
          codeHook = list(
            enableCodeHookInvocation = TRUE|FALSE,
            active = TRUE|FALSE,
            invocationLabel = "string",
            postCodeHookSpecification = list(
              successResponse = list(
                messageGroups = list(
                  list(
                    message = list(
                      plainTextMessage = list(
                        value = "string"
                      ),
                      customPayload = list(
                        value = "string"
                      ),
                      ssmlMessage = list(
                        value = "string"
                      ),
                      imageResponseCard = list(
                        title = "string",
                        subtitle = "string",
                        imageUrl = "string",
                        buttons = list(
                          list(
                            text = "string",
                            value = "string"
                          )
                        )
                      )
                    ),
                    variations = list(
                      list(
                        plainTextMessage = list(
                          value = "string"
                        ),
                        customPayload = list(
                          value = "string"
                        ),
                        ssmlMessage = list(
                          value = "string"
                        ),
                        imageResponseCard = list(
                          title = "string",
                          subtitle = "string",
                          imageUrl = "string",
                          buttons = list(
                            list(
                              text = "string",
                              value = "string"
                            )
                          )
                        )
                      )
                    )
                  )
                ),
                allowInterrupt = TRUE|FALSE
              ),
              successNextStep = list(
                dialogAction = list(
                  type = "ElicitIntent"|"StartIntent"|"ElicitSlot"|"EvaluateConditional"|"InvokeDialogCodeHook"|"ConfirmIntent"|"FulfillIntent"|"CloseIntent"|"EndConversation",
                  slotToElicit = "string",
                  suppressNextMessage = TRUE|FALSE
                ),
                intent = list(
                  name = "string",
                  slots = list(
                    list(
                      shape = "Scalar"|"List",
                      value = list(
                        interpretedValue = "string"
                      ),
                      values = list(
                        list()
                      )
                    )
                  )
                ),
                sessionAttributes = list(
                  "string"
                )
              ),
              successConditional = list(
                active = TRUE|FALSE,
                conditionalBranches = list(
                  list(
                    name = "string",
                    condition = list(
                      expressionString = "string"
                    ),
                    nextStep = list(
                      dialogAction = list(
                        type = "ElicitIntent"|"StartIntent"|"ElicitSlot"|"EvaluateConditional"|"InvokeDialogCodeHook"|"ConfirmIntent"|"FulfillIntent"|"CloseIntent"|"EndConversation",
                        slotToElicit = "string",
                        suppressNextMessage = TRUE|FALSE
                      ),
                      intent = list(
                        name = "string",
                        slots = list(
                          list(
                            shape = "Scalar"|"List",
                            value = list(
                              interpretedValue = "string"
                            ),
                            values = list(
                              list()
                            )
                          )
                        )
                      ),
                      sessionAttributes = list(
                        "string"
                      )
                    ),
                    response = list(
                      messageGroups = list(
                        list(
                          message = list(
                            plainTextMessage = list(
                              value = "string"
                            ),
                            customPayload = list(
                              value = "string"
                            ),
                            ssmlMessage = list(
                              value = "string"
                            ),
                            imageResponseCard = list(
                              title = "string",
                              subtitle = "string",
                              imageUrl = "string",
                              buttons = list(
                                list(
                                  text = "string",
                                  value = "string"
                                )
                              )
                            )
                          ),
                          variations = list(
                            list(
                              plainTextMessage = list(
                                value = "string"
                              ),
                              customPayload = list(
                                value = "string"
                              ),
                              ssmlMessage = list(
                                value = "string"
                              ),
                              imageResponseCard = list(
                                title = "string",
                                subtitle = "string",
                                imageUrl = "string",
                                buttons = list(
                                  list(
                                    text = "string",
                                    value = "string"
                                  )
                                )
                              )
                            )
                          )
                        )
                      ),
                      allowInterrupt = TRUE|FALSE
                    )
                  )
                ),
                defaultBranch = list(
                  nextStep = list(
                    dialogAction = list(
                      type = "ElicitIntent"|"StartIntent"|"ElicitSlot"|"EvaluateConditional"|"InvokeDialogCodeHook"|"ConfirmIntent"|"FulfillIntent"|"CloseIntent"|"EndConversation",
                      slotToElicit = "string",
                      suppressNextMessage = TRUE|FALSE
                    ),
                    intent = list(
                      name = "string",
                      slots = list(
                        list(
                          shape = "Scalar"|"List",
                          value = list(
                            interpretedValue = "string"
                          ),
                          values = list(
                            list()
                          )
                        )
                      )
                    ),
                    sessionAttributes = list(
                      "string"
                    )
                  ),
                  response = list(
                    messageGroups = list(
                      list(
                        message = list(
                          plainTextMessage = list(
                            value = "string"
                          ),
                          customPayload = list(
                            value = "string"
                          ),
                          ssmlMessage = list(
                            value = "string"
                          ),
                          imageResponseCard = list(
                            title = "string",
                            subtitle = "string",
                            imageUrl = "string",
                            buttons = list(
                              list(
                                text = "string",
                                value = "string"
                              )
                            )
                          )
                        ),
                        variations = list(
                          list(
                            plainTextMessage = list(
                              value = "string"
                            ),
                            customPayload = list(
                              value = "string"
                            ),
                            ssmlMessage = list(
                              value = "string"
                            ),
                            imageResponseCard = list(
                              title = "string",
                              subtitle = "string",
                              imageUrl = "string",
                              buttons = list(
                                list(
                                  text = "string",
                                  value = "string"
                                )
                              )
                            )
                          )
                        )
                      )
                    ),
                    allowInterrupt = TRUE|FALSE
                  )
                )
              ),
              failureResponse = list(
                messageGroups = list(
                  list(
                    message = list(
                      plainTextMessage = list(
                        value = "string"
                      ),
                      customPayload = list(
                        value = "string"
                      ),
                      ssmlMessage = list(
                        value = "string"
                      ),
                      imageResponseCard = list(
                        title = "string",
                        subtitle = "string",
                        imageUrl = "string",
                        buttons = list(
                          list(
                            text = "string",
                            value = "string"
                          )
                        )
                      )
                    ),
                    variations = list(
                      list(
                        plainTextMessage = list(
                          value = "string"
                        ),
                        customPayload = list(
                          value = "string"
                        ),
                        ssmlMessage = list(
                          value = "string"
                        ),
                        imageResponseCard = list(
                          title = "string",
                          subtitle = "string",
                          imageUrl = "string",
                          buttons = list(
                            list(
                              text = "string",
                              value = "string"
                            )
                          )
                        )
                      )
                    )
                  )
                ),
                allowInterrupt = TRUE|FALSE
              ),
              failureNextStep = list(
                dialogAction = list(
                  type = "ElicitIntent"|"StartIntent"|"ElicitSlot"|"EvaluateConditional"|"InvokeDialogCodeHook"|"ConfirmIntent"|"FulfillIntent"|"CloseIntent"|"EndConversation",
                  slotToElicit = "string",
                  suppressNextMessage = TRUE|FALSE
                ),
                intent = list(
                  name = "string",
                  slots = list(
                    list(
                      shape = "Scalar"|"List",
                      value = list(
                        interpretedValue = "string"
                      ),
                      values = list(
                        list()
                      )
                    )
                  )
                ),
                sessionAttributes = list(
                  "string"
                )
              ),
              failureConditional = list(
                active = TRUE|FALSE,
                conditionalBranches = list(
                  list(
                    name = "string",
                    condition = list(
                      expressionString = "string"
                    ),
                    nextStep = list(
                      dialogAction = list(
                        type = "ElicitIntent"|"StartIntent"|"ElicitSlot"|"EvaluateConditional"|"InvokeDialogCodeHook"|"ConfirmIntent"|"FulfillIntent"|"CloseIntent"|"EndConversation",
                        slotToElicit = "string",
                        suppressNextMessage = TRUE|FALSE
                      ),
                      intent = list(
                        name = "string",
                        slots = list(
                          list(
                            shape = "Scalar"|"List",
                            value = list(
                              interpretedValue = "string"
                            ),
                            values = list(
                              list()
                            )
                          )
                        )
                      ),
                      sessionAttributes = list(
                        "string"
                      )
                    ),
                    response = list(
                      messageGroups = list(
                        list(
                          message = list(
                            plainTextMessage = list(
                              value = "string"
                            ),
                            customPayload = list(
                              value = "string"
                            ),
                            ssmlMessage = list(
                              value = "string"
                            ),
                            imageResponseCard = list(
                              title = "string",
                              subtitle = "string",
                              imageUrl = "string",
                              buttons = list(
                                list(
                                  text = "string",
                                  value = "string"
                                )
                              )
                            )
                          ),
                          variations = list(
                            list(
                              plainTextMessage = list(
                                value = "string"
                              ),
                              customPayload = list(
                                value = "string"
                              ),
                              ssmlMessage = list(
                                value = "string"
                              ),
                              imageResponseCard = list(
                                title = "string",
                                subtitle = "string",
                                imageUrl = "string",
                                buttons = list(
                                  list(
                                    text = "string",
                                    value = "string"
                                  )
                                )
                              )
                            )
                          )
                        )
                      ),
                      allowInterrupt = TRUE|FALSE
                    )
                  )
                ),
                defaultBranch = list(
                  nextStep = list(
                    dialogAction = list(
                      type = "ElicitIntent"|"StartIntent"|"ElicitSlot"|"EvaluateConditional"|"InvokeDialogCodeHook"|"ConfirmIntent"|"FulfillIntent"|"CloseIntent"|"EndConversation",
                      slotToElicit = "string",
                      suppressNextMessage = TRUE|FALSE
                    ),
                    intent = list(
                      name = "string",
                      slots = list(
                        list(
                          shape = "Scalar"|"List",
                          value = list(
                            interpretedValue = "string"
                          ),
                          values = list(
                            list()
                          )
                        )
                      )
                    ),
                    sessionAttributes = list(
                      "string"
                    )
                  ),
                  response = list(
                    messageGroups = list(
                      list(
                        message = list(
                          plainTextMessage = list(
                            value = "string"
                          ),
                          customPayload = list(
                            value = "string"
                          ),
                          ssmlMessage = list(
                            value = "string"
                          ),
                          imageResponseCard = list(
                            title = "string",
                            subtitle = "string",
                            imageUrl = "string",
                            buttons = list(
                              list(
                                text = "string",
                                value = "string"
                              )
                            )
                          )
                        ),
                        variations = list(
                          list(
                            plainTextMessage = list(
                              value = "string"
                            ),
                            customPayload = list(
                              value = "string"
                            ),
                            ssmlMessage = list(
                              value = "string"
                            ),
                            imageResponseCard = list(
                              title = "string",
                              subtitle = "string",
                              imageUrl = "string",
                              buttons = list(
                                list(
                                  text = "string",
                                  value = "string"
                                )
                              )
                            )
                          )
                        )
                      )
                    ),
                    allowInterrupt = TRUE|FALSE
                  )
                )
              ),
              timeoutResponse = list(
                messageGroups = list(
                  list(
                    message = list(
                      plainTextMessage = list(
                        value = "string"
                      ),
                      customPayload = list(
                        value = "string"
                      ),
                      ssmlMessage = list(
                        value = "string"
                      ),
                      imageResponseCard = list(
                        title = "string",
                        subtitle = "string",
                        imageUrl = "string",
                        buttons = list(
                          list(
                            text = "string",
                            value = "string"
                          )
                        )
                      )
                    ),
                    variations = list(
                      list(
                        plainTextMessage = list(
                          value = "string"
                        ),
                        customPayload = list(
                          value = "string"
                        ),
                        ssmlMessage = list(
                          value = "string"
                        ),
                        imageResponseCard = list(
                          title = "string",
                          subtitle = "string",
                          imageUrl = "string",
                          buttons = list(
                            list(
                              text = "string",
                              value = "string"
                            )
                          )
                        )
                      )
                    )
                  )
                ),
                allowInterrupt = TRUE|FALSE
              ),
              timeoutNextStep = list(
                dialogAction = list(
                  type = "ElicitIntent"|"StartIntent"|"ElicitSlot"|"EvaluateConditional"|"InvokeDialogCodeHook"|"ConfirmIntent"|"FulfillIntent"|"CloseIntent"|"EndConversation",
                  slotToElicit = "string",
                  suppressNextMessage = TRUE|FALSE
                ),
                intent = list(
                  name = "string",
                  slots = list(
                    list(
                      shape = "Scalar"|"List",
                      value = list(
                        interpretedValue = "string"
                      ),
                      values = list(
                        list()
                      )
                    )
                  )
                ),
                sessionAttributes = list(
                  "string"
                )
              ),
              timeoutConditional = list(
                active = TRUE|FALSE,
                conditionalBranches = list(
                  list(
                    name = "string",
                    condition = list(
                      expressionString = "string"
                    ),
                    nextStep = list(
                      dialogAction = list(
                        type = "ElicitIntent"|"StartIntent"|"ElicitSlot"|"EvaluateConditional"|"InvokeDialogCodeHook"|"ConfirmIntent"|"FulfillIntent"|"CloseIntent"|"EndConversation",
                        slotToElicit = "string",
                        suppressNextMessage = TRUE|FALSE
                      ),
                      intent = list(
                        name = "string",
                        slots = list(
                          list(
                            shape = "Scalar"|"List",
                            value = list(
                              interpretedValue = "string"
                            ),
                            values = list(
                              list()
                            )
                          )
                        )
                      ),
                      sessionAttributes = list(
                        "string"
                      )
                    ),
                    response = list(
                      messageGroups = list(
                        list(
                          message = list(
                            plainTextMessage = list(
                              value = "string"
                            ),
                            customPayload = list(
                              value = "string"
                            ),
                            ssmlMessage = list(
                              value = "string"
                            ),
                            imageResponseCard = list(
                              title = "string",
                              subtitle = "string",
                              imageUrl = "string",
                              buttons = list(
                                list(
                                  text = "string",
                                  value = "string"
                                )
                              )
                            )
                          ),
                          variations = list(
                            list(
                              plainTextMessage = list(
                                value = "string"
                              ),
                              customPayload = list(
                                value = "string"
                              ),
                              ssmlMessage = list(
                                value = "string"
                              ),
                              imageResponseCard = list(
                                title = "string",
                                subtitle = "string",
                                imageUrl = "string",
                                buttons = list(
                                  list(
                                    text = "string",
                                    value = "string"
                                  )
                                )
                              )
                            )
                          )
                        )
                      ),
                      allowInterrupt = TRUE|FALSE
                    )
                  )
                ),
                defaultBranch = list(
                  nextStep = list(
                    dialogAction = list(
                      type = "ElicitIntent"|"StartIntent"|"ElicitSlot"|"EvaluateConditional"|"InvokeDialogCodeHook"|"ConfirmIntent"|"FulfillIntent"|"CloseIntent"|"EndConversation",
                      slotToElicit = "string",
                      suppressNextMessage = TRUE|FALSE
                    ),
                    intent = list(
                      name = "string",
                      slots = list(
                        list(
                          shape = "Scalar"|"List",
                          value = list(
                            interpretedValue = "string"
                          ),
                          values = list(
                            list()
                          )
                        )
                      )
                    ),
                    sessionAttributes = list(
                      "string"
                    )
                  ),
                  response = list(
                    messageGroups = list(
                      list(
                        message = list(
                          plainTextMessage = list(
                            value = "string"
                          ),
                          customPayload = list(
                            value = "string"
                          ),
                          ssmlMessage = list(
                            value = "string"
                          ),
                          imageResponseCard = list(
                            title = "string",
                            subtitle = "string",
                            imageUrl = "string",
                            buttons = list(
                              list(
                                text = "string",
                                value = "string"
                              )
                            )
                          )
                        ),
                        variations = list(
                          list(
                            plainTextMessage = list(
                              value = "string"
                            ),
                            customPayload = list(
                              value = "string"
                            ),
                            ssmlMessage = list(
                              value = "string"
                            ),
                            imageResponseCard = list(
                              title = "string",
                              subtitle = "string",
                              imageUrl = "string",
                              buttons = list(
                                list(
                                  text = "string",
                                  value = "string"
                                )
                              )
                            )
                          )
                        )
                      )
                    ),
                    allowInterrupt = TRUE|FALSE
                  )
                )
              )
            )
          ),
          elicitationCodeHook = list(
            enableCodeHookInvocation = TRUE|FALSE,
            invocationLabel = "string"
          )
        )
      ),
      obfuscationSetting = list(
        obfuscationSettingType = "None"|"DefaultObfuscation"
      ),
      botId = "string",
      botVersion = "string",
      localeId = "string",
      intentId = "string",
      multipleValuesSetting = list(
        allowMultipleValues = TRUE|FALSE
      ),
      subSlotSetting = list(
        expression = "string",
        slotSpecifications = list(
          list(
            slotTypeId = "string",
            valueElicitationSetting = list(
              defaultValueSpecification = list(
                defaultValueList = list(
                  list(
                    defaultValue = "string"
                  )
                )
              ),
              promptSpecification = list(
                messageGroups = list(
                  list(
                    message = list(
                      plainTextMessage = list(
                        value = "string"
                      ),
                      customPayload = list(
                        value = "string"
                      ),
                      ssmlMessage = list(
                        value = "string"
                      ),
                      imageResponseCard = list(
                        title = "string",
                        subtitle = "string",
                        imageUrl = "string",
                        buttons = list(
                          list(
                            text = "string",
                            value = "string"
                          )
                        )
                      )
                    ),
                    variations = list(
                      list(
                        plainTextMessage = list(
                          value = "string"
                        ),
                        customPayload = list(
                          value = "string"
                        ),
                        ssmlMessage = list(
                          value = "string"
                        ),
                        imageResponseCard = list(
                          title = "string",
                          subtitle = "string",
                          imageUrl = "string",
                          buttons = list(
                            list(
                              text = "string",
                              value = "string"
                            )
                          )
                        )
                      )
                    )
                  )
                ),
                maxRetries = 123,
                allowInterrupt = TRUE|FALSE,
                messageSelectionStrategy = "Random"|"Ordered",
                promptAttemptsSpecification = list(
                  list(
                    allowInterrupt = TRUE|FALSE,
                    allowedInputTypes = list(
                      allowAudioInput = TRUE|FALSE,
                      allowDTMFInput = TRUE|FALSE
                    ),
                    audioAndDTMFInputSpecification = list(
                      startTimeoutMs = 123,
                      audioSpecification = list(
                        maxLengthMs = 123,
                        endTimeoutMs = 123
                      ),
                      dtmfSpecification = list(
                        maxLength = 123,
                        endTimeoutMs = 123,
                        deletionCharacter = "string",
                        endCharacter = "string"
                      )
                    ),
                    textInputSpecification = list(
                      startTimeoutMs = 123
                    )
                  )
                )
              ),
              sampleUtterances = list(
                list(
                  utterance = "string"
                )
              ),
              waitAndContinueSpecification = list(
                waitingResponse = list(
                  messageGroups = list(
                    list(
                      message = list(
                        plainTextMessage = list(
                          value = "string"
                        ),
                        customPayload = list(
                          value = "string"
                        ),
                        ssmlMessage = list(
                          value = "string"
                        ),
                        imageResponseCard = list(
                          title = "string",
                          subtitle = "string",
                          imageUrl = "string",
                          buttons = list(
                            list(
                              text = "string",
                              value = "string"
                            )
                          )
                        )
                      ),
                      variations = list(
                        list(
                          plainTextMessage = list(
                            value = "string"
                          ),
                          customPayload = list(
                            value = "string"
                          ),
                          ssmlMessage = list(
                            value = "string"
                          ),
                          imageResponseCard = list(
                            title = "string",
                            subtitle = "string",
                            imageUrl = "string",
                            buttons = list(
                              list(
                                text = "string",
                                value = "string"
                              )
                            )
                          )
                        )
                      )
                    )
                  ),
                  allowInterrupt = TRUE|FALSE
                ),
                continueResponse = list(
                  messageGroups = list(
                    list(
                      message = list(
                        plainTextMessage = list(
                          value = "string"
                        ),
                        customPayload = list(
                          value = "string"
                        ),
                        ssmlMessage = list(
                          value = "string"
                        ),
                        imageResponseCard = list(
                          title = "string",
                          subtitle = "string",
                          imageUrl = "string",
                          buttons = list(
                            list(
                              text = "string",
                              value = "string"
                            )
                          )
                        )
                      ),
                      variations = list(
                        list(
                          plainTextMessage = list(
                            value = "string"
                          ),
                          customPayload = list(
                            value = "string"
                          ),
                          ssmlMessage = list(
                            value = "string"
                          ),
                          imageResponseCard = list(
                            title = "string",
                            subtitle = "string",
                            imageUrl = "string",
                            buttons = list(
                              list(
                                text = "string",
                                value = "string"
                              )
                            )
                          )
                        )
                      )
                    )
                  ),
                  allowInterrupt = TRUE|FALSE
                ),
                stillWaitingResponse = list(
                  messageGroups = list(
                    list(
                      message = list(
                        plainTextMessage = list(
                          value = "string"
                        ),
                        customPayload = list(
                          value = "string"
                        ),
                        ssmlMessage = list(
                          value = "string"
                        ),
                        imageResponseCard = list(
                          title = "string",
                          subtitle = "string",
                          imageUrl = "string",
                          buttons = list(
                            list(
                              text = "string",
                              value = "string"
                            )
                          )
                        )
                      ),
                      variations = list(
                        list(
                          plainTextMessage = list(
                            value = "string"
                          ),
                          customPayload = list(
                            value = "string"
                          ),
                          ssmlMessage = list(
                            value = "string"
                          ),
                          imageResponseCard = list(
                            title = "string",
                            subtitle = "string",
                            imageUrl = "string",
                            buttons = list(
                              list(
                                text = "string",
                                value = "string"
                              )
                            )
                          )
                        )
                      )
                    )
                  ),
                  frequencyInSeconds = 123,
                  timeoutInSeconds = 123,
                  allowInterrupt = TRUE|FALSE
                ),
                active = TRUE|FALSE
              )
            )
          )
        )
      )
    )