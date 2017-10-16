x <- list(
  Metadata = list(
    ApplicationId = '00000000-0000-0000-0000-000000000000',
    ApplicationInstanceId = '00000000-0000-0000-0000-000000000000'
  ),
  LoggingDocument = list(
    Message = '',
    Detail = '',
    LoggingLevel = 100,
    Archive = FALSE,
    ShowUser = FALSE,
    DocumentId = '00000000-0000-0000-0000-000000000000',
    Source = '',
    FileName = '',
    LineNumber = 0,
    EntryTimestampUtc = lubridate::ymd('0001-01-01', tz = 'UTC'),
    CorrelationId = '00000000-0000-0000-0000-000000000000',
    OperationId = '00000000-0000-0000-0000-000000000000',
    CallerMemberName = '',
    AdvancedDataItems = ''
  )
)
x <- jsonlite::toJSON(x, auto_unbox = TRUE, null = 'null', na = 'null',
                      POSIXt = 'ISO8601', pretty = TRUE)
writeLines(x, file.path('inst', 'templates', 'TraceMessage.json'))





x <- list(
  Metadata = list(
    ApplicationId = '00000000-0000-0000-0000-000000000000',
    ApplicationInstanceId = '00000000-0000-0000-0000-000000000000'
  ),
  LoggingDocument = list(
    Message = '',
    Detail = '',
    LoggingLevel = 200,
    Archive = FALSE,
    ShowUser = FALSE,
    DocumentId = '00000000-0000-0000-0000-000000000000',
    Source = NA_character_,
    FileName = NA_character_,
    LineNumber = 0,
    EntryTimestampUtc = lubridate::ymd('0001-01-01', tz = 'UTC'),
    CorrelationId = '00000000-0000-0000-0000-000000000000',
    OperationId = '00000000-0000-0000-0000-000000000000',
    CallerMemberName = '',
    AdvancedDataItems = ''
  )
)
x <- jsonlite::toJSON(x, auto_unbox = TRUE, null = 'null', na = 'null',
                      POSIXt = 'ISO8601', pretty = TRUE)
writeLines(x, file.path('inst', 'templates', 'DebugMessage.json'))






x <- list(
  Metadata = list(
    ApplicationId = '00000000-0000-0000-0000-000000000000',
    ApplicationInstanceId = '00000000-0000-0000-0000-000000000000'
  ),
  LoggingDocument = list(
    ErrorMessage = '',
    ErrorDetail = '',
    Stacktrace = '',
    ObjectReference = '',
    LoggingLevel = 300,
    Archive = FALSE,
    ShowUser = FALSE,
    DocumentId = '00000000-0000-0000-0000-000000000000',
    Source = '',
    FileName = '',
    LineNumber = 0,
    EntryTimestampUtc = lubridate::ymd('0001-01-01', tz = 'UTC'),
    CorrelationId = '00000000-0000-0000-0000-000000000000',
    OperationId = '00000000-0000-0000-0000-000000000000',
    CallerMemberName = '',
    AdvancedDataItems = ''
  )
)
x <- jsonlite::toJSON(x, auto_unbox = TRUE, null = 'null', na = 'null',
                      POSIXt = 'ISO8601', pretty = TRUE)
writeLines(x, file.path('inst', 'templates', 'ErrorMessage.json'))








x <- list(
  Metadata = list(
    ApplicationId = '00000000-0000-0000-0000-000000000000',
    ApplicationInstanceId = '00000000-0000-0000-0000-000000000000'
  ),
  LoggingDocument = list(
    ClosedApplication = FALSE,
    ErrorMessage = '',
    ErrorDetail = '',
    Stacktrace = '',
    ObjectReference = '',
    LoggingLevel = 400,
    Archive = FALSE,
    ShowUser = FALSE,
    DocumentId = '00000000-0000-0000-0000-000000000000',
    Source = '',
    FileName = '',
    LineNumber = 0,
    EntryTimestampUtc = lubridate::ymd('0001-01-01', tz = 'UTC'),
    CorrelationId = '00000000-0000-0000-0000-000000000000',
    OperationId = '00000000-0000-0000-0000-000000000000',
    CallerMemberName = '',
    AdvancedDataItems = ''
  )
)
x <- jsonlite::toJSON(x, auto_unbox = TRUE, null = 'null', na = 'null',
                      POSIXt = 'ISO8601', pretty = TRUE)
writeLines(x, file.path('inst', 'templates', 'FatalMessage.json'))
