as.event_ls <- function(x) UseMethod("as.event_ls", x)

as.event_ls.data.frame <- function(x) {

  out <- tibble::tibble(
    etag = x$etag,
    id = x$id,
    status = x$status %||% NA_character_,
    htmlLink = x$htmlLink,
    created = x$created, # RFC3339 Timestamp
    updated = x$updated, # RFC3339 Timestamp
    summary = x$summary,
    description = x$description %||% NA_character_,
    location = x$location %||% NA_character_,
    colorId = x$colorId %||% NA_character_,
    creator.id = x$creator.id %||% NA_character_,
    creator.email = x$creator.email %||% NA_character_,
    creator.displayName = x$creator.displayName %||% NA_character_,
    creator.self = (x$creator.self %||% FALSE) %|% FALSE,
    organizer.id = x$organizer.id %||% NA_character_,
    organizer.email = x$organizer.email %||% NA_character_,
    organizer.displayName = x$organizer.displayName %||% NA_character_,
    organizer.self = (x$organizer.self %||% FALSE) %|% FALSE,
    start.date = x$start.date %||% NA_character_,
    start.dateTime = x$start.dateTime,
    start.timeZone = x$start.timeZone %||% NA_character_,
    end.date = x$end.date %||% NA_character_,
    end.dateTime = x$end.dateTime,
    end.timeZone = x$end.timeZone %||% NA_character_,
    endTimeUnspecified = (x$endTimeUnspecified %||% FALSE) %|% FALSE,
    recurringEventId = x$recurringEventId %||% NA_character_,
    originalStartTime.date =
      x$originalStartTime.date %||% NA_character_,
    originalStartTime.dateTime =
      x$originalStartTime.dateTime %||% NA_character_,
    originalStartTime.timeZone =
      x$originalStartTime.timeZone %||% NA_character_,
    transparency = (x$transparency %||% "opaque") %|% "opaque",
    visibility = (x$visibility %||% "default") %|% "default",
    iCalUID = x$iCalUID,
    sequence = x$sequence,
    attendeesOmitted = (x$attendeesOmitted %||% FALSE) %|% FALSE,
    hangoutLink = x$hangoutLink %||% NA_character_,
    anyoneCanAddSelf = (x$anyoneCanAddSelf %||% FALSE) %|% FALSE,
    guestsCanInviteOthers = (x$guestsCanInviteOthers %||% TRUE) %|% TRUE,
    guestsCanModify = (x$guestsCanModify %||% FALSE) %|% FALSE,
    guestsCanSeeOtherGuests =
      (x$guestsCanSeeOtherGuests %||% TRUE) %|% TRUE,
    privateCopy = (x$privateCopy %||% FALSE) %|% FALSE,
    locked =( x$locked %||% FALSE) %|% FALSE,
    reminders.useDefault = (x$reminders.useDefault %||% TRUE) %|% TRUE,
    source.url = x$source.url %||% NA_character_,
    source.title = x$source.title %||% NA_character_
  )
  structure(out, class = c("event_ls", class(out)))

}

