# Chronos-Scheduler logic notes

Non-obvious reasoning behind `utils/chronos_utils.R` that doesn't fit as inline comments. Code changes; read this alongside the source, not instead of it.

## `DEFAULT_IGNORED_SUMMARIES` only covers `stay`/`vacation`/`holidays`

These are personal calendar blocks or a subscribed holiday calendar - never invites, so they carry no `ATTENDEE` property. A title-keyword match is the only signal available for them.

`optional` and `coffee together` used to be in this list too. They aren't anymore: both are meeting-style invites, and `hrafnagud`'s own event filtering (`isRelevantEvent`/`isUnaccepted` in `server/facts.ts`) now drops any event where the owner is a listed attendee who hasn't `ACCEPTED` - a real acceptance-state check, stronger than matching a title substring.

## `process_calendar_df` checks the original `STATUS` before overwriting it

Deleting a single occurrence of a recurring series doesn't remove anything from the feed - it adds a separate override `VEVENT` (same UID, a `RECURRENCE-ID` for that instance, `STATUS:CANCELLED`) alongside the still-active master. `ical_parse_df()` returns that override as an ordinary row with the deleted instance's own real date.

The very next step recomputes `status` purely from `start == Sys.Date()`/tomorrow, with no memory of what `STATUS` originally said - so without an explicit check, a cancellation would just get relabeled `TODAY`/`TOMORROW` and shown as if it still existed. `is_cancelled` is computed from the incoming `status` column first, before that column gets reassigned.

## `process_recurring_events` cross-checks against override rows

Fixing the above isn't enough on its own: the RRULE math has no idea a specific date was deleted (or moved) either. Left alone, it will happily recompute "today is a valid occurrence" for a series regardless of what happened to that one instance - reintroducing the same bug through a different path, and for a *moved* (not cancelled) instance, actually duplicating it (once at its real time via the override row, once as a stale phantom at the original slot via the recurrence engine).

`override_instances` collects every `(uid, date)` where a row has no `rrule_freq` (only a master row repeats, so this reliably selects one-off and override rows) and cross-checks it against each series' own computed occurrence. An override always wins - either its real edit shows through `process_calendar_df` normally, or, if cancelled, nothing shows at all.

## `rrule_freq`/`rrule_byday`/etc. depend on a fix in the `ical` fork

RRULE parsing was attempted upstream in 2020 (`ical` package, commit `cc93efd`), then dropped again one commit later (`fbac860`, `ical_parsed_clean$rrule <- NULL`) - the raw `ICAL.Recur` object's shape via the V8/jsonlite bridge is inconsistent (a data frame whose columns vary by which `BY*` parts are used, or a bare `NA` when nothing in the feed recurs), which the package's generic list-flattening can't handle safely.

`DeepanshKhurana/ical@aeec3a0` fixes this by extracting each RRULE sub-field (`freq`, `byday`, `count`, `interval`, `bymonth`, `until`) as its own flat per-event array directly in JS - the same approach already used for the `attendees` field - so `ical_parse_df()` always returns definite scalar columns instead of an ambiguous nested one. If `process_recurring_events` ever stops firing again, check whether Chronos-Scheduler's `renv.lock` has drifted back to a version of the fork without this fix.
