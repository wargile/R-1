# Roland MC-4/MC-8 logic
# ----------------------

# Main functions:
# - DAC output
# - MIDI input
# - ADC input
# - EEPROM storage of songs and patterns and setup/properties
# - Song/Pattern storage, structure and navigation
# - Note/CV/GateTime/Reference storage and handling

# Pattern properties:
# - Per note:
#   - Note length (1-32?)
#   - Note pitch (1-12) * Transpose(1-8?)
#   - Gate length (global or per note?)
# - Global:
#   - Name/ID

# Song properties:
# - Per song:
#   - Number of repeats or endless repeat
#   - Patterns
#     - ID for pattern
#     - Repeat for pattern
#     - Next pattern in song

set.seed(16071962)
SetStandardOptions()

max.patterns <- 48 # TODO: Use longint vars to store 16 0/1 (free/taken) slots in each longint
max.songs <- 16 # TODO: Use longint vars to store 16 0/1 (free/taken) slots in each longint
pattern.npos <- 1 # Active/current pattern pos in song 
note.pos <- 1 # Active/current note pos in pattern
run.status <- 1 # TODO: Const for stopped/running/stepping statuses
patterns <- list()
songs <- list()

pattern <- list("MyPattern", 1, 8, integer(0), integer(0), integer(0), integer(0)) # Initial structure of a pattern
names(pattern) <- c("Name","Id","Length","NoteLength","NotePitch","GateLength","Transpose")
pattern$Length <- 8
pattern$NoteLength <- rep(4, pattern$Length)
pattern$NotePitch <- rep(1, pattern$Length)
pattern$GateLength <- rep(1, pattern$Length)
pattern$Transpose <- 1 # No transpose as initial value
pattern

song <- list("MySong", 1, 8, integer(0), integer(0), T) # Initial structure of a song
names(song) <- c("Name","Id","Length","PatternId","PatternRepeatLength","DoSongRepeat")
song$PatternId <- rep(0, song$Length)
song$PatternRepeatLength <- rep(1, song$Length)
song

# Do a wwe test with a couple of patterns in a song...
NicePattern1 <- pattern
NicePattern1$Id <- 10
NicePattern1$Name <- "My nice pattern #10"
NicePattern1$NotePitch <- sample(12, 12, rep=T)
#if (length(patterns) >= NicePattern1$Id) {
#  if (is.null(patterns[[NicePattern1$Id]])) {
    patterns[[NicePattern1$Id]] <- NicePattern1 # Store the pattern in the pattern list. Slot 1 is now not available for NEW patterns
#  }
#} else {
#  patterns[[NicePattern1$Id]] <- NicePattern1
#}

NicePattern2 <- pattern
NicePattern2$Id <- 5
NicePattern2$Name <- "My nice pattern #5"
NicePattern2$NotePitch <- sample(12, 12, rep=T)
#if (length(patterns) >= NicePattern2$Id) {
#  if (is.null(patterns[[NicePattern2$Id]])) {
    patterns[[NicePattern2$Id]] <- NicePattern2 # Store the pattern in the pattern list. Slot 1 is now not available for NEW patterns
#  }
#} else {
#  patterns[[NicePattern2$Id]] <- NicePattern2
#}

NiceSong1 <- song
NiceSong1$Id <- 10
NiceSong1$Name <- "My nice song #10"
# TODO: Store patternId/patternLength as pairs to save space?
NiceSong1$PatternId <- c(rep(NicePattern1$Id, 4), rep(NicePattern2$Id, 6))
#if (length(songs) >= NiceSong1$Id) {
#  if (is.null(songs[[NiceSong1$Id]])) {
    songs[[NiceSong1$Id]] <- NiceSong1
#  }
#} else {
#  songs[[NiceSong1$Id]] <- NiceSong1 # No songs added yet
#}

notes <- PlaySong(10)
notes
plot(notes, type="o", pch=21, main="Notes in song", bg=notes, cex=1.5)
grid()

PlaySong <- function(songId) {
  notes <- integer(0)
  pos <- 1
  
  for (patternCounter in 1:length(songs[[songId]]$PatternId)) { # Or: check patternId/patternLength pairs?
    for (notesCounter in 1:patterns[[ songs[[songId]]$PatternId[patternCounter] ]]$Length) {
      notes[pos] <- patterns[[ songs[[songId]]$PatternId[patternCounter] ]]$NotePitch[notesCounter]
      pos <- pos + 1
    }
  }
  
  return (notes)    
}
