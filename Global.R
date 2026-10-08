library(dplyr)

#----------READ IN DATA----------
pitches <- read.csv("AnalyticsQuestionnairePitchData.csv")

#----------FIX DATA----------
#Remove Pickoff Attempts (they have NULL data)
pitches <- pitches %>%
  filter(PitchCall != "pickoff_attempt_1b")

#Fix Inning 0 Error
pitches <- pitches %>%
  mutate(Inning = ifelse(Inning == 0, 1, Inning))

#Convert the Horizontal and Vertical Break to Inches
pitches <- pitches %>%
  mutate(HorizBreakIn = TrajectoryHorizontalBreak * 12, IVBIn = TrajectoryVerticalBreakInduced *
           12)

#Create True/False Categories to make categorizing pitches easier later on
in_play_calls <- c("field_out", "force_out", "grounded_into_double_play", "sac_bunt",
                   "field_error", "single", "double", "triple", "home_run")
hit_calls <- c("single", "double", "triple", "home_run")
whiff_calls <- c("swinging_strike", "swinging_strike_blocked", "foul_tip")
swing_calls   <- c(whiff_calls, "foul", "foul_bunt", in_play_calls)
strike_calls <- c("strikeout", "called_strike", swing_calls)


pitches <- pitches %>%
  mutate(
    IsStrike = PitchCall %in% strike_calls,
    IsSwing = PitchCall %in% swing_calls,
    IsWhiff = PitchCall %in% whiff_calls,
    IsCSW = PitchCall %in% c(whiff_calls, "called_strike"),
    IsInPlay  = PitchCall %in% in_play_calls,
    IsHit = PitchCall %in% hit_calls,
    IsK = PitchCall == "strikeout",
    IsBB = PitchCall == "walk",
    IsHR = PitchCall == "home_run",
    IsPAEnd = PitchCall %in% c(in_play_calls, "strikeout", "walk"),
    IsFirstPitch = Balls == 0 & Strikes == 0,
    InZone  = abs(TrajectoryLocationX) <= 0.83 &
      TrajectoryLocationZ >= StrikeZoneBottom &
      TrajectoryLocationZ <= StrikeZoneTop
  )

colSums(pitches[, c("IsStrike", "IsSwing", "IsWhiff", "IsInPlay", "IsPAEnd", "InZone")])

#Summary Table For All Pitchers
summarize_pitchers <- function(data) {
  data %>%
    group_by(PitcherId, PitcherHand) %>%
    summarise(
      Pitches = n(),
      BF = sum(IsPAEnd),
      K = sum(IsK),
      BB = sum(IsBB),
      H = sum(IsHit),
      HR = sum(IsHR),
      `Strike%` = round(100 * mean(IsStrike), 1),
      `1st Pitch Strike%` = round(100 * mean(IsStrike[IsFirstPitch]), 1),
      `Zone%` = round(100 * mean(InZone, na.rm = TRUE), 1),
      `Whiff%` = round(100 * sum(IsWhiff) / sum(IsSwing), 1),
      `CSW%` = round(100 * mean(IsCSW), 1),
      `FB Velo` = round(mean(ReleaseSpeed[PitchType %in% c("FF", "SI")], na.rm = TRUE), 1),
      .groups = "drop"
    ) %>%
    arrange(desc(Pitches))
}

pitcher_summary <- summarize_pitchers(pitches)
print(pitcher_summary, width = Inf)

summarize_arsenal <- function(data) {
  data %>%
    filter(!is.na(PitchType)) %>%
    group_by(PitcherId) %>%
    mutate(TotalPitches = n()) %>%
    group_by(PitcherId, PitchType) %>%
    summarise(
      Pitches = n(),
      `Usage%` = round(100 * n() / first(TotalPitches), 1),
      Velo  = round(mean(ReleaseSpeed, na.rm = TRUE), 1),
      'Spin (rpm)' = round(mean(ReleaseSpinRate, na.rm = TRUE)),
      'IVB (in)' = round(mean(IVBIn, na.rm = TRUE), 1),
      'HB (in)'   = round(mean(HorizBreakIn, na.rm = TRUE), 1),
      'Ext (ft)' = round(mean(ReleaseExtension, na.rm = TRUE), 1),
      'VAA' = round(mean(TrajectoryVerticalApproachAngle, na.rm = TRUE), 3),
      'HAA' = round(mean(TrajectoryHorizontalApproachAngle, na.rm=TRUE), 3),
      'Strike%' = round(100 * mean(IsStrike), 1),
      'Whiff%' = round(100 * sum(IsWhiff) / sum(IsSwing), 1),
      .groups = "drop"
    ) %>%
    arrange(PitcherId, desc(Pitches))
}

arsenal <- summarize_arsenal(pitches)
print(filter(arsenal, PitcherId == 1), width = Inf)
