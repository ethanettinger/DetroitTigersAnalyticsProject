library(dplyr)
library(ggplot2)
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

#----------FUNCTIONS----------
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

#Arsenal Table For Each Pitcher
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

#----------GRAPHS----------
pitch_colors <- c(
  FF = "#D22D49",  
  SI = "#FE9D00",  
  FC = "#933F2C", 
  SL = "#EEE716",  
  CU = "#00D1ED",  
  KC = "#6236CD",  
  CH = "#1DBE3A"   
)

pitches <- pitches %>%
  mutate(Outcome = case_when(
    IsInPlay ~ "Hit Into Play",
    IsStrike ~ "Strike",
    TRUE     ~ "Ball"
  ))

#Movement Chart
plot_movement <- function(data, pitcher) {
  data %>%
    filter(PitcherId == pitcher, !is.na(PitchType)) %>%
    ggplot(aes(x = HorizBreakIn, y = IVBIn, color = PitchType)) +
    geom_hline(yintercept = 0, color = "grey70") +
    geom_vline(xintercept = 0, color = "grey70") +
    geom_point(size = 3, alpha = 0.8) +
    scale_color_manual(values = pitch_colors) +
    coord_fixed(xlim = c(-25, 25), ylim = c(-25, 25)) +
    labs(title = paste("Pitch Movement: Pitcher", pitcher),
         x = "Horizontal Break (in)", y = "Induced Vertical Break (in)",
         color = "Pitch") +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))
}

plot_movement(pitches, 1)

outcome_colors <- c(
  "Strike"        = "#1F77B4",
  "Ball"          = "grey60",
  "Hit Into Play" = "#D62728"
)

#Location Plot
plot_location <- function(data, pitcher) {
  d <- filter(data, PitcherId == pitcher, !is.na(TrajectoryLocationX))
  zone_bottom <- mean(d$StrikeZoneBottom)
  zone_top    <- mean(d$StrikeZoneTop)
  
  ggplot(d, aes(x = TrajectoryLocationX, y = TrajectoryLocationZ)) +
    annotate("rect", xmin = -0.83, xmax = 0.83, ymin = zone_bottom, ymax = zone_top,
             fill = NA, color = "black", linewidth = 1) +
    geom_point(aes(color = Outcome, shape = PitchType), size = 3, alpha = 0.85) +
    scale_color_manual(values = outcome_colors) +
    coord_fixed(xlim = c(-2.5, 2.5), ylim = c(0, 5)) +
    facet_wrap(~ BatterSide, labeller = labeller(BatterSide = c(L = "vs. LHB", R = "vs. RHB"))) +
    labs(title = paste("Pitch Locations: Pitcher", pitcher),
         x = "Horizontal Location (ft)", y = "Height (ft)",
         color = "Result", shape = "Pitch") +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
}

plot_location(pitches, 1)
