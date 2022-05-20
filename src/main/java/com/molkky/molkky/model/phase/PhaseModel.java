package com.molkky.molkky.model.phase;

import com.molkky.molkky.domain.Round;
import lombok.Data;
import type.PhaseStatus;
import type.PhaseType;
import type.ScoreMode;

import java.util.List;

@Data
public class PhaseModel {
    private PhaseType phaseType;
    private PhaseStatus status;
    private Integer nbPhase;
    private Boolean terrainAffectation;
    private Integer nbCourts;
    private Integer numStartCourt;
    private boolean managePlanning;
    private String hourPhaseStart;
    private String timePhase;
    private ScoreMode scoreMode;
    private Integer nbSets;
    private Boolean seedingSystem;
    private Boolean topSeeds;
    private Boolean ranking;
    private boolean notifBeginningPhase;
    private Integer nbTeamsQualified;
    private boolean consolation;
    private Integer numberConsolationQualify;
    private boolean playoff;
    private Integer numberPlayoffQualify;
    private Integer victoryValue;
    private Integer nbFinnish;
    private boolean randomDraw;
    private boolean notifEveryRound;
    private Integer nbPools;
    private boolean notifEachDay;
    private boolean playTeamSameClub;
    private Integer nbSubRounds;
    private Integer tournament;
    private List<Round> rounds;
}
