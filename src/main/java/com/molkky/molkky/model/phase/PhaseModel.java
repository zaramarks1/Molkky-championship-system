package com.molkky.molkky.model.phase;

import com.molkky.molkky.domain.Phase;
import com.molkky.molkky.domain.Round;
import com.molkky.molkky.domain.rounds.Finnish;
import com.molkky.molkky.domain.rounds.Knockout;
import com.molkky.molkky.domain.rounds.Pool;
import com.molkky.molkky.domain.rounds.SwissPool;
import lombok.Data;
import lombok.NoArgsConstructor;
import type.PhaseStatus;
import type.PhaseType;
import type.ScoreMode;

import java.util.List;

@Data
@NoArgsConstructor
public class PhaseModel {
    private Integer id;
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

    public PhaseModel(Phase phase){
        this.id = phase.getId();
        this.phaseType = phase.getDecriminatorValue();
        this.status = phase.getStatus();
        this.nbPhase = phase.getNbPhase();
        this.terrainAffectation = phase.getTerrainAffectation();
        this.nbCourts = phase.getNbCourts();
        this.numStartCourt = phase.getNumStartCourt();
        this.managePlanning = phase.isManagePlanning();
        this.hourPhaseStart = null;
        this.timePhase = null;
        this.scoreMode = phase.getScoreMode();
        this.nbSets = phase.getNbSets();
        this.seedingSystem = phase.getSeedingSystem();
        this.topSeeds = phase.getTopSeeds();
        this.ranking = phase.getRanking();
        this.notifBeginningPhase = phase.isNotifBeginningPhase();
        this.nbTeamsQualified = phase.getNbTeamsQualified();
        this.consolation = phase.isConsolation();
        this.numberConsolationQualify = phase.getNumberConsolationQualify();
        this.playoff = phase.isPlayoff();
        this.numberPlayoffQualify = phase.getNumberPlayoffQualify();
        this.victoryValue = phase.getVictoryValue();
        switch (phase.getDecriminatorValue()){
            case FINNISH:
                Finnish finnish = (Finnish) phase;
                this.nbFinnish = finnish.getNbFinnish();
                break;
            case KNOCKOUT:
                Knockout knockout = (Knockout) phase;
                this.randomDraw = knockout.isRandomDraw();
                this.notifEveryRound = knockout.isNotifEveryRound();
                break;
            case POOL:
                Pool pool = (Pool) phase;
                this.nbPools = pool.getNbPools();
                this.notifEachDay = pool.isNotifEachDay();
                this.playTeamSameClub = pool.isPlayTeamSameClub();
                break;
            case SWISSPOOL:
                SwissPool swissPool = (SwissPool) phase;
                this.nbSubRounds = swissPool.getNbSubRounds();
                break;
            default:
                break;
        }
        this.tournament = phase.getTournament().getId();
    }
}
