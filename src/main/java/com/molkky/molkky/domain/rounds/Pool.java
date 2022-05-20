package com.molkky.molkky.domain.rounds;

import com.molkky.molkky.domain.Phase;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.model.phase.PhaseModel;
import lombok.Data;
import type.PhaseStatus;

import javax.persistence.Column;
import javax.persistence.DiscriminatorValue;
import javax.persistence.Entity;
import java.text.ParseException;

@Data
@Entity
@DiscriminatorValue("POOL")
public class Pool extends Phase {

    @Column(name = "notifEachDay")
    private boolean notifEachDay;

    @Column(name = "nbPools")
    private Integer nbPools;

    @Column(name = "isPlayTeamSameClub")
    private boolean playTeamSameClub;

    public Pool(){
        this.setNotifBeginningPhase(false);
        this.notifEachDay = false;

    }

    public Pool(PhaseModel poolModel, Tournament tournament) throws ParseException {
        this.setStatus(PhaseStatus.NOTSTARTED);
        this.setNbPools(poolModel.getNbPools());
        this.setNbSets(poolModel.getNbSets());
        this.setVictoryValue(poolModel.getVictoryValue());
        this.setRanking(poolModel.getRanking());
        this.setTopSeeds(poolModel.getTopSeeds());
        this.setPlayTeamSameClub(poolModel.isPlayTeamSameClub());
        this.setTerrainAffectation(poolModel.getTerrainAffectation());
        this.setNbCourts(poolModel.getNbCourts());
        this.setNumStartCourt(poolModel.getNumStartCourt());
        this.setManagePlanning(poolModel.isManagePlanning());
        this.setHourPhaseStart(poolModel.getHourPhaseStart());
        this.setTimePhase(poolModel.getTimePhase());
        this.setScoreMode(poolModel.getScoreMode());
        this.setNotifBeginningPhase(poolModel.isNotifBeginningPhase());
        this.setNotifEachDay(poolModel.isNotifEachDay());
        this.setNbTeamsQualified(poolModel.getNbTeamsQualified());
        this.setPlayoff(poolModel.isPlayoff());
        this.setNumberPlayoffQualify(poolModel.getNumberPlayoffQualify());
        this.setConsolation(poolModel.isConsolation());
        this.setNumberConsolationQualify(poolModel.getNumberConsolationQualify());
        this.setTournament(tournament);
    }
}
