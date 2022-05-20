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
@DiscriminatorValue("KNOCKOUT")
public class Knockout extends Phase {

    @Column(name = "nbMatch")
    private Integer nbMatch;

    /*quarter-finals height of finals etc*/
    @Column(name = "teamsRemaining")
    private Integer teamsRemaining;

    @Column(name = "isRandomDraw")
    private boolean randomDraw;

    @Column(name = "isNotifEveryRound")
    private boolean notifEveryRound;

    public Knockout(PhaseModel knockoutModel, Tournament tournament) throws ParseException {
        this.setStatus(PhaseStatus.NOTSTARTED);
        this.setNbSets(knockoutModel.getNbSets());
        this.setRanking(knockoutModel.getRanking());
        this.setTopSeeds(knockoutModel.getTopSeeds());
        this.setRandomDraw(knockoutModel.isRandomDraw());
        this.setTerrainAffectation(knockoutModel.getTerrainAffectation());
        this.setNbCourts(knockoutModel.getNbCourts());
        this.setNumStartCourt(knockoutModel.getNumStartCourt());
        this.setManagePlanning(knockoutModel.isManagePlanning());
        this.setHourPhaseStart(knockoutModel.getHourPhaseStart());
        this.setTimePhase(knockoutModel.getTimePhase());
        this.setScoreMode(knockoutModel.getScoreMode());
        this.setNotifEveryRound(knockoutModel.isNotifEveryRound());
        this.setTournament(tournament);
    }

    public Knockout() {
    }


}
