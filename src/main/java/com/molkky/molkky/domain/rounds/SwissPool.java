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
@DiscriminatorValue("SWISSPOOL")
public class SwissPool extends Phase {
    @Column(name = "nbSubRounds")
    private Integer nbSubRounds;

    public SwissPool(){

    }

    public SwissPool(Integer nbSubRounds, Integer nbTeamsQualified){
        this.nbSubRounds = nbSubRounds;
        this.setNbTeamsQualified(nbTeamsQualified);
    }

    public SwissPool(PhaseModel swissModel, Tournament tournament) throws ParseException {
        this.setStatus(PhaseStatus.NOTSTARTED);
        this.setNbSubRounds(swissModel.getNbSubRounds());
        this.setNbSets(swissModel.getNbSets());
        this.setVictoryValue(swissModel.getVictoryValue());
        this.setTerrainAffectation(swissModel.getTerrainAffectation());
        this.setNbCourts(swissModel.getNbCourts());
        this.setNumStartCourt(swissModel.getNumStartCourt());
        this.setManagePlanning(swissModel.isManagePlanning());
        this.setHourPhaseStart(swissModel.getHourPhaseStart());
        this.setTimePhase(swissModel.getTimePhase());
        this.setScoreMode(swissModel.getScoreMode());
        this.setNotifBeginningPhase(swissModel.isNotifBeginningPhase());
        this.setNbTeamsQualified(swissModel.getNbTeamsQualified());
        this.setPlayoff(swissModel.isPlayoff());
        this.setNumberPlayoffQualify(swissModel.getNumberPlayoffQualify());
        this.setTournament(tournament);
    }
}
