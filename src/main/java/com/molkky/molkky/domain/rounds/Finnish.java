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
@DiscriminatorValue("FINNISH")
public class Finnish extends Phase {

    @Column(name = "nbFinnish")
    private Integer nbFinnish;

    public Finnish(PhaseModel finnishModel, Tournament tournament) throws ParseException {
        this.setStatus(PhaseStatus.NOTSTARTED);
        this.setNbFinnish(finnishModel.getNbFinnish());
        this.setRanking(finnishModel.getRanking());
        this.setTopSeeds(finnishModel.getTopSeeds());
        this.setTerrainAffectation(finnishModel.getTerrainAffectation());
        this.setNbCourts(finnishModel.getNbCourts());
        this.setNumStartCourt(finnishModel.getNumStartCourt());
        this.setManagePlanning(finnishModel.isManagePlanning());
        this.setHourPhaseStart(finnishModel.getHourPhaseStart());
        this.setTimePhase(finnishModel.getTimePhase());
        this.setScoreMode(finnishModel.getScoreMode());
        this.setNotifBeginningPhase(finnishModel.isNotifBeginningPhase());
        this.setNbTeamsQualified(finnishModel.getNbTeamsQualified());
        this.setPlayoff(finnishModel.isPlayoff());
        this.setNumberPlayoffQualify(finnishModel.getNumberPlayoffQualify());
        this.setConsolation(finnishModel.isConsolation());
        this.setNumberConsolationQualify(finnishModel.getNumberConsolationQualify());
        this.setTournament(tournament);
    }

    public Finnish() {
    }
}
