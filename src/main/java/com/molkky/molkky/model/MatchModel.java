package com.molkky.molkky.model;

import com.molkky.molkky.domain.Match;
import com.molkky.molkky.domain.Set;
import com.molkky.molkky.domain.Team;
import lombok.Data;

import java.util.List;

@Data
public class MatchModel {
    private Integer id;
    private Boolean finished;
    private Integer nbSets;
    private Team winner;
    private List<TeamModel> teams;
    private List<Set> sets;

    public MatchModel(Match match) {
        this.id = match.getId();
        this.finished = match.getFinished();
        this.nbSets = match.getNbSets();
        this.winner = match.getWinner();
        this.teams = TeamModel.CreateTeamModels(match.getTeams());
        this.sets = match.getSets();
    }
}
