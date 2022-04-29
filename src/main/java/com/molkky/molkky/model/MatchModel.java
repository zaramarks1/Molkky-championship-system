package com.molkky.molkky.model;

import com.molkky.molkky.domain.Match;
import com.molkky.molkky.domain.Round;
import com.molkky.molkky.domain.Set;
import com.molkky.molkky.domain.Team;
import lombok.Data;

import java.util.ArrayList;
import java.util.List;

@Data
public class MatchModel {
    private Integer id;
    private Boolean finished;
    private Integer nbSets;
    private Team winner;
    private List<TeamModel> teams;
    private List<Set> sets;

    private Round round;

    public MatchModel(Match match) {
        this.id = match.getId();
        this.finished = match.getFinished();
        this.nbSets = match.getNbSets();
        this.winner = match.getWinner();
        this.teams = TeamModel.createTeamModels(match.getTeams());
        this.sets = match.getSets();
        this.round = match.getRound();
    }

    public static List<MatchModel> createMatchModels(List<Match> matches) {
        List<MatchModel> matchModels = new ArrayList<>();
        if (matches != null) {
            for (Match match : matches) {
                matchModels.add(new MatchModel(match));
            }
        }
        return matchModels;
    }
}
