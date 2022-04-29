package com.molkky.molkky.model;

import com.molkky.molkky.domain.Match;
import com.molkky.molkky.domain.Round;
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
    private Round round;
    public MatchModel(Match match) {
        this.id = match.getId();
        this.finished = match.getFinished();
        this.nbSets = match.getNbSets();
        this.winner = match.getWinner();
        this.round = match.getRound();
    }

    public static List<MatchModel> createMatchModels(List<Match> matches) {
        List<MatchModel> matchModels = new ArrayList<>();
        if (matches != null) {
            for (Match match : matches) {
                if(match.getId() != null) {
                    matchModels.add(new MatchModel(match));
                }
            }
        }
        return matchModels;
    }
}
